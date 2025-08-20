use bridge_adapters::add_builtin;
use compile_state::state::SloshVm;
use slvm::{VMError, VMResult, Value};
use std::collections::HashMap;
use std::sync::Mutex;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use nix::unistd::pipe;

use slosh_fuse::FuseMount;

// Registry for active mounts - only created when first mount is created
use std::sync::OnceLock;
static MOUNT_REGISTRY: OnceLock<Mutex<HashMap<String, FuseMount>>> = OnceLock::new();

fn get_registry() -> &'static Mutex<HashMap<String, FuseMount>> {
    MOUNT_REGISTRY.get_or_init(|| Mutex::new(HashMap::new()))
}

fn mount_eval_fs(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_vm(
            "mount-eval-fs: requires one argument (mount-path)".to_string(),
        ));
    }

    let mount_path = registers[0].get_string(vm)?;
    let mount_point = PathBuf::from(mount_path);

    // Create mount point if it doesn't exist
    if !mount_point.exists() {
        std::fs::create_dir_all(&mount_point)
            .map_err(|e| VMError::new_vm(format!("Failed to create mount point: {}", e)))?;
    }

    // Create communication pipes
    let (_comm_read, comm_write) = pipe()
        .map_err(|e| VMError::new_vm(format!("Failed to create communication pipe: {}", e)))?;
    let (eval_read, eval_write) = pipe()
        .map_err(|e| VMError::new_vm(format!("Failed to create eval pipe: {}", e)))?;

    let mount_id = format!("mount-{}", uuid::Uuid::new_v4());
    let mut mount = FuseMount::new(mount_point.clone());

    // Build the server binary name
    let server_name = "slosh-fuse-server";

    // Try multiple possible locations for the server binary
    let possible_paths = vec![
        // Same directory as current executable
        std::env::current_exe()
            .ok()
            .and_then(|p| p.parent().map(|p| p.join(server_name))),
        // In target/debug or target/release
        std::env::current_dir()
            .ok()
            .map(|p| p.join("target").join(if cfg!(debug_assertions) { "debug" } else { "release" }).join(server_name)),
        // System PATH
        Some(PathBuf::from(server_name)),
    ];

    let server_path = possible_paths
        .into_iter()
        .flatten()
        .find(|p| p.exists())
        .ok_or_else(|| VMError::new_vm(format!("Could not find slosh-fuse-server binary. Make sure to build it with 'cargo build --bin slosh-fuse-server'")))?;

    let child = Command::new(&server_path)
        .arg(mount_point.to_str().unwrap())
        .arg(eval_read.to_string())
        .arg(eval_write.to_string())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit()) // Let errors go to stderr for debugging
        .spawn()
        .map_err(|e| VMError::new_vm(format!("Failed to spawn FUSE server at {:?}: {}", server_path, e)))?;

    mount.process_handle = Some(child);
    mount.comm_write_fd = Some(comm_write);

    // Store mount in registry
    get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?
        .insert(mount_id.clone(), mount);

    Ok(vm.alloc_string(mount_id))
}

fn register_eval_file(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 3 {
        return Err(VMError::new_vm(
            "register-eval-file: requires three arguments (mount-id path expression)".to_string(),
        ));
    }

    let mount_id = registers[0].get_string(vm)?;
    let path = registers[1].get_string(vm)?;
    let expression = registers[2].get_string(vm)?;

    let registry = get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?;
    
    if let Some(mount) = registry.get(mount_id) {
        mount.register_file(&path, expression.to_string());
        Ok(Value::True)
    } else {
        Err(VMError::new_vm(format!("Invalid mount id: {}", mount_id)))
    }
}

fn unmount_eval_fs(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_vm(
            "unmount-eval-fs: requires one argument (mount-id)".to_string(),
        ));
    }

    let mount_id = registers[0].get_string(vm)?;

    let mut registry = get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?;
    
    if let Some(mut mount) = registry.remove(mount_id) {
        mount.unmount()
            .map_err(|e| VMError::new_vm(format!("Failed to unmount: {}", e)))?;
        
        // Also unmount the FUSE filesystem
        if let Err(e) = nix::mount::umount(&mount.mount_point) {
            // It's okay if unmount fails - the process termination should handle it
            eprintln!("Warning: umount failed: {}", e);
        }
        Ok(Value::True)
    } else {
        Err(VMError::new_vm(format!("Invalid mount id: {}", mount_id)))
    }
}

fn list_eval_files(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_vm(
            "list-eval-files: requires one argument (mount-id)".to_string(),
        ));
    }

    let mount_id = registers[0].get_string(vm)?;

    let registry = get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?;
    
    if let Some(mount) = registry.get(mount_id) {
        let mapping = mount.file_mapping
            .lock()
            .map_err(|e| VMError::new_vm(format!("File mapping lock poisoned: {}", e)))?;
        
        let files: Vec<Value> = mapping.list_files()
            .into_iter()
            .map(|f| vm.alloc_string(f))
            .collect();
        
        Ok(vm.alloc_vector(files))
    } else {
        Err(VMError::new_vm(format!("Invalid mount id: {}", mount_id)))
    }
}

fn list_mounts(vm: &mut SloshVm, _registers: &[Value]) -> VMResult<Value> {
    let registry = get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?;
    
    let mut mounts = Vec::new();

    for (mount_id, mount) in registry.iter() {
        let file_count = mount.file_mapping
            .lock()
            .map_err(|e| VMError::new_vm(format!("File mapping lock poisoned: {}", e)))?
            .list_files()
            .len();

        let mount_info = vec![
            vm.alloc_string("mount-id".to_string()),
            vm.alloc_string(mount_id.clone()),
            vm.alloc_string("mount-point".to_string()),
            vm.alloc_string(mount.mount_point.display().to_string()),
            vm.alloc_string("file-count".to_string()),
            Value::from(file_count as i64),
        ];
        mounts.push(vm.alloc_vector(mount_info));
    }

    Ok(vm.alloc_vector(mounts))
}

pub fn add_fuse_builtins(env: &mut SloshVm) {
    add_builtin(
        env,
        "mount-eval-fs",
        mount_eval_fs,
        r#"Usage: (mount-eval-fs path)

Mount an evaluation filesystem at the specified path. Returns a mount ID.

Section: fuse

Example:
(def mount-id (mount-eval-fs "/tmp/eval-fs"))
"#,
    );

    add_builtin(
        env,
        "register-eval-file",
        register_eval_file,
        r#"Usage: (register-eval-file mount-id path expression)

Register a file with a slosh expression that will be evaluated when the file is read.

Section: fuse

Example:
(register-eval-file mount-id "config.env" "(str \"HOST=\" (hostname))")
"#,
    );

    add_builtin(
        env,
        "unmount-eval-fs",
        unmount_eval_fs,
        r#"Usage: (unmount-eval-fs mount-id)

Unmount an evaluation filesystem.

Section: fuse

Example:
(unmount-eval-fs mount-id)
"#,
    );

    add_builtin(
        env,
        "list-eval-files",
        list_eval_files,
        r#"Usage: (list-eval-files mount-id)

List all files registered in a mount.

Section: fuse

Example:
(list-eval-files mount-id)
"#,
    );

    add_builtin(
        env,
        "list-mounts",
        list_mounts,
        r#"Usage: (list-mounts)

List all active FUSE mounts with their mount IDs, mount points, and file counts.

Section: fuse

Example:
(list-mounts)
; Returns a list of vectors, each containing:
; ["mount-id" <id> "mount-point" <path> "file-count" <count>]
"#,
    );
}