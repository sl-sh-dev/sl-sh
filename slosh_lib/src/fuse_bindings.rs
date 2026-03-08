use bridge_adapters::add_builtin;
use compile_state::state::SloshVm;
use slvm::{VMError, VMResult, Value};
use std::collections::HashMap;
use std::sync::Mutex;
use std::path::PathBuf;

use slosh_fuse::FuseMount;
use slosh_fuse::auto_start::ensure_daemon_running;
use slosh_fuse::daemon::DaemonConfig;

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

    let config = DaemonConfig::default_for_user()
        .map_err(|e| VMError::new_vm(format!("Failed to get daemon config: {}", e)))?;

    let client = ensure_daemon_running(&config)
        .map_err(|e| VMError::new_vm(format!("Failed to connect to FUSE daemon: {}", e)))?;

    let mount = FuseMount::new_daemon(mount_point, client)
        .map_err(|e| VMError::new_vm(format!("Failed to create mount: {}", e)))?;

    let mount_id = mount.mount_id.clone();

    get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?
        .insert(mount_id.clone(), mount);

    Ok(vm.alloc_string(mount_id))
}

fn register_eval_file(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 3 {
        return Err(VMError::new_vm(
            "register-eval-file: requires three arguments (mount-id path content)".to_string(),
        ));
    }

    let mount_id = registers[0].get_string(vm)?;
    let path = registers[1].get_string(vm)?;
    let content = registers[2].get_string(vm)?;

    let mut registry = get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?;

    if let Some(mount) = registry.get_mut(mount_id) {
        mount
            .register_file(&path, content.as_bytes())
            .map_err(|e| VMError::new_vm(format!("Failed to register file: {}", e)))?;
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
        mount
            .unmount()
            .map_err(|e| VMError::new_vm(format!("Failed to unmount: {}", e)))?;
        Ok(Value::True)
    } else {
        Err(VMError::new_vm(format!("Invalid mount id: {}", mount_id)))
    }
}

fn concat_eval_files(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() < 3 {
        return Err(VMError::new_vm(
            "concat-eval-files: requires at least three arguments (mount-id vpath source-path...)".to_string(),
        ));
    }

    let mount_id = registers[0].get_string(vm)?;
    let vpath = registers[1].get_string(vm)?;

    let source_paths: Vec<String> = registers[2..]
        .iter()
        .map(|v| v.get_string(vm).map(|s| s.to_string()))
        .collect::<Result<Vec<_>, _>>()?;

    let source_refs: Vec<&str> = source_paths.iter().map(|s| s.as_str()).collect();

    let mut registry = get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?;

    if let Some(mount) = registry.get_mut(mount_id) {
        mount
            .register_concat_file(&vpath, &source_refs)
            .map_err(|e| VMError::new_vm(format!("Failed to register concat file: {}", e)))?;
        Ok(Value::True)
    } else {
        Err(VMError::new_vm(format!("Invalid mount id: {}", mount_id)))
    }
}

fn remove_eval_file(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 2 {
        return Err(VMError::new_vm(
            "remove-eval-file: requires two arguments (mount-id path)".to_string(),
        ));
    }

    let mount_id = registers[0].get_string(vm)?;
    let path = registers[1].get_string(vm)?;

    let mut registry = get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?;

    if let Some(mount) = registry.get_mut(mount_id) {
        mount
            .remove_file(&path)
            .map_err(|e| VMError::new_vm(format!("Failed to remove file: {}", e)))?;
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

    let mut registry = get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?;

    if let Some(mount) = registry.get_mut(mount_id) {
        let files = mount.list_files()
            .map_err(|e| VMError::new_vm(format!("Failed to list files: {}", e)))?;
        let values: Vec<Value> = files
            .iter()
            .map(|f| vm.alloc_string(f.clone()))
            .collect();
        Ok(vm.alloc_vector(values))
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
        let mount_info = vec![
            vm.alloc_string("mount-id".to_string()),
            vm.alloc_string(mount_id.clone()),
            vm.alloc_string("mount-point".to_string()),
            vm.alloc_string(mount.mount_point.display().to_string()),
            vm.alloc_string("file-count".to_string()),
            Value::from(mount.registered_paths.len() as i64),
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
Uses a shared per-user FUSE daemon (auto-started if needed).

Section: fuse

Example:
(def mount-id (mount-eval-fs "/tmp/eval-fs"))
"#,
    );

    add_builtin(
        env,
        "register-eval-file",
        register_eval_file,
        r#"Usage: (register-eval-file mount-id path content)

Register a file with static content in the FUSE filesystem.
Content is evaluated at registration time and sent to the FUSE daemon.

Section: fuse

Example:
(register-eval-file mount-id "config.txt" "HOST=myhost\nPORT=8080\n")
"#,
    );

    add_builtin(
        env,
        "concat-eval-files",
        concat_eval_files,
        r#"Usage: (concat-eval-files mount-id vpath source-path...)

Present multiple real files as a single virtual file in the FUSE filesystem.
The virtual file contents are the concatenation of the source files, read lazily
on demand so changes to source files are reflected automatically.

Section: fuse

Example:
(concat-eval-files mount-id "combined.txt" "/path/a.txt" "/path/b.txt" "/path/c.txt")
"#,
    );

    add_builtin(
        env,
        "remove-eval-file",
        remove_eval_file,
        r#"Usage: (remove-eval-file mount-id path)

Remove a file from the FUSE filesystem.

Section: fuse

Example:
(remove-eval-file mount-id "config.txt")
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
