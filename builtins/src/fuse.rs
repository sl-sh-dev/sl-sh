use bridge_adapters::add_builtin;
use compile_state::state::SloshVm;
use slvm::{VMError, VMResult, Value};
use std::collections::HashMap;
use std::sync::Mutex;
use std::path::PathBuf;

// Simple registry to track mount points and their expressions
lazy_static::lazy_static! {
    static ref FUSE_REGISTRY: Mutex<HashMap<String, FuseInfo>> = Mutex::new(HashMap::new());
}

struct FuseInfo {
    mount_point: PathBuf,
    expressions: HashMap<String, String>,
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

    let mount_id = format!("mount-{}", uuid::Uuid::new_v4());

    let info = FuseInfo {
        mount_point: mount_point.clone(),
        expressions: HashMap::new(),
    };

    FUSE_REGISTRY.lock().unwrap().insert(mount_id.clone(), info);

    // For now, return the mount ID. The actual FUSE mounting will be done separately
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

    let mut registry = FUSE_REGISTRY.lock().unwrap();
    if let Some(info) = registry.get_mut(mount_id) {
        info.expressions.insert(path.to_string(), expression.to_string());
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

    let mut registry = FUSE_REGISTRY.lock().unwrap();
    if registry.remove(mount_id).is_some() {
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

    let registry = FUSE_REGISTRY.lock().unwrap();
    if let Some(info) = registry.get(mount_id) {
        let files: Vec<Value> = info.expressions.keys()
            .map(|k| vm.alloc_string(k.clone()))
            .collect();
        Ok(vm.alloc_vector(files))
    } else {
        Err(VMError::new_vm(format!("Invalid mount id: {}", mount_id)))
    }
}

fn list_mounts(vm: &mut SloshVm, _registers: &[Value]) -> VMResult<Value> {
    let registry = FUSE_REGISTRY.lock().unwrap();
    let mut mounts = Vec::new();

    for (mount_id, info) in registry.iter() {
        let mount_info = vec![
            vm.alloc_string("mount-id"),
            vm.alloc_string(mount_id.clone()),
            vm.alloc_string("mount-point"),
            vm.alloc_string(info.mount_point.display().to_string()),
            vm.alloc_string("file-count"),
            vm.alloc_int(info.expressions.len() as i64),
        ];
        mounts.push(vm.alloc_vector(mount_info));
    }

    Ok(vm.alloc_vector(mounts))
}

// For testing - evaluate an expression for a file
fn eval_file_expression(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 2 {
        return Err(VMError::new_vm(
            "eval-file-expression: requires two arguments (mount-id path)".to_string(),
        ));
    }

    let mount_id = registers[0].get_string(vm)?;
    let path = registers[1].get_string(vm)?;

    let registry = FUSE_REGISTRY.lock().unwrap();
    if let Some(info) = registry.get(mount_id) {
        if let Some(expression) = info.expressions.get(path) {
            // For now, just return the expression itself
            // In the real implementation, this would evaluate the expression
            Ok(vm.alloc_string(format!("Would evaluate: {}", expression)))
        } else {
            Err(VMError::new_vm(format!("File not registered: {}", path)))
        }
    } else {
        Err(VMError::new_vm(format!("Invalid mount id: {}", mount_id)))
    }
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

    add_builtin(
        env,
        "eval-file-expression",
        eval_file_expression,
        r#"Usage: (eval-file-expression mount-id path)

Test function to evaluate the expression for a file. For debugging.

Section: fuse

Example:
(eval-file-expression mount-id "config.env")
"#,
    );
}