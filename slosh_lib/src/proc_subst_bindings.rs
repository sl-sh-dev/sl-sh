use bridge_adapters::add_builtin;
use compile_state::state::SloshVm;
use slvm::{VMError, VMResult, Value};
use std::collections::HashMap;
use std::sync::Mutex;

use slosh_fuse::proc_subst::ProcSubst;

// Registry for process substitution instances
use std::sync::OnceLock;
static PROC_SUBST_REGISTRY: OnceLock<Mutex<HashMap<String, ProcSubst>>> = OnceLock::new();

fn get_registry() -> &'static Mutex<HashMap<String, ProcSubst>> {
    PROC_SUBST_REGISTRY.get_or_init(|| Mutex::new(HashMap::new()))
}

fn create_proc_subst(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 0 {
        return Err(VMError::new_vm(
            "create-proc-subst: requires no arguments".to_string(),
        ));
    }

    let ps = ProcSubst::new()
        .map_err(|e| VMError::new_vm(format!("Failed to create proc subst: {}", e)))?;

    let ps_id = format!("proc-subst-{}", uuid::Uuid::new_v4());
    
    get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?
        .insert(ps_id.clone(), ps);

    Ok(vm.alloc_string(ps_id))
}

fn proc_subst_file(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 3 {
        return Err(VMError::new_vm(
            "proc-subst-file: requires three arguments (ps-id name command)".to_string(),
        ));
    }

    let ps_id = registers[0].get_string(vm)?;
    let name = registers[1].get_string(vm)?;
    let command = registers[2].get_string(vm)?;

    let registry = get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?;
    
    if let Some(ps) = registry.get(ps_id) {
        let path = ps.create_file(&name, &command)
            .map_err(|e| VMError::new_vm(format!("Failed to create file: {}", e)))?;
        
        Ok(vm.alloc_string(path.display().to_string()))
    } else {
        Err(VMError::new_vm(format!("Invalid proc-subst id: {}", ps_id)))
    }
}

fn proc_subst_eval_file(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 3 {
        return Err(VMError::new_vm(
            "proc-subst-eval-file: requires three arguments (ps-id name expression)".to_string(),
        ));
    }

    let ps_id = registers[0].get_string(vm)?;
    let name = registers[1].get_string(vm)?;
    let expression = registers[2].get_string(vm)?;

    // Build slosh command to evaluate the expression
    let command = format!("slosh -c 'pr {}'", expression);

    let registry = get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?;
    
    if let Some(ps) = registry.get(ps_id) {
        let path = ps.create_file(&name, &command)
            .map_err(|e| VMError::new_vm(format!("Failed to create file: {}", e)))?;
        
        Ok(vm.alloc_string(path.display().to_string()))
    } else {
        Err(VMError::new_vm(format!("Invalid proc-subst id: {}", ps_id)))
    }
}

fn proc_subst_remove(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 2 {
        return Err(VMError::new_vm(
            "proc-subst-remove: requires two arguments (ps-id name)".to_string(),
        ));
    }

    let ps_id = registers[0].get_string(vm)?;
    let name = registers[1].get_string(vm)?;

    let registry = get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?;
    
    if let Some(ps) = registry.get(ps_id) {
        ps.remove_file(&name)
            .map_err(|e| VMError::new_vm(format!("Failed to remove file: {}", e)))?;
        Ok(Value::True)
    } else {
        Err(VMError::new_vm(format!("Invalid proc-subst id: {}", ps_id)))
    }
}

fn proc_subst_list(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_vm(
            "proc-subst-list: requires one argument (ps-id)".to_string(),
        ));
    }

    let ps_id = registers[0].get_string(vm)?;

    let registry = get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?;
    
    if let Some(ps) = registry.get(ps_id) {
        let files: Vec<Value> = ps.list_files()
            .into_iter()
            .map(|f| vm.alloc_string(f))
            .collect();
        
        Ok(vm.alloc_vector(files))
    } else {
        Err(VMError::new_vm(format!("Invalid proc-subst id: {}", ps_id)))
    }
}

fn proc_subst_path(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 2 {
        return Err(VMError::new_vm(
            "proc-subst-path: requires two arguments (ps-id name)".to_string(),
        ));
    }

    let ps_id = registers[0].get_string(vm)?;
    let name = registers[1].get_string(vm)?;

    let registry = get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?;
    
    if let Some(ps) = registry.get(ps_id) {
        if let Some(path) = ps.get_file_path(&name) {
            Ok(vm.alloc_string(path.display().to_string()))
        } else {
            Err(VMError::new_vm(format!("File not found: {}", name)))
        }
    } else {
        Err(VMError::new_vm(format!("Invalid proc-subst id: {}", ps_id)))
    }
}

fn destroy_proc_subst(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_vm(
            "destroy-proc-subst: requires one argument (ps-id)".to_string(),
        ));
    }

    let ps_id = registers[0].get_string(vm)?;

    let mut registry = get_registry()
        .lock()
        .map_err(|e| VMError::new_vm(format!("Registry lock poisoned: {}", e)))?;
    
    if registry.remove(ps_id).is_some() {
        Ok(Value::True)
    } else {
        Err(VMError::new_vm(format!("Invalid proc-subst id: {}", ps_id)))
    }
}

pub fn add_proc_subst_builtins(env: &mut SloshVm) {
    add_builtin(
        env,
        "create-proc-subst",
        create_proc_subst,
        r#"Usage: (create-proc-subst)

Create a new process substitution context. Returns an ID.

Section: proc-subst

Example:
(def ps-id (create-proc-subst))
"#,
    );

    add_builtin(
        env,
        "proc-subst-file",
        proc_subst_file,
        r#"Usage: (proc-subst-file ps-id name command)

Create a virtual file backed by a command. Returns the file path.

Section: proc-subst

Example:
(proc-subst-file ps-id "date.txt" "date +%Y-%m-%d")
"#,
    );

    add_builtin(
        env,
        "proc-subst-eval-file",
        proc_subst_eval_file,
        r#"Usage: (proc-subst-eval-file ps-id name expression)

Create a virtual file backed by a slosh expression. Returns the file path.

Section: proc-subst

Example:
(proc-subst-eval-file ps-id "config.env" "(str \"HOST=\" (hostname))")
"#,
    );

    add_builtin(
        env,
        "proc-subst-remove",
        proc_subst_remove,
        r#"Usage: (proc-subst-remove ps-id name)

Remove a virtual file.

Section: proc-subst

Example:
(proc-subst-remove ps-id "date.txt")
"#,
    );

    add_builtin(
        env,
        "proc-subst-list",
        proc_subst_list,
        r#"Usage: (proc-subst-list ps-id)

List all virtual files in a proc-subst context.

Section: proc-subst

Example:
(proc-subst-list ps-id)
"#,
    );

    add_builtin(
        env,
        "proc-subst-path",
        proc_subst_path,
        r#"Usage: (proc-subst-path ps-id name)

Get the path to a virtual file.

Section: proc-subst

Example:
(proc-subst-path ps-id "date.txt")
"#,
    );

    add_builtin(
        env,
        "destroy-proc-subst",
        destroy_proc_subst,
        r#"Usage: (destroy-proc-subst ps-id)

Destroy a process substitution context and clean up all files.

Section: proc-subst

Example:
(destroy-proc-subst ps-id)
"#,
    );
}