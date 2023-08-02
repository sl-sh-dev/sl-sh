use crate::add_builtin;
use compile_state::state::SloshVm;
use slvm::{VMError, VMResult, Value};
use std::collections::HashMap;
use std::fs::File;
use std::path::Path;

fn fs_meta(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), None) = (i.next(), i.next()) {
        let name = string.pretty_value(vm);
        let file = File::open(name)?;
        let meta = file.metadata()?;
        let mut map = HashMap::new();
        let ftype = if meta.is_dir() {
            "dir"
        } else if meta.is_file() {
            "file"
        } else if meta.is_symlink() {
            "symlink"
        } else {
            "unknown"
        };
        let ro = if meta.permissions().readonly() {
            Value::True
        } else {
            Value::False
        };
        map.insert(Value::Keyword(vm.intern_static("readonly")), ro);
        map.insert(
            Value::Keyword(vm.intern_static("len")),
            vm.alloc_u64(meta.len()),
        );
        map.insert(
            Value::Keyword(vm.intern_static("type")),
            Value::Keyword(vm.intern_static(ftype)),
        );
        // XXX TODO- include times.
        Ok(vm.alloc_map(map))
    } else {
        Err(VMError::new(
            "io",
            "fs-meta: takes a filename as only arg".to_string(),
        ))
    }
}

fn fs_exists(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), None) = (i.next(), i.next()) {
        let string = string.pretty_value(vm);
        let p = Path::new(&string);
        if p.exists() {
            Ok(Value::True)
        } else {
            Ok(Value::False)
        }
    } else {
        Err(VMError::new(
            "io",
            "fs-exists?: takes a filename as only arg".to_string(),
        ))
    }
}

pub fn add_io_builtins(env: &mut SloshVm) {
    add_builtin(
        env,
        "fs-meta",
        fs_meta,
        r#"Usage: (fs-meta [FILENAME]) -> map

Returns a map if a files meta data.

Section: io
"#,
    );

    add_builtin(
        env,
        "fs-exists?",
        fs_exists,
        r#"
 Usage: (fs-exists? path-to-test)

Does the given path exist?

Section: file

Example:
(sh "mkdir /tmp/tst-fs-exists")
(sh "touch /tmp/tst-fs-exists/fs-exists")
(test::assert-true (fs-exists? "/tmp/tst-fs-exists/fs-exists"))
(test::assert-true (fs-exists? "/tmp/tst-fs-exists"))
(test::assert-false (fs-exists? "/tmp/tst-fs-exists/fs-exists-nope"))
(sh "rm /tmp/tst-fs-exists/fs-exists")
(sh "rmdir /tmp/tst-fs-exists")
"#,
    );
}
