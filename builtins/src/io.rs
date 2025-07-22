use bridge_adapters::add_builtin;
use compile_state::state::SloshVm;
use slvm::{VMError, VMResult, Value};
use std::fs::{self, OpenOptions};
use std::io::{Read, Write};
extern crate unicode_reader;
use bridge_macros::sl_sh_fn;
use shell::builtins::expand_tilde;
use slvm::io::HeapIo;

fn fopen(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut args = registers.iter();
    if let Some(a) = args.next() {
        if let Value::Keyword(sym) = a {
            let ret = match vm.get_interned(*sym) {
                "stdin" => Some(HeapIo::stdin()),
                "stdout" => Some(HeapIo::stdout()),
                "stderr" => Some(HeapIo::stderr()),
                _ => None,
            };
            if let Some(ret) = ret {
                if args.next().is_some() {
                    return Err(VMError::new(
                        "io",
                        "fopen: if first form is a symbol then other forms not valid".to_string(),
                    ));
                }
                return Ok(vm.alloc_io(ret));
            }
        }
        let file_name = match a {
            Value::String(h) => vm.get_string(*h),
            Value::StringConst(i) => vm.get_interned(*i),
            _ => {
                return Err(VMError::new(
                    "io",
                    "fopen: first form must evaluate to a string (filename) or :stdin, :stdout, :stderr",
                ));
            }
        };
        let file_name = expand_tilde(file_name.into());
        let mut opts = OpenOptions::new();
        let mut is_read = false;
        let mut is_write = false;
        let mut error_nil = false;
        for a in args {
            if let Value::Keyword(i) = a {
                match vm.get_interned(*i) {
                    "read" => {
                        is_read = true;
                        opts.read(true);
                    }
                    "write" => {
                        is_write = true;
                        opts.write(true);
                    }
                    "append" => {
                        is_write = true;
                        opts.append(true);
                    }
                    "truncate" => {
                        is_write = true;
                        opts.write(true);
                        opts.truncate(true);
                    }
                    "create" => {
                        is_write = true;
                        opts.write(true);
                        opts.create(true);
                    }
                    "create-new" => {
                        is_write = true;
                        opts.write(true);
                        opts.create_new(true);
                    }
                    "on-error-nil" => {
                        error_nil = true;
                    }
                    _ => {
                        let msg = format!("open: invalid directive, {}", vm.get_interned(*i));
                        return Err(VMError::new("io", msg));
                    }
                };
            } else {
                let msg = format!("fopen: {} invalid", a.display_type(vm));
                return Err(VMError::new("io", msg));
            }
        }
        if !is_write && !is_read {
            is_read = true;
            opts.read(true);
        }
        let file = match opts.open(&file_name) {
            Ok(file) => file,
            Err(err) => {
                if error_nil {
                    return Ok(Value::Nil);
                } else {
                    return Err(VMError::new(
                        "io",
                        format!("fopen: Error opening {}: {}", file_name.display(), err),
                    ));
                }
            }
        };
        return match (is_read, is_write) {
            (true, true) | (false, false) => {
                let io = HeapIo::from_file(file);
                Ok(vm.alloc_io(io))
            }
            (true, false) => {
                let io = HeapIo::from_file(file);
                io.to_buf_reader()
                    .expect("Could not create a buf reader for file open to read!");
                Ok(vm.alloc_io(io))
            }
            (false, true) => {
                let io = HeapIo::from_file(file);
                io.to_buf_writer()
                    .expect("Could not create a buf writer for file open to write!");
                Ok(vm.alloc_io(io))
            }
        };
    }
    Err(VMError::new(
        "io",
        "fopen takes at least one form (a file name)",
    ))
}

fn fclose(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut args = registers.iter();
    if let (Some(exp), None) = (args.next(), args.next()) {
        return if let Value::Io(h) = exp {
            let io = vm.get_io(*h);
            io.close();
            Ok(Value::True)
        } else {
            Err(VMError::new("io", "fclose requires a file"))
        };
    }
    Err(VMError::new("io", "fclose takes one form (file to close)"))
}

fn builtin_read_line(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut args = registers.iter();
    if let (Some(Value::Io(h)), None) = (args.next(), args.next()) {
        let mut io = vm.get_io(*h).get_io();

        let mut byte = [0_u8];
        let mut line_bytes = Vec::new();
        while io.read(&mut byte)? == 1 {
            line_bytes.push(byte[0]);
            if byte[0] == b'\n' {
                break;
            }
        }
        let line = match String::from_utf8(line_bytes) {
            Ok(line) => line,
            Err(e) => return Err(VMError::new("read", e.to_string())),
        };
        drop(io);
        if line.is_empty() {
            Ok(Value::Nil)
        } else {
            Ok(vm.alloc_string(line))
        }
    } else {
        Err(VMError::new("io", "read-line takes one form (file)"))
    }
}

fn builtin_flush(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut args = registers.iter();
    if let (Some(Value::Io(h)), None) = (args.next(), args.next()) {
        let mut io = vm.get_io(*h).get_io();
        io.flush()?;
        Ok(Value::True)
    } else {
        Err(VMError::new("io", "fflush takes one form (file)"))
    }
}

/// Usage: (fs-rm path) => boolean
///
/// Remove a file or directory (recursively).
///
/// Arguments:
/// - path: A string. Path to the file or directory to remove.
/// - boolean: A boolean. True on successful removal.
///
/// Directories are removed recursively with all their contents.
/// Returns an error if the path doesn't exist.
///
/// Section: file
///
/// Example:
/// (def fp nil)
/// (let (a-file (get-temp-file))
///     (test::assert-true (fs-exists? a-file))
///     (set! fp a-file)
///     (fs-rm a-file))
/// (test::assert-false (nil? fp))
/// (test::assert-false (fs-exists? fp))
#[sl_sh_fn(fn_name = "fs-rm")]
fn fs_rm(path: String) -> VMResult<Value> {
    let path = expand_tilde(path.into());
    let p = path.as_path();
    if p.exists() {
        if p.is_dir() {
            fs::remove_dir_all(p).map_err(|e| VMError::new("io", e.to_string()))?;
        } else {
            fs::remove_file(p).map_err(|e| VMError::new("io", e.to_string()))?;
        };
        Ok(Value::True)
    } else {
        Err(VMError::new(
            "io",
            format!("path does not exist: {}", path.display()),
        ))
    }
}

pub fn add_io_builtins(env: &mut SloshVm) {
    intern_fs_rm(env);

    add_builtin(
        env,
        "fopen",
        fopen,
        "Usage: (fopen filename & options) => file-handle

Open a file and return a file handle.

Arguments:
- filename: A string. Path to the file to open. Can also be :stdin, :stdout, or :stderr.
- options: Keywords. Zero or more option keywords:
  - :read - Open for reading
  - :write - Open for writing  
  - :append - Append to end of file (implies :write)
  - :truncate - Truncate file to zero length (implies :write)
  - :create - Create file if it doesn't exist (implies :write)
  - :create-new - Create new file, fail if exists (implies :write)
  - :on-error-nil - Return nil on error instead of throwing
- file-handle: A file handle object or nil (with :on-error-nil).

If you use :read and :write then you get a read/write unbuffered file. Including
one of :read or :write will provide a file buffered for read or write (this is faster).
Defaults to :read if no options specified.

Section: file

Example:
(with-temp-file (fn (tmp-file)
    (let (test-open-f (fopen tmp-file :create :truncate))
        (fprn test-open-f \"Test Line One\")
        (fclose test-open-f)
        (set! test-open-f (fopen tmp-file :read))
        (defer (fclose test-open-f))
        (test::assert-equal \"Test Line One\n\" (read-line test-open-f)))))
",
    );
    add_builtin(
        env,
        "fclose",
        fclose,
        "Usage: (fclose file-handle) => true

Close an open file handle.

Arguments:
- file-handle: A file handle. The file to close.
- true: Always returns true.

Section: file

Example:
(with-temp-file (fn (tmp-file)
    (let (tst-file (fopen tmp-file :create :truncate))
        (fprn tst-file \"Test Line Two\")
        (fclose tst-file)
        (set! tst-file (fopen tmp-file :read))
        (defer (fclose tst-file))
        (test::assert-equal \"Test Line Two\n\" (read-line tst-file)))))
",
    );
    add_builtin(
        env,
        "read-line",
        builtin_read_line,
        r#"Usage: (read-line file-handle) => string-or-nil

Read a line from a file.

Arguments:
- file-handle: A file handle. The file to read from.
- string-or-nil: A string containing the line (including newline), or nil at EOF.

Reads characters until a newline is found or end of file is reached.
The returned string includes the terminating newline character.

Section: file

Example:
(with-temp-file (fn (tmp)
    (let (tst-file (fopen tmp :create :truncate))
        (fprn tst-file "Test Line Read Line One")
        (fpr tst-file "Test Line Read Line Two")
        (fclose tst-file)
        (set! tst-file (fopen tmp :read))
        (defer (fclose tst-file))
        (test::assert-equal "Test Line Read Line One\n" (read-line tst-file))
        (test::assert-equal "Test Line Read Line Two" (read-line tst-file)))))
"#,
    );
    add_builtin(
        env,
        "fflush",
        builtin_flush,
        "Usage: (fflush file-handle) => true

Flush buffered data to a file.

Arguments:
- file-handle: A file handle. The file to flush.
- true: Always returns true.

Forces any buffered output to be written immediately.

Section: file

Example:
(with-temp-file (fn (tmp-file)
    (let (tst-file (fopen tmp-file :create :truncate)
          tst-file-read (fopen tmp-file :read))
        (defer (fclose tst-file))
        (defer (fclose tst-file-read))
        (fprn tst-file \"Test Line Three\")
        (fflush tst-file)
        (test::assert-equal \"Test Line Three\n\" (read-line tst-file-read)))))
",
    );
}
