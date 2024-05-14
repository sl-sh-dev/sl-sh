use bridge_adapters::add_builtin;
use compile_state::state::SloshVm;
use slvm::{VMError, VMResult, Value};
use std::borrow::Cow;
use std::fs::{self, OpenOptions};
use std::io::{Seek, SeekFrom, Write};
extern crate unicode_reader;
use bridge_macros::sl_sh_fn;
use shell::builtins::expand_tilde;
use slvm::io::HeapIo;
use unicode_reader::Graphemes;

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
                return Err(VMError::new("io", "fopen: first form must evaluate to a string (filename) or :stdin, :stdout, :stderr"));
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
        if is_read && is_write {
            return Err(VMError::new(
                "io",
                "fopen: only open file for read or write not both",
            ));
        }
        if !is_write {
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
        return if !is_write {
            let io = HeapIo::from_file(file);
            io.to_buf_reader()
                .expect("Could not create a buf reader for file open to read!");
            Ok(vm.alloc_io(io))
            /*let fd: i64 = file.as_raw_fd() as i64;
            let file_iter: CharIter = Box::new(
                Graphemes::from(BufReader::new(file))
                    .map(|s| {
                        if let Ok(s) = s {
                            Cow::Owned(s)
                        } else {
                            Cow::Borrowed("")
                        }
                    })
                    .peekable(),
            );
            Ok(Expression::alloc_data(ExpEnum::File(Rc::new(
                RefCell::new(FileState::Read(Some(file_iter), fd)),
            ))))*/
        } else {
            let io = HeapIo::from_file(file);
            io.to_buf_writer()
                .expect("Could not create a buf writer for file open to write!");
            Ok(vm.alloc_io(io))
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
        let io_len = io.stream_position()?;
        let mut f_iter: Box<dyn Iterator<Item = Cow<'static, str>>> =
            Box::new(Graphemes::from(io).map(|s| {
                if let Ok(s) = s {
                    Cow::Owned(s)
                } else {
                    Cow::Borrowed("")
                }
            }));
        let mut line = String::new();
        let mut out_ch = f_iter.next();
        if out_ch.is_none() {
            return Ok(Value::Nil);
        }
        while let Some(ch) = out_ch {
            line.push_str(&ch);
            if ch == "\n" {
                break;
            }
            out_ch = f_iter.next();
        }
        drop(f_iter);
        // Graphemes will pre-read (apparently) so reset the pos when done.
        vm.get_io(*h)
            .get_io()
            .seek(SeekFrom::Start(io_len + line.len() as u64))?;
        Ok(vm.alloc_string(line))
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

/// Usage: (fs-rm \"/dir/or/file/to/remove\")
///
/// Takes a file or directory as a string and removes it. Works recursively for directories.
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
        "Usage: (fopen filename option*)

Open a file.

Options are:
    :read
    :write
    :append
    :truncate
    :create
    :create-new
    :on-error-nil

Section: file

Example:
(def tmp (get-temp))
(def test-open-f (fopen (str tmp \"/slsh-tst-open.txt\") :create :truncate))
(fprn test-open-f \"Test Line One\")
(fclose test-open-f)
(test::assert-equal \"Test Line One\n\" (read-line (fopen (str tmp \"/slsh-tst-open.txt\"))))
",
    );
    add_builtin(
        env,
        "fclose",
        fclose,
        "Usage: (fclose file)

Close a file.

Section: file

Example:
(def tmp (get-temp))
(def tst-file (fopen (str tmp \"/slsh-tst-open.txt\") :create :truncate))
(fprn tst-file \"Test Line Two\")
(fclose tst-file)
(def tst-file (fopen (str tmp \"/slsh-tst-open.txt\") :read))
(test::assert-equal \"Test Line Two\n\" (read-line tst-file))
(fclose tst-file)
",
    );
    add_builtin(
        env,
        "read-line",
        builtin_read_line,
        r#"Usage: (read-line file) -> string

Read a line from a file.

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
        "Usage: (flush file)

Flush a file.

Section: file

Example:
(def tmp (get-temp))
(def tst-file (fopen (str tmp \"/slsh-tst-open.txt\") :create :truncate))
(fprn tst-file \"Test Line Three\")
(fflush tst-file)
(def tst-file (fopen (str tmp \"/slsh-tst-open.txt\") :read))
(test::assert-equal \"Test Line Three\n\" (read-line tst-file))
(fclose tst-file)
",
    );
}
