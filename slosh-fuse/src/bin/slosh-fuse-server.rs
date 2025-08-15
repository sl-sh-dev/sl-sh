use fuser::{MountOption, Session};
use slosh_fuse::{EvalFs, FileMapping};
use std::sync::{Arc, Mutex};
use std::env;
use std::os::unix::io::RawFd;

fn main() {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    if args.len() < 4 {
        eprintln!("Usage: {} <mount_point> <read_fd> <write_fd>", args[0]);
        std::process::exit(1);
    }

    let mount_point = &args[1];
    let read_fd: RawFd = args[2].parse().expect("Invalid read fd");
    let write_fd: RawFd = args[3].parse().expect("Invalid write fd");

    let file_mapping = Arc::new(Mutex::new(FileMapping::new()));

    // Pre-populate with test data if provided
    if args.len() > 4 {
        let mut mapping = file_mapping.lock().unwrap();
        for i in (4..args.len()).step_by(2) {
            if i + 1 < args.len() {
                mapping.register(&args[i], args[i + 1].clone());
            }
        }
    }

    let fs = EvalFs::new(file_mapping, read_fd, write_fd);

    let options = vec![
        MountOption::FSName("slosh-eval".to_string()),
        MountOption::AllowOther,
        MountOption::DefaultPermissions,
    ];

    let mut session = Session::new(fs, mount_point.as_ref(), &options).expect("Failed to mount");

    // Run until unmounted
    session.run().expect("Failed to run session");
}