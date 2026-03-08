use base64::Engine;
use fuser::{MountOption, Session};
use slosh_fuse::{EvalFs, FileMapping};
use std::io::BufRead;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::env;

fn main() {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <mount_point>", args[0]);
        std::process::exit(1);
    }

    let mount_point = &args[1];
    let file_mapping = Arc::new(Mutex::new(FileMapping::new()));

    // Spawn a background thread to read commands from stdin
    let mapping_for_reader = Arc::clone(&file_mapping);
    std::thread::spawn(move || {
        let stdin = std::io::stdin();
        let reader = stdin.lock();
        for line in reader.lines() {
            let line = match line {
                Ok(l) => l,
                Err(e) => {
                    log::error!("Error reading stdin: {}", e);
                    break;
                }
            };

            let parts: Vec<&str> = line.split('\t').collect();
            match parts.first().copied() {
                Some("REGISTER") if parts.len() == 3 => {
                    let path = parts[1];
                    let b64_content = parts[2];
                    match base64::engine::general_purpose::STANDARD.decode(b64_content) {
                        Ok(content) => {
                            let mut mapping = mapping_for_reader.lock().unwrap();
                            mapping.register(path, content);
                            log::info!("Registered file: {}", path);
                        }
                        Err(e) => {
                            log::error!("Failed to decode base64 for {}: {}", path, e);
                        }
                    }
                }
                Some("CONCAT") if parts.len() >= 3 => {
                    let vpath = parts[1];
                    let source_paths: Vec<PathBuf> =
                        parts[2..].iter().map(PathBuf::from).collect();
                    let mut mapping = mapping_for_reader.lock().unwrap();
                    mapping.register_concat(vpath, source_paths);
                    log::info!("Registered concat file: {} ({} sources)", vpath, parts.len() - 2);
                }
                Some("REMOVE") if parts.len() >= 2 => {
                    let path = parts[1];
                    let mut mapping = mapping_for_reader.lock().unwrap();
                    mapping.remove(path);
                    log::info!("Removed file: {}", path);
                }
                _ => {
                    log::warn!("Unknown command: {}", line);
                }
            }
        }
        log::info!("Stdin reader thread exiting");
    });

    let fs = EvalFs::new(file_mapping);

    let options = vec![
        MountOption::FSName("slosh-eval".to_string()),
        MountOption::AutoUnmount,
        MountOption::DefaultPermissions,
    ];

    let mut session = Session::new(fs, mount_point.as_ref(), &options)
        .expect("Failed to create FUSE session");

    session.run().expect("FUSE session failed");
}
