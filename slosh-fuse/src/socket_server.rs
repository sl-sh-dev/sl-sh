use crate::file_mapping::FileMapping;
use crate::mount_manager::MountManager;
use base64::Engine;
use std::io::{BufRead, BufReader, Write};
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

/// Run the daemon accept loop. Blocks until `shutdown` is set.
pub fn run_server(
    socket_path: &Path,
    shutdown: Arc<AtomicBool>,
) -> std::io::Result<()> {
    // Remove stale socket if it exists
    let _ = std::fs::remove_file(socket_path);

    let listener = UnixListener::bind(socket_path)?;
    listener.set_nonblocking(true)?;

    log::info!("Listening on {:?}", socket_path);

    let manager = Arc::new(Mutex::new(MountManager::new()));

    while !shutdown.load(Ordering::Relaxed) {
        match listener.accept() {
            Ok((stream, _addr)) => {
                log::info!("Client connected");
                let mgr = Arc::clone(&manager);
                let shut = Arc::clone(&shutdown);
                std::thread::Builder::new()
                    .name("fuse-client".to_string())
                    .spawn(move || {
                        if let Err(e) = handle_client(stream, mgr, shut) {
                            log::error!("Client handler error: {}", e);
                        }
                        log::info!("Client disconnected");
                    })
                    .ok();
            }
            Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                // No pending connection, sleep briefly
                std::thread::sleep(std::time::Duration::from_millis(100));
            }
            Err(e) => {
                log::error!("Accept error: {}", e);
                std::thread::sleep(std::time::Duration::from_millis(100));
            }
        }
    }

    log::info!("Shutting down server");

    // Clean unmount all
    {
        let mut mgr = manager.lock().unwrap();
        mgr.unmount_all();
    }

    // Remove socket
    let _ = std::fs::remove_file(socket_path);

    Ok(())
}

fn handle_client(
    stream: UnixStream,
    manager: Arc<Mutex<MountManager>>,
    shutdown: Arc<AtomicBool>,
) -> std::io::Result<()> {
    stream.set_nonblocking(false)?;
    let reader = BufReader::new(stream.try_clone()?);
    let mut writer = stream;

    for line in reader.lines() {
        if shutdown.load(Ordering::Relaxed) {
            break;
        }

        let line = line?;
        if line.is_empty() {
            continue;
        }

        let response = dispatch_command(&line, &manager);
        writer.write_all(response.as_bytes())?;
        writer.write_all(b"\n")?;
        writer.flush()?;
    }

    Ok(())
}

fn dispatch_command(
    line: &str,
    manager: &Arc<Mutex<MountManager>>,
) -> String {
    let parts: Vec<&str> = line.split('\t').collect();
    let cmd = parts.first().copied().unwrap_or("");

    match cmd {
        "PING" => "OK\tPONG".to_string(),

        "MOUNT" if parts.len() == 2 => {
            let mount_point = parts[1];
            match manager.lock().unwrap().mount(mount_point) {
                Ok(mount_id) => format!("OK\t{}", mount_id),
                Err(e) => format!("ERR\t{}", e),
            }
        }

        "REGISTER" if parts.len() == 4 => {
            let mount_id = parts[1];
            let path = parts[2];
            let b64 = parts[3];

            let mgr = manager.lock().unwrap();
            if let Some(mapping) = mgr.get_mapping(mount_id) {
                match base64::engine::general_purpose::STANDARD.decode(b64) {
                    Ok(content) => {
                        mapping.lock().unwrap().register(path, content);
                        "OK".to_string()
                    }
                    Err(e) => format!("ERR\tbase64 decode failed: {}", e),
                }
            } else {
                format!("ERR\tunknown mount-id: {}", mount_id)
            }
        }

        "CONCAT" if parts.len() >= 4 => {
            let mount_id = parts[1];
            let vpath = parts[2];
            let source_paths: Vec<PathBuf> =
                parts[3..].iter().map(|p| PathBuf::from(p)).collect();

            let mgr = manager.lock().unwrap();
            if let Some(mapping) = mgr.get_mapping(mount_id) {
                mapping.lock().unwrap().register_concat(vpath, source_paths);
                "OK".to_string()
            } else {
                format!("ERR\tunknown mount-id: {}", mount_id)
            }
        }

        "REMOVE" if parts.len() == 3 => {
            let mount_id = parts[1];
            let path = parts[2];

            let mgr = manager.lock().unwrap();
            if let Some(mapping) = mgr.get_mapping(mount_id) {
                mapping.lock().unwrap().remove(path);
                "OK".to_string()
            } else {
                format!("ERR\tunknown mount-id: {}", mount_id)
            }
        }

        "UNMOUNT" if parts.len() == 2 => {
            let mount_id = parts[1];
            match manager.lock().unwrap().unmount(mount_id) {
                Ok(()) => "OK".to_string(),
                Err(e) => format!("ERR\t{}", e),
            }
        }

        "LIST" if parts.len() == 2 => {
            let mount_id = parts[1];
            let mgr = manager.lock().unwrap();
            if let Some(mapping) = mgr.get_mapping(mount_id) {
                let files = mapping.lock().unwrap().list_files();
                let mut resp = "OK".to_string();
                for f in files {
                    resp.push('\t');
                    resp.push_str(&f);
                }
                resp
            } else {
                format!("ERR\tunknown mount-id: {}", mount_id)
            }
        }

        _ => format!("ERR\tunknown command: {}", line),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dispatch_ping() {
        let mgr = Arc::new(Mutex::new(MountManager::new()));
        let resp = dispatch_command("PING", &mgr);
        assert_eq!(resp, "OK\tPONG");
    }

    #[test]
    fn dispatch_unknown() {
        let mgr = Arc::new(Mutex::new(MountManager::new()));
        let resp = dispatch_command("BOGUS", &mgr);
        assert!(resp.starts_with("ERR"));
    }

    #[test]
    fn dispatch_register_unknown_mount() {
        let mgr = Arc::new(Mutex::new(MountManager::new()));
        let resp = dispatch_command("REGISTER\tbad-id\tfile.txt\tYQ==", &mgr);
        assert!(resp.starts_with("ERR"));
    }
}
