use crate::client::DaemonClient;
use crate::daemon::{DaemonConfig, PidFile};
use std::io;
use std::path::Path;

/// Ensure the FUSE daemon is running and return a connected client.
///
/// Tries to connect + ping. If that fails, cleans stale files and spawns
/// the current executable with `--fuse-daemon`, then polls for the socket.
pub fn ensure_daemon_running(config: &DaemonConfig) -> io::Result<DaemonClient> {
    // Try connecting to an existing daemon
    if let Ok(mut client) = DaemonClient::connect(&config.socket_path) {
        if client.ping().is_ok() {
            return Ok(client);
        }
    }

    // No daemon running. Clean up stale files.
    if PidFile::check_running(&config.pid_file).is_none() {
        let _ = std::fs::remove_file(&config.socket_path);
        let _ = std::fs::remove_file(&config.pid_file);
    } else {
        // PID file is locked but socket isn't responding. Wait a bit
        // in case the daemon is still starting up.
        return wait_for_socket(&config.socket_path, 5);
    }

    // Spawn daemon using current executable
    let exe = std::env::current_exe().map_err(|e| {
        io::Error::new(io::ErrorKind::NotFound, format!("cannot find current exe: {}", e))
    })?;

    std::process::Command::new(&exe)
        .arg("--fuse-daemon")
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn()
        .map_err(|e| {
            io::Error::new(
                io::ErrorKind::Other,
                format!("failed to spawn FUSE daemon: {}", e),
            )
        })?;

    wait_for_socket(&config.socket_path, 5)
}

/// Poll for the daemon socket to appear, then connect + ping.
fn wait_for_socket(socket_path: &Path, timeout_secs: u64) -> io::Result<DaemonClient> {
    let deadline = std::time::Instant::now()
        + std::time::Duration::from_secs(timeout_secs);

    while std::time::Instant::now() < deadline {
        if let Ok(mut client) = DaemonClient::connect(socket_path) {
            if client.ping().is_ok() {
                return Ok(client);
            }
        }
        std::thread::sleep(std::time::Duration::from_millis(100));
    }

    Err(io::Error::new(
        io::ErrorKind::TimedOut,
        "timed out waiting for FUSE daemon to start",
    ))
}
