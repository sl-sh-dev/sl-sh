pub mod eval_fs;
pub mod file_mapping;
pub mod proc_subst;

pub mod auto_start;
pub mod client;
pub mod daemon;
pub mod mount_manager;
pub mod socket_server;

pub use eval_fs::EvalFs;
pub use file_mapping::{FileMapping, FileEntry};

use std::path::PathBuf;

use crate::client::DaemonClient;

/// A handle to a single FUSE mount managed by the daemon.
pub struct FuseMount {
    pub mount_point: PathBuf,
    pub mount_id: String,
    client: DaemonClient,
    pub registered_paths: Vec<String>,
}

impl FuseMount {
    /// Create a new mount via the daemon. Calls `client.mount()` to get a
    /// mount-id from the running daemon.
    pub fn new_daemon(
        mount_point: PathBuf,
        mut client: DaemonClient,
    ) -> Result<Self, std::io::Error> {
        let mount_id = client.mount(mount_point.to_str().unwrap_or(""))?;
        Ok(Self {
            mount_point,
            mount_id,
            client,
            registered_paths: Vec::new(),
        })
    }

    pub fn register_file(&mut self, path: &str, content: &[u8]) -> Result<(), std::io::Error> {
        self.client.register_file(&self.mount_id, path, content)?;
        if !self.registered_paths.contains(&path.to_string()) {
            self.registered_paths.push(path.to_string());
        }
        Ok(())
    }

    pub fn register_concat_file(
        &mut self,
        vpath: &str,
        source_paths: &[&str],
    ) -> Result<(), std::io::Error> {
        self.client
            .register_concat(&self.mount_id, vpath, source_paths)?;
        if !self.registered_paths.contains(&vpath.to_string()) {
            self.registered_paths.push(vpath.to_string());
        }
        Ok(())
    }

    pub fn remove_file(&mut self, path: &str) -> Result<(), std::io::Error> {
        self.client.remove_file(&self.mount_id, path)?;
        self.registered_paths.retain(|p| p != path);
        Ok(())
    }

    pub fn unmount(&mut self) -> Result<(), std::io::Error> {
        self.client.unmount(&self.mount_id)
    }

    pub fn list_files(&mut self) -> Result<Vec<String>, std::io::Error> {
        self.client.list_files(&self.mount_id)
    }
}

/// Entry point for the daemon process. Called from `slosh --fuse-daemon`.
pub fn run_fuse_daemon(foreground: bool) -> Result<(), Box<dyn std::error::Error>> {
    use crate::daemon::{DaemonConfig, PidFile, daemonize, init_daemon_logging};
    use crate::socket_server::run_server;
    use std::sync::atomic::AtomicBool;
    use std::sync::Arc;

    let config = DaemonConfig::default_for_user()?;

    if !foreground {
        daemonize()?;
    }

    // Acquire PID file (single instance check)
    let _pid_file = PidFile::acquire(&config.pid_file)?;

    // Set up logging to file
    init_daemon_logging(&config.log_file)?;
    log::info!("FUSE daemon starting (pid={})", std::process::id());

    let shutdown = Arc::new(AtomicBool::new(false));
    signal_hook::flag::register(signal_hook::consts::SIGTERM, Arc::clone(&shutdown))?;
    signal_hook::flag::register(signal_hook::consts::SIGINT, Arc::clone(&shutdown))?;

    run_server(&config.socket_path, shutdown)?;

    log::info!("FUSE daemon exiting");
    Ok(())
}
