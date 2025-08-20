pub mod eval_fs;
pub mod file_mapping;

pub use eval_fs::EvalFs;
pub use file_mapping::{FileMapping, FileEntry};

use std::sync::{Arc, Mutex};
use std::path::PathBuf;

pub struct FuseMount {
    pub mount_point: PathBuf,
    pub file_mapping: Arc<Mutex<FileMapping>>,
    pub process_handle: Option<std::process::Child>,
    pub comm_write_fd: Option<std::os::unix::io::RawFd>,
}

impl FuseMount {
    pub fn new(mount_point: PathBuf) -> Self {
        Self {
            mount_point,
            file_mapping: Arc::new(Mutex::new(FileMapping::new())),
            process_handle: None,
            comm_write_fd: None,
        }
    }

    pub fn register_file(&self, path: &str, expression: String) {
        if let Ok(mut mapping) = self.file_mapping.lock() {
            mapping.register(path, expression);
        }
        // Silently ignore lock poisoning - the file just won't be registered
    }

    pub fn unmount(&mut self) -> Result<(), std::io::Error> {
        if let Some(mut handle) = self.process_handle.take() {
            handle.kill()?;
            handle.wait()?;
        }
        Ok(())
    }
}