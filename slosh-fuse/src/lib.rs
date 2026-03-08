pub mod eval_fs;
pub mod file_mapping;
pub mod proc_subst;

pub use eval_fs::EvalFs;
pub use file_mapping::{FileMapping, FileEntry};

use base64::Engine;
use std::io::Write;
use std::path::PathBuf;
use std::process::ChildStdin;

pub struct FuseMount {
    pub mount_point: PathBuf,
    pub process_handle: Option<std::process::Child>,
    pub stdin_handle: Option<ChildStdin>,
    pub registered_paths: Vec<String>,
}

impl FuseMount {
    pub fn new(mount_point: PathBuf) -> Self {
        Self {
            mount_point,
            process_handle: None,
            stdin_handle: None,
            registered_paths: Vec::new(),
        }
    }

    fn send_command(&mut self, cmd: &str) -> Result<(), std::io::Error> {
        if let Some(ref mut stdin) = self.stdin_handle {
            stdin.write_all(cmd.as_bytes())?;
            stdin.flush()?;
            Ok(())
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::NotConnected,
                "FUSE server stdin not connected",
            ))
        }
    }

    pub fn register_file(&mut self, path: &str, content: &[u8]) -> Result<(), std::io::Error> {
        let b64 = base64::engine::general_purpose::STANDARD.encode(content);
        let cmd = format!("REGISTER\t{}\t{}\n", path, b64);
        self.send_command(&cmd)?;
        if !self.registered_paths.contains(&path.to_string()) {
            self.registered_paths.push(path.to_string());
        }
        Ok(())
    }

    pub fn register_concat_file(&mut self, vpath: &str, source_paths: &[&str]) -> Result<(), std::io::Error> {
        let mut cmd = format!("CONCAT\t{}", vpath);
        for p in source_paths {
            cmd.push('\t');
            cmd.push_str(p);
        }
        cmd.push('\n');
        self.send_command(&cmd)?;
        if !self.registered_paths.contains(&vpath.to_string()) {
            self.registered_paths.push(vpath.to_string());
        }
        Ok(())
    }

    pub fn remove_file(&mut self, path: &str) -> Result<(), std::io::Error> {
        let cmd = format!("REMOVE\t{}\n", path);
        self.send_command(&cmd)?;
        self.registered_paths.retain(|p| p != path);
        Ok(())
    }

    pub fn unmount(&mut self) -> Result<(), std::io::Error> {
        // Drop stdin to signal EOF to the server's reader thread
        self.stdin_handle.take();

        if let Some(mut handle) = self.process_handle.take() {
            handle.kill()?;
            handle.wait()?;
        }
        Ok(())
    }
}
