use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::os::unix::fs::symlink;
use std::os::unix::io::{AsRawFd, FromRawFd, IntoRawFd};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::{Arc, Mutex};

/// Process substitution implementation that creates /dev/fd/* symlinks
pub struct ProcSubst {
    /// Map of virtual paths to their backing processes
    processes: Arc<Mutex<HashMap<String, ProcInfo>>>,
    /// Base directory for symlinks (e.g., /tmp/slosh-proc-subst-XXXXX)
    base_dir: PathBuf,
}

struct ProcInfo {
    process: Child,
    fd: i32,
    symlink_path: PathBuf,
}

impl ProcSubst {
    pub fn new() -> Result<Self, std::io::Error> {
        let base_dir = tempfile::tempdir()?.into_path();
        Ok(Self {
            processes: Arc::new(Mutex::new(HashMap::new())),
            base_dir,
        })
    }

    /// Create a virtual file backed by a process
    pub fn create_file(&self, name: &str, command: &str) -> Result<PathBuf, std::io::Error> {
        // Create the symlink path
        let symlink_path = self.base_dir.join(name);
        
        // Ensure parent directory exists
        if let Some(parent) = symlink_path.parent() {
            fs::create_dir_all(parent)?;
        }

        // Spawn the process
        let mut child = Command::new("sh")
            .arg("-c")
            .arg(command)
            .stdout(Stdio::piped())
            .spawn()?;

        // Get the file descriptor
        let stdout = child.stdout.take()
            .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::Other, "No stdout"))?;
        let fd = stdout.as_raw_fd();

        // Create symlink to /dev/fd/N
        let fd_path = format!("/dev/fd/{}", fd);
        symlink(&fd_path, &symlink_path)?;

        // Store process info (leak the fd to keep it open)
        std::mem::forget(stdout);
        
        let info = ProcInfo {
            process: child,
            fd,
            symlink_path: symlink_path.clone(),
        };

        self.processes.lock().unwrap()
            .insert(name.to_string(), info);

        Ok(symlink_path)
    }

    /// Remove a virtual file
    pub fn remove_file(&self, name: &str) -> Result<(), std::io::Error> {
        let mut processes = self.processes.lock().unwrap();
        
        if let Some(mut info) = processes.remove(name) {
            // Remove the symlink
            let _ = fs::remove_file(&info.symlink_path);
            
            // Kill the process
            let _ = info.process.kill();
            let _ = info.process.wait();
            
            // Close the file descriptor
            unsafe {
                libc::close(info.fd);
            }
        }
        
        Ok(())
    }

    /// List all virtual files
    pub fn list_files(&self) -> Vec<String> {
        self.processes.lock().unwrap()
            .keys()
            .cloned()
            .collect()
    }

    /// Get the path to a virtual file
    pub fn get_file_path(&self, name: &str) -> Option<PathBuf> {
        self.processes.lock().unwrap()
            .get(name)
            .map(|info| info.symlink_path.clone())
    }
}

impl Drop for ProcSubst {
    fn drop(&mut self) {
        // Clean up all processes
        let mut processes = self.processes.lock().unwrap();
        for (_, mut info) in processes.drain() {
            let _ = fs::remove_file(&info.symlink_path);
            let _ = info.process.kill();
            let _ = info.process.wait();
            unsafe {
                libc::close(info.fd);
            }
        }
        
        // Remove base directory
        let _ = fs::remove_dir_all(&self.base_dir);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::Read;

    #[test]
    fn test_proc_subst() {
        let ps = ProcSubst::new().unwrap();
        
        // Create a virtual file
        let path = ps.create_file("test.txt", "echo 'Hello, World!'").unwrap();
        
        // Read from it
        let mut file = File::open(&path).unwrap();
        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();
        
        assert_eq!(contents.trim(), "Hello, World!");
        
        // Clean up
        ps.remove_file("test.txt").unwrap();
    }
}