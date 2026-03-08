use std::fs::{self, File};
use std::io;
use std::os::unix::io::AsRawFd;
use std::path::{Path, PathBuf};
use nix::sys::stat::{umask, Mode};

/// Paths used by the FUSE daemon.
pub struct DaemonConfig {
    pub pid_file: PathBuf,
    pub socket_path: PathBuf,
    pub log_file: PathBuf,
}

impl DaemonConfig {
    /// Build paths under `$HOME/.local/share/slosh/`.
    pub fn default_for_user() -> io::Result<Self> {
        let home = std::env::var("HOME").map_err(|_| {
            io::Error::new(io::ErrorKind::NotFound, "HOME not set")
        })?;
        let dir = PathBuf::from(home).join(".local/share/slosh");
        fs::create_dir_all(&dir)?;
        Ok(Self {
            pid_file: dir.join("fuse-daemon.pid"),
            socket_path: dir.join("fuse.sock"),
            log_file: dir.join("fuse-daemon.log"),
        })
    }
}

/// Advisory-lock-based PID file.
///
/// The lock is held for the lifetime of this struct (via the open fd).
/// On drop the file is removed and the lock released (fd close).
pub struct PidFile {
    path: PathBuf,
    _file: File,
}

impl PidFile {
    /// Create (or overwrite) the PID file and acquire an exclusive advisory
    /// lock via `libc::flock`. Returns `Err` if another process holds the lock.
    pub fn acquire(path: &Path) -> io::Result<Self> {
        let file = fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(path)?;

        let fd = file.as_raw_fd();
        let ret = unsafe { libc::flock(fd, libc::LOCK_EX | libc::LOCK_NB) };
        if ret != 0 {
            return Err(io::Error::new(
                io::ErrorKind::AddrInUse,
                "another FUSE daemon is already running",
            ));
        }

        use std::io::Write;
        // Write PID via a borrow of the File
        (&file).write_all(format!("{}", std::process::id()).as_bytes())?;
        (&file).flush()?;

        Ok(Self {
            path: path.to_path_buf(),
            _file: file,
        })
    }

    /// Check whether a daemon process is already running by trying to
    /// acquire the lock. If the lock is held, returns the PID from the file.
    pub fn check_running(path: &Path) -> Option<u32> {
        let file = match fs::OpenOptions::new().read(true).open(path) {
            Ok(f) => f,
            Err(_) => return None,
        };

        let fd = file.as_raw_fd();
        let ret = unsafe { libc::flock(fd, libc::LOCK_EX | libc::LOCK_NB) };
        if ret == 0 {
            // We got the lock -> no daemon running. Release immediately.
            unsafe { libc::flock(fd, libc::LOCK_UN) };
            None
        } else {
            // Lock is held -> read PID from file
            use std::io::Read;
            let mut buf = String::new();
            let mut f = &file;
            let _ = f.read_to_string(&mut buf);
            buf.trim().parse::<u32>().ok()
        }
    }
}

impl Drop for PidFile {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}

/// Double-fork + setsid to become a proper daemon.
///
/// Returns `Ok(())` in the daemon (grandchild) process.
/// The parent and intermediate child processes call `std::process::exit(0)`.
pub fn daemonize() -> io::Result<()> {
    use nix::unistd::{self, ForkResult, fork, setsid};

    // First fork
    match unsafe { fork() } {
        Ok(ForkResult::Parent { .. }) => std::process::exit(0),
        Ok(ForkResult::Child) => {}
        Err(e) => return Err(io::Error::new(io::ErrorKind::Other, e)),
    }

    // New session leader
    setsid().map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

    // Second fork (prevent re-acquiring a controlling terminal)
    match unsafe { fork() } {
        Ok(ForkResult::Parent { .. }) => std::process::exit(0),
        Ok(ForkResult::Child) => {}
        Err(e) => return Err(io::Error::new(io::ErrorKind::Other, e)),
    }

    // Clear the file mode creation mask
    umask(Mode::empty());

    // Change working directory so we don't hold a mountpoint
    std::env::set_current_dir("/")?;

    // Redirect stdio to /dev/null
    let devnull = fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open("/dev/null")?;
    let fd = devnull.as_raw_fd();
    unistd::dup2(fd, 0)?;
    unistd::dup2(fd, 1)?;
    unistd::dup2(fd, 2)?;

    Ok(())
}

/// Set up file-based logging for the daemon.
pub fn init_daemon_logging(log_path: &Path) -> io::Result<()> {
    let file = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(log_path)?;

    env_logger::Builder::new()
        .target(env_logger::Target::Pipe(Box::new(file)))
        .filter_level(log::LevelFilter::Info)
        .parse_default_env()
        .format(|buf, record| {
            use std::io::Write;
            writeln!(
                buf,
                "{} [{}] {}",
                epoch_secs(),
                record.level(),
                record.args()
            )
        })
        .init();

    Ok(())
}

fn epoch_secs() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn pid_file_acquire_and_drop() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("test.pid");

        {
            let _pid = PidFile::acquire(&path).unwrap();
            assert!(path.exists());

            // Second acquire should fail (lock held)
            let result = PidFile::acquire(&path);
            assert!(result.is_err());
        }

        // After drop, file is removed
        assert!(!path.exists());
    }

    #[test]
    fn check_running_no_file() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("test.pid");
        assert!(PidFile::check_running(&path).is_none());
    }

    #[test]
    fn check_running_with_locked_pid() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("test.pid");

        let _pid = PidFile::acquire(&path).unwrap();
        let running = PidFile::check_running(&path);
        assert!(running.is_some());
        assert_eq!(running.unwrap(), std::process::id());
    }

    #[test]
    fn daemon_config_default() {
        if std::env::var("HOME").is_ok() {
            let config = DaemonConfig::default_for_user().unwrap();
            assert!(config.pid_file.to_str().unwrap().contains("fuse-daemon.pid"));
            assert!(config.socket_path.to_str().unwrap().contains("fuse.sock"));
        }
    }
}
