use nix::dir::Dir;
use nix::fcntl::{AtFlags, OFlag};
use nix::sys::stat::{self, Mode};
use std::io::{Read, Seek, SeekFrom};
use std::os::unix::io::{AsRawFd, FromRawFd};

/// Holds a pre-opened directory file descriptor for passthrough reads.
///
/// Opened BEFORE the FUSE mount covers the directory, so all operations
/// via `openat`/`fstatat` relative to this fd bypass the FUSE layer entirely.
pub struct BackingDir {
    dir: Dir,
}

impl BackingDir {
    /// Open a directory fd BEFORE the FUSE mount covers it.
    pub fn open(path: &str) -> Result<Self, String> {
        let dir = Dir::open(
            path,
            OFlag::O_RDONLY | OFlag::O_DIRECTORY,
            Mode::empty(),
        )
        .map_err(|e| format!("failed to open backing dir {:?}: {}", path, e))?;

        Ok(Self { dir })
    }

    /// Stat a file in the backing dir. Returns None if not found.
    /// Returns (size, is_regular_file).
    pub fn stat(&self, name: &str) -> Option<(u64, bool)> {
        let fd = self.dir.as_raw_fd();
        match stat::fstatat(Some(fd), name, AtFlags::AT_SYMLINK_NOFOLLOW) {
            Ok(st) => {
                let is_regular = (st.st_mode & libc::S_IFMT) == libc::S_IFREG;
                Some((st.st_size as u64, is_regular))
            }
            Err(_) => None,
        }
    }

    /// Read bytes from a file in the backing dir.
    pub fn read_file(&self, name: &str, offset: u64, size: u32) -> Vec<u8> {
        let fd = self.dir.as_raw_fd();
        let file_fd = match nix::fcntl::openat(
            Some(fd),
            name,
            OFlag::O_RDONLY,
            Mode::empty(),
        ) {
            Ok(fd) => fd,
            Err(e) => {
                log::warn!("backing_dir: failed to open {:?}: {}", name, e);
                return Vec::new();
            }
        };

        // Safety: file_fd is a valid fd we just opened
        let mut file = unsafe { std::fs::File::from_raw_fd(file_fd) };

        if offset > 0 {
            if let Err(e) = file.seek(SeekFrom::Start(offset)) {
                log::warn!("backing_dir: seek failed on {:?}: {}", name, e);
                return Vec::new();
            }
        }

        let mut buf = vec![0u8; size as usize];
        match file.read(&mut buf) {
            Ok(n) => {
                buf.truncate(n);
                buf
            }
            Err(e) => {
                log::warn!("backing_dir: read failed on {:?}: {}", name, e);
                Vec::new()
            }
        }
    }

    /// List all regular files in the backing dir.
    pub fn list_files(&self) -> Vec<String> {
        let fd = self.dir.as_raw_fd();
        // Open a fresh Dir from the same fd so we can iterate without
        // consuming self. We dup the fd first.
        let dup_fd = match nix::unistd::dup(fd) {
            Ok(d) => d,
            Err(e) => {
                log::warn!("backing_dir: dup failed: {}", e);
                return Vec::new();
            }
        };

        let mut dir = match Dir::from_fd(dup_fd) {
            Ok(d) => d,
            Err(e) => {
                log::warn!("backing_dir: Dir::from_fd failed: {}", e);
                let _ = nix::unistd::close(dup_fd);
                return Vec::new();
            }
        };

        let mut files = Vec::new();
        for entry in dir.iter() {
            match entry {
                Ok(e) => {
                    let name = e.file_name().to_string_lossy().to_string();
                    if name == "." || name == ".." {
                        continue;
                    }
                    // Only include regular files
                    if let Some((_, true)) = self.stat(&name) {
                        files.push(name);
                    }
                }
                Err(e) => {
                    log::warn!("backing_dir: readdir error: {}", e);
                    break;
                }
            }
        }

        files
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn open_and_stat() {
        let dir = tempfile::tempdir().unwrap();
        let file_path = dir.path().join("hello.txt");
        std::fs::write(&file_path, b"hello").unwrap();

        let backing = BackingDir::open(dir.path().to_str().unwrap()).unwrap();
        let (size, is_regular) = backing.stat("hello.txt").unwrap();
        assert_eq!(size, 5);
        assert!(is_regular);

        assert!(backing.stat("nonexistent.txt").is_none());
    }

    #[test]
    fn read_file_full() {
        let dir = tempfile::tempdir().unwrap();
        let file_path = dir.path().join("data.txt");
        std::fs::write(&file_path, b"hello world").unwrap();

        let backing = BackingDir::open(dir.path().to_str().unwrap()).unwrap();
        let data = backing.read_file("data.txt", 0, 100);
        assert_eq!(data, b"hello world");
    }

    #[test]
    fn read_file_with_offset() {
        let dir = tempfile::tempdir().unwrap();
        let file_path = dir.path().join("data.txt");
        std::fs::write(&file_path, b"hello world").unwrap();

        let backing = BackingDir::open(dir.path().to_str().unwrap()).unwrap();
        let data = backing.read_file("data.txt", 6, 5);
        assert_eq!(data, b"world");
    }

    #[test]
    fn list_files_returns_regular_only() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("a.txt"), b"a").unwrap();
        std::fs::write(dir.path().join("b.txt"), b"b").unwrap();
        std::fs::create_dir(dir.path().join("subdir")).unwrap();

        let backing = BackingDir::open(dir.path().to_str().unwrap()).unwrap();
        let mut files = backing.list_files();
        files.sort();
        assert_eq!(files, vec!["a.txt", "b.txt"]);
    }

    #[test]
    fn read_nonexistent_returns_empty() {
        let dir = tempfile::tempdir().unwrap();
        let backing = BackingDir::open(dir.path().to_str().unwrap()).unwrap();
        let data = backing.read_file("nope.txt", 0, 100);
        assert!(data.is_empty());
    }
}
