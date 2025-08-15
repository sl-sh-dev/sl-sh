use fuser::{
    FileAttr, FileType, Filesystem, ReplyAttr, ReplyData, ReplyDirectory, ReplyEntry,
    Request, FUSE_ROOT_ID,
};
use libc::{ENOENT, ENOTDIR};
use std::collections::HashMap;
use std::ffi::OsStr;
use std::sync::{Arc, Mutex};
use std::time::{Duration, SystemTime};
use std::os::unix::io::{RawFd, FromRawFd, IntoRawFd};
use std::io::{Read, Write};

use crate::file_mapping::FileMapping;

const TTL: Duration = Duration::from_secs(1);

pub struct EvalFs {
    file_mapping: Arc<Mutex<FileMapping>>,
    inode_to_path: HashMap<u64, String>,
    path_to_inode: HashMap<String, u64>,
    next_inode: u64,
    eval_pipe_read: RawFd,
    eval_pipe_write: RawFd,
}

impl EvalFs {
    pub fn new(file_mapping: Arc<Mutex<FileMapping>>, eval_pipe_read: RawFd, eval_pipe_write: RawFd) -> Self {
        let mut fs = Self {
            file_mapping,
            inode_to_path: HashMap::new(),
            path_to_inode: HashMap::new(),
            next_inode: 2, // 1 is reserved for root
            eval_pipe_read,
            eval_pipe_write,
        };

        // Pre-populate inodes for registered files
        {
            let mapping = fs.file_mapping.lock().unwrap();
            let paths = mapping.list_files();
            drop(mapping);
            for path in paths {
                fs.allocate_inode(&path);
            }
        }

        fs
    }

    fn allocate_inode(&mut self, path: &str) -> u64 {
        if let Some(&inode) = self.path_to_inode.get(path) {
            return inode;
        }

        let inode = self.next_inode;
        self.next_inode += 1;
        self.inode_to_path.insert(inode, path.to_string());
        self.path_to_inode.insert(path.to_string(), inode);
        inode
    }

    fn get_inode_for_path(&self, path: &str) -> Option<u64> {
        self.path_to_inode.get(path).copied()
    }

    fn get_path_for_inode(&self, inode: u64) -> Option<&str> {
        self.inode_to_path.get(&inode).map(|s| s.as_str())
    }

    fn evaluate_expression(&self, expression: &str) -> String {
        // Send expression to parent process for evaluation
        let msg = format!("{}\n", expression);
        let mut pipe_write = unsafe { std::fs::File::from_raw_fd(self.eval_pipe_write) };
        if let Err(e) = pipe_write.write_all(msg.as_bytes()) {
            log::error!("Failed to send expression for evaluation: {}", e);
            // Prevent closing the fd when File is dropped
            let _ = pipe_write.into_raw_fd();
            return format!("ERROR: {}", e);
        }
        // Prevent closing the fd when File is dropped
        let _ = pipe_write.into_raw_fd();

        // Read response
        let mut pipe_read = unsafe { std::fs::File::from_raw_fd(self.eval_pipe_read) };
        let mut response = String::new();
        let mut buffer = [0u8; 4096];

        loop {
            match pipe_read.read(&mut buffer) {
                Ok(0) => break,
                Ok(n) => {
                    response.push_str(&String::from_utf8_lossy(&buffer[..n]));
                    if response.ends_with('\n') {
                        response.pop(); // Remove trailing newline
                        break;
                    }
                }
                Err(e) => {
                    log::error!("Failed to read evaluation result: {}", e);
                    // Prevent closing the fd when File is dropped
                    let _ = pipe_read.into_raw_fd();
                    return format!("ERROR: {}", e);
                }
            }
        }
        // Prevent closing the fd when File is dropped
        let _ = pipe_read.into_raw_fd();

        response
    }

    fn file_attr(inode: u64, size: u64) -> FileAttr {
        let now = SystemTime::now();
        FileAttr {
            ino: inode,
            size,
            blocks: 1,
            atime: now,
            mtime: now,
            ctime: now,
            crtime: now,
            kind: FileType::RegularFile,
            perm: 0o644,
            nlink: 1,
            uid: unsafe { libc::getuid() },
            gid: unsafe { libc::getgid() },
            rdev: 0,
            blksize: 512,
            flags: 0,
        }
    }

    fn dir_attr(inode: u64) -> FileAttr {
        let now = SystemTime::now();
        FileAttr {
            ino: inode,
            size: 0,
            blocks: 0,
            atime: now,
            mtime: now,
            ctime: now,
            crtime: now,
            kind: FileType::Directory,
            perm: 0o755,
            nlink: 2,
            uid: unsafe { libc::getuid() },
            gid: unsafe { libc::getgid() },
            rdev: 0,
            blksize: 512,
            flags: 0,
        }
    }
}

impl Filesystem for EvalFs {
    fn lookup(&mut self, _req: &Request, parent: u64, name: &OsStr, reply: ReplyEntry) {
        if parent != FUSE_ROOT_ID {
            reply.error(ENOENT);
            return;
        }

        let name_str = name.to_string_lossy();

        let found = {
            let mapping = self.file_mapping.lock().unwrap();
            mapping.get(&name_str).is_some()
        };

        if found {
            let inode = self.allocate_inode(&name_str);
            reply.entry(&TTL, &Self::file_attr(inode, 0), 0);
        } else {
            reply.error(ENOENT);
        }
    }

    fn getattr(&mut self, _req: &Request, ino: u64, reply: ReplyAttr) {
        if ino == FUSE_ROOT_ID {
            reply.attr(&TTL, &Self::dir_attr(FUSE_ROOT_ID));
        } else if let Some(path) = self.get_path_for_inode(ino) {
            let (is_valid, expression) = {
                let mapping = self.file_mapping.lock().unwrap();
                if let Some(entry) = mapping.get(path) {
                    if entry.is_cache_valid() {
                        let content = entry.cached_value.as_ref().unwrap().clone();
                        reply.attr(&TTL, &Self::file_attr(ino, content.len() as u64));
                        return;
                    }
                    (true, entry.expression.clone())
                } else {
                    (false, String::new())
                }
            };

            if is_valid {
                let content = self.evaluate_expression(&expression);
                reply.attr(&TTL, &Self::file_attr(ino, content.len() as u64));
            } else {
                reply.error(ENOENT);
            }
        } else {
            reply.error(ENOENT);
        }
    }

    fn read(
        &mut self,
        _req: &Request,
        ino: u64,
        _fh: u64,
        offset: i64,
        size: u32,
        _flags: i32,
        _lock: Option<u64>,
        reply: ReplyData,
    ) {
        if let Some(path) = self.get_path_for_inode(ino) {
            let mut mapping = self.file_mapping.lock().unwrap();
            if let Some(entry) = mapping.get_mut(path) {
                let content = if entry.is_cache_valid() {
                    entry.cached_value.as_ref().unwrap().clone()
                } else {
                    let expr = entry.expression.clone();
                    drop(mapping);
                    let result = self.evaluate_expression(&expr);

                    // Update cache if caching is enabled
                    let mut mapping = self.file_mapping.lock().unwrap();
                    if let Some(entry) = mapping.get_mut(path) {
                        if entry.cache_ttl_secs > 0 {
                            entry.update_cache(result.clone());
                        }
                    }
                    result
                };

                let data = content.as_bytes();
                let start = offset as usize;
                let end = (start + size as usize).min(data.len());

                if start < data.len() {
                    reply.data(&data[start..end]);
                } else {
                    reply.data(&[]);
                }
            } else {
                reply.error(ENOENT);
            }
        } else {
            reply.error(ENOENT);
        }
    }

    fn readdir(
        &mut self,
        _req: &Request,
        ino: u64,
        _fh: u64,
        offset: i64,
        mut reply: ReplyDirectory,
    ) {
        if ino != FUSE_ROOT_ID {
            reply.error(ENOTDIR);
            return;
        }

        let entries = vec![
            (FUSE_ROOT_ID, FileType::Directory, "."),
            (FUSE_ROOT_ID, FileType::Directory, ".."),
        ];

        for (i, (inode, file_type, name)) in entries.iter().enumerate() {
            if offset <= i as i64 {
                if reply.add(*inode, (i + 1) as i64, *file_type, name) {
                    break;
                }
            }
        }

        let mapping = self.file_mapping.lock().unwrap();
        let files = mapping.list_files();
        drop(mapping);

        for (i, path) in files.iter().enumerate() {
            let index = i + entries.len();
            if offset <= index as i64 {
                let inode = self.allocate_inode(path);
                if reply.add(inode, (index + 1) as i64, FileType::RegularFile, path) {
                    break;
                }
            }
        }

        reply.ok();
    }
}