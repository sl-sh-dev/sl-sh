use fuser::{
    FileAttr, FileType, Filesystem, ReplyAttr, ReplyData, ReplyDirectory, ReplyEntry,
    Request, FUSE_ROOT_ID,
};
use libc::{ENOENT, ENOTDIR};
use std::collections::HashMap;
use std::ffi::OsStr;
use std::sync::{Arc, Mutex};
use std::time::{Duration, SystemTime};

use crate::file_mapping::FileMapping;
use crate::resolve::{FileResolver, MountRegistry};

const TTL: Duration = Duration::from_secs(1);

pub struct EvalFs {
    file_mapping: Arc<Mutex<FileMapping>>,
    registry: MountRegistry,
    inode_to_path: HashMap<u64, String>,
    path_to_inode: HashMap<String, u64>,
    next_inode: u64,
}

impl EvalFs {
    pub fn new(file_mapping: Arc<Mutex<FileMapping>>, registry: MountRegistry) -> Self {
        let mut fs = Self {
            file_mapping,
            registry,
            inode_to_path: HashMap::new(),
            path_to_inode: HashMap::new(),
            next_inode: 2, // 1 is reserved for root
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

    fn get_path_for_inode(&self, inode: u64) -> Option<&str> {
        self.inode_to_path.get(&inode).map(|s| s.as_str())
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

        let entry = {
            let mapping = self.file_mapping.lock().unwrap();
            mapping.get(&name_str).cloned()
        };

        if let Some(entry) = entry {
            let mut resolver = FileResolver::new(&self.registry);
            let size = resolver.total_size(&entry);
            let inode = self.allocate_inode(&name_str);
            reply.entry(&TTL, &Self::file_attr(inode, size), 0);
        } else {
            reply.error(ENOENT);
        }
    }

    fn getattr(&mut self, _req: &Request, ino: u64, reply: ReplyAttr) {
        if ino == FUSE_ROOT_ID {
            reply.attr(&TTL, &Self::dir_attr(FUSE_ROOT_ID));
        } else if let Some(path) = self.get_path_for_inode(ino) {
            let entry = {
                let mapping = self.file_mapping.lock().unwrap();
                mapping.get(path).cloned()
            };

            if let Some(entry) = entry {
                let mut resolver = FileResolver::new(&self.registry);
                let size = resolver.total_size(&entry);
                reply.attr(&TTL, &Self::file_attr(ino, size));
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
            let entry = {
                let mapping = self.file_mapping.lock().unwrap();
                mapping.get(path).cloned()
            };
            if let Some(entry) = entry {
                let mut resolver = FileResolver::new(&self.registry);
                let data = resolver.read_range(&entry, offset as u64, size);
                reply.data(&data);
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
