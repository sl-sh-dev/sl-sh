use std::collections::HashSet;
use std::fs;
use std::io::{Read, Seek, SeekFrom};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use crate::file_mapping::{FileEntry, FileMapping};

struct MountInfo {
    original_mount: PathBuf,
    canonical_mount: PathBuf,
    mapping: Arc<Mutex<FileMapping>>,
}

/// Shared registry of active FUSE mount points. Clone is cheap (Arc).
#[derive(Clone)]
pub struct MountRegistry {
    mounts: Arc<Mutex<Vec<MountInfo>>>,
}

impl MountRegistry {
    pub fn new() -> Self {
        Self {
            mounts: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Register a mount point and its file mapping.
    pub fn add(&self, path: &Path, mapping: Arc<Mutex<FileMapping>>) {
        let canonical = fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
        let mut mounts = self.mounts.lock().unwrap();
        mounts.push(MountInfo {
            original_mount: path.to_path_buf(),
            canonical_mount: canonical,
            mapping,
        });
    }

    /// Unregister a mount point.
    pub fn remove(&self, path: &Path) {
        let mut mounts = self.mounts.lock().unwrap();
        mounts.retain(|m| m.original_mount != path);
    }

    /// Check if `path` is under any registered mount. Returns the virtual
    /// path (relative to mount root) and the mapping if found.
    /// Checks both original and canonical mount paths.
    /// Lock is held briefly; the Arc<Mutex<FileMapping>> is cloned out.
    fn resolve_by_prefix(&self, path: &Path) -> Option<(String, Arc<Mutex<FileMapping>>)> {
        let mounts = self.mounts.lock().unwrap();
        for info in mounts.iter() {
            if let Ok(rel) = path.strip_prefix(&info.original_mount) {
                let vpath = rel.to_string_lossy().to_string();
                return Some((vpath, Arc::clone(&info.mapping)));
            }
            if let Ok(rel) = path.strip_prefix(&info.canonical_mount) {
                let vpath = rel.to_string_lossy().to_string();
                return Some((vpath, Arc::clone(&info.mapping)));
            }
        }
        None
    }
}

/// Per-request file resolver with cycle detection.
/// Created fresh for each FUSE read/lookup to track visited paths.
pub struct FileResolver<'a> {
    registry: &'a MountRegistry,
    visited: HashSet<PathBuf>,
}

impl<'a> FileResolver<'a> {
    pub fn new(registry: &'a MountRegistry) -> Self {
        Self {
            registry,
            visited: HashSet::new(),
        }
    }

    /// Compute total size for a FileEntry, resolving internal paths.
    pub fn total_size(&mut self, entry: &FileEntry) -> u64 {
        match entry {
            FileEntry::Static { content } => content.len() as u64,
            FileEntry::Concat { source_paths } => {
                source_paths.iter().map(|p| self.file_size(p)).sum()
            }
        }
    }

    /// Read a range from a FileEntry, resolving internal paths.
    pub fn read_range(&mut self, entry: &FileEntry, offset: u64, size: u32) -> Vec<u8> {
        match entry {
            FileEntry::Static { content } => {
                let start = offset as usize;
                let end = (start + size as usize).min(content.len());
                if start < content.len() {
                    content[start..end].to_vec()
                } else {
                    Vec::new()
                }
            }
            FileEntry::Concat { source_paths } => {
                self.read_concat(source_paths, offset, size)
            }
        }
    }

    /// Get the size of a file at `path`, resolving internal paths and detecting cycles.
    fn file_size(&mut self, path: &Path) -> u64 {
        if self.visited.contains(path) {
            log::warn!("Cycle detected resolving size for {:?}", path);
            return 0;
        }

        if let Some((vpath, mapping)) = self.try_resolve_internal(path) {
            self.visited.insert(path.to_path_buf());
            let entry = {
                let m = mapping.lock().unwrap();
                m.get(&vpath).cloned()
            };
            let result = match entry {
                Some(entry) => self.total_size(&entry),
                None => 0,
            };
            self.visited.remove(path);
            return result;
        }

        // External file — use filesystem metadata
        fs::metadata(path).map(|m| m.len()).unwrap_or(0)
    }

    /// Read a range spanning multiple source files (concat logic).
    fn read_concat(&mut self, source_paths: &[PathBuf], offset: u64, size: u32) -> Vec<u8> {
        let mut result = Vec::new();
        let mut remaining = size as u64;
        let mut cursor = 0u64;

        for path in source_paths {
            if remaining == 0 {
                break;
            }

            let file_len = self.file_size(path);
            if file_len == 0 {
                continue;
            }

            let file_end = cursor + file_len;

            if offset >= file_end {
                cursor = file_end;
                continue;
            }

            let local_offset = if offset > cursor { offset - cursor } else { 0 };
            let bytes_available = file_len - local_offset;
            let to_read = remaining.min(bytes_available);

            let data = self.read_file(path, local_offset, to_read as u32);
            remaining -= data.len() as u64;
            result.extend_from_slice(&data);

            cursor = file_end;
        }

        result
    }

    /// Read bytes from a file, resolving internal paths and detecting cycles.
    fn read_file(&mut self, path: &Path, offset: u64, size: u32) -> Vec<u8> {
        if self.visited.contains(path) {
            log::warn!("Cycle detected reading {:?}", path);
            return Vec::new();
        }

        if let Some((vpath, mapping)) = self.try_resolve_internal(path) {
            self.visited.insert(path.to_path_buf());
            let entry = {
                let m = mapping.lock().unwrap();
                m.get(&vpath).cloned()
            };
            let result = match entry {
                Some(entry) => self.read_range(&entry, offset, size),
                None => Vec::new(),
            };
            self.visited.remove(path);
            return result;
        }

        // External file
        read_external_file(path, offset, size)
    }

    /// Try to resolve a path as internal to a FUSE mount.
    /// First tries raw prefix match (no syscalls, safe for mount-internal paths).
    /// Then tries fs::canonicalize on the path — only safe because we only
    /// reach here if the raw prefix match failed, meaning the path is NOT
    /// directly under a mount (could be a symlink pointing into one).
    fn try_resolve_internal(&self, path: &Path) -> Option<(String, Arc<Mutex<FileMapping>>)> {
        // Raw prefix match first — no syscalls, no deadlock risk
        if let Some(result) = self.registry.resolve_by_prefix(path) {
            return Some(result);
        }

        // Canonicalize fallback — safe because path is NOT under a mount
        // (raw prefix match would have caught it). This handles symlinks
        // pointing into a mount from outside.
        if let Ok(canonical) = fs::canonicalize(path) {
            if canonical != path {
                return self.registry.resolve_by_prefix(&canonical);
            }
        }

        None
    }
}

/// Read bytes from an external (non-FUSE) file on disk.
pub fn read_external_file(path: &Path, offset: u64, size: u32) -> Vec<u8> {
    let mut file = match fs::File::open(path) {
        Ok(f) => f,
        Err(e) => {
            log::warn!("Failed to open external file {:?}: {}", path, e);
            return Vec::new();
        }
    };

    if offset > 0 {
        if let Err(e) = file.seek(SeekFrom::Start(offset)) {
            log::warn!("Seek failed on {:?}: {}", path, e);
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
            log::warn!("Read failed on {:?}: {}", path, e);
            Vec::new()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    fn create_temp_file(content: &[u8]) -> (tempfile::NamedTempFile, PathBuf) {
        let mut f = tempfile::NamedTempFile::new().unwrap();
        f.write_all(content).unwrap();
        f.flush().unwrap();
        let path = f.path().to_path_buf();
        (f, path)
    }

    fn make_registry() -> MountRegistry {
        MountRegistry::new()
    }

    fn make_mapping() -> Arc<Mutex<FileMapping>> {
        Arc::new(Mutex::new(FileMapping::new()))
    }

    // Test 1: Static entry passthrough
    #[test]
    fn static_entry_passthrough() {
        let registry = make_registry();
        let mut resolver = FileResolver::new(&registry);
        let entry = FileEntry::new_static(b"hello world".to_vec());

        assert_eq!(resolver.total_size(&entry), 11);
        assert_eq!(resolver.read_range(&entry, 0, 5), b"hello");
        assert_eq!(resolver.read_range(&entry, 6, 5), b"world");
        assert_eq!(resolver.read_range(&entry, 100, 5), b"");
    }

    // Test 2: Concat of external files
    #[test]
    fn concat_external_files() {
        let registry = make_registry();
        let (_f1, p1) = create_temp_file(b"aaa");
        let (_f2, p2) = create_temp_file(b"bbb");
        let entry = FileEntry::new_concat(vec![p1, p2]);

        let mut resolver = FileResolver::new(&registry);
        assert_eq!(resolver.total_size(&entry), 6);
        assert_eq!(resolver.read_range(&entry, 0, 6), b"aaabbb");
        assert_eq!(resolver.read_range(&entry, 2, 3), b"ab");
    }

    // Test 3: Concat with internal static file
    #[test]
    fn concat_with_internal_static() {
        let mount_dir = tempfile::tempdir().unwrap();
        let mount_path = mount_dir.path();

        let mapping = make_mapping();
        {
            let mut m = mapping.lock().unwrap();
            m.register("inner.txt", b"INNER".to_vec());
        }

        let registry = make_registry();
        registry.add(mount_path, Arc::clone(&mapping));

        let (_f_ext, p_ext) = create_temp_file(b"EXT");
        let internal_path = mount_path.join("inner.txt");

        let entry = FileEntry::new_concat(vec![p_ext, internal_path]);
        let mut resolver = FileResolver::new(&registry);

        assert_eq!(resolver.total_size(&entry), 8); // 3 + 5
        assert_eq!(resolver.read_range(&entry, 0, 8), b"EXTINNER");
    }

    // Test 4: Concat with internal concat (recursive)
    #[test]
    fn concat_recursive_internal() {
        let mount_dir = tempfile::tempdir().unwrap();
        let mount_path = mount_dir.path();

        let (_f_ext, p_ext) = create_temp_file(b"EXT");

        let mapping = make_mapping();
        {
            let mut m = mapping.lock().unwrap();
            // inner_concat.txt is itself a concat of an external file + static
            m.register("leaf.txt", b"LEAF".to_vec());
            m.register_concat(
                "inner_concat.txt",
                vec![p_ext.clone(), mount_path.join("leaf.txt")],
            );
        }

        let registry = make_registry();
        registry.add(mount_path, Arc::clone(&mapping));

        // Top-level concat references inner_concat.txt
        let entry = FileEntry::new_concat(vec![mount_path.join("inner_concat.txt")]);
        let mut resolver = FileResolver::new(&registry);

        assert_eq!(resolver.total_size(&entry), 7); // EXT(3) + LEAF(4)
        assert_eq!(resolver.read_range(&entry, 0, 7), b"EXTLEAF");
    }

    // Test 5: Direct cycle (A -> A) returns empty
    #[test]
    fn direct_cycle_returns_empty() {
        let mount_dir = tempfile::tempdir().unwrap();
        let mount_path = mount_dir.path();

        let mapping = make_mapping();
        {
            let mut m = mapping.lock().unwrap();
            // a.txt sources from itself
            m.register_concat("a.txt", vec![mount_path.join("a.txt")]);
        }

        let registry = make_registry();
        registry.add(mount_path, Arc::clone(&mapping));

        let entry = FileEntry::new_concat(vec![mount_path.join("a.txt")]);
        let mut resolver = FileResolver::new(&registry);

        assert_eq!(resolver.total_size(&entry), 0);
        assert_eq!(resolver.read_range(&entry, 0, 100), b"");
    }

    // Test 6: Indirect cycle (A -> B -> A) returns empty
    #[test]
    fn indirect_cycle_returns_empty() {
        let mount_dir = tempfile::tempdir().unwrap();
        let mount_path = mount_dir.path();

        let mapping = make_mapping();
        {
            let mut m = mapping.lock().unwrap();
            m.register_concat("a.txt", vec![mount_path.join("b.txt")]);
            m.register_concat("b.txt", vec![mount_path.join("a.txt")]);
        }

        let registry = make_registry();
        registry.add(mount_path, Arc::clone(&mapping));

        let entry = FileEntry::new_concat(vec![mount_path.join("a.txt")]);
        let mut resolver = FileResolver::new(&registry);

        assert_eq!(resolver.total_size(&entry), 0);
        assert_eq!(resolver.read_range(&entry, 0, 100), b"");
    }

    // Test 7: Mixed internal/external total_size
    #[test]
    fn mixed_internal_external_size() {
        let mount_dir = tempfile::tempdir().unwrap();
        let mount_path = mount_dir.path();

        let (_f_ext, p_ext) = create_temp_file(b"12345");

        let mapping = make_mapping();
        {
            let mut m = mapping.lock().unwrap();
            m.register("internal.txt", b"abc".to_vec());
        }

        let registry = make_registry();
        registry.add(mount_path, Arc::clone(&mapping));

        let entry = FileEntry::new_concat(vec![
            p_ext,
            mount_path.join("internal.txt"),
        ]);
        let mut resolver = FileResolver::new(&registry);

        assert_eq!(resolver.total_size(&entry), 8); // 5 + 3
    }

    // Test 8: Symlink into mount detected via canonicalize
    #[test]
    fn symlink_into_mount_resolved_internally() {
        use std::os::unix::fs::symlink;

        let mount_dir = tempfile::tempdir().unwrap();
        let mount_path = mount_dir.path();

        // We need a real file at the mount path for canonicalize to resolve
        // the symlink target. Write a placeholder.
        let real_file = mount_path.join("inner.txt");
        fs::write(&real_file, b"PLACEHOLDER").unwrap();

        let mapping = make_mapping();
        {
            let mut m = mapping.lock().unwrap();
            m.register("inner.txt", b"INNER".to_vec());
        }

        let registry = make_registry();
        registry.add(mount_path, Arc::clone(&mapping));

        // Create symlink from outside the mount pointing into it
        let link_dir = tempfile::tempdir().unwrap();
        let link_path = link_dir.path().join("link.txt");
        symlink(&real_file, &link_path).unwrap();

        let entry = FileEntry::new_concat(vec![link_path]);
        let mut resolver = FileResolver::new(&registry);

        // Should resolve via canonicalize -> internal mapping, not read PLACEHOLDER
        assert_eq!(resolver.total_size(&entry), 5);
        assert_eq!(resolver.read_range(&entry, 0, 10), b"INNER");
    }

    // Test 9: Symlink cycle
    #[test]
    fn symlink_cycle_returns_empty() {
        use std::os::unix::fs::symlink;

        let mount_dir = tempfile::tempdir().unwrap();
        let mount_path = mount_dir.path();

        // Create placeholder file at mount path so canonicalize works
        let real_file = mount_path.join("a.txt");
        fs::write(&real_file, b"PLACEHOLDER").unwrap();

        // Create symlink elsewhere -> mount/a.txt
        let link_dir = tempfile::tempdir().unwrap();
        let link_path = link_dir.path().join("link.txt");
        symlink(&real_file, &link_path).unwrap();

        let mapping = make_mapping();
        {
            let mut m = mapping.lock().unwrap();
            // a.txt is a concat that sources from the symlink
            m.register_concat("a.txt", vec![link_path.clone()]);
        }

        let registry = make_registry();
        registry.add(mount_path, Arc::clone(&mapping));

        // Reading a.txt -> link.txt -> (canonicalize) -> mount/a.txt -> cycle!
        let entry = FileEntry::new_concat(vec![mount_path.join("a.txt")]);
        let mut resolver = FileResolver::new(&registry);

        assert_eq!(resolver.total_size(&entry), 0);
        assert_eq!(resolver.read_range(&entry, 0, 100), b"");
    }
}
