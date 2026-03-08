use std::collections::HashMap;
use std::fs;
use std::io::{Read, Seek, SeekFrom};
use std::path::PathBuf;

#[derive(Clone)]
pub enum FileEntry {
    Static { content: Vec<u8> },
    Concat { source_paths: Vec<PathBuf> },
}

impl FileEntry {
    pub fn new_static(content: Vec<u8>) -> Self {
        FileEntry::Static { content }
    }

    pub fn new_concat(source_paths: Vec<PathBuf>) -> Self {
        FileEntry::Concat { source_paths }
    }

    pub fn total_size(&self) -> u64 {
        match self {
            FileEntry::Static { content } => content.len() as u64,
            FileEntry::Concat { source_paths } => {
                source_paths
                    .iter()
                    .filter_map(|p| fs::metadata(p).ok())
                    .map(|m| m.len())
                    .sum()
            }
        }
    }

    pub fn read_range(&self, offset: u64, size: u32) -> Vec<u8> {
        match self {
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
                read_concat_range(source_paths, offset, size)
            }
        }
    }
}

fn read_concat_range(source_paths: &[PathBuf], offset: u64, size: u32) -> Vec<u8> {
    let mut result = Vec::new();
    let mut remaining = size as u64;
    let mut cursor = 0u64; // cumulative offset across all files

    for path in source_paths {
        if remaining == 0 {
            break;
        }

        let file_len = match fs::metadata(path) {
            Ok(m) => m.len(),
            Err(e) => {
                log::warn!("Concat: skipping missing/unreadable file {:?}: {}", path, e);
                continue;
            }
        };

        let file_end = cursor + file_len;

        if offset >= file_end {
            // Requested range starts after this file
            cursor = file_end;
            continue;
        }

        // How far into this file do we start reading?
        let local_offset = if offset > cursor { offset - cursor } else { 0 };
        let bytes_available = file_len - local_offset;
        let to_read = remaining.min(bytes_available);

        match fs::File::open(path) {
            Ok(mut f) => {
                if local_offset > 0 {
                    if let Err(e) = f.seek(SeekFrom::Start(local_offset)) {
                        log::warn!("Concat: seek failed on {:?}: {}", path, e);
                        cursor = file_end;
                        continue;
                    }
                }
                let mut buf = vec![0u8; to_read as usize];
                match f.read_exact(&mut buf) {
                    Ok(()) => {
                        result.extend_from_slice(&buf);
                        remaining -= to_read;
                    }
                    Err(e) => {
                        log::warn!("Concat: read failed on {:?}: {}", path, e);
                    }
                }
            }
            Err(e) => {
                log::warn!("Concat: failed to open {:?}: {}", path, e);
            }
        }

        cursor = file_end;
    }

    result
}

pub struct FileMapping {
    files: HashMap<String, FileEntry>,
}

impl FileMapping {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
    }

    fn normalize_path(path: &str) -> String {
        if path.starts_with('/') {
            path[1..].to_string()
        } else {
            path.to_string()
        }
    }

    pub fn register(&mut self, path: &str, content: Vec<u8>) {
        let normalized = Self::normalize_path(path);
        self.files.insert(normalized, FileEntry::new_static(content));
    }

    pub fn register_concat(&mut self, path: &str, source_paths: Vec<PathBuf>) {
        let normalized = Self::normalize_path(path);
        self.files.insert(normalized, FileEntry::new_concat(source_paths));
    }

    pub fn get(&self, path: &str) -> Option<&FileEntry> {
        let normalized = if path.starts_with('/') {
            &path[1..]
        } else {
            path
        };
        self.files.get(normalized)
    }

    pub fn get_mut(&mut self, path: &str) -> Option<&mut FileEntry> {
        let normalized = Self::normalize_path(path);
        self.files.get_mut(&normalized)
    }

    pub fn remove(&mut self, path: &str) -> Option<FileEntry> {
        let normalized = Self::normalize_path(path);
        self.files.remove(&normalized)
    }

    pub fn list_files(&self) -> Vec<String> {
        self.files.keys().cloned().collect()
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

    #[test]
    fn static_entry_total_size() {
        let entry = FileEntry::new_static(b"hello".to_vec());
        assert_eq!(entry.total_size(), 5);
    }

    #[test]
    fn static_entry_read_range() {
        let entry = FileEntry::new_static(b"hello world".to_vec());
        assert_eq!(entry.read_range(0, 5), b"hello");
        assert_eq!(entry.read_range(6, 5), b"world");
        assert_eq!(entry.read_range(6, 100), b"world");
        assert_eq!(entry.read_range(100, 5), b"");
    }

    #[test]
    fn concat_total_size() {
        let (_f1, p1) = create_temp_file(b"aaa");
        let (_f2, p2) = create_temp_file(b"bbbbb");
        let entry = FileEntry::new_concat(vec![p1, p2]);
        assert_eq!(entry.total_size(), 8);
    }

    #[test]
    fn concat_read_within_one_file() {
        let (_f1, p1) = create_temp_file(b"hello");
        let (_f2, p2) = create_temp_file(b"world");
        let entry = FileEntry::new_concat(vec![p1, p2]);
        assert_eq!(entry.read_range(0, 5), b"hello");
    }

    #[test]
    fn concat_read_spanning_two_files() {
        let (_f1, p1) = create_temp_file(b"hello");
        let (_f2, p2) = create_temp_file(b"world");
        let entry = FileEntry::new_concat(vec![p1, p2]);
        assert_eq!(entry.read_range(3, 4), b"lowo");
    }

    #[test]
    fn concat_read_second_file_only() {
        let (_f1, p1) = create_temp_file(b"hello");
        let (_f2, p2) = create_temp_file(b"world");
        let entry = FileEntry::new_concat(vec![p1, p2]);
        assert_eq!(entry.read_range(5, 5), b"world");
    }

    #[test]
    fn concat_read_past_end() {
        let (_f1, p1) = create_temp_file(b"hello");
        let (_f2, p2) = create_temp_file(b"world");
        let entry = FileEntry::new_concat(vec![p1, p2]);
        assert_eq!(entry.read_range(100, 5), b"");
    }

    #[test]
    fn concat_missing_file_skipped() {
        let (_f1, p1) = create_temp_file(b"hello");
        let missing = PathBuf::from("/tmp/nonexistent-slosh-fuse-test-file");
        let (_f2, p2) = create_temp_file(b"world");
        let entry = FileEntry::new_concat(vec![p1, missing, p2]);
        // total_size skips missing
        assert_eq!(entry.total_size(), 10);
        // read spans across present files, skipping missing
        assert_eq!(entry.read_range(0, 10), b"helloworld");
    }

    #[test]
    fn concat_reflects_file_changes() {
        let (mut f1, p1) = create_temp_file(b"old");
        let (_f2, p2) = create_temp_file(b"data");
        let entry = FileEntry::new_concat(vec![p1, p2]);
        assert_eq!(entry.total_size(), 7);
        assert_eq!(entry.read_range(0, 7), b"olddata");

        // Overwrite first file with longer content
        f1.as_file_mut().set_len(0).unwrap();
        f1.seek(SeekFrom::Start(0)).unwrap();
        f1.write_all(b"newcontent").unwrap();
        f1.flush().unwrap();

        assert_eq!(entry.total_size(), 13); // 10 + 3... wait: "newcontent" = 10, "data" = 4
        assert_eq!(entry.read_range(0, 14), b"newcontentdata");
    }

    #[test]
    fn file_mapping_register_concat() {
        let (_f1, p1) = create_temp_file(b"aaa");
        let (_f2, p2) = create_temp_file(b"bbb");

        let mut mapping = FileMapping::new();
        mapping.register_concat("combined.txt", vec![p1, p2]);

        let entry = mapping.get("combined.txt").unwrap();
        assert_eq!(entry.total_size(), 6);
        assert_eq!(entry.read_range(0, 6), b"aaabbb");
    }
}
