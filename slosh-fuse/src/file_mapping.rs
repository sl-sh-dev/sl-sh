use std::collections::HashMap;
use std::time::SystemTime;

#[derive(Clone)]
pub struct FileEntry {
    pub expression: String,
    pub cached_value: Option<String>,
    pub cache_time: Option<SystemTime>,
    pub cache_ttl_secs: u64,
}

impl FileEntry {
    pub fn new(expression: String) -> Self {
        Self {
            expression,
            cached_value: None,
            cache_time: None,
            cache_ttl_secs: 0, // No caching by default
        }
    }

    pub fn is_cache_valid(&self) -> bool {
        if self.cache_ttl_secs == 0 {
            return false;
        }

        if let (Some(cached_time), Some(_)) = (self.cache_time, &self.cached_value) {
            if let Ok(elapsed) = cached_time.elapsed() {
                return elapsed.as_secs() < self.cache_ttl_secs;
            }
        }
        false
    }

    pub fn update_cache(&mut self, value: String) {
        self.cached_value = Some(value);
        self.cache_time = Some(SystemTime::now());
    }
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

    pub fn register(&mut self, path: &str, expression: String) {
        let normalized_path = if path.starts_with('/') {
            path[1..].to_string()
        } else {
            path.to_string()
        };

        self.files.insert(normalized_path, FileEntry::new(expression));
    }

    pub fn get(&self, path: &str) -> Option<&FileEntry> {
        let normalized_path = if path.starts_with('/') {
            &path[1..]
        } else {
            path
        };

        self.files.get(normalized_path)
    }

    pub fn get_mut(&mut self, path: &str) -> Option<&mut FileEntry> {
        let normalized_path = if path.starts_with('/') {
            path[1..].to_string()
        } else {
            path.to_string()
        };

        self.files.get_mut(&normalized_path)
    }

    pub fn remove(&mut self, path: &str) -> Option<FileEntry> {
        let normalized_path = if path.starts_with('/') {
            path[1..].to_string()
        } else {
            path.to_string()
        };

        self.files.remove(&normalized_path)
    }

    pub fn list_files(&self) -> Vec<String> {
        self.files.keys().cloned().collect()
    }
}