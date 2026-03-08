use crate::eval_fs::EvalFs;
use crate::file_mapping::FileMapping;
use crate::resolve::MountRegistry;
use fuser::{MountOption, Session};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::thread::JoinHandle;

struct MountState {
    mount_point: PathBuf,
    mapping: Arc<Mutex<FileMapping>>,
    _thread: JoinHandle<()>,
}

pub struct MountManager {
    mounts: HashMap<String, MountState>,
    next_id: u64,
    registry: MountRegistry,
}

impl MountManager {
    pub fn new() -> Self {
        Self {
            mounts: HashMap::new(),
            next_id: 1,
            registry: MountRegistry::new(),
        }
    }

    /// Mount a new FUSE filesystem. Returns a mount-id string.
    pub fn mount(&mut self, mount_point: &str) -> Result<String, String> {
        let path = PathBuf::from(mount_point);

        // Create mount point if needed
        if !path.exists() {
            std::fs::create_dir_all(&path)
                .map_err(|e| format!("failed to create mount point: {}", e))?;
        }

        let mapping = Arc::new(Mutex::new(FileMapping::new()));
        let mapping_for_fuse = Arc::clone(&mapping);
        let mount_path = path.clone();

        let mount_id = format!("mount-{}", self.next_id);
        self.next_id += 1;

        self.registry.add(&path, Arc::clone(&mapping));
        let registry_for_fuse = self.registry.clone();

        let thread = std::thread::Builder::new()
            .name(format!("fuse-{}", mount_id))
            .spawn(move || {
                let fs = EvalFs::new(mapping_for_fuse, registry_for_fuse);
                let options = vec![
                    MountOption::FSName("slosh-eval".to_string()),
                    MountOption::AutoUnmount,
                    MountOption::AllowOther,
                ];

                match Session::new(fs, mount_path.as_ref(), &options) {
                    Ok(mut session) => {
                        log::info!("FUSE session started for {:?}", mount_path);
                        if let Err(e) = session.run() {
                            log::error!("FUSE session error for {:?}: {}", mount_path, e);
                        }
                        log::info!("FUSE session ended for {:?}", mount_path);
                    }
                    Err(e) => {
                        log::error!("Failed to create FUSE session for {:?}: {}", mount_path, e);
                    }
                }
            })
            .map_err(|e| format!("failed to spawn FUSE thread: {}", e))?;

        self.mounts.insert(
            mount_id.clone(),
            MountState {
                mount_point: path,
                mapping,
                _thread: thread,
            },
        );

        Ok(mount_id)
    }

    /// Get the FileMapping for a given mount-id.
    pub fn get_mapping(&self, mount_id: &str) -> Option<Arc<Mutex<FileMapping>>> {
        self.mounts.get(mount_id).map(|s| Arc::clone(&s.mapping))
    }

    /// Unmount a FUSE filesystem using fusermount3.
    pub fn unmount(&mut self, mount_id: &str) -> Result<(), String> {
        let state = self.mounts.remove(mount_id)
            .ok_or_else(|| format!("unknown mount-id: {}", mount_id))?;

        self.registry.remove(&state.mount_point);

        let mount_str = state.mount_point.to_string_lossy().to_string();
        Self::fusermount_unmount(&mount_str)?;

        // The FUSE thread will exit once the session is unmounted.
        // We don't join it here because fusermount3 -u is async with
        // respect to the FUSE thread finishing.

        Ok(())
    }

    /// Unmount all active mounts. Used during shutdown.
    pub fn unmount_all(&mut self) {
        let ids: Vec<String> = self.mounts.keys().cloned().collect();
        for id in ids {
            if let Err(e) = self.unmount(&id) {
                log::error!("Failed to unmount {}: {}", id, e);
            }
        }
    }

    /// List mount IDs.
    pub fn list_mounts(&self) -> Vec<(String, String)> {
        self.mounts
            .iter()
            .map(|(id, state)| {
                (id.clone(), state.mount_point.to_string_lossy().to_string())
            })
            .collect()
    }

    fn fusermount_unmount(mount_point: &str) -> Result<(), String> {
        let status = std::process::Command::new("fusermount3")
            .arg("-u")
            .arg(mount_point)
            .status()
            .map_err(|e| format!("failed to run fusermount3: {}", e))?;

        if status.success() {
            Ok(())
        } else {
            Err(format!(
                "fusermount3 -u {} exited with {}",
                mount_point,
                status.code().unwrap_or(-1)
            ))
        }
    }
}
