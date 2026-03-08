use base64::Engine;
use std::io::{self, BufRead, BufReader, Write};
use std::os::unix::net::UnixStream;
use std::path::Path;

/// Client for communicating with the FUSE daemon over a Unix socket.
pub struct DaemonClient {
    reader: BufReader<UnixStream>,
    writer: UnixStream,
}

impl DaemonClient {
    /// Connect to the daemon socket.
    pub fn connect(socket_path: &Path) -> io::Result<Self> {
        let stream = UnixStream::connect(socket_path)?;
        let reader = BufReader::new(stream.try_clone()?);
        Ok(Self {
            reader,
            writer: stream,
        })
    }

    /// Send a raw command line and read the response.
    /// Returns `Ok(response_fields)` on `OK\t...` or `Err` on `ERR\t...`.
    fn send_command(&mut self, cmd: &str) -> io::Result<Vec<String>> {
        self.writer.write_all(cmd.as_bytes())?;
        if !cmd.ends_with('\n') {
            self.writer.write_all(b"\n")?;
        }
        self.writer.flush()?;

        let mut line = String::new();
        self.reader.read_line(&mut line)?;
        let line = line.trim_end_matches('\n').trim_end_matches('\r');

        let parts: Vec<&str> = line.split('\t').collect();
        match parts.first().copied() {
            Some("OK") => Ok(parts[1..].iter().map(|s| s.to_string()).collect()),
            Some("ERR") => {
                let msg = parts[1..].join("\t");
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unexpected response: {}", line),
            )),
        }
    }

    /// Ping the daemon to verify it's alive.
    pub fn ping(&mut self) -> io::Result<()> {
        let fields = self.send_command("PING")?;
        if fields.first().map(|s| s.as_str()) == Some("PONG") {
            Ok(())
        } else {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "unexpected ping response",
            ))
        }
    }

    /// Request a new FUSE mount. Returns the mount-id.
    pub fn mount(&mut self, mount_point: &str) -> io::Result<String> {
        let fields = self.send_command(&format!("MOUNT\t{}", mount_point))?;
        fields.into_iter().next().ok_or_else(|| {
            io::Error::new(io::ErrorKind::InvalidData, "no mount-id in response")
        })
    }

    /// Request an overlay FUSE mount on an existing directory. Returns the mount-id.
    pub fn mount_overlay(&mut self, mount_point: &str) -> io::Result<String> {
        let fields = self.send_command(&format!("MOUNT_OVERLAY\t{}", mount_point))?;
        fields.into_iter().next().ok_or_else(|| {
            io::Error::new(io::ErrorKind::InvalidData, "no mount-id in response")
        })
    }

    /// Register a file with static content.
    pub fn register_file(
        &mut self,
        mount_id: &str,
        path: &str,
        content: &[u8],
    ) -> io::Result<()> {
        let b64 = base64::engine::general_purpose::STANDARD.encode(content);
        self.send_command(&format!("REGISTER\t{}\t{}\t{}", mount_id, path, b64))?;
        Ok(())
    }

    /// Register a concat file (virtual file backed by multiple real files).
    pub fn register_concat(
        &mut self,
        mount_id: &str,
        vpath: &str,
        source_paths: &[&str],
    ) -> io::Result<()> {
        let mut cmd = format!("CONCAT\t{}\t{}", mount_id, vpath);
        for p in source_paths {
            cmd.push('\t');
            cmd.push_str(p);
        }
        self.send_command(&cmd)?;
        Ok(())
    }

    /// Remove a file from a mount.
    pub fn remove_file(&mut self, mount_id: &str, path: &str) -> io::Result<()> {
        self.send_command(&format!("REMOVE\t{}\t{}", mount_id, path))?;
        Ok(())
    }

    /// Unmount a FUSE filesystem.
    pub fn unmount(&mut self, mount_id: &str) -> io::Result<()> {
        self.send_command(&format!("UNMOUNT\t{}", mount_id))?;
        Ok(())
    }

    /// List files in a mount.
    pub fn list_files(&mut self, mount_id: &str) -> io::Result<Vec<String>> {
        self.send_command(&format!("LIST\t{}", mount_id))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::net::UnixListener;

    fn mock_server(socket_path: &Path, response: &str) -> std::thread::JoinHandle<()> {
        let listener = UnixListener::bind(socket_path).unwrap();
        let resp = response.to_string();
        std::thread::spawn(move || {
            let (stream, _) = listener.accept().unwrap();
            let mut reader = BufReader::new(stream.try_clone().unwrap());
            let mut writer = stream;
            let mut line = String::new();
            reader.read_line(&mut line).unwrap();
            writer.write_all(resp.as_bytes()).unwrap();
            writer.write_all(b"\n").unwrap();
            writer.flush().unwrap();
        })
    }

    #[test]
    fn client_ping() {
        let dir = tempfile::TempDir::new().unwrap();
        let sock = dir.path().join("test.sock");

        let handle = mock_server(&sock, "OK\tPONG");

        let mut client = DaemonClient::connect(&sock).unwrap();
        client.ping().unwrap();

        handle.join().unwrap();
    }

    #[test]
    fn client_mount() {
        let dir = tempfile::TempDir::new().unwrap();
        let sock = dir.path().join("test.sock");

        let handle = mock_server(&sock, "OK\tmount-1");

        let mut client = DaemonClient::connect(&sock).unwrap();
        let id = client.mount("/tmp/test").unwrap();
        assert_eq!(id, "mount-1");

        handle.join().unwrap();
    }

    #[test]
    fn client_error_response() {
        let dir = tempfile::TempDir::new().unwrap();
        let sock = dir.path().join("test.sock");

        let handle = mock_server(&sock, "ERR\tsomething went wrong");

        let mut client = DaemonClient::connect(&sock).unwrap();
        let result = client.ping();
        assert!(result.is_err());

        handle.join().unwrap();
    }
}
