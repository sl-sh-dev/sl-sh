use std::fs::File;
use std::io;
use std::io::{BufReader, BufWriter, ErrorKind, Read, Seek, SeekFrom, Write};
use std::sync::{Arc, Mutex, MutexGuard};

#[derive(Copy, Clone, Debug)]
pub enum HeapIoError {
    Closed,
    NotFile,
}

#[derive(Clone)]
pub struct HeapIo {
    io: Arc<Mutex<Io>>,
}

impl HeapIo {
    pub fn from_file(file: File) -> Self {
        let io = Arc::new(Mutex::new(Io::File(Some(file))));
        Self { io }
    }

    pub fn stdin() -> Self {
        let io = Arc::new(Mutex::new(Io::StdIn));
        Self { io }
    }

    pub fn stdout() -> Self {
        let io = Arc::new(Mutex::new(Io::StdOut));
        Self { io }
    }

    pub fn stderr() -> Self {
        let io = Arc::new(Mutex::new(Io::StdErr));
        Self { io }
    }

    pub fn close(&self) {
        if let Ok(mut guard) = self.io.lock() {
            *guard = Io::Closed
        }
    }

    pub fn to_buf_reader(&self) -> Result<(), HeapIoError> {
        if let Ok(mut guard) = self.io.lock() {
            match &mut *guard {
                Io::File(f) => match f.take() {
                    Some(f) => *guard = Io::FileReadBuf(BufReader::new(f)),
                    None => panic!("file without a file"),
                },
                Io::FileReadBuf(_) => return Err(HeapIoError::NotFile),
                Io::FileWriteBuf(_) => return Err(HeapIoError::NotFile),
                Io::StdIn => return Err(HeapIoError::NotFile),
                Io::StdOut => return Err(HeapIoError::NotFile),
                Io::StdErr => return Err(HeapIoError::NotFile),
                Io::Closed => return Err(HeapIoError::Closed),
            }
        }
        Ok(())
    }

    pub fn to_buf_writer(&self) -> Result<(), HeapIoError> {
        if let Ok(mut guard) = self.io.lock() {
            match &mut *guard {
                Io::File(f) => match f.take() {
                    Some(f) => *guard = Io::FileWriteBuf(BufWriter::new(f)),
                    None => panic!("file without a file"),
                },
                Io::FileReadBuf(_) => return Err(HeapIoError::NotFile),
                Io::FileWriteBuf(_) => return Err(HeapIoError::NotFile),
                Io::StdIn => return Err(HeapIoError::NotFile),
                Io::StdOut => return Err(HeapIoError::NotFile),
                Io::StdErr => return Err(HeapIoError::NotFile),
                Io::Closed => return Err(HeapIoError::Closed),
            }
        }
        Ok(())
    }

    pub fn get_io(&self) -> IoGuard {
        let io = self.io.lock().unwrap();
        IoGuard { io }
    }
}

pub struct IoGuard<'a> {
    io: MutexGuard<'a, Io>,
}

impl<'a> Read for IoGuard<'a> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.io.read(buf)
    }
}

impl<'a> Write for IoGuard<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.io.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.io.flush()
    }
}

impl<'a> Seek for IoGuard<'a> {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        self.io.seek(pos)
    }
}

enum Io {
    File(Option<File>),
    FileReadBuf(BufReader<File>),
    FileWriteBuf(BufWriter<File>),
    StdIn,
    StdOut,
    StdErr,
    Closed,
}

impl Read for Io {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            Io::File(Some(f)) => f.read(buf),
            Io::File(None) => panic!("file is missing a file"),
            Io::FileReadBuf(io) => io.read(buf),
            Io::FileWriteBuf(_) => Err(io::Error::new(
                ErrorKind::Unsupported,
                "read not supported for a write buffer",
            )),
            Io::StdIn => io::stdin().read(buf),
            Io::StdOut => Err(io::Error::new(
                ErrorKind::Unsupported,
                "read not supported for stdout",
            )),
            Io::StdErr => Err(io::Error::new(
                ErrorKind::Unsupported,
                "read not supported for stderr",
            )),
            Io::Closed => Err(io::Error::new(
                ErrorKind::Unsupported,
                "read not supported for closed",
            )),
        }
    }
}

impl Write for Io {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self {
            Io::File(Some(f)) => f.write(buf),
            Io::File(None) => panic!("file is missing a file"),
            Io::FileReadBuf(_) => Err(io::Error::new(
                ErrorKind::Unsupported,
                "write not supported for a read buffer",
            )),
            Io::FileWriteBuf(io) => io.write(buf),
            Io::StdIn => Err(io::Error::new(
                ErrorKind::Unsupported,
                "write not supported for stdin",
            )),
            Io::StdOut => io::stdout().write(buf),
            Io::StdErr => io::stderr().write(buf),
            Io::Closed => Err(io::Error::new(
                ErrorKind::Unsupported,
                "write not supported for closed",
            )),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            Io::File(Some(f)) => f.flush(),
            Io::File(None) => panic!("file is missing a file"),
            Io::FileReadBuf(_) => Err(io::Error::new(
                ErrorKind::Unsupported,
                "flush not supported for a read buffer",
            )),
            Io::FileWriteBuf(io) => io.flush(),
            Io::StdIn => Err(io::Error::new(
                ErrorKind::Unsupported,
                "flush not supported for stdin",
            )),
            Io::StdOut => io::stdout().flush(),
            Io::StdErr => io::stderr().flush(),
            Io::Closed => Err(io::Error::new(
                ErrorKind::Unsupported,
                "flush not supported for closed",
            )),
        }
    }
}

impl Seek for Io {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        match self {
            Io::File(Some(f)) => f.seek(pos),
            Io::File(None) => panic!("file is missing a file"),
            Io::FileReadBuf(io) => io.seek(pos),
            Io::FileWriteBuf(io) => io.seek(pos),
            Io::StdIn => Err(io::Error::new(
                ErrorKind::Unsupported,
                "seek not supported for stdin",
            )),
            Io::StdOut => Err(io::Error::new(
                ErrorKind::Unsupported,
                "seek not supported for stdout",
            )),
            Io::StdErr => Err(io::Error::new(
                ErrorKind::Unsupported,
                "seek not supported for stderr",
            )),
            Io::Closed => Err(io::Error::new(
                ErrorKind::Unsupported,
                "seek not supported for closed",
            )),
        }
    }
}
