use std::error::Error;
use std::fmt;
use std::io;

#[derive(Clone, Copy, Debug)]
pub enum VMErrorType {
    VM,
    Chunk,
    Heap,
    Value,
    IO,
    Compile,
    Other,
}

impl fmt::Display for VMErrorType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::VM => write!(f, "VM"),
            Self::Chunk => write!(f, "CHUNK"),
            Self::Heap => write!(f, "HEAP"),
            Self::Value => write!(f, "VALUE"),
            Self::IO => write!(f, "IO"),
            Self::Compile => write!(f, "COMPILE"),
            Self::Other => write!(f, "OTHER"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct VMError {
    pub reason: String,
    pub err_type: VMErrorType,
}

impl Error for VMError {}

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]: {}", self.err_type, self.reason)
    }
}

impl From<io::Error> for VMError {
    fn from(item: io::Error) -> Self {
        VMError {
            reason: item.to_string(),
            err_type: VMErrorType::IO,
        }
    }
}

impl VMError {
    pub fn new<S: Into<String>>(reason: S, err_type: VMErrorType) -> Self {
        VMError {
            reason: reason.into(),
            err_type,
        }
    }

    pub fn new_vm<S: Into<String>>(reason: S) -> Self {
        VMError::new(reason, VMErrorType::VM)
    }

    pub fn new_chunk<S: Into<String>>(reason: S) -> Self {
        VMError::new(reason, VMErrorType::Chunk)
    }

    pub fn new_heap<S: Into<String>>(reason: S) -> Self {
        VMError::new(reason, VMErrorType::Heap)
    }

    pub fn new_value<S: Into<String>>(reason: S) -> Self {
        VMError::new(reason, VMErrorType::Value)
    }

    pub fn new_compile<S: Into<String>>(reason: S) -> Self {
        VMError::new(reason, VMErrorType::Compile)
    }

    pub fn new_other<S: Into<String>>(reason: S) -> Self {
        VMError::new(reason, VMErrorType::Other)
    }
}

pub type VMResult<T> = Result<T, VMError>;
