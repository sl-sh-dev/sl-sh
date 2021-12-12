use std::error::Error;
use std::fmt;
use std::io;

use crate::value::*;

#[derive(Clone, Debug)]
pub enum VMErrorObj {
    Message(String),
    Object(Value),
}

#[derive(Clone, Debug)]
pub struct VMError {
    pub key: &'static str,
    pub obj: VMErrorObj,
}

impl Error for VMError {}

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.obj {
            VMErrorObj::Message(msg) => write!(f, "[{}]: {}", self.key, msg),
            VMErrorObj::Object(val) => write!(f, "[{}]: {:?}", self.key, val),
        }
    }
}

impl From<io::Error> for VMError {
    fn from(item: io::Error) -> Self {
        VMError::new("io", item.to_string())
    }
}

impl VMError {
    pub fn new<S: Into<String>>(key: &'static str, reason: S) -> Self {
        let reason: String = reason.into();
        VMError {
            key,
            obj: VMErrorObj::Message(reason),
        }
    }

    pub fn new_vm<S: Into<String>>(reason: S) -> Self {
        VMError::new("rt", reason)
    }

    pub fn new_chunk<S: Into<String>>(reason: S) -> Self {
        VMError::new("rt", reason)
    }

    pub fn new_heap<S: Into<String>>(reason: S) -> Self {
        VMError::new("mem", reason)
    }

    pub fn new_value<S: Into<String>>(reason: S) -> Self {
        VMError::new("rt", reason)
    }

    pub fn new_compile<S: Into<String>>(reason: S) -> Self {
        VMError::new("compile", reason)
    }

    pub fn new_other<S: Into<String>>(reason: S) -> Self {
        VMError::new("error", reason)
    }
}

pub type VMResult<T> = Result<T, VMError>;
