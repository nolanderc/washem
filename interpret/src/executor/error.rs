
pub use crate::error::*;

use failure::Fail;

#[derive(Debug, Clone, Fail)]
pub enum InstantiationError {
    #[fail(display = "module `{}` not found", _0)]
    ModuleNotFound(String),
    #[fail(display = "export not found")]
    ExportNotFound,
    #[fail(display = "imported value did not match expected type")]
    ImportTypeMismatch,
    #[fail(display = "import did not reference a valid type")]
    TypeNotFound,
    #[fail(display = "function address did not point to valid function")]
    InvalidFunctionAddress,
    #[fail(display = "element segment out of range")]
    ElementSegmentOutOfRange,
    #[fail(display = "data segment out of range")]
    DataSegmentOutOfRange,
}

pub type InstantiationResult<T, E = InstantiationError> = Result<T, E>;


#[derive(Debug, Clone, Fail)]
pub enum RuntimeError {
    #[fail(display = "function not defined (invalid address)")]
    FunctionNotDefined,
    #[fail(display = "stack was invalid")]
    InvalidStack,
    #[fail(display = "no stack frame established")]
    MissingStackFrame,
}

pub type RuntimeResult<T, E = RuntimeError> = Result<T, E>;
