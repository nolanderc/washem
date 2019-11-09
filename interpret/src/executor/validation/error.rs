
pub use crate::error::*;
use crate::ast::indices::Index;

use failure::Fail;

#[derive(Debug, Copy, Clone, Fail)]
pub enum ValidationError {
    #[fail(display = "invalid index: {:?}", _0)]
    InvalidIndex(Index),
    #[fail(display = "multiple return values is not supported")]
    MultipleReturns,
    #[fail(display = "invalid function type")]
    InvalidFunctionType,
    #[fail(display = "not enough contral frames on stack")]
    ControlFrameUnderflow,
    #[fail(display = "not enough operands on stack")]
    OperandUnderflow,
    #[fail(display = "operand type mismatch")]
    UnexpectedOperand,
    #[fail(display = "unexpected number of operands on stack")]
    OperandLeak,
    #[fail(display = "local variable not found")]
    LocalNotFound,
    #[fail(display = "global variable not found")]
    GlobalNotFound,
    #[fail(display = "memory not found")]
    MemoryNotFound,
    #[fail(display = "memory aligment too large")]
    InvalidMemoryAlignment,
    #[fail(display = "label not found")]
    LabelNotFound,
    #[fail(display = "label types did not match")]
    LabelTypeMismatch,
    #[fail(display = "return not allowed in context")]
    ContextDisallowReturn,
    #[fail(display = "function not found")]
    FunctionNotFound,
    #[fail(display = "type not found")]
    TypeNotFound,
    #[fail(display = "table not found")]
    TableNotFound,
    #[fail(display = "invalid limits")]
    InvalidLimits,
    #[fail(display = "instruction is not constant")]
    InstructionNotConstant,
    #[fail(display = "multiple memories is not supported")]
    MultipleMemories,
    #[fail(display = "multiple tables is not supported")]
    MultipleTables,
    #[fail(display = "multiple exports with the same name")]
    DuplicateExportName,
}

pub type ValidationResult<T, E = ValidationError> = Result<T, E>;

