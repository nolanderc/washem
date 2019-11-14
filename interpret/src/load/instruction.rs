use super::error::*;
use super::indices::*;
use super::structure::*;
use super::types::*;
use super::values::*;
use crate::ast::instruction::*;
use nom::{
    branch::*, bytes::complete::*, combinator::*, multi::*, number::complete::*, sequence::*,
};

pub const END_BYTE: u8 = 0x0b;
pub const ELSE_BYTE: u8 = 0x05;

pub fn instruction(bytes: &[u8]) -> ParseResult<Instruction> {
    wrap_context(parse_instruction, ParseLocation::Instruction)(bytes)
}

fn instruction_sequence(bytes: &[u8]) -> ParseResult<Vec<Instruction>> {
    map(
        many_till(instruction, tag(&[END_BYTE])),
        |(instructions, _)| instructions,
    )(bytes)
}

fn op(bytes: &[u8]) -> ParseResult<u8> {
    if bytes.is_empty() {
        Err(ParseErrorKind::UnexpectedEof.into())
    } else {
        Ok((&bytes[1..], bytes[0]))
    }
}

fn parse_instruction(bytes: &[u8]) -> ParseResult<Instruction> {
    let (bytes, op) = op(bytes)?;

    let (bytes, instruction) = match op {
        // Control
        0x00 => (bytes, Instruction::Unreachable),
        0x01 => (bytes, Instruction::Nop),
        0x02 => block(bytes)?,
        0x03 => looping(bytes)?,
        0x04 => conditional(bytes)?,
        0x0C => map(label_index, Instruction::Branch)(bytes)?,
        0x0D => map(label_index, Instruction::BranchConditional)(bytes)?,
        0x0E => branch_table(bytes)?,
        0x0F => (bytes, Instruction::Return),
        0x10 => map(function_index, Instruction::Call)(bytes)?,
        0x11 => call_indirect(bytes)?,
        0x1A => (bytes, Instruction::Drop),
        0x1B => (bytes, Instruction::Select),

        // Variable
        0x20 => map(local_index, Instruction::LocalGet)(bytes)?,
        0x21 => map(local_index, Instruction::LocalSet)(bytes)?,
        0x22 => map(local_index, Instruction::LocalTee)(bytes)?,
        0x23 => map(global_index, Instruction::GlobalGet)(bytes)?,
        0x24 => map(global_index, Instruction::GlobalSet)(bytes)?,

        // Memory
        0x28 => map(memory_argument, Instruction::I32Load)(bytes)?,
        0x29 => map(memory_argument, Instruction::I64Load)(bytes)?,
        0x2a => map(memory_argument, Instruction::F32Load)(bytes)?,
        0x2b => map(memory_argument, Instruction::F64Load)(bytes)?,
        0x2c => map(memory_argument, Instruction::I32Load8Signed)(bytes)?,
        0x2d => map(memory_argument, Instruction::I32Load8Unsigned)(bytes)?,
        0x2e => map(memory_argument, Instruction::I32Load16Signed)(bytes)?,
        0x2f => map(memory_argument, Instruction::I32Load16Unsigned)(bytes)?,
        0x30 => map(memory_argument, Instruction::I64Load8Signed)(bytes)?,
        0x31 => map(memory_argument, Instruction::I64Load8Unsigned)(bytes)?,
        0x32 => map(memory_argument, Instruction::I64Load16Signed)(bytes)?,
        0x33 => map(memory_argument, Instruction::I64Load16Unsigned)(bytes)?,
        0x34 => map(memory_argument, Instruction::I64Load32Signed)(bytes)?,
        0x35 => map(memory_argument, Instruction::I64Load32Unsigned)(bytes)?,
        0x36 => map(memory_argument, Instruction::I32Store)(bytes)?,
        0x37 => map(memory_argument, Instruction::I64Store)(bytes)?,
        0x38 => map(memory_argument, Instruction::F32Store)(bytes)?,
        0x39 => map(memory_argument, Instruction::F64Store)(bytes)?,
        0x3a => map(memory_argument, Instruction::I32Store8)(bytes)?,
        0x3b => map(memory_argument, Instruction::I32Store16)(bytes)?,
        0x3c => map(memory_argument, Instruction::I64Store8)(bytes)?,
        0x3d => map(memory_argument, Instruction::I64Store16)(bytes)?,
        0x3e => map(memory_argument, Instruction::I64Store32)(bytes)?,
        0x3f => map(tag(&[0x00]), |_| Instruction::MemorySize)(bytes)?,
        0x40 => map(tag(&[0x00]), |_| Instruction::MemoryGrow)(bytes)?,

        // Numeric
        0x41 => map(leb_s32, Instruction::I32Const)(bytes)?,
        0x42 => map(leb_s64, Instruction::I64Const)(bytes)?,
        0x43 => map(le_f32, Instruction::F32Const)(bytes)?,
        0x44 => map(le_f64, Instruction::F64Const)(bytes)?,

        // 32 bit integer comparison
        0x45 => (bytes, Instruction::I32EqualZero),
        0x46 => (bytes, Instruction::I32Equal),
        0x47 => (bytes, Instruction::I32NotEqual),
        0x48 => (bytes, Instruction::I32LessThanSigned),
        0x49 => (bytes, Instruction::I32LessThanUnsigned),
        0x4a => (bytes, Instruction::I32GreaterThanSigned),
        0x4b => (bytes, Instruction::I32GreaterThanUnsigned),
        0x4c => (bytes, Instruction::I32LessEqualSigned),
        0x4d => (bytes, Instruction::I32LessEqualUnsigned),
        0x4e => (bytes, Instruction::I32GreaterEqualSigned),
        0x4f => (bytes, Instruction::I32GreaterEqualUnsigned),

        // 64 bit integer comparison
        0x50 => (bytes, Instruction::I64EqualZero),
        0x51 => (bytes, Instruction::I64Equal),
        0x52 => (bytes, Instruction::I64NotEqual),
        0x53 => (bytes, Instruction::I64LessThanSigned),
        0x54 => (bytes, Instruction::I64LessThanUnsigned),
        0x55 => (bytes, Instruction::I64GreaterThanSigned),
        0x56 => (bytes, Instruction::I64GreaterThanUnsigned),
        0x57 => (bytes, Instruction::I64LessEqualSigned),
        0x58 => (bytes, Instruction::I64LessEqualUnsigned),
        0x59 => (bytes, Instruction::I64GreaterEqualSigned),
        0x5a => (bytes, Instruction::I64GreaterEqualUnsigned),

        // Single precision floating point comparison
        0x5b => (bytes, Instruction::F32Equal),
        0x5c => (bytes, Instruction::F32NotEqual),
        0x5d => (bytes, Instruction::F32LessThan),
        0x5e => (bytes, Instruction::F32GreaterThan),
        0x5f => (bytes, Instruction::F32LessEqual),
        0x60 => (bytes, Instruction::F32GreaterEqual),

        // Double precision floating point comparison
        0x61 => (bytes, Instruction::F64Equal),
        0x62 => (bytes, Instruction::F64NotEqual),
        0x63 => (bytes, Instruction::F64LessThan),
        0x64 => (bytes, Instruction::F64GreaterThan),
        0x65 => (bytes, Instruction::F64LessEqual),
        0x66 => (bytes, Instruction::F64GreaterEqual),

        // 32 bit integer floating point operations
        0x67 => (bytes, Instruction::I32LeadingZeros),
        0x68 => (bytes, Instruction::I32TrailingZeros),
        0x69 => (bytes, Instruction::I32CountOnes),
        0x6a => (bytes, Instruction::I32Add),
        0x6b => (bytes, Instruction::I32Sub),
        0x6c => (bytes, Instruction::I32Mul),
        0x6d => (bytes, Instruction::I32DivSigned),
        0x6e => (bytes, Instruction::I32DivUnsigned),
        0x6f => (bytes, Instruction::I32RemainderSigned),
        0x70 => (bytes, Instruction::I32RemainderUnsigned),
        0x71 => (bytes, Instruction::I32And),
        0x72 => (bytes, Instruction::I32Or),
        0x73 => (bytes, Instruction::I32Xor),
        0x74 => (bytes, Instruction::I32ShiftLeft),
        0x75 => (bytes, Instruction::I32ShiftRightSigned),
        0x76 => (bytes, Instruction::I32ShiftRightUnsigned),
        0x77 => (bytes, Instruction::I32RotateLeft),
        0x78 => (bytes, Instruction::I32RotateRight),

        // 64 bit integer floating point operations
        0x79 => (bytes, Instruction::I64LeadingZeros),
        0x7a => (bytes, Instruction::I64TrailingZeros),
        0x7b => (bytes, Instruction::I64CountOnes),
        0x7c => (bytes, Instruction::I64Add),
        0x7d => (bytes, Instruction::I64Sub),
        0x7e => (bytes, Instruction::I64Mul),
        0x7f => (bytes, Instruction::I64DivSigned),
        0x80 => (bytes, Instruction::I64DivUnsigned),
        0x81 => (bytes, Instruction::I64RemainderSigned),
        0x82 => (bytes, Instruction::I64RemainderUnsigned),
        0x83 => (bytes, Instruction::I64And),
        0x84 => (bytes, Instruction::I64Or),
        0x85 => (bytes, Instruction::I64Xor),
        0x86 => (bytes, Instruction::I64ShiftLeft),
        0x87 => (bytes, Instruction::I64ShiftRightSigned),
        0x88 => (bytes, Instruction::I64ShiftRightUnsigned),
        0x89 => (bytes, Instruction::I64RotateLeft),
        0x8a => (bytes, Instruction::I64RotateRight),

        // Single precision floating point operations
        0x8b => (bytes, Instruction::F32Abs),
        0x8c => (bytes, Instruction::F32Neg),
        0x8d => (bytes, Instruction::F32Ceil),
        0x8e => (bytes, Instruction::F32Floor),
        0x8f => (bytes, Instruction::F32Trunc),
        0x90 => (bytes, Instruction::F32Nearest),
        0x91 => (bytes, Instruction::F32Sqrt),
        0x92 => (bytes, Instruction::F32Add),
        0x93 => (bytes, Instruction::F32Sub),
        0x94 => (bytes, Instruction::F32Mul),
        0x95 => (bytes, Instruction::F32Div),
        0x96 => (bytes, Instruction::F32Min),
        0x97 => (bytes, Instruction::F32Max),
        0x98 => (bytes, Instruction::F32Copysign),

        // Double precision floating point operations
        0x99 => (bytes, Instruction::F64Abs),
        0x9a => (bytes, Instruction::F64Neg),
        0x9b => (bytes, Instruction::F64Ceil),
        0x9c => (bytes, Instruction::F64Floor),
        0x9d => (bytes, Instruction::F64Trunc),
        0x9e => (bytes, Instruction::F64Nearest),
        0x9f => (bytes, Instruction::F64Sqrt),
        0xa0 => (bytes, Instruction::F64Add),
        0xa1 => (bytes, Instruction::F64Sub),
        0xa2 => (bytes, Instruction::F64Mul),
        0xa3 => (bytes, Instruction::F64Div),
        0xa4 => (bytes, Instruction::F64Min),
        0xa5 => (bytes, Instruction::F64Max),
        0xa6 => (bytes, Instruction::F64Copysign),

        // Conversions
        0xa7 => (bytes, Instruction::I32WrapI64),
        0xa8 => (bytes, Instruction::I32TruncF32Signed),
        0xa9 => (bytes, Instruction::I32TruncF32Unsigned),
        0xaa => (bytes, Instruction::I32TruncF64Signed),
        0xab => (bytes, Instruction::I32TruncF64Unsigned),
        0xac => (bytes, Instruction::I64ExtendI32Signed),
        0xad => (bytes, Instruction::I64ExtendI32Unsigned),
        0xae => (bytes, Instruction::I64TruncF32Signed),
        0xaf => (bytes, Instruction::I64TruncF32Unsigned),
        0xb0 => (bytes, Instruction::I64TruncF64Signed),
        0xb1 => (bytes, Instruction::I64TruncF64Unsigned),
        0xb2 => (bytes, Instruction::F32ConvertI32Signed),
        0xb3 => (bytes, Instruction::F32ConvertI32Unsigned),
        0xb4 => (bytes, Instruction::F32ConvertI64Signed),
        0xb5 => (bytes, Instruction::F32ConvertI64Unsigned),
        0xb6 => (bytes, Instruction::F32DemoteF64),
        0xb7 => (bytes, Instruction::F64ConvertI32Signed),
        0xb8 => (bytes, Instruction::F64ConvertI32Unsigned),
        0xb9 => (bytes, Instruction::F64ConvertI64Signed),
        0xba => (bytes, Instruction::F64ConvertI64Unsigned),
        0xbb => (bytes, Instruction::F64PromoteF32),
        0xbc => (bytes, Instruction::I32ReinterpretF32),
        0xbd => (bytes, Instruction::I64ReinterpretF64),
        0xbe => (bytes, Instruction::F32ReinterpretI32),
        0xbf => (bytes, Instruction::F64ReinterpretI64),

        op => return Err(ParseErrorKind::InvalidOpCode(op).into()),
    };

    Ok((bytes, instruction))
}

fn block(bytes: &[u8]) -> ParseResult<Instruction> {
    map(
        tuple((result_type, instruction_sequence)),
        |(result, instructions)| Instruction::Block {
            result,
            instructions,
        },
    )(bytes)
}

fn looping(bytes: &[u8]) -> ParseResult<Instruction> {
    map(
        tuple((result_type, instruction_sequence)),
        |(result, instructions)| Instruction::Loop {
            result,
            instructions,
        },
    )(bytes)
}

fn conditional(bytes: &[u8]) -> ParseResult<Instruction> {
    let body = many_till(
        instruction,
        alt((
            map(tag(&[END_BYTE]), |_| Vec::new()),
            preceded(tag(&[ELSE_BYTE]), instruction_sequence),
        )),
    );

    map(
        tuple((result_type, body)),
        |(result, (success, failure))| Instruction::Conditional {
            result,
            success,
            failure,
        },
    )(bytes)
}

fn branch_table(bytes: &[u8]) -> ParseResult<Instruction> {
    map(
        tuple((vec(label_index), label_index)),
        |(targets, default)| Instruction::BranchTable { targets, default },
    )(bytes)
}

fn call_indirect(bytes: &[u8]) -> ParseResult<Instruction> {
    map(
        terminated(type_index, tag(&[0x00])),
        Instruction::CallIndirect,
    )(bytes)
}

fn memory_argument(bytes: &[u8]) -> ParseResult<MemoryArgument> {
    map(tuple((leb_u32, leb_u32)), |(align, offset)| {
        MemoryArgument { align, offset }
    })(bytes)
}
