use super::error::*;
use super::values::*;
use super::indices::*;
use nom::{branch::*, combinator::*, number::complete::*, sequence::*};

pub const END_BYTE: u8 = 0x0b;

#[derive(Debug)]
pub enum Instruction {
    // Control
    Unreachable,
    Nop,

    // Parametric
    Drop,
    Select,

    // Variable
    LocalGet(LocalIndex),
    LocalSet(LocalIndex),
    LocalTee(LocalIndex),
    GlobalGet(GlobalIndex),
    GlobalSet(GlobalIndex),

    // Memory

    // Numeric
    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),

    I32EqualZero,
    I32Equal,
    I32NotEqual,
    I32LessThanSigned,
    I32LessThanUnsigned,
    I32GreaterThanSigned,
    I32GreaterThanUnsigned,
    I32LessEqualSigned,
    I32LessEqualUnsigned,
    I32GreaterEqualSigned,
    I32GreaterEqualUnsigned,

    I64EqualZero,
    I64Equal,
    I64NotEqual,
    I64LessThanSigned,
    I64LessThanUnsigned,
    I64GreaterThanSigned,
    I64GreaterThanUnsigned,
    I64LessEqualSigned,
    I64LessEqualUnsigned,
    I64GreaterEqualSigned,
    I64GreaterEqualUnsigned,

    F32Equal,
    F32NotEqual,
    F32LessThan,
    F32GreaterThan,
    F32LessEqual,
    F32GreaterEqual,

    F64Equal,
    F64NotEqual,
    F64LessThan,
    F64GreaterThan,
    F64LessEqual,
    F64GreaterEqual,

    I32LeadingZeros,
    I32TrailingZeros,
    I32CountOnes,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivSigned,
    I32DivUnsigned,
    I32RemainderSigned,
    I32RemainderUnsigned,
    I32And,
    I32Or,
    I32Xor,
    I32ShiftLeft,
    I32ShiftRightSigned,
    I32ShiftRightUnsigned,
    I32RotateLeft,
    I32RotateRight,

    I64LeadingZeros,
    I64TrailingZeros,
    I64CountOnes,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivSigned,
    I64DivUnsigned,
    I64RemainderSigned,
    I64RemainderUnsigned,
    I64And,
    I64Or,
    I64Xor,
    I64ShiftLeft,
    I64ShiftRightSigned,
    I64ShiftRightUnsigned,
    I64RotateLeft,
    I64RotateRight,

    F32Abs,
    F32Neg,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,
    F32Sqrt,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Min,
    F32Max,
    F32Copysign,

    F64Abs,
    F64Neg,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,
    F64Sqrt,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64Copysign,

    I32WrapI64,
    I32TruncF32Signed,
    I32TruncF32Unsigned,
    I32TruncF64Signed,
    I32TruncF64Unsigned,
    I64ExtendI32Signed,
    I64ExtendI32Unsigned,
    I64TruncF32Signed,
    I64TruncF32Unsigned,
    I64TruncF64Signed,
    I64TruncF64Unsigned,
    F32ConvertI32Signed,
    F32ConvertI32Unsigned,
    F32ConvertI64Signed,
    F32ConvertI64Unsigned,
    F32DemoteF64,
    F64ConvertI32Signed,
    F64ConvertI32Unsigned,
    F64ConvertI64Signed,
    F64ConvertI64Unsigned,
    F64PromoteF32,
    I32ReinterpretF32,
    I64ReinterpretF64,
    F32ReinterpretI32,
    F64ReinterpretI64,
}

pub fn instruction(bytes: &[u8]) -> ParseResult<Instruction> {
    eprintln!("{:x?}", bytes);
    wrap_context(
        alt((
            control_instruction,
            paremetric_instruction,
            variable_instructions,
            numeric_instructions,
        )),
        ParseLocation::Instruction,
    )(bytes)
}

fn op(code: u8) -> impl Fn(&[u8]) -> ParseResult<u8> {
    move |bytes| {
        if bytes.is_empty() {
            Err(ParseErrorKind::UnexpectedEof.into())
        } else {
            let op = bytes[0];
            if op == code {
                Ok((&bytes[1..], op))
            } else {
                Err(ParseErrorKind::InvalidOpCode(op).into())
            }
        }
    }
}

fn control_instruction(bytes: &[u8]) -> ParseResult<Instruction> {
    alt((
        map(op(0x00), |_| Instruction::Unreachable),
        map(op(0x01), |_| Instruction::Nop),
    ))(bytes)
}

fn paremetric_instruction(bytes: &[u8]) -> ParseResult<Instruction> {
    alt((
        map(op(0x1A), |_| Instruction::Drop),
        map(op(0x1B), |_| Instruction::Select),
    ))(bytes)
}

fn variable_instructions(bytes: &[u8]) -> ParseResult<Instruction> {
    alt((
        map(preceded(op(0x20), local_index), Instruction::LocalGet),
        map(preceded(op(0x21), local_index), Instruction::LocalSet),
        map(preceded(op(0x22), local_index), Instruction::LocalTee),
        map(preceded(op(0x23), global_index), Instruction::GlobalGet),
        map(preceded(op(0x24), global_index), Instruction::GlobalSet),
    ))(bytes)
}

fn numeric_instructions(bytes: &[u8]) -> ParseResult<Instruction> {
    alt((
        numeric_const,
        numeric_i32_comparison,
        numeric_i64_comparison,
        numeric_f32_comparison,
        numeric_f64_comparison,
        numeric_i32_operation,
        numeric_i64_operation,
        numeric_f32_operation,
        numeric_f64_operation,
        numeric_conversion,
    ))(bytes)
}

fn numeric_const(bytes: &[u8]) -> ParseResult<Instruction> {
    alt((
        map(preceded(op(0x41), leb_s32), Instruction::I32Const),
        map(preceded(op(0x41), leb_s64), Instruction::I64Const),
        map(preceded(op(0x41), le_f32), Instruction::F32Const),
        map(preceded(op(0x41), le_f64), Instruction::F64Const),
    ))(bytes)
}

fn numeric_i32_comparison(bytes: &[u8]) -> ParseResult<Instruction> {
    alt((
        map(op(0x45), |_| Instruction::I32EqualZero),
        map(op(0x46), |_| Instruction::I32Equal),
        map(op(0x47), |_| Instruction::I32NotEqual),
        map(op(0x48), |_| Instruction::I32LessThanSigned),
        map(op(0x49), |_| Instruction::I32LessThanUnsigned),
        map(op(0x4a), |_| Instruction::I32GreaterThanSigned),
        map(op(0x4b), |_| Instruction::I32GreaterThanUnsigned),
        map(op(0x4c), |_| Instruction::I32LessEqualSigned),
        map(op(0x4d), |_| Instruction::I32LessEqualUnsigned),
        map(op(0x4e), |_| Instruction::I32GreaterEqualSigned),
        map(op(0x4f), |_| Instruction::I32GreaterEqualUnsigned),
    ))(bytes)
}

fn numeric_i64_comparison(bytes: &[u8]) -> ParseResult<Instruction> {
    alt((
        map(op(0x50), |_| Instruction::I64EqualZero),
        map(op(0x51), |_| Instruction::I64Equal),
        map(op(0x52), |_| Instruction::I64NotEqual),
        map(op(0x53), |_| Instruction::I64LessThanSigned),
        map(op(0x54), |_| Instruction::I64LessThanUnsigned),
        map(op(0x55), |_| Instruction::I64GreaterThanSigned),
        map(op(0x56), |_| Instruction::I64GreaterThanUnsigned),
        map(op(0x57), |_| Instruction::I64LessEqualSigned),
        map(op(0x58), |_| Instruction::I64LessEqualUnsigned),
        map(op(0x59), |_| Instruction::I64GreaterEqualSigned),
        map(op(0x5a), |_| Instruction::I64GreaterEqualUnsigned),
    ))(bytes)
}

fn numeric_f32_comparison(bytes: &[u8]) -> ParseResult<Instruction> {
    alt((
        map(op(0x5b), |_| Instruction::F32Equal),
        map(op(0x5c), |_| Instruction::F32NotEqual),
        map(op(0x5d), |_| Instruction::F32LessThan),
        map(op(0x5e), |_| Instruction::F32GreaterThan),
        map(op(0x5f), |_| Instruction::F32LessEqual),
        map(op(0x60), |_| Instruction::F32GreaterEqual),
    ))(bytes)
}

fn numeric_f64_comparison(bytes: &[u8]) -> ParseResult<Instruction> {
    alt((
        map(op(0x61), |_| Instruction::F64Equal),
        map(op(0x62), |_| Instruction::F64NotEqual),
        map(op(0x63), |_| Instruction::F64LessThan),
        map(op(0x64), |_| Instruction::F64GreaterThan),
        map(op(0x65), |_| Instruction::F64LessEqual),
        map(op(0x66), |_| Instruction::F64GreaterEqual),
    ))(bytes)
}

fn numeric_i32_operation(bytes: &[u8]) -> ParseResult<Instruction> {
    alt((
        map(op(0x67), |_| Instruction::I32LeadingZeros),
        map(op(0x68), |_| Instruction::I32TrailingZeros),
        map(op(0x69), |_| Instruction::I32CountOnes),
        map(op(0x6a), |_| Instruction::I32Add),
        map(op(0x6b), |_| Instruction::I32Sub),
        map(op(0x6c), |_| Instruction::I32Mul),
        map(op(0x6d), |_| Instruction::I32DivSigned),
        map(op(0x6e), |_| Instruction::I32DivUnsigned),
        map(op(0x6f), |_| Instruction::I32RemainderSigned),
        map(op(0x70), |_| Instruction::I32RemainderUnsigned),
        map(op(0x71), |_| Instruction::I32And),
        map(op(0x72), |_| Instruction::I32Or),
        map(op(0x73), |_| Instruction::I32Xor),
        map(op(0x74), |_| Instruction::I32ShiftLeft),
        map(op(0x75), |_| Instruction::I32ShiftRightSigned),
        map(op(0x76), |_| Instruction::I32ShiftRightUnsigned),
        map(op(0x77), |_| Instruction::I32RotateLeft),
        map(op(0x78), |_| Instruction::I32RotateRight),
    ))(bytes)
}

fn numeric_i64_operation(bytes: &[u8]) -> ParseResult<Instruction> {
    alt((
        map(op(0x79), |_| Instruction::I64LeadingZeros),
        map(op(0x7a), |_| Instruction::I64TrailingZeros),
        map(op(0x7b), |_| Instruction::I64CountOnes),
        map(op(0x7c), |_| Instruction::I64Add),
        map(op(0x7d), |_| Instruction::I64Sub),
        map(op(0x7e), |_| Instruction::I64Mul),
        map(op(0x7f), |_| Instruction::I64DivSigned),
        map(op(0x80), |_| Instruction::I64DivUnsigned),
        map(op(0x81), |_| Instruction::I64RemainderSigned),
        map(op(0x82), |_| Instruction::I64RemainderUnsigned),
        map(op(0x83), |_| Instruction::I64And),
        map(op(0x84), |_| Instruction::I64Or),
        map(op(0x85), |_| Instruction::I64Xor),
        map(op(0x86), |_| Instruction::I64ShiftLeft),
        map(op(0x87), |_| Instruction::I64ShiftRightSigned),
        map(op(0x88), |_| Instruction::I64ShiftRightUnsigned),
        map(op(0x89), |_| Instruction::I64RotateLeft),
        map(op(0x8a), |_| Instruction::I64RotateRight),
    ))(bytes)
}

fn numeric_f32_operation(bytes: &[u8]) -> ParseResult<Instruction> {
    alt((
        map(op(0x8b), |_| Instruction::F32Abs),
        map(op(0x8c), |_| Instruction::F32Neg),
        map(op(0x8d), |_| Instruction::F32Ceil),
        map(op(0x8e), |_| Instruction::F32Floor),
        map(op(0x8f), |_| Instruction::F32Trunc),
        map(op(0x90), |_| Instruction::F32Nearest),
        map(op(0x91), |_| Instruction::F32Sqrt),
        map(op(0x92), |_| Instruction::F32Add),
        map(op(0x93), |_| Instruction::F32Sub),
        map(op(0x94), |_| Instruction::F32Mul),
        map(op(0x95), |_| Instruction::F32Div),
        map(op(0x96), |_| Instruction::F32Min),
        map(op(0x97), |_| Instruction::F32Max),
        map(op(0x98), |_| Instruction::F32Copysign),
    ))(bytes)
}

fn numeric_f64_operation(bytes: &[u8]) -> ParseResult<Instruction> {
    alt((
        map(op(0x99), |_| Instruction::F64Abs),
        map(op(0x9a), |_| Instruction::F64Neg),
        map(op(0x9b), |_| Instruction::F64Ceil),
        map(op(0x9c), |_| Instruction::F64Floor),
        map(op(0x9d), |_| Instruction::F64Trunc),
        map(op(0x9e), |_| Instruction::F64Nearest),
        map(op(0x9f), |_| Instruction::F64Sqrt),
        map(op(0xa0), |_| Instruction::F64Add),
        map(op(0xa1), |_| Instruction::F64Sub),
        map(op(0xa2), |_| Instruction::F64Mul),
        map(op(0xa3), |_| Instruction::F64Div),
        map(op(0xa4), |_| Instruction::F64Min),
        map(op(0xa5), |_| Instruction::F64Max),
        map(op(0xa6), |_| Instruction::F64Copysign),
    ))(bytes)
}

fn numeric_conversion(bytes: &[u8]) -> ParseResult<Instruction> {
    alt((
        alt((
            map(op(0xa7), |_| Instruction::I32WrapI64),
            map(op(0xa8), |_| Instruction::I32TruncF32Signed),
            map(op(0xa9), |_| Instruction::I32TruncF32Unsigned),
            map(op(0xaa), |_| Instruction::I32TruncF64Signed),
            map(op(0xab), |_| Instruction::I32TruncF64Unsigned),
            map(op(0xac), |_| Instruction::I64ExtendI32Signed),
            map(op(0xad), |_| Instruction::I64ExtendI32Unsigned),
            map(op(0xae), |_| Instruction::I64TruncF32Signed),
            map(op(0xaf), |_| Instruction::I64TruncF32Unsigned),
            map(op(0xb0), |_| Instruction::I64TruncF64Signed),
            map(op(0xb1), |_| Instruction::I64TruncF64Unsigned),
            map(op(0xb2), |_| Instruction::F32ConvertI32Signed),
            map(op(0xb3), |_| Instruction::F32ConvertI32Unsigned),
        )),
        alt((
            map(op(0xb4), |_| Instruction::F32ConvertI64Signed),
            map(op(0xb5), |_| Instruction::F32ConvertI64Unsigned),
            map(op(0xb6), |_| Instruction::F32DemoteF64),
            map(op(0xb7), |_| Instruction::F64ConvertI32Signed),
            map(op(0xb8), |_| Instruction::F64ConvertI32Unsigned),
            map(op(0xb9), |_| Instruction::F64ConvertI64Signed),
            map(op(0xba), |_| Instruction::F64ConvertI64Unsigned),
            map(op(0xbb), |_| Instruction::F64PromoteF32),
            map(op(0xbc), |_| Instruction::I32ReinterpretF32),
            map(op(0xbd), |_| Instruction::I64ReinterpretF64),
            map(op(0xbe), |_| Instruction::F32ReinterpretI32),
            map(op(0xbf), |_| Instruction::F64ReinterpretI64),
        )),
    ))(bytes)
}
