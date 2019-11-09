use super::combinator::*;
use super::error::*;
use super::structure::*;
use super::values::*;
use crate::ast::types::*;
use nom::{branch::*, bytes::complete::*, combinator::*, sequence::*};

pub fn value_type(bytes: &[u8]) -> ParseResult<ValueType> {
    alt((
        map(tag(&[0x7F]), |_| ValueType::I32),
        map(tag(&[0x7E]), |_| ValueType::I64),
        map(tag(&[0x7D]), |_| ValueType::F32),
        map(tag(&[0x7C]), |_| ValueType::F64),
    ))(bytes)
}

pub fn result_type(bytes: &[u8]) -> ParseResult<ResultType> {
    alt((
        map(tag(&[0x40]), |_| ResultType { types: None }),
        map(value_type, |value| ResultType { types: Some(value) }),
    ))(bytes)
}

pub fn function_type(bytes: &[u8]) -> ParseResult<FunctionType> {
    map(
        tuple((tag(&[0x60]), vec(value_type), vec(value_type))),
        |(_, parameters, results)| FunctionType {
            parameters,
            results,
        },
    )(bytes)
}

pub fn limits(bytes: &[u8]) -> ParseResult<Limits> {
    map(
        alt((
            tuple((tag(&[0x00]), leb_u32, constant(None))),
            tuple((tag(&[0x01]), leb_u32, map(leb_u32, Some))),
        )),
        |(_, lower, upper)| Limits { lower, upper },
    )(bytes)
}

pub fn memory_type(bytes: &[u8]) -> ParseResult<MemoryType> {
    map(limits, |limits| MemoryType { limits })(bytes)
}

pub fn table_type(bytes: &[u8]) -> ParseResult<TableType> {
    map(tuple((element_type, limits)), |(element, limits)| {
        TableType { element, limits }
    })(bytes)
}

pub fn element_type(bytes: &[u8]) -> ParseResult<ElementType> {
    map(tag(&[0x70]), |_| ElementType::FunctionReference)(bytes)
}

pub fn global_type(bytes: &[u8]) -> ParseResult<GlobalType> {
    map(tuple((value_type, mutability)), |(ty, mutability)| {
        GlobalType { ty, mutability }
    })(bytes)
}

pub fn mutability(bytes: &[u8]) -> ParseResult<Mutability> {
    alt((
        map(tag(&[0x00]), |_| Mutability::Constant),
        map(tag(&[0x01]), |_| Mutability::Variable),
    ))(bytes)
}
