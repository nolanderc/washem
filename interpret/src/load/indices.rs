use super::error::*;
use super::values::*;
use crate::ast::indices::*;
use nom::combinator::*;

pub fn type_index(bytes: &[u8]) -> ParseResult<TypeIndex> {
    map(leb_u32, TypeIndex)(bytes)
}

pub fn function_index(bytes: &[u8]) -> ParseResult<FunctionIndex> {
    map(leb_u32, FunctionIndex)(bytes)
}

pub fn table_index(bytes: &[u8]) -> ParseResult<TableIndex> {
    map(leb_u32, TableIndex)(bytes)
}

pub fn memory_index(bytes: &[u8]) -> ParseResult<MemoryIndex> {
    map(leb_u32, MemoryIndex)(bytes)
}

pub fn global_index(bytes: &[u8]) -> ParseResult<GlobalIndex> {
    map(leb_u32, GlobalIndex)(bytes)
}

pub fn local_index(bytes: &[u8]) -> ParseResult<LocalIndex> {
    map(leb_u32, LocalIndex)(bytes)
}

pub fn label_index(bytes: &[u8]) -> ParseResult<LabelIndex> {
    map(leb_u32, LabelIndex)(bytes)
}
