#![allow(dead_code)]

use super::error::*;
use super::values::*;
use nom::combinator::*;

#[derive(Debug)]
pub struct TypeIndex(u32);

pub fn type_index(bytes: &[u8]) -> ParseResult<TypeIndex> {
    map(leb_u32, TypeIndex)(bytes)
}

#[derive(Debug)]
pub struct FunctionIndex(u32);

pub fn function_index(bytes: &[u8]) -> ParseResult<FunctionIndex> {
    map(leb_u32, FunctionIndex)(bytes)
}

#[derive(Debug)]
pub struct TableIndex(u32);

pub fn table_index(bytes: &[u8]) -> ParseResult<TableIndex> {
    map(leb_u32, TableIndex)(bytes)
}

#[derive(Debug)]
pub struct MemoryIndex(u32);

pub fn memory_index(bytes: &[u8]) -> ParseResult<MemoryIndex> {
    map(leb_u32, MemoryIndex)(bytes)
}

#[derive(Debug)]
pub struct GlobalIndex(u32);

pub fn global_index(bytes: &[u8]) -> ParseResult<GlobalIndex> {
    map(leb_u32, GlobalIndex)(bytes)
}

#[derive(Debug)]
pub struct LocalIndex(u32);

pub fn local_index(bytes: &[u8]) -> ParseResult<LocalIndex> {
    map(leb_u32, LocalIndex)(bytes)
}

#[derive(Debug)]
pub struct LabelIndex(u32);

pub fn label_index(bytes: &[u8]) -> ParseResult<LabelIndex> {
    map(leb_u32, LabelIndex)(bytes)
}
