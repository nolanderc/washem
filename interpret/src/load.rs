mod error;
mod numbers;
mod structure;

use self::error::*;
use self::numbers::*;
use self::structure::*;
use nom::{bytes::complete::*, number::complete::*, sequence::*, IResult};
use std::convert::TryFrom;

pub struct Module {}

struct ModuleSections {
    pre_types: CustomSection,
    types: TypeSection,
    pre_import: CustomSection,
    import: ImportSection,
    pre_function: CustomSection,
    function: FunctionSection,
    pre_table: CustomSection,
    table: TableSection,
    pre_memory: CustomSection,
    memory: MemorySection,
    pre_global: CustomSection,
    global: GlobalSection,
    pre_export: CustomSection,
    export: ExportSection,
    pre_start: CustomSection,
    start: StartSection,
    pre_element: CustomSection,
    element: ElementSection,
    pre_code: CustomSection,
    code: CodeSection,
    pre_data: CustomSection,
    data: DataSection,
}

pub fn parse_module(bytes: &[u8]) -> Result<Module> {
    match module(bytes) {
        Ok((_, module)) => Ok(module),
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            Err(format!("Failed to parse module: {:?}", e).into())
        }
        Err(nom::Err::Incomplete(_)) => unreachable!("parse incomplete"),
    }
}

fn module(bytes: &[u8]) -> ParseResult<Module> {
    let (_, bytes) = module_magic(bytes)?;
    let (_, bytes) = module_version(bytes)?;

    Ok((bytes, Module {}))
}

fn module_magic(bytes: &[u8]) -> ParseResult<&[u8]> {
    tag(b"\0asm")(bytes)
}

fn module_version(bytes: &[u8]) -> ParseResult<&[u8]> {
    tag(b"\x01\x00\x00\x00")(bytes)
}

#[derive(Debug, Copy, Clone)]
struct SectionHeader {
    kind: SectionKind,
    size: u32,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum SectionKind {
    Custom,
    Type,
    Import,
    Function,
    Table,
    Memory,
    Global,
    Export,
    Start,
    Element,
    Code,
    Data,
}

impl TryFrom<u8> for SectionKind {
    type Error = ParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        let kind = match value {
            0 => SectionKind::Custom,
            1 => SectionKind::Type,
            2 => SectionKind::Import,
            3 => SectionKind::Function,
            4 => SectionKind::Table,
            5 => SectionKind::Memory,
            6 => SectionKind::Global,
            7 => SectionKind::Export,
            8 => SectionKind::Start,
            9 => SectionKind::Element,
            10 => SectionKind::Code,
            11 => SectionKind::Data,
            _ => return Err(ParseError::UnknownSectionKind),
        };

        Ok(kind)
    }
}

fn section_header(bytes: &[u8]) -> ParseResult<SectionHeader> {
    let (bytes, kind) = section_kind(bytes)?;
    let (bytes, size) = leb_u32(bytes)?;

    Ok((bytes, SectionHeader { kind, size }))
}

fn section_kind(bytes: &[u8]) -> ParseResult<SectionKind> {
    let (bytes, byte) = le_u8(bytes)?;
    let kind = SectionKind::try_from(byte)?;

    Ok((bytes, kind))
}

#[derive(Debug)]
struct CustomSection {}
#[derive(Debug)]
struct ImportSection {}
#[derive(Debug)]
struct FunctionSection {}
#[derive(Debug)]
struct TableSection {}
#[derive(Debug)]
struct MemorySection {}
#[derive(Debug)]
struct GlobalSection {}
#[derive(Debug)]
struct ExportSection {}
#[derive(Debug)]
struct StartSection {}
#[derive(Debug)]
struct ElementSection {}
#[derive(Debug)]
struct CodeSection {}
#[derive(Debug)]
struct DataSection {}

fn expect_section(kind: SectionKind) -> impl Fn(&[u8]) -> ParseResult<SectionHeader> {
    move |bytes| {
        let (bytes, header) = section_header(bytes)?;
        if header.kind != kind {
            Err(ParseError::SectionKindMismatch)?
        } else {
            Ok((bytes, header))
        }
    }
}

fn custom_section(bytes: &[u8]) -> ParseResult<CustomSection> {
    let (bytes, header) = expect_section(SectionKind::Custom)(bytes)?;
    let (bytes, _content) = take(header.size)(bytes)?;

    Ok((bytes, CustomSection {}))
}

#[derive(Debug)]
struct TypeSection {
    functions: Vec<FunctionType>,
}

#[derive(Debug)]
struct FunctionType {
    parameters: Vec<ValueType>,
    results: Vec<ValueType>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum ValueType {
    I32,
    I64,
    F32,
    F64,
}

fn type_section(bytes: &[u8]) -> ParseResult<TypeSection> {
    let (bytes, header) = expect_section(SectionKind::Type)(bytes)?;
    let (bytes, content) = take(header.size)(bytes)?;

    let (bytes, functions) = vec(function_type)(bytes)?;

    Ok((bytes, TypeSection { functions }))
}

fn function_type(bytes: &[u8]) -> ParseResult<FunctionType> {
    let (bytes, _) = tag(b"0x60")(bytes)?;
    let (bytes, parameters) = vec(value_type)(bytes)?;
    let (bytes, results) = vec(value_type)(bytes)?;

    Ok((
        bytes,
        FunctionType {
            parameters,
            results,
        },
    ))
}

fn value_type(bytes: &[u8]) -> ParseResult<ValueType> {
    let value_type = match bytes.get(0).ok_or(ParseError::UnexpectedEof)? {
        0x7F => ValueType::I32,
        0x7E => ValueType::I64,
        0x7D => ValueType::F32,
        0x7C => ValueType::F64,
        _ => Err(ParseError::InvalidValueType)?,
    };

    Ok((&bytes[1..], value_type))
}
