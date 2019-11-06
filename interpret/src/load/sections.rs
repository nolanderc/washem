use super::combinator::*;
use super::error::*;
use super::expression::*;
use super::indices::*;
use super::structure::*;
use super::types::*;
use super::values::*;
use nom::{branch::*, bytes::complete::*, combinator::*, multi::*, sequence::*};
use std::convert::TryFrom;
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug, Copy, Clone)]
struct SectionHeader {
    kind: SectionKind,
    size: u32,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SectionKind {
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
            _ => return Err(ParseErrorKind::UnknownSectionKind.into()),
        };

        Ok(kind)
    }
}

impl Display for SectionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let text = match self {
            SectionKind::Custom => "custom",
            SectionKind::Type => "type",
            SectionKind::Import => "import",
            SectionKind::Function => "function",
            SectionKind::Table => "table",
            SectionKind::Memory => "memory",
            SectionKind::Global => "global",
            SectionKind::Export => "export",
            SectionKind::Start => "start",
            SectionKind::Element => "element",
            SectionKind::Code => "code",
            SectionKind::Data => "data",
        };

        Display::fmt(text, f)
    }
}

fn section_header(bytes: &[u8]) -> ParseResult<SectionHeader> {
    let (bytes, kind) = section_kind(bytes)?;
    let (bytes, size) = leb_u32(bytes)?;

    Ok((bytes, SectionHeader { kind, size }))
}

fn section_kind(bytes: &[u8]) -> ParseResult<SectionKind> {
    let (bytes, byte) = byte(bytes)?;
    let kind = SectionKind::try_from(byte)?;

    Ok((bytes, kind))
}

fn expect_section(kind: SectionKind) -> impl Fn(&[u8]) -> ParseResult<&[u8]> {
    move |bytes| {
        let (bytes, header) = section_header(bytes)?;

        if header.kind != kind {
            Err(ParseErrorKind::SectionKindMismatch.into())
        } else {
            Ok(take(header.size)(bytes)?)
        }
    }
}

fn parse_section<'a, T: 'a>(
    kind: SectionKind,
    f: impl Fn(&'a [u8]) -> ParseResult<'a, T> + 'a,
) -> impl Fn(&'a [u8]) -> ParseResult<'a, T> + 'a {
    wrap_context(
        map_parser(expect_section(kind), cut(exact(f))),
        ParseLocation::Section(kind),
    )
}

#[derive(Debug, Copy, Clone)]
pub struct CustomSection {}

pub fn custom_section(bytes: &[u8]) -> ParseResult<CustomSection> {
    parse_section(SectionKind::Custom, constant(CustomSection {}))(bytes)
}

#[derive(Debug)]
pub struct TypeSection {
    functions: Vec<FunctionType>,
}

pub fn type_section(bytes: &[u8]) -> ParseResult<TypeSection> {
    parse_section(
        SectionKind::Type,
        map(vec(function_type), |functions| TypeSection { functions }),
    )(bytes)
}

#[derive(Debug)]
pub struct ImportSection {
    imports: Vec<Import>,
}

#[derive(Debug)]
pub struct Import {
    module: String,
    name: String,
    descriptor: ImportDescriptor,
}

#[derive(Debug)]
pub enum ImportDescriptor {
    Function(TypeIndex),
    Table(TableType),
    Memory(MemoryType),
    Global(GlobalType),
}

pub fn import_section(bytes: &[u8]) -> ParseResult<ImportSection> {
    parse_section(
        SectionKind::Import,
        cut(exact(map(vec(import), |imports| ImportSection { imports }))),
    )(bytes)
}

pub fn import(bytes: &[u8]) -> ParseResult<Import> {
    map(
        tuple((name, name, import_descriptor)),
        |(module, name, descriptor)| Import {
            module: module.to_string(),
            name: name.to_string(),
            descriptor,
        },
    )(bytes)
}

pub fn import_descriptor(bytes: &[u8]) -> ParseResult<ImportDescriptor> {
    use ImportDescriptor::*;
    alt((
        map(preceded(tag(&[0x00]), type_index), Function),
        map(preceded(tag(&[0x01]), table_type), Table),
        map(preceded(tag(&[0x02]), memory_type), Memory),
        map(preceded(tag(&[0x03]), global_type), Global),
    ))(bytes)
}

#[derive(Debug)]
pub struct FunctionSection {
    types: Vec<TypeIndex>,
}

pub fn function_section(bytes: &[u8]) -> ParseResult<FunctionSection> {
    parse_section(
        SectionKind::Function,
        map(vec(type_index), |types| FunctionSection { types }),
    )(bytes)
}

#[derive(Debug)]
pub struct TableSection {
    tables: Vec<Table>,
}

#[derive(Debug)]
pub struct Table {
    ty: TableType,
}

pub fn table_section(bytes: &[u8]) -> ParseResult<TableSection> {
    parse_section(
        SectionKind::Table,
        map(vec(table), |tables| TableSection { tables }),
    )(bytes)
}

pub fn table(bytes: &[u8]) -> ParseResult<Table> {
    map(table_type, |ty| Table { ty })(bytes)
}

#[derive(Debug)]
pub struct MemorySection {
    memories: Vec<Memory>,
}

#[derive(Debug)]
pub struct Memory {
    ty: MemoryType,
}

pub fn memory_section(bytes: &[u8]) -> ParseResult<MemorySection> {
    parse_section(
        SectionKind::Memory,
        map(vec(memory), |memories| MemorySection { memories }),
    )(bytes)
}

pub fn memory(bytes: &[u8]) -> ParseResult<Memory> {
    map(memory_type, |ty| Memory { ty })(bytes)
}

#[derive(Debug)]
pub struct GlobalSection {
    globals: Vec<Global>,
}

#[derive(Debug)]
pub struct Global {
    ty: GlobalType,
    expression: Expression,
}

pub fn global_section(bytes: &[u8]) -> ParseResult<GlobalSection> {
    parse_section(
        SectionKind::Global,
        map(vec(global), |globals| GlobalSection { globals }),
    )(bytes)
}

pub fn global(bytes: &[u8]) -> ParseResult<Global> {
    map(tuple((global_type, expression)), |(ty, expression)| {
        Global { ty, expression }
    })(bytes)
}

#[derive(Debug)]
pub struct ExportSection {
    exports: Vec<Export>,
}

#[derive(Debug)]
pub struct Export {
    name: String,
    descriptor: ExportDescriptor,
}

#[derive(Debug)]
pub enum ExportDescriptor {
    Function(FunctionIndex),
    Table(TableIndex),
    Memory(MemoryIndex),
    Global(GlobalIndex),
}

pub fn export_section(bytes: &[u8]) -> ParseResult<ExportSection> {
    parse_section(
        SectionKind::Export,
        map(vec(export), |exports| ExportSection { exports }),
    )(bytes)
}

pub fn export(bytes: &[u8]) -> ParseResult<Export> {
    map(tuple((name, export_descriptor)), |(name, descriptor)| {
        Export {
            name: name.to_string(),
            descriptor,
        }
    })(bytes)
}

pub fn export_descriptor(bytes: &[u8]) -> ParseResult<ExportDescriptor> {
    use ExportDescriptor::*;
    alt((
        map(preceded(tag(&[0x00]), function_index), Function),
        map(preceded(tag(&[0x01]), table_index), Table),
        map(preceded(tag(&[0x02]), memory_index), Memory),
        map(preceded(tag(&[0x03]), global_index), Global),
    ))(bytes)
}

#[derive(Debug)]
pub struct StartSection {
    start: FunctionIndex,
}

pub fn start_section(bytes: &[u8]) -> ParseResult<StartSection> {
    parse_section(
        SectionKind::Start,
        map(function_index, |start| StartSection { start }),
    )(bytes)
}

#[derive(Debug)]
pub struct ElementSection {
    elements: Vec<Element>,
}

#[derive(Debug)]
pub struct Element {
    table: TableIndex,
    offset: Expression,
    init: Vec<FunctionIndex>,
}

pub fn element_section(bytes: &[u8]) -> ParseResult<ElementSection> {
    parse_section(
        SectionKind::Element,
        map(vec(element), |elements| ElementSection { elements }),
    )(bytes)
}

fn element(bytes: &[u8]) -> ParseResult<Element> {
    map(
        tuple((table_index, expression, vec(function_index))),
        |(table, offset, init)| Element {
            table,
            offset,
            init,
        },
    )(bytes)
}

#[derive(Debug)]
pub struct CodeSection {
    segments: Vec<Code>,
}

#[derive(Debug)]
pub struct Code {
    locals: Vec<Locals>,
    body: Expression,
}

#[derive(Debug)]
pub struct Locals {
    count: u32,
    ty: ValueType,
}

pub fn code_section(bytes: &[u8]) -> ParseResult<CodeSection> {
    parse_section(
        SectionKind::Code,
        map(vec(code), |segments| CodeSection { segments }),
    )(bytes)
}

fn code(bytes: &[u8]) -> ParseResult<Code> {
    length_value(
        leb_u32,
        map(tuple((vec(locals), expression)), |(locals, body)| Code {
            locals,
            body,
        }),
    )(bytes)
}

fn locals(bytes: &[u8]) -> ParseResult<Locals> {
    map(tuple((leb_u32, value_type)), |(count, ty)| Locals {
        count,
        ty,
    })(bytes)
}

#[derive(Debug)]
pub struct DataSection {
    segments: Vec<DataSegment>,
}

#[derive(Debug)]
struct DataSegment {
    memory: MemoryIndex,
    offset: Expression,
    bytes: Vec<u8>,
}

pub fn data_section(bytes: &[u8]) -> ParseResult<DataSection> {
    parse_section(
        SectionKind::Code,
        map(vec(data_segment), |segments| DataSection { segments }),
    )(bytes)
}

fn data_segment(bytes: &[u8]) -> ParseResult<DataSegment> {
    map(
        tuple((memory_index, expression, vec(byte))),
        |(memory, offset, bytes)| DataSegment {
            memory,
            offset,
            bytes,
        },
    )(bytes)
}
