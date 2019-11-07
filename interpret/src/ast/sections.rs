use super::expression::*;
use super::indices::*;
use super::types::*;
use std::fmt::{Display, Formatter, Result as FmtResult};

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
#[derive(Debug, Copy, Clone)]
pub struct CustomSection {}

#[derive(Debug)]
pub struct TypeSection {
    pub functions: Vec<FunctionType>,
}

#[derive(Debug)]
pub struct ImportSection {
    pub imports: Vec<Import>,
}

#[derive(Debug)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub descriptor: ImportDescriptor,
}

#[derive(Debug)]
pub enum ImportDescriptor {
    Function(TypeIndex),
    Table(TableType),
    Memory(MemoryType),
    Global(GlobalType),
}

#[derive(Debug)]
pub struct FunctionSection {
    pub types: Vec<TypeIndex>,
}

#[derive(Debug)]
pub struct TableSection {
    pub tables: Vec<Table>,
}

#[derive(Debug)]
pub struct Table {
    pub ty: TableType,
}

#[derive(Debug)]
pub struct MemorySection {
    pub memories: Vec<Memory>,
}

#[derive(Debug)]
pub struct Memory {
    pub ty: MemoryType,
}

#[derive(Debug)]
pub struct GlobalSection {
    pub globals: Vec<Global>,
}

#[derive(Debug)]
pub struct Global {
    pub ty: GlobalType,
    pub expression: Expression,
}

#[derive(Debug)]
pub struct ExportSection {
    pub exports: Vec<Export>,
}

#[derive(Debug)]
pub struct Export {
    pub name: String,
    pub descriptor: ExportDescriptor,
}

#[derive(Debug)]
pub enum ExportDescriptor {
    Function(FunctionIndex),
    Table(TableIndex),
    Memory(MemoryIndex),
    Global(GlobalIndex),
}

#[derive(Debug)]
pub struct StartSection {
    pub start: FunctionIndex,
}

#[derive(Debug)]
pub struct ElementSection {
    pub elements: Vec<Element>,
}

#[derive(Debug)]
pub struct Element {
    pub table: TableIndex,
    pub offset: Expression,
    pub init: Vec<FunctionIndex>,
}

#[derive(Debug)]
pub struct CodeSection {
    pub segments: Vec<Code>,
}

#[derive(Debug)]
pub struct Code {
    pub locals: Vec<Locals>,
    pub body: Expression,
}

#[derive(Debug)]
pub struct Locals {
    pub count: u32,
    pub ty: ValueType,
}

#[derive(Debug)]
pub struct DataSection {
    pub segments: Vec<DataSegment>,
}

#[derive(Debug)]
pub struct DataSegment {
    pub memory: MemoryIndex,
    pub offset: Expression,
    pub bytes: Vec<u8>,
}
