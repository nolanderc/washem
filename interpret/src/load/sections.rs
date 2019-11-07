use super::combinator::*;
use super::error::*;
use super::expression::*;
use super::indices::*;
use super::structure::*;
use super::types::*;
use super::values::*;
use crate::ast::sections::*;
use nom::{branch::*, bytes::complete::*, combinator::*, multi::*, sequence::*};
use std::convert::TryFrom;

#[derive(Debug, Copy, Clone)]
struct SectionHeader {
    kind: SectionKind,
    size: u32,
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

pub fn custom_section(bytes: &[u8]) -> ParseResult<CustomSection> {
    parse_section(SectionKind::Custom, map(rest, |_bytes| CustomSection {}))(bytes)
}

pub fn type_section(bytes: &[u8]) -> ParseResult<TypeSection> {
    parse_section(
        SectionKind::Type,
        map(vec(function_type), |functions| TypeSection { functions }),
    )(bytes)
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

pub fn function_section(bytes: &[u8]) -> ParseResult<FunctionSection> {
    parse_section(
        SectionKind::Function,
        map(vec(type_index), |types| FunctionSection { types }),
    )(bytes)
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

pub fn memory_section(bytes: &[u8]) -> ParseResult<MemorySection> {
    parse_section(
        SectionKind::Memory,
        map(vec(memory), |memories| MemorySection { memories }),
    )(bytes)
}

pub fn memory(bytes: &[u8]) -> ParseResult<Memory> {
    map(memory_type, |ty| Memory { ty })(bytes)
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

pub fn start_section(bytes: &[u8]) -> ParseResult<StartSection> {
    parse_section(
        SectionKind::Start,
        map(function_index, |start| StartSection { start }),
    )(bytes)
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
