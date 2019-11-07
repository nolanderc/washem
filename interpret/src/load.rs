#[macro_use]
mod error;
mod combinator;
mod expression;
mod indices;
mod instruction;
mod sections;
mod structure;
mod types;
mod values;

use self::error::*;
use self::sections::*;
use crate::ast::*;
use nom::{bytes::complete::*, combinator::*};

pub fn parse_module(bytes: &[u8]) -> Result<Module> {
    match module(bytes) {
        Ok((_, module)) => Ok(module),
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => Err(e.into()),
        Err(nom::Err::Incomplete(_)) => unreachable!("parse incomplete"),
    }
}

fn module(bytes: &[u8]) -> ParseResult<Module> {
    let (bytes, _) = module_magic(bytes)?;
    let (bytes, _) = module_version(bytes)?;

    let (bytes, pre_types) = opt(custom_section)(bytes)?;
    let (bytes, types) = opt(type_section)(bytes)?;
    let (bytes, pre_import) = opt(custom_section)(bytes)?;
    let (bytes, import) = opt(import_section)(bytes)?;
    let (bytes, pre_function) = opt(custom_section)(bytes)?;
    let (bytes, function) = opt(function_section)(bytes)?;
    let (bytes, pre_table) = opt(custom_section)(bytes)?;
    let (bytes, table) = opt(table_section)(bytes)?;
    let (bytes, pre_memory) = opt(custom_section)(bytes)?;
    let (bytes, memory) = opt(memory_section)(bytes)?;
    let (bytes, pre_global) = opt(custom_section)(bytes)?;
    let (bytes, global) = opt(global_section)(bytes)?;
    let (bytes, pre_export) = opt(custom_section)(bytes)?;
    let (bytes, export) = opt(export_section)(bytes)?;
    let (bytes, pre_start) = opt(custom_section)(bytes)?;
    let (bytes, start) = opt(start_section)(bytes)?;
    let (bytes, pre_element) = opt(custom_section)(bytes)?;
    let (bytes, element) = opt(element_section)(bytes)?;
    let (bytes, pre_code) = opt(custom_section)(bytes)?;
    let (bytes, code) = opt(code_section)(bytes)?;
    let (bytes, pre_data) = opt(custom_section)(bytes)?;
    let (bytes, data) = opt(data_section)(bytes)?;

    Ok((
        bytes,
        Module {
            sections: ModuleSections {
                pre_types,
                types,
                pre_import,
                import,
                pre_function,
                function,
                pre_table,
                table,
                pre_memory,
                memory,
                pre_global,
                global,
                pre_export,
                export,
                pre_start,
                start,
                pre_element,
                element,
                pre_code,
                code,
                pre_data,
                data,
            },
        },
    ))
}

fn module_magic(bytes: &[u8]) -> ParseResult<&[u8]> {
    tag(b"\0asm")(bytes)
}

fn module_version(bytes: &[u8]) -> ParseResult<&[u8]> {
    tag(b"\x01\x00\x00\x00")(bytes)
}
