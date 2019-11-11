use crate::ast::prelude::*;
use crate::ast::{Module as AstModule, ModuleSections};
use crate::error::*;

#[derive(Debug)]
pub struct Module {
    pub types: Vec<FunctionType>,
    pub functions: Vec<Function>,
    pub tables: Vec<Table>,
    pub memories: Vec<Memory>,
    pub globals: Vec<Global>,
    pub elements: Vec<Element>,
    pub data_segments: Vec<DataSegment>,
    pub start: Option<Start>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub ty: TypeIndex,
    pub locals: Vec<ValueType>,
    pub body: Expression,
}

#[derive(Debug, Copy, Clone)]
pub struct Start {
    pub function: FunctionIndex,
}

impl Module {
    pub fn from_ast(ast: AstModule) -> Result<Module> {
        let ModuleSections {
            types,
            import,
            function,
            table,
            memory,
            global,
            export,
            start,
            element,
            code,
            data,
            ..
        } = ast.sections;

        let types = types.map(|section| section.functions).unwrap_or_default();

        let type_indices = function.map(|section| section.types).unwrap_or_default();
        let code_segments = code.map(|section| section.segments).unwrap_or_default();
        let functions = type_indices
            .into_iter()
            .zip(code_segments)
            .map(|(ty, Code { locals, body })| Function {
                ty,
                locals: flatten_locals(locals),
                body,
            })
            .collect();

        let tables = table.map(|section| section.tables).unwrap_or_default();
        let elements = element.map(|section| section.elements).unwrap_or_default();

        let globals = global.map(|section| section.globals).unwrap_or_default();
        let memories = memory.map(|section| section.memories).unwrap_or_default();
        let data_segments = data.map(|section| section.segments).unwrap_or_default();

        let start = start.map(|section| Start {
            function: section.start,
        });

        let imports = import.map(|section| section.imports).unwrap_or_default();
        let exports = export.map(|section| section.exports).unwrap_or_default();

        let module = Module {
            types,
            functions,
            tables,
            memories,
            globals,
            elements,
            data_segments,
            start,
            imports,
            exports,
        };

        Ok(module)
    }
}

pub fn flatten_locals(locals: Vec<Locals>) -> Vec<ValueType> {
    let mut flattened = Vec::new();

    for locals in locals {
        flattened.resize(flattened.len() + locals.count as usize, locals.ty);
    }

    flattened
}
