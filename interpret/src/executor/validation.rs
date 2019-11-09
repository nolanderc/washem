#![warn(dead_code)]

mod error;
mod instruction;

use self::error::*;
use self::instruction::*;
use crate::ast::prelude::*;
use crate::module::*;
use std::collections::HashSet;

#[derive(Debug, Clone)]
struct Context {
    types: Vec<FunctionType>,
    functions: Vec<FunctionType>,
    tables: Vec<TableType>,
    memories: Vec<MemoryType>,
    globals: Vec<GlobalType>,
    locals: Vec<ValueType>,
    ret: Option<ResultType>,
}

pub fn validate_module(module: &Module) -> ValidationResult<()> {
    let context = Context::new(module)?;

    validate_context(&context)?;

    validate_function_types(module)?;
    validate_functions(module, &context)?;
    validate_tables(module)?;
    validate_memories(module)?;
    validate_globals(module, &context)?;
    validate_element_segments(module, &context)?;
    validate_data_segments(module, &context)?;
    validate_start(module, &context)?;
    validate_exports(module, &context)?;
    validate_imports(module, &context)?;

    Ok(())
}

fn validate_context(context: &Context) -> ValidationResult<()> {
    if context.memories.len() > 1 {
        Err(ValidationError::MultipleMemories)
    } else if context.tables.len() > 1 {
        Err(ValidationError::MultipleTables)
    } else {
        Ok(())
    }
}

fn validate_function_types(module: &Module) -> ValidationResult<()> {
    for ty in &module.types {
        validate_function_type(ty)?;
    }

    Ok(())
}

fn validate_function_type(ty: &FunctionType) -> ValidationResult<()> {
    if ty.results.len() > 1 {
        Err(ValidationError::MultipleReturns)
    } else {
        Ok(())
    }
}

fn validate_functions(module: &Module, context: &Context) -> ValidationResult<()> {
    for func in &module.functions {
        let TypeIndex(index) = func.ty;

        match context.types.get(index as usize) {
            None => return Err(ValidationError::InvalidFunctionType),
            Some(ty) => {
                assert!(ty.results.len() <= 1);

                let value = ty.results.last().copied();

                let result = ResultType { types: value };

                let inner_context = Context {
                    locals: ty.parameters.iter().chain(&func.locals).copied().collect(),
                    ret: Some(result.clone()),
                    ..context.clone()
                };

                validate_expression(&func.body, &result, &inner_context)?;
            }
        }
    }

    Ok(())
}

fn validate_tables(module: &Module) -> ValidationResult<()> {
    for table in &module.tables {
        validate_table_type(&table.ty)?;
    }

    Ok(())
}

fn validate_table_type(table: &TableType) -> ValidationResult<()> {
    validate_limits(table.limits, u32::max_value())
}

fn validate_memories(module: &Module) -> ValidationResult<()> {
    for memory in &module.memories {
        validate_memory_type(&memory.ty)?;
    }

    Ok(())
}

fn validate_memory_type(table: &MemoryType) -> ValidationResult<()> {
    validate_limits(table.limits, 1 << 16)
}

fn validate_limits(limits: Limits, range: u32) -> ValidationResult<()> {
    if limits.lower > range {
        return Err(ValidationError::InvalidLimits);
    }

    if let Some(upper) = limits.upper {
        if upper > range || upper < limits.lower {
            return Err(ValidationError::InvalidLimits);
        }
    }

    Ok(())
}

fn validate_globals(module: &Module, context: &Context) -> ValidationResult<()> {
    for global in &module.globals {
        let result = ResultType {
            types: Some(global.ty.ty),
        };
        validate_expression(&global.expression, &result, context)?;
        validate_constant_expression(&global.expression, context)?;
    }

    Ok(())
}

fn validate_element_segments(module: &Module, context: &Context) -> ValidationResult<()> {
    for element in &module.elements {
        let TableIndex(table_index) = element.table;
        let table = context
            .tables
            .get(table_index as usize)
            .ok_or(ValidationError::TableNotFound)?;

        match table.element {
            ElementType::FunctionReference => (),
        }

        let result = ResultType {
            types: Some(ValueType::I32),
        };
        validate_expression(&element.offset, &result, context)?;
        validate_constant_expression(&element.offset, context)?;

        for &FunctionIndex(index) in &element.init {
            context
                .functions
                .get(index as usize)
                .ok_or(ValidationError::FunctionNotFound)?;
        }
    }

    Ok(())
}

fn validate_data_segments(module: &Module, context: &Context) -> ValidationResult<()> {
    for data in &module.data_segments {
        let MemoryIndex(memory_index) = data.memory;
        let _memory = context
            .memories
            .get(memory_index as usize)
            .ok_or(ValidationError::MemoryNotFound)?;

        let result = ResultType {
            types: Some(ValueType::I32),
        };
        validate_expression(&data.offset, &result, context)?;
        validate_constant_expression(&data.offset, context)?;
    }

    Ok(())
}

fn validate_start(module: &Module, context: &Context) -> ValidationResult<()> {
    if let Some(start) = module.start {
        let FunctionIndex(start_index) = start.function;
        let function = context
            .functions
            .get(start_index as usize)
            .ok_or(ValidationError::FunctionNotFound)?;

        if !function.parameters.is_empty() || !function.results.is_empty() {
            return Err(ValidationError::InvalidFunctionType);
        }
    }

    Ok(())
}

fn validate_exports(module: &Module, context: &Context) -> ValidationResult<()> {
    let mut names = HashSet::new();

    for export in &module.exports {
        if !names.insert(export.name.as_str()) {
            return Err(ValidationError::DuplicateExportName);
        }

        match export.descriptor {
            ExportDescriptor::Function(FunctionIndex(index)) => {
                context
                    .functions
                    .get(index as usize)
                    .ok_or(ValidationError::FunctionNotFound)?;
            }
            ExportDescriptor::Table(TableIndex(index)) => {
                context
                    .tables
                    .get(index as usize)
                    .ok_or(ValidationError::TableNotFound)?;
            }
            ExportDescriptor::Memory(MemoryIndex(index)) => {
                context
                    .memories
                    .get(index as usize)
                    .ok_or(ValidationError::MemoryNotFound)?;
            }
            ExportDescriptor::Global(GlobalIndex(index)) => {
                context
                    .globals
                    .get(index as usize)
                    .ok_or(ValidationError::GlobalNotFound)?;
            }
        }
    }

    Ok(())
}

fn validate_imports(module: &Module, context: &Context) -> ValidationResult<()> {
    for import in &module.imports {
        match import.descriptor {
            ImportDescriptor::Function(TypeIndex(index)) => {
                context
                    .types
                    .get(index as usize)
                    .ok_or(ValidationError::FunctionNotFound)?;
            }
            ImportDescriptor::Table(ty) => validate_table_type(&ty)?,
            ImportDescriptor::Memory(memory) => {
                validate_memory_type(&memory)?;
            }
            ImportDescriptor::Global(_) => {}
        }
    }

    Ok(())
}

impl Context {
    pub fn new(module: &Module) -> ValidationResult<Context> {
        let context = Context {
            types: module.types.clone(),
            functions: Self::functions(module)?,
            tables: Self::tables(module),
            memories: Self::memories(module),
            globals: Self::globals(module),
            locals: Vec::new(),
            ret: None,
        };

        Ok(context)
    }

    fn functions(module: &Module) -> ValidationResult<Vec<FunctionType>> {
        module
            .imports
            .iter()
            .filter_map(|import| import.descriptor.function())
            .chain(module.functions.iter().map(|function| function.ty))
            .map(|type_index| {
                let TypeIndex(index) = type_index;
                let func_type = module
                    .types
                    .get(index as usize)
                    .ok_or_else(|| ValidationError::InvalidIndex(type_index.into()))?;
                Ok(func_type.clone())
            })
            .collect()
    }

    fn tables(module: &Module) -> Vec<TableType> {
        module
            .imports
            .iter()
            .filter_map(|import| import.descriptor.table())
            .chain(module.tables.iter().map(|table| table.ty))
            .collect()
    }

    fn memories(module: &Module) -> Vec<MemoryType> {
        module
            .imports
            .iter()
            .filter_map(|import| import.descriptor.memory())
            .chain(module.memories.iter().map(|memory| memory.ty))
            .collect()
    }

    fn globals(module: &Module) -> Vec<GlobalType> {
        module
            .imports
            .iter()
            .filter_map(|import| import.descriptor.global())
            .chain(module.globals.iter().map(|global| global.ty))
            .collect()
    }
}
