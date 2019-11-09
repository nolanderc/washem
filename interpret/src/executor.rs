#![allow(dead_code)]

mod address;
mod validation;

use self::address::*;
use self::validation::*;
use crate::ast::prelude::*;
use crate::error::*;
use crate::module::*;

#[derive(Debug)]
pub struct Executor {
    store: Store,
}

#[derive(Debug)]
pub struct Store {
    functions: Vec<FunctionInstance>,
    tables: Vec<TableInstance>,
    memories: Vec<MemoryInstance>,
    globals: Vec<GlobalInstance>,
}

#[derive(Debug)]
pub struct FunctionInstance {
    ty: FunctionType,
    code: FunctionCode,
}

#[derive(Debug)]
enum FunctionCode {
    Local(Function),
    Host(HostFunction),
}

#[derive(Debug)]
pub struct HostFunction {}

#[derive(Debug)]
pub struct TableInstance {
    elements: Vec<Option<TableElement>>,
    max_size: Option<u32>,
}

#[derive(Debug)]
enum TableElement {
    Function(FunctionAddress),
}

#[derive(Debug)]
pub struct MemoryInstance {
    bytes: Vec<u8>,
    max_size: Option<u32>,
}

#[derive(Debug)]
pub struct GlobalInstance {
    value: Value,
    mutability: Mutability,
}

#[derive(Debug, Copy, Clone)]
enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Debug)]
pub struct ModuleInstance {
    types: Vec<FunctionType>,
    functions: Vec<FunctionAddress>,
    tables: Vec<TableAddress>,
    memories: Vec<MemoryAddress>,
    globals: Vec<GlobalAddress>,
    exports: Vec<ExportInstance>,
}

#[derive(Debug)]
pub struct ExportInstance {
    name: String,
    external: ExternalValue,
}

#[derive(Debug)]
enum ExternalValue {
    Function(FunctionAddress),
    Table(TableAddress),
    Memory(MemoryAddress),
    Global(GlobalAddress),
}

#[derive(Debug)]
pub struct Stack {
    items: Vec<StackItem>,
}

#[derive(Debug)]
enum StackItem {
    Value(Value),
    Label(Label),
    Frame(Frame),
}

#[derive(Debug)]
pub struct Label {
    arity: u32,
}

#[derive(Debug)]
pub struct Frame {
    arity: u32,
    locals: Vec<Value>,
}

impl Executor {
    pub fn new() -> Self {
        Executor {
            store: Store::new(),
        }
    }

    pub fn instantiate(&mut self, module: Module) -> Result<ModuleInstance> {
        self.validate(&module)?;

        Ok(ModuleInstance {
            types: Vec::new(),
            functions: Vec::new(),
            tables: Vec::new(),
            memories: Vec::new(),
            globals: Vec::new(),
            exports: Vec::new(),
        })
    }

    fn validate(&self, module: &Module) -> Result<()> {
        eprintln!("Warning: validation not fully implemented");

        // 1. Check that module is valid
        validate_module(module)?;

        // 2. Assert: module is valid with external types classifying its imports.

        // 3. Assert: the number m of imports is not equal to the number n of provided external values

        // 4. For each external value and external type:
        // 4.1. Assert: externval is valid with an external type in store S
        // 4.2. Assert: the externval's type matches the corresponding external type

        Ok(())
    }
}

impl Store {
    pub fn new() -> Self {
        Store {
            functions: Vec::new(),
            tables: Vec::new(),
            memories: Vec::new(),
            globals: Vec::new(),
        }
    }
}
