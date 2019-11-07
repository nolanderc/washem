use crate::error::*;

mod address;

use self::address::*;
use crate::ast::expression::*;
use crate::ast::indices::*;
use crate::ast::types::*;
use crate::ast::*;

#[derive(Debug)]
pub struct Executor {
    store: Store,
}

#[derive(Debug)]
struct Store {
    functions: Vec<FunctionInstance>,
    tables: Vec<TableInstance>,
    memories: Vec<MemoryInstance>,
    globals: Vec<GlobalInstance>,
}

#[derive(Debug)]
struct FunctionInstance {
    ty: FunctionType,
    code: FunctionCode,
}

#[derive(Debug)]
enum FunctionCode {
    Local(Function),
    Host(HostFunction),
}

#[derive(Debug)]
struct Function {
    ty: TypeIndex,
    locals: Vec<ValueType>,
    body: Expression,
}

#[derive(Debug)]
struct HostFunction {}

#[derive(Debug)]
struct TableInstance {
    elements: Vec<Option<TableElement>>,
    max_size: Option<u32>,
}

#[derive(Debug)]
enum TableElement {
    Function(FunctionAddress),
}

#[derive(Debug)]
struct MemoryInstance {
    bytes: Vec<u8>,
    max_size: Option<u32>,
}

#[derive(Debug)]
struct GlobalInstance {
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
struct ModuleInstance {
    types: Vec<FunctionType>,
    functions: Vec<FunctionAddress>,
    tables: Vec<TableAddress>,
    memories: Vec<MemoryAddress>,
    globals: Vec<GlobalAddress>,
    exports: Vec<ExportInstance>,
}

#[derive(Debug)]
struct ExportInstance {
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
struct Stack {
    items: Vec<StackItem>,
}

#[derive(Debug)]
enum StackItem {
    Value(Value),
    Label(Label),
    Frame(Frame),
}

#[derive(Debug)]
struct Label {
    arity: u32,
}

#[derive(Debug)]
struct Frame {
    arity: u32,
    locals: Vec<Value>,
}

impl Executor {
    pub fn new() -> Self {
        Executor {
            store: Store::new(),
        }
    }

    pub fn instantiate(&mut self, module: Module) -> Result<()> {
        self.validate(&module)?;

        Ok(())
    }

    fn validate(&self, _module: &Module) -> Result<()> {
        eprintln!("Warning: validation not implemented");
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

