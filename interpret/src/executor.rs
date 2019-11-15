#![allow(dead_code)]

mod address;
mod error;
mod execution;
mod validation;

use self::address::*;
use self::error::*;
use self::execution::*;
use self::validation::*;
use crate::ast::prelude::*;
use crate::module::*;
use derive_more::{Deref, DerefMut, From};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::rc::Rc;

pub use self::address::*;

#[derive(Debug)]
pub struct Executor {
    store: Store,
    modules: HashMap<String, Rc<ModuleInstance>>,
    stack: Stack,
}

#[derive(Debug, Clone)]
pub struct Store {
    functions: Vec<FunctionInstance>,
    tables: Vec<TableInstance>,
    memories: Vec<MemoryInstance>,
    globals: Vec<GlobalInstance>,
}

#[derive(Debug, Clone)]
pub struct FunctionInstance {
    module: Rc<ModuleInstance>,
    ty: FunctionType,
    code: FunctionCode,
}

#[derive(Debug, Clone)]
enum FunctionCode {
    Local(Function),
    Host(HostFunction),
}

#[derive(Debug, Clone)]
pub enum HostFunction {
    Print,
}

#[derive(Debug, Clone)]
pub struct TableInstance {
    ty: TableType,
    elements: Vec<Option<TableElement>>,
    max_size: Option<u32>,
}

#[derive(Debug, Copy, Clone)]
enum TableElement {
    Function(FunctionAddress),
}

#[derive(Debug, Clone)]
pub struct MemoryInstance {
    ty: MemoryType,
    bytes: ByteArray,
    max_size: Option<u32>,
}

#[derive(Clone, From, Deref, DerefMut)]
struct ByteArray {
    bytes: Vec<u8>,
}

#[derive(Debug, Copy, Clone)]
pub struct GlobalInstance {
    value: Value,
    mutability: Mutability,
}

#[derive(Debug)]
pub struct ModuleInstance {
    types: Vec<FunctionType>,
    functions: Vec<FunctionAddress>,
    tables: Vec<TableAddress>,
    memories: Vec<MemoryAddress>,
    globals: Vec<GlobalAddress>,
    exports: HashMap<String, ExternalValue>,
}

#[derive(Debug, Copy, Clone)]
enum ExternalValue {
    Function(FunctionAddress),
    Table(TableAddress),
    Memory(MemoryAddress),
    Global(GlobalAddress),
}

impl Executor {
    pub fn new() -> Self {
        Executor {
            store: Store::new(),
            modules: HashMap::new(),
            stack: Stack::new(),
        }
    }

    pub fn instantiate_env(&mut self) -> Result<()> {
        let function_instances = vec![(
            "print",
            FunctionType {
                parameters: vec![ValueType::I32],
                results: vec![],
            },
            FunctionCode::Host(HostFunction::Print),
        )];

        let mut types = Vec::new();
        let mut functions = Vec::new();
        let tables = Vec::new();
        let memories = Vec::new();
        let globals = Vec::new();
        let mut exports = HashMap::new();

        for (i, (name, ty, _)) in function_instances.iter().enumerate() {
            types.push(ty.clone());

            let index = self.store.functions.len() + i;
            let addr = FunctionAddress(index as u32);
            functions.push(addr);
            exports.insert(String::from(*name), ExternalValue::Function(addr));
        }

        let module = ModuleInstance {
            types,
            functions,
            tables,
            memories,
            globals,
            exports,
        };

        let module = Rc::new(module);

        for (_, ty, code) in function_instances {
            self.store.functions.push(FunctionInstance {
                module: Rc::clone(&module),
                ty,
                code,
            })
        }

        self.modules.insert("env".to_owned(), module);

        Ok(())
    }

    pub fn instantiate(
        &mut self,
        name: impl Into<String>,
        module: Module,
    ) -> Result<Rc<ModuleInstance>> {
        validate_module(&module)?;

        let start = module.start;

        let imports = self.resolve_imports(&module)?;
        let globals = self.evaluate_globals(&module, &imports)?;
        let instance = self.store.allocate_module(module, imports, globals)?;

        self.modules.insert(name.into(), Rc::clone(&instance));

        if let Some(start) = start {
            let FunctionIndex(index) = start.function;
            let function = instance.functions[index as usize];
            let mut context = self.store.as_context();
            let mut stack = Stack::new();

            context.invoke(function, &mut stack)?;
        }

        Ok(instance)
    }

    pub fn call(&mut self, function: FunctionAddress, args: Vec<Value>) -> Result<Vec<Value>> {
        let FunctionAddress(address) = function;
        let instance = self
            .store
            .functions
            .get(address as usize)
            .ok_or_else(|| err!("function not found: {}", address))?;

        expect_argument_match(&instance.ty, &args)?;

        let dummy = Frame {
            arity: 0,
            locals: Vec::new(),
            module: Rc::new(ModuleInstance::new()),
        };

        self.stack.push(dummy);

        for arg in args {
            self.stack.push(arg);
        }

        let mut context = Context {
            functions: &self.store.functions,
            tables: &mut self.store.tables,
            memories: &mut self.store.memories,
            globals: &mut self.store.globals,
        };

        context.invoke(function, &mut self.stack)?;

        (0..instance.ty.results.len())
            .map(|_| self.stack.pop_value())
            .collect::<Option<_>>()
            .ok_or_else(|| err!("failed to get function results"))
    }

    fn resolve_imports(&self, module: &Module) -> InstantiationResult<Vec<ExternalValue>> {
        use InstantiationError::*;

        module
            .imports
            .iter()
            .map(|import: &Import| -> InstantiationResult<_> {
                let instance = self
                    .modules
                    .get(&import.module)
                    .ok_or_else(|| ModuleNotFound(import.module.clone()))?;

                let export = *instance.exports.get(&import.name).ok_or(ExportNotFound)?;

                match export {
                    ExternalValue::Function(func) => {
                        let TypeIndex(type_index) =
                            import.descriptor.function().ok_or(ImportTypeMismatch)?;

                        let expected_type =
                            module.types.get(type_index as usize).ok_or(TypeNotFound)?;

                        let func_instance =
                            self.store.function(func).ok_or(InvalidFunctionAddress)?;

                        if !func_instance.ty.matches(expected_type) {
                            return Err(ImportTypeMismatch);
                        }
                    }

                    ExternalValue::Table(table) => {
                        let expected = import.descriptor.table().ok_or(ImportTypeMismatch)?;
                        let actual = self.store.table(table).ok_or(InvalidFunctionAddress)?;

                        if !actual.ty.matches(&expected) {
                            return Err(ImportTypeMismatch);
                        }
                    }

                    ExternalValue::Memory(memory) => {
                        let expected = import.descriptor.memory().ok_or(ImportTypeMismatch)?;
                        let actual = self.store.memory(memory).ok_or(InvalidFunctionAddress)?;

                        if !actual.ty.matches(&expected) {
                            return Err(ImportTypeMismatch);
                        }
                    }

                    ExternalValue::Global(global) => {
                        let expected = import.descriptor.global().ok_or(ImportTypeMismatch)?;
                        let actual = self.store.global(global).ok_or(InvalidFunctionAddress)?;

                        if !actual.ty().matches(expected) {
                            return Err(ImportTypeMismatch);
                        }
                    }
                }

                Ok(export)
            })
            .collect()
    }

    fn evaluate_globals(
        &self,
        module: &Module,
        imports: &[ExternalValue],
    ) -> InstantiationResult<Vec<GlobalInstance>> {
        let mut stack = Stack::new();

        let instance = ModuleInstance {
            types: Vec::new(),
            functions: Vec::new(),
            tables: Vec::new(),
            memories: Vec::new(),
            globals: imports
                .iter()
                .filter_map(|import| match import {
                    ExternalValue::Global(global) => Some(*global),
                    _ => None,
                })
                .collect(),
            exports: HashMap::new(),
        };

        let instance = Rc::new(instance);

        stack.push(Frame {
            arity: 1,
            locals: Vec::new(),
            module: instance,
        });

        Ok(module
            .globals
            .iter()
            .map(|global| {
                let value =
                    evaluate_constant_expression(&global.expression, &self.store.globals, &mut stack);

                GlobalInstance {
                    value,
                    mutability: global.ty.mutability,
                }
            })
            .collect())
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

    pub fn function(&self, FunctionAddress(address): FunctionAddress) -> Option<&FunctionInstance> {
        self.functions.get(address as usize)
    }

    pub fn table(&self, TableAddress(address): TableAddress) -> Option<&TableInstance> {
        self.tables.get(address as usize)
    }

    pub fn memory(&self, MemoryAddress(address): MemoryAddress) -> Option<&MemoryInstance> {
        self.memories.get(address as usize)
    }

    pub fn global(&self, GlobalAddress(address): GlobalAddress) -> Option<&GlobalInstance> {
        self.globals.get(address as usize)
    }

    pub(self) fn as_context(&mut self) -> Context {
        Context {
            functions: &self.functions,
            tables: &mut self.tables,
            memories: &mut self.memories,
            globals: &mut self.globals,
        }
    }

    pub(self) fn allocate_module(
        &mut self,
        module: Module,
        imports: Vec<ExternalValue>,
        globals: Vec<GlobalInstance>,
    ) -> InstantiationResult<Rc<ModuleInstance>> {
        let mut instance = ModuleInstance {
            types: module.types.clone(),
            functions: imports.iter().filter_map(|i| i.function()).collect(),
            tables: imports.iter().filter_map(|i| i.table()).collect(),
            memories: imports.iter().filter_map(|i| i.memory()).collect(),
            globals: imports.iter().filter_map(|i| i.global()).collect(),
            exports: HashMap::new(),
        };

        // TODO: remove this clone
        let mut store = self.clone();
        let mut function_code = Vec::new();

        for (i, func) in module.functions.into_iter().enumerate() {
            let TypeIndex(index) = func.ty;
            let ty = module.types[index as usize].clone();
            let code = FunctionCode::Local(func.clone());
            function_code.push((ty, code));

            // We cannot create function instances until we have a module
            let addr = store.functions.len() + i;
            let addr = FunctionAddress(addr as u32);
            instance.functions.push(addr);
        }

        for table in module.tables {
            let addr = TableAddress(store.tables.len() as u32);
            let table_instance = TableInstance {
                ty: table.ty,
                elements: vec![None; table.ty.limits.lower as usize],
                max_size: table.ty.limits.upper,
            };
            store.tables.push(table_instance);
            instance.tables.push(addr);
        }

        for memory in module.memories {
            let addr = MemoryAddress(store.memories.len() as u32);
            let memory_instance = MemoryInstance {
                ty: memory.ty,
                bytes: vec![0; memory.ty.limits.lower as usize * PAGE_SIZE].into(),
                max_size: memory.ty.limits.upper,
            };
            store.memories.push(memory_instance);
            instance.memories.push(addr);
        }

        for global in globals {
            let addr = GlobalAddress(store.globals.len() as u32);
            store.globals.push(global);
            instance.globals.push(addr);
        }

        for export in module.exports.iter() {
            let value = match export.descriptor {
                ExportDescriptor::Function(FunctionIndex(index)) => {
                    ExternalValue::Function(instance.functions[index as usize])
                }
                ExportDescriptor::Table(TableIndex(index)) => {
                    ExternalValue::Table(instance.tables[index as usize])
                }
                ExportDescriptor::Memory(MemoryIndex(index)) => {
                    ExternalValue::Memory(instance.memories[index as usize])
                }
                ExportDescriptor::Global(GlobalIndex(index)) => {
                    ExternalValue::Global(instance.globals[index as usize])
                }
            };

            instance.exports.insert(export.name.clone(), value);
        }

        let instance = Rc::new(instance);

        for (ty, code) in function_code {
            store.functions.push(FunctionInstance {
                module: Rc::clone(&instance),
                ty,
                code,
            })
        }

        let mut stack = Stack::new();
        stack.push(Frame {
            arity: 0,
            locals: Vec::new(),
            module: Rc::clone(&instance),
        });

        store.initialize_elements(module.elements, &instance, &mut stack)?;
        store.initialize_data_segments(module.data_segments, &instance, &mut stack)?;

        std::mem::replace(self, store);

        Ok(instance)
    }

    fn initialize_elements(
        &mut self,
        elements: Vec<Element>,
        module_instance: &ModuleInstance,
        stack: &mut Stack,
    ) -> InstantiationResult<()> {
        for element in elements {
            let offset = evaluate_constant_expression(&element.offset, &self.globals, stack);
            match offset {
                Value::I32(offset) => {
                    let offset = offset as usize;
                    let TableIndex(index) = element.table;
                    let TableAddress(addr) = module_instance.tables[index as usize];
                    let table = &mut self.tables[addr as usize];
                    let end = offset + element.init.len();
                    if end > table.elements.len() {
                        return Err(InstantiationError::ElementSegmentOutOfRange);
                    } else {
                        for (i, FunctionIndex(index)) in element.init.iter().copied().enumerate() {
                            let addr = module_instance.functions[index as usize];
                            table.elements[offset + i] = Some(TableElement::Function(addr));
                        }
                    }
                }
                _ => unreachable!("validation: got non i32 value when evaluating element offset"),
            }
        }

        Ok(())
    }

    fn initialize_data_segments(
        &mut self,
        data: Vec<DataSegment>,
        module_instance: &ModuleInstance,
        stack: &mut Stack,
    ) -> InstantiationResult<()> {
        for segment in data {
            let offset = evaluate_constant_expression(&segment.offset, &self.globals, stack);
            match offset {
                Value::I32(offset) => {
                    let offset = offset as usize;
                    let MemoryIndex(index) = segment.memory;
                    let MemoryAddress(addr) = module_instance.memories[index as usize];
                    let memory = &mut self.memories[addr as usize];
                    let end = offset + segment.bytes.len();
                    if end > memory.bytes.len() {
                        return Err(InstantiationError::DataSegmentOutOfRange);
                    } else {
                        memory.bytes[offset..end].copy_from_slice(&segment.bytes);
                    }
                }
                _ => unreachable!("validation: got non i32 value when evaluating element offset"),
            }
        }

        Ok(())
    }
}

impl Value {
    pub fn ty(self) -> ValueType {
        match self {
            Value::I32(_) => ValueType::I32,
            Value::I64(_) => ValueType::I64,
            Value::F32(_) => ValueType::F32,
            Value::F64(_) => ValueType::F64,
        }
    }
}

impl GlobalInstance {
    pub fn ty(&self) -> GlobalType {
        GlobalType {
            ty: self.value.ty(),
            mutability: self.mutability,
        }
    }
}

impl ExternalValue {
    pub fn function(self) -> Option<FunctionAddress> {
        match self {
            ExternalValue::Function(function) => Some(function),
            _ => None,
        }
    }

    pub fn table(self) -> Option<TableAddress> {
        match self {
            ExternalValue::Table(table) => Some(table),
            _ => None,
        }
    }

    pub fn memory(self) -> Option<MemoryAddress> {
        match self {
            ExternalValue::Memory(memory) => Some(memory),
            _ => None,
        }
    }

    pub fn global(self) -> Option<GlobalAddress> {
        match self {
            ExternalValue::Global(global) => Some(global),
            _ => None,
        }
    }
}

impl ModuleInstance {
    pub fn new() -> ModuleInstance {
        ModuleInstance {
            types: Vec::new(),
            functions: Vec::new(),
            tables: Vec::new(),
            memories: Vec::new(),
            globals: Vec::new(),
            exports: HashMap::new(),
        }
    }

    // Get function with name from exports
    pub fn function(&self, name: &str) -> Result<FunctionAddress> {
        self.exports
            .get(name)
            .and_then(|a| a.function())
            .ok_or_else(|| err!("function not fonud in module exports: {}", name))
    }
}

impl MemoryInstance {
    pub fn grow(&mut self, additional: usize) -> Option<usize> {
        if additional > 0 {
            let size = self.bytes.len() / PAGE_SIZE;
            let max = self.max_size.unwrap_or(1 << 16) as usize;
            let remaining = max - size;

            if additional > remaining {
                return None;
            }

            let new_size = PAGE_SIZE * (size + additional);
            self.bytes.bytes.resize(new_size, 0);

            Some(size)
        } else {
            None
        }
    }
}

impl Debug for ByteArray {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "[u8; {}]", self.bytes.len())?;

        Ok(())
    }
}

fn expect_argument_match(function: &FunctionType, args: &[Value]) -> Result<()> {
    let expected_count = function.parameters.len();
    if expected_count != args.len() {
        return Err(err!(
            "invalid number of argumnets: expected {} found {}",
            expected_count,
            args.len()
        ));
    }

    for (expected_type, value) in function.parameters.iter().zip(args) {
        if *expected_type != value.ty() {
            return Err(err!(
                "invalid function argument: expected {:?} found {:?}",
                expected_type,
                value.ty()
            ));
        }
    }

    Ok(())
}

impl FunctionCode {
    pub fn locals<'a>(&'a self) -> impl Iterator<Item = ValueType> + 'a {
        let locals = match self {
            FunctionCode::Local(function) => {
                function.locals.as_slice()
            }
            FunctionCode::Host(_) => &[],
        };

        locals.iter().copied()
    }
}


