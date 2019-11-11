#![allow(dead_code)]

mod address;
mod error;
mod validation;

use self::address::*;
use self::error::*;
use self::validation::*;
use crate::ast::prelude::*;
use crate::module::*;
use derive_more::{From, Deref, DerefMut};
use std::collections::HashMap;
use std::rc::Rc;
use std::fmt::{Debug, Formatter, Result as FmtResult};

#[derive(Debug)]
pub struct Executor {
    store: Store,
    modules: HashMap<String, Rc<ModuleInstance>>,
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

#[derive(Debug, Copy, Clone)]
pub enum Value {
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
    exports: HashMap<String, ExternalValue>,
}

#[derive(Debug, Copy, Clone)]
enum ExternalValue {
    Function(FunctionAddress),
    Table(TableAddress),
    Memory(MemoryAddress),
    Global(GlobalAddress),
}

#[derive(Debug)]
pub struct Stack {
    kinds: Vec<StackItemKind>,
    values: Vec<Value>,
    labels: Vec<Label>,
    frames: Vec<Frame>,
}

#[derive(Debug, From)]
enum StackItem {
    Value(Value),
    Label(Label),
    Frame(Frame),
}

#[derive(Debug, From)]
enum StackItemKind {
    Value,
    Label,
    Frame,
}

#[derive(Debug)]
pub struct Label {
    arity: u32,
}

#[derive(Debug)]
pub struct Frame {
    arity: u32,
    locals: Vec<Value>,
    module: Rc<ModuleInstance>,
}

impl Executor {
    pub fn new() -> Self {
        Executor {
            store: Store::new(),
            modules: HashMap::new(),
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
            self.invoke(start.function)?;
        }

        Ok(instance)
    }

    pub fn invoke(&mut self, function: FunctionIndex) -> Result<Vec<Value>> {
        unimplemented!();
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
                    evaluate_constant_expression(&global.expression, &self.store, &mut stack);

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
            let offset = evaluate_constant_expression(&element.offset, self, stack);
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
            let offset = evaluate_constant_expression(&segment.offset, self, stack);
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
                        memory.bytes[offset..].copy_from_slice(&segment.bytes);
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

impl Stack {
    pub(self) fn new() -> Stack {
        Stack {
            kinds: Vec::new(),
            values: Vec::new(),
            labels: Vec::new(),
            frames: Vec::new(),
        }
    }

    pub(self) fn push(&mut self, item: impl Into<StackItem>) {
        match item.into() {
            StackItem::Value(value) => {
                self.values.push(value);
                self.kinds.push(StackItemKind::Value);
            }
            StackItem::Label(label) => {
                self.labels.push(label);
                self.kinds.push(StackItemKind::Label);
            }
            StackItem::Frame(frame) => {
                self.frames.push(frame);
                self.kinds.push(StackItemKind::Frame);
            }
        }
    }

    pub(self) fn pop(&mut self) -> Option<StackItem> {
        match self.kinds.pop()? {
            StackItemKind::Value => self.values.pop().map(StackItem::Value),
            StackItemKind::Label => self.labels.pop().map(StackItem::Label),
            StackItemKind::Frame => self.frames.pop().map(StackItem::Frame),
        }
    }

    pub(self) fn frame(&self) -> Option<&Frame> {
        self.frames.last()
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
}

impl Debug for ByteArray {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "[u8; {}]", self.bytes.len())?;

        Ok(())
    }
}

fn evaluate_constant_expression(
    expression: &Expression,
    store: &Store,
    stack: &mut Stack,
) -> Value {
    for instruction in &expression.instructions {
        match instruction {
            Instruction::I32Const(value) => stack.push(Value::I32(*value)),
            Instruction::I64Const(value) => stack.push(Value::I64(*value)),
            Instruction::F32Const(value) => stack.push(Value::F32(*value)),
            Instruction::F64Const(value) => stack.push(Value::F64(*value)),

            Instruction::GlobalGet(GlobalIndex(index)) => {
                // Logic error to have a stack without a frame
                let frame = stack.frame().unwrap();

                // Validation ensures that the global exists
                let GlobalAddress(addr) = frame.module.globals.get(*index as usize).unwrap();
                let global = store.globals.get(*addr as usize).unwrap();
                stack.push(global.value);
            }

            _ => unreachable!("validation: non-constant instruction in constant expression"),
        }
    }

    // Should never panic because of validation
    match stack.pop().unwrap() {
        StackItem::Value(value) => value,
        _ => unreachable!("validation: got non-value while evaluating constant expression"),
    }
}
