use crate::ast::prelude::*;
use crate::executor::address::*;
use crate::executor::*;
use derive_more::From;
use std::rc::Rc;

#[derive(Debug)]
pub struct Context<'a> {
    pub functions: &'a [FunctionInstance],
    pub tables: &'a mut [TableInstance],
    pub memories: &'a mut [MemoryInstance],
    pub globals: &'a mut [GlobalInstance],
}

#[derive(Debug)]
pub struct Stack {
    kinds: Vec<StackItemKind>,
    values: Vec<Value>,
    labels: Vec<Label>,
    frames: Vec<Frame>,
}

#[derive(Debug, Copy, Clone)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Debug, From)]
pub enum StackItem {
    Value(Value),
    Label(Label),
    Frame(Frame),
}

#[derive(Debug, PartialEq, From)]
enum StackItemKind {
    Value,
    Label,
    Frame,
}

#[derive(Debug, Copy, Clone)]
pub struct Label {
    pub arity: u32,
}

#[derive(Debug)]
pub struct Frame {
    pub arity: u32,
    pub locals: Vec<Value>,
    pub module: Rc<ModuleInstance>,
}

#[derive(Debug)]
enum Exit {
    Escape,
    Return,
    Continue(u32),
}

impl Stack {
    pub fn new() -> Stack {
        Stack {
            kinds: Vec::new(),
            values: Vec::new(),
            labels: Vec::new(),
            frames: Vec::new(),
        }
    }

    pub fn push(&mut self, item: impl Into<StackItem>) {
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

    pub fn pop(&mut self) -> Option<StackItem> {
        match self.kinds.pop()? {
            StackItemKind::Value => self.values.pop().map(StackItem::Value),
            StackItemKind::Label => self.labels.pop().map(StackItem::Label),
            StackItemKind::Frame => self.frames.pop().map(StackItem::Frame),
        }
    }

    pub fn frame(&self) -> Option<&Frame> {
        self.frames.last()
    }

    pub fn frame_mut(&mut self) -> Option<&mut Frame> {
        self.frames.last_mut()
    }

    pub fn pop_value(&mut self) -> Option<Value> {
        match self.kinds.pop()? {
            StackItemKind::Value => self.values.pop(),
            kind => {
                self.kinds.push(kind);
                None
            }
        }
    }

    pub fn pop_value_i32(&mut self) -> Option<i32> {
        match self.pop_value()? {
            Value::I32(value) => Some(value),
            value => {
                self.push(value);
                None
            }
        }
    }

    pub fn pop_value_i64(&mut self) -> Option<i64> {
        match self.pop_value()? {
            Value::I64(value) => Some(value),
            value => {
                self.push(value);
                None
            }
        }
    }

    pub fn pop_value_f32(&mut self) -> Option<f32> {
        match self.pop_value()? {
            Value::F32(value) => Some(value),
            value => {
                self.push(value);
                None
            }
        }
    }

    pub fn pop_value_f64(&mut self) -> Option<f64> {
        match self.pop_value()? {
            Value::F64(value) => Some(value),
            value => {
                self.push(value);
                None
            }
        }
    }

    pub fn pop_values(&mut self, count: usize) -> Option<Vec<Value>> {
        let has_values = self.kinds[self.kinds.len() - count..]
            .iter()
            .any(|kind| *kind != StackItemKind::Value);

        if has_values {
            None
        } else {
            self.kinds.split_off(self.kinds.len() - count);
            Some(self.values.split_off(self.values.len() - count))
        }
    }

    pub fn pop_label(&mut self) -> Option<Label> {
        match self.kinds.pop()? {
            StackItemKind::Label => self.labels.pop(),
            kind => {
                self.kinds.push(kind);
                None
            }
        }
    }

    pub fn remove_label(&mut self) -> Option<Label> {
        let pos = self
            .kinds
            .iter()
            .rev()
            .position(|kind| *kind == StackItemKind::Label)?;
        self.kinds.remove(self.kinds.len() - pos - 1);
        self.labels.pop()
    }

    pub fn pop_frame(&mut self) -> Option<Frame> {
        match self.kinds.pop()? {
            StackItemKind::Frame => self.frames.pop(),
            kind => {
                self.kinds.push(kind);
                None
            }
        }
    }

    pub fn pop_until_frame(&mut self) {
        loop {
            if let StackItemKind::Frame = self.kinds.pop().unwrap() {
                self.kinds.push(StackItemKind::Frame);
                return;
            }
        }
    }

    pub fn remove_frame(&mut self) -> Option<Frame> {
        let pos = self
            .kinds
            .iter()
            .rev()
            .position(|kind| *kind == StackItemKind::Frame)?;
        self.kinds.remove(self.kinds.len() - pos - 1);
        self.frames.pop()
    }

    pub fn unwind(&mut self, label: LabelIndex) {
        let LabelIndex(index) = label;
        let label = *self.labels.iter().nth_back(index as usize).unwrap();
        let values = self.pop_values(label.arity as usize).unwrap();

        for _ in 0..=index {
            while self.pop_value().is_some() {}
            self.pop_label().unwrap();
        }

        for value in values {
            self.push(value);
        }
    }
}

pub fn evaluate_constant_expression(
    expression: &Expression,
    globals: &[GlobalInstance],
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
                let global = globals.get(*addr as usize).unwrap();
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

impl<'a> Context<'a> {
    pub fn invoke(&mut self, function: FunctionAddress, stack: &mut Stack) -> Result<Vec<Value>> {
        let arity = self.invoke_internal(function, stack)?;
        Ok(stack.pop_values(arity as usize).unwrap())
    }

    fn invoke_internal(&mut self, function: FunctionAddress, stack: &mut Stack) -> Result<u32> {
        let FunctionAddress(address) = function;
        let instance = &self.functions[address as usize];

        let ty = &instance.ty;

        let mut values = stack.pop_values(ty.parameters.len()).unwrap();

        let locals = instance.code.locals().map(zeroed);
        values.extend(locals);

        let result_arity = ty.results.len() as u32;

        let frame = Frame {
            arity: result_arity,
            locals: values,
            module: Rc::clone(&instance.module),
        };

        stack.push(frame);

        let exit = match &instance.code {
            FunctionCode::Local(function) => {
                let label = Label {
                    arity: result_arity,
                };
                stack.push(label);
                self.execute_sequence(&function.body.instructions, stack)?
            }
            FunctionCode::Host(host) => self.execute_host_function(&host, stack)?,
        };

        if let Exit::Escape = exit {
            stack.remove_frame();
        }

        Ok(result_arity)
    }

    fn execute_host_function(&self, function: &HostFunction, stack: &mut Stack) -> Result<Exit> {
        match function {
            HostFunction::Print => {
                let value = stack.frame().unwrap().locals[0];
                match value {
                    Value::I32(value) => println!("{}", value),
                    Value::I64(value) => println!("{}", value),
                    Value::F32(value) => println!("{}", value),
                    Value::F64(value) => println!("{}", value),
                }
            }
        }

        Ok(Exit::Escape)
    }

    fn execute_sequence(&mut self, instructions: &[Instruction], stack: &mut Stack) -> Result<Exit> {
        use Instruction::*;
        for instruction in instructions {
            match instruction {
                I32Const(value) => stack.push(Value::I32(*value)),
                I64Const(value) => stack.push(Value::I64(*value)),
                F32Const(value) => stack.push(Value::F32(*value)),
                F64Const(value) => stack.push(Value::F64(*value)),

                I32Add => {
                    let b = stack.pop_value_i32().unwrap();
                    let a = stack.pop_value_i32().unwrap();
                    stack.push(Value::I32(a + b));
                }

                I32Sub => {
                    let b = stack.pop_value_i32().unwrap();
                    let a = stack.pop_value_i32().unwrap();
                    stack.push(Value::I32(a - b));
                }

                I32NotEqual => {
                    let b = stack.pop_value_i32().unwrap();
                    let a = stack.pop_value_i32().unwrap();
                    stack.push(Value::I32((a != b) as i32));
                }

                I32LessThanUnsigned => {
                    let b = stack.pop_value_i32().unwrap() as u32;
                    let a = stack.pop_value_i32().unwrap() as u32;
                    stack.push(Value::I32((a < b) as i32));
                }

                LocalSet(LocalIndex(index)) => {
                    let value = stack.pop_value().unwrap();
                    let frame = stack.frame_mut().unwrap();
                    frame.locals[*index as usize] = value;
                }

                LocalGet(LocalIndex(index)) => {
                    let frame = stack.frame_mut().unwrap();
                    let value = frame.locals[*index as usize];
                    stack.push(value);
                }

                LocalTee(LocalIndex(index)) => {
                    let value = stack.pop_value().unwrap();
                    let frame = stack.frame_mut().unwrap();
                    frame.locals[*index as usize] = value;
                    stack.push(value);
                }

                GlobalSet(GlobalIndex(index)) => {
                    let value = stack.pop_value().unwrap();
                    let frame = stack.frame().unwrap();
                    let GlobalAddress(address) = frame.module.globals[*index as usize];
                    self.globals[address as usize].value = value;
                }

                GlobalGet(GlobalIndex(index)) => {
                    let frame = stack.frame().unwrap();
                    let GlobalAddress(address) = frame.module.globals[*index as usize];
                    let value = self.globals[address as usize].value;
                    stack.push(value);
                }

                Block {
                    instructions,
                    result,
                } => {
                    let label = Label {
                        arity: result.arity(),
                    };
                    stack.push(label);
                    match self.execute_sequence(instructions, stack)? {
                        Exit::Return => return Ok(Exit::Return),
                        Exit::Escape | Exit::Continue(0) => (),
                        Exit::Continue(depth) => return Ok(Exit::Continue(depth - 1)),
                    }
                }

                Loop { instructions, .. } => loop {
                    let label = Label { arity: 0 };
                    stack.push(label);
                    match self.execute_sequence(instructions, stack)? {
                        Exit::Continue(0) => continue,
                        Exit::Continue(depth) => return Ok(Exit::Continue(depth - 1)),
                        Exit::Escape => break,
                        Exit::Return => return Ok(Exit::Return),
                    }
                },

                Branch(target) => {
                    stack.unwind(*target);
                    return Ok(Exit::Continue(target.0));
                }

                BranchConditional(target) => {
                    let value = stack.pop_value_i32().unwrap();
                    if value != 0 {
                        stack.unwind(*target);
                        return Ok(Exit::Continue(target.0));
                    }
                }

                Return => {
                    let arity = stack.frame().unwrap().arity;
                    let values = stack.pop_values(arity as usize).unwrap();
                    stack.pop_until_frame();
                    stack.pop_frame().unwrap();

                    for value in values {
                        stack.push(value);
                    }

                    return Ok(Exit::Return);
                }

                Call(FunctionIndex(index)) => {
                    let frame = stack.frame().unwrap();
                    let address = frame.module.functions[*index as usize];
                    self.invoke_internal(address, stack)?;
                }

                _ => unimplemented!("execute: {:?}", instruction),
            }
        }

        stack.remove_label();

        Ok(Exit::Escape)
    }
}

fn zeroed(ty: ValueType) -> Value {
    match ty {
        ValueType::I32 => Value::I32(0),
        ValueType::I64 => Value::I64(0),
        ValueType::F32 => Value::F32(0.0),
        ValueType::F64 => Value::F64(0.0),
    }
}
