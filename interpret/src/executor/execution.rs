//!
//! This module expects all modules to be validated. Failure to do so can result in a panic. Any
//! traps are represented by returning an `Err`
//!

use crate::ast::prelude::*;
use crate::executor::address::*;
use crate::executor::*;
use derive_more::From;
use std::convert::TryInto;
use std::ops::Neg;
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

#[derive(Debug, Copy, Clone, From)]
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

#[derive(Debug)]
enum Sign {
    Signed,
    Unsigned,
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

    fn execute_sequence(
        &mut self,
        instructions: &[Instruction],
        stack: &mut Stack,
    ) -> Result<Exit> {
        use Instruction::*;
        use Sign::*;
        use ValueType::*;

        for instruction in instructions {
            macro_rules! bin_op {
                ($pop:ident $(as $ty:ty)?, |$lhs:ident, $rhs:ident| $expr:expr) => {{
                    let $rhs = stack.$pop().unwrap() $(as $ty)?;
                    let $lhs = stack.$pop().unwrap() $(as $ty)?;
                    stack.push(Value::from($expr));
                }};
            }

            macro_rules! unary_op {
                ($pop:ident $(as $ty:ty)?, |$lhs:ident| $expr:expr) => {{
                    let $lhs = stack.$pop().unwrap() $(as $ty)?;
                    stack.push(Value::from($expr));
                }};
            }

            match instruction {
                Unreachable => return Err(RuntimeError::Unreachable.into()),
                Nop => {}

                // Numeric
                I32Const(value) => stack.push(Value::I32(*value)),
                I64Const(value) => stack.push(Value::I64(*value)),
                F32Const(value) => stack.push(Value::F32(*value)),
                F64Const(value) => stack.push(Value::F64(*value)),

                I32LeadingZeros => unary_op!(pop_value_i32, |a| a.leading_zeros() as i32),
                I32TrailingZeros => unary_op!(pop_value_i32, |a| a.trailing_zeros() as i32),
                I32CountOnes => unary_op!(pop_value_i32, |a| a.count_ones() as i32),
                I32Add => bin_op!(pop_value_i32, |a, b| a + b),
                I32Sub => bin_op!(pop_value_i32, |a, b| a - b),
                I32Mul => bin_op!(pop_value_i32, |a, b| a * b),
                I32DivSigned => bin_op!(pop_value_i32, |a, b| a / b),
                I32DivUnsigned => bin_op!(pop_value_i32 as u32, |a, b| (a / b) as i32),
                I32RemainderSigned => bin_op!(pop_value_i32, |a, b| a % b),
                I32RemainderUnsigned => bin_op!(pop_value_i32 as u32, |a, b| (a % b) as i32),
                I32And => bin_op!(pop_value_i32, |a, b| a & b),
                I32Or => bin_op!(pop_value_i32, |a, b| a | b),
                I32Xor => bin_op!(pop_value_i32, |a, b| a ^ b),
                I32ShiftLeft => bin_op!(pop_value_i32, |a, b| a << b),
                I32ShiftRightSigned => bin_op!(pop_value_i32, |a, b| a >> b),
                I32ShiftRightUnsigned => bin_op!(pop_value_i32 as u32, |a, b| (a >> b) as i32),
                I32RotateLeft => bin_op!(pop_value_i32, |a, b| a.rotate_left(b as u32)),
                I32RotateRight => bin_op!(pop_value_i32, |a, b| a.rotate_right(b as u32)),

                I32EqualZero => unary_op!(pop_value_i32, |a| (a == 0) as i32),
                I32Equal => bin_op!(pop_value_i32, |a, b| (a == b) as i32),
                I32NotEqual => bin_op!(pop_value_i32, |a, b| (a != b) as i32),
                I32LessThanSigned => bin_op!(pop_value_i32, |a, b| (a < b) as i32),
                I32LessThanUnsigned => bin_op!(pop_value_i32 as u32, |a, b| (a < b) as i32),
                I32GreaterThanSigned => bin_op!(pop_value_i32, |a, b| (a > b) as i32),
                I32GreaterThanUnsigned => bin_op!(pop_value_i32 as u32, |a, b| (a > b) as i32),
                I32LessEqualSigned => bin_op!(pop_value_i32, |a, b| (a <= b) as i32),
                I32LessEqualUnsigned => bin_op!(pop_value_i32 as u32, |a, b| (a <= b) as i32),
                I32GreaterEqualSigned => bin_op!(pop_value_i32, |a, b| (a >= b) as i32),
                I32GreaterEqualUnsigned => bin_op!(pop_value_i32 as u32, |a, b| (a >= b) as i32),

                I64LeadingZeros => unary_op!(pop_value_i64, |a| a.leading_zeros() as i64),
                I64TrailingZeros => unary_op!(pop_value_i64, |a| a.trailing_zeros() as i64),
                I64CountOnes => unary_op!(pop_value_i64, |a| a.count_ones() as i64),
                I64Add => bin_op!(pop_value_i64, |a, b| a + b),
                I64Sub => bin_op!(pop_value_i64, |a, b| a - b),
                I64Mul => bin_op!(pop_value_i64, |a, b| a * b),
                I64DivSigned => bin_op!(pop_value_i64, |a, b| a / b),
                I64DivUnsigned => bin_op!(pop_value_i64 as u64, |a, b| (a / b) as i64),
                I64RemainderSigned => bin_op!(pop_value_i64, |a, b| a % b),
                I64RemainderUnsigned => bin_op!(pop_value_i64 as u64, |a, b| (a % b) as i64),
                I64And => bin_op!(pop_value_i64, |a, b| a & b),
                I64Or => bin_op!(pop_value_i64, |a, b| a | b),
                I64Xor => bin_op!(pop_value_i64, |a, b| a ^ b),
                I64ShiftLeft => bin_op!(pop_value_i64, |a, b| a << b),
                I64ShiftRightSigned => bin_op!(pop_value_i64, |a, b| a >> b),
                I64ShiftRightUnsigned => bin_op!(pop_value_i64 as u64, |a, b| (a >> b) as i64),
                I64RotateLeft => bin_op!(pop_value_i64, |a, b| a.rotate_left(b as u32)),
                I64RotateRight => bin_op!(pop_value_i64, |a, b| a.rotate_right(b as u32)),

                I64EqualZero => unary_op!(pop_value_i64, |a| (a == 0) as i64),
                I64Equal => bin_op!(pop_value_i64, |a, b| (a == b) as i64),
                I64NotEqual => bin_op!(pop_value_i64, |a, b| (a != b) as i64),
                I64LessThanSigned => bin_op!(pop_value_i64, |a, b| (a < b) as i64),
                I64LessThanUnsigned => bin_op!(pop_value_i64 as u64, |a, b| (a < b) as i64),
                I64GreaterThanSigned => bin_op!(pop_value_i64, |a, b| (a > b) as i64),
                I64GreaterThanUnsigned => bin_op!(pop_value_i64 as u64, |a, b| (a > b) as i64),
                I64LessEqualSigned => bin_op!(pop_value_i64, |a, b| (a <= b) as i64),
                I64LessEqualUnsigned => bin_op!(pop_value_i64 as u64, |a, b| (a <= b) as i64),
                I64GreaterEqualSigned => bin_op!(pop_value_i64, |a, b| (a >= b) as i64),
                I64GreaterEqualUnsigned => bin_op!(pop_value_i64 as u64, |a, b| (a >= b) as i64),

                F32Abs => unary_op!(pop_value_f32, |a| a.abs()),
                F32Neg => unary_op!(pop_value_f32, |a| a.neg()),
                F32Ceil => unary_op!(pop_value_f32, |a| a.ceil()),
                F32Floor => unary_op!(pop_value_f32, |a| a.floor()),
                F32Trunc => unary_op!(pop_value_f32, |a| a.trunc()),
                F32Nearest => unary_op!(pop_value_f32, |a| a.round()),
                F32Sqrt => unary_op!(pop_value_f32, |a| a.sqrt()),
                F32Add => bin_op!(pop_value_f32, |a, b| a + b),
                F32Sub => bin_op!(pop_value_f32, |a, b| a - b),
                F32Mul => bin_op!(pop_value_f32, |a, b| a * b),
                F32Div => bin_op!(pop_value_f32, |a, b| a / b),
                F32Min => bin_op!(pop_value_f32, |a, b| a.min(b)),
                F32Max => bin_op!(pop_value_f32, |a, b| a.max(b)),
                F32Copysign => bin_op!(pop_value_f32, |a, b| a.copysign(b)),

                #[allow(clippy::float_cmp)]
                F32Equal => bin_op!(pop_value_f32, |a, b| (a == b) as i32),
                #[allow(clippy::float_cmp)]
                F32NotEqual => bin_op!(pop_value_f32, |a, b| (a != b) as i32),
                F32LessThan => bin_op!(pop_value_f32, |a, b| (a < b) as i32),
                F32GreaterThan => bin_op!(pop_value_f32, |a, b| (a > b) as i32),
                F32LessEqual => bin_op!(pop_value_f32, |a, b| (a <= b) as i32),
                F32GreaterEqual => bin_op!(pop_value_f32, |a, b| (a >= b) as i32),

                F64Abs => unary_op!(pop_value_f64, |a| a.abs()),
                F64Neg => unary_op!(pop_value_f64, |a| a.neg()),
                F64Ceil => unary_op!(pop_value_f64, |a| a.ceil()),
                F64Floor => unary_op!(pop_value_f64, |a| a.floor()),
                F64Trunc => unary_op!(pop_value_f64, |a| a.trunc()),
                F64Nearest => unary_op!(pop_value_f64, |a| a.round()),
                F64Sqrt => unary_op!(pop_value_f64, |a| a.sqrt()),
                F64Add => bin_op!(pop_value_f64, |a, b| a + b),
                F64Sub => bin_op!(pop_value_f64, |a, b| a - b),
                F64Mul => bin_op!(pop_value_f64, |a, b| a * b),
                F64Div => bin_op!(pop_value_f64, |a, b| a / b),
                F64Min => bin_op!(pop_value_f64, |a, b| a.min(b)),
                F64Max => bin_op!(pop_value_f64, |a, b| a.max(b)),
                F64Copysign => bin_op!(pop_value_f64, |a, b| a.copysign(b)),

                #[allow(clippy::float_cmp)]
                F64Equal => bin_op!(pop_value_f64, |a, b| (a == b) as i64),
                #[allow(clippy::float_cmp)]
                F64NotEqual => bin_op!(pop_value_f64, |a, b| (a != b) as i64),
                F64LessThan => bin_op!(pop_value_f64, |a, b| (a < b) as i64),
                F64GreaterThan => bin_op!(pop_value_f64, |a, b| (a > b) as i64),
                F64LessEqual => bin_op!(pop_value_f64, |a, b| (a <= b) as i64),
                F64GreaterEqual => bin_op!(pop_value_f64, |a, b| (a >= b) as i64),

                I32WrapI64 => unary_op!(pop_value_i64, |a| a as i32),
                I32TruncF32Signed => unary_op!(pop_value_f32, |a| a as i32),
                I32TruncF32Unsigned => unary_op!(pop_value_f32, |a| a as u32 as i32),
                I32TruncF64Signed => unary_op!(pop_value_f64, |a| a as i32),
                I32TruncF64Unsigned => unary_op!(pop_value_f64, |a| a as u32 as i32),

                I64ExtendI32Signed => unary_op!(pop_value_i32, |a| a as i64),
                I64ExtendI32Unsigned => unary_op!(pop_value_i32, |a| a as u64 as i64),
                I64TruncF32Signed => unary_op!(pop_value_f32, |a| a as i64),
                I64TruncF32Unsigned => unary_op!(pop_value_f32, |a| a as u64 as i64),
                I64TruncF64Signed => unary_op!(pop_value_f64, |a| a as i64),
                I64TruncF64Unsigned => unary_op!(pop_value_f64, |a| a as u64 as i64),

                F32ConvertI32Signed => unary_op!(pop_value_i32, |a| a as f32),
                F32ConvertI32Unsigned => unary_op!(pop_value_i32, |a| a as u32 as f32),
                F32ConvertI64Signed => unary_op!(pop_value_i64, |a| a as f32),
                F32ConvertI64Unsigned => unary_op!(pop_value_i64, |a| a as u64 as f32),
                F32DemoteF64 => unary_op!(pop_value_f64, |a| a as f32),

                F64ConvertI32Signed => unary_op!(pop_value_i32, |a| a as f64),
                F64ConvertI32Unsigned => unary_op!(pop_value_i32, |a| a as u32 as f64),
                F64ConvertI64Signed => unary_op!(pop_value_i64, |a| a as f64),
                F64ConvertI64Unsigned => unary_op!(pop_value_i64, |a| a as u64 as f64),
                F64PromoteF32 => unary_op!(pop_value_f32, |a| a as f64),

                I32ReinterpretF32 => unary_op!(pop_value_f32, |a| a.to_bits() as i32),
                I64ReinterpretF64 => unary_op!(pop_value_f64, |a| a.to_bits() as i64),
                F32ReinterpretI32 => unary_op!(pop_value_i32, |a| f32::from_bits(a as u32)),
                F64ReinterpretI64 => unary_op!(pop_value_i64, |a| f64::from_bits(a as u64)),

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

                Conditional {
                    result,
                    success,
                    failure,
                } => {
                    let value = stack.pop_value_i32().unwrap();
                    let instructions = if value != 0 { success } else { failure };

                    stack.push(Label {
                        arity: result.arity(),
                    });

                    match self.execute_sequence(instructions, stack)? {
                        Exit::Return => return Ok(Exit::Return),
                        Exit::Escape | Exit::Continue(0) => (),
                        Exit::Continue(depth) => return Ok(Exit::Continue(depth - 1)),
                    }
                }

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

                BranchTable { targets, default } => {
                    let value = stack.pop_value_i32().unwrap() as usize;
                    let target = if value < targets.len() {
                        targets[value]
                    } else {
                        *default
                    };
                    
                    stack.unwind(target);
                    return Ok(Exit::Continue(target.0));
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

                // Parametric
                Drop => {
                    stack.pop_value().unwrap();
                }
                Select => {
                    let condition = stack.pop_value_i32().unwrap();
                    let failure = stack.pop_value().unwrap();
                    let success = stack.pop_value().unwrap();
                    if condition != 0 {
                        stack.push(success);
                    } else {
                        stack.push(failure);
                    }
                }

                // Memory
                I32Load(memarg) => self.load(*memarg, 4, I32, Signed, stack)?,
                I64Load(memarg) => self.load(*memarg, 8, I64, Signed, stack)?,
                F32Load(memarg) => self.load(*memarg, 4, F32, Signed, stack)?,
                F64Load(memarg) => self.load(*memarg, 8, F64, Signed, stack)?,
                I32Load8Signed(memarg) => self.load(*memarg, 1, I32, Signed, stack)?,
                I32Load8Unsigned(memarg) => self.load(*memarg, 1, I32, Unsigned, stack)?,
                I32Load16Signed(memarg) => self.load(*memarg, 2, I32, Signed, stack)?,
                I32Load16Unsigned(memarg) => self.load(*memarg, 2, I32, Unsigned, stack)?,
                I64Load8Signed(memarg) => self.load(*memarg, 1, I64, Signed, stack)?,
                I64Load8Unsigned(memarg) => self.load(*memarg, 1, I64, Unsigned, stack)?,
                I64Load16Signed(memarg) => self.load(*memarg, 2, I64, Signed, stack)?,
                I64Load16Unsigned(memarg) => self.load(*memarg, 2, I64, Unsigned, stack)?,
                I64Load32Signed(memarg) => self.load(*memarg, 4, I64, Signed, stack)?,
                I64Load32Unsigned(memarg) => self.load(*memarg, 4, I64, Signed, stack)?,

                I32Store(memarg) => self.store(*memarg, 4, stack)?,
                I64Store(memarg) => self.store(*memarg, 8, stack)?,
                F32Store(memarg) => self.store(*memarg, 4, stack)?,
                F64Store(memarg) => self.store(*memarg, 8, stack)?,
                I32Store8(memarg) => self.store(*memarg, 1, stack)?,
                I32Store16(memarg) => self.store(*memarg, 2, stack)?,
                I64Store8(memarg) => self.store(*memarg, 1, stack)?,
                I64Store16(memarg) => self.store(*memarg, 2, stack)?,
                I64Store32(memarg) => self.store(*memarg, 4, stack)?,

                MemorySize => {
                    let frame = stack.frame().unwrap();
                    let MemoryAddress(address) = frame.module.memories[0];
                    let memory = &self.memories[address as usize];
                    let size = memory.bytes.len() / PAGE_SIZE;
                    stack.push(Value::I32(size as i32));
                }
                MemoryGrow => {
                    let frame = stack.frame().unwrap();
                    let MemoryAddress(address) = frame.module.memories[0];
                    let memory = &mut self.memories[address as usize];

                    let additional = stack.pop_value_i32().unwrap() as usize;

                    if let Some(size) = memory.grow(additional) {
                        stack.push(Value::I32(size as i32));
                    } else {
                        stack.push(Value::I32(-1));
                    }
                }

                _ => unimplemented!("instruction: {:?}", instruction),
            }
        }

        stack.remove_label();

        Ok(Exit::Escape)
    }

    fn load(
        &self,
        arg: MemoryArgument,
        count: usize,
        ty: ValueType,
        sign: Sign,
        stack: &mut Stack,
    ) -> Result<()> {
        let frame = stack.frame().unwrap();
        let MemoryAddress(address) = frame.module.memories[0];
        let memory = &self.memories[address as usize];

        let pointer = stack.pop_value_i32().unwrap();

        let address = (arg.offset as i32 + pointer) as usize;
        if address + count > memory.bytes.len() {
            Err(RuntimeError::ReadOutOfBounds.into())
        } else {
            let bytes = &memory.bytes[address..address + count];

            macro_rules! cast_into {
                ($target:expr, $value:expr) => {{
                    match $target {
                        ValueType::I32 => Value::I32($value as i32),
                        ValueType::I64 => Value::I64($value as i64),
                        ValueType::F32 => Value::F32($value as f32),
                        ValueType::F64 => Value::F64($value as f64),
                    }
                }};
            }

            macro_rules! into_value {
                ($signed:ty, $unsigned:ty) => {{
                    let arr = bytes.try_into().unwrap();
                    match sign {
                        Sign::Signed => {
                            let value = <$signed>::from_le_bytes(arr);
                            cast_into!(ty, value)
                        }
                        Sign::Unsigned => {
                            let value = <$unsigned>::from_le_bytes(arr);
                            cast_into!(ty, value)
                        }
                    }
                }};
            }

            let value = match count {
                1 => into_value!(i8, u8),
                2 => into_value!(i16, u16),
                4 => into_value!(i32, u32),
                8 => into_value!(i64, u64),
                _ => unreachable!(),
            };

            stack.push(value);

            Ok(())
        }
    }

    fn store(&mut self, arg: MemoryArgument, count: usize, stack: &mut Stack) -> Result<()> {
        let frame = stack.frame().unwrap();
        let MemoryAddress(address) = frame.module.memories[0];
        let memory = &mut self.memories[address as usize];

        let value = stack.pop_value().unwrap();
        let pointer = stack.pop_value_i32().unwrap();

        let address = (arg.offset as i32 + pointer) as usize;
        let end = address + count;
        if end > memory.bytes.len() {
            Err(RuntimeError::WriteOutOfBounds.into())
        } else {
            let bytes = &mut memory.bytes[address..end];

            macro_rules! write_data {
                ($bytes:expr) => {{
                    let source = $bytes;
                    bytes.copy_from_slice(&source[..count]);
                }};
            }

            match value {
                Value::I32(value) => write_data!(value.to_le_bytes()),
                Value::I64(value) => write_data!(value.to_le_bytes()),
                Value::F32(value) => write_data!(value.to_bits().to_le_bytes()),
                Value::F64(value) => write_data!(value.to_bits().to_le_bytes()),
            }

            Ok(())
        }
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
