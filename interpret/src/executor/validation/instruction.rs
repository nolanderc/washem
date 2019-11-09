use super::error::*;
use super::Context;
use crate::ast::prelude::*;
use derive_more::From;

#[derive(Debug)]
pub struct ValidationStack {
    operands: Vec<Operand>,
    frames: Vec<ControlFrame>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, From)]
pub enum Operand {
    Value(ValueType),
    Unknown,
}

#[derive(Debug)]
struct ControlFrame {
    label_types: Vec<ValueType>,
    end_types: Vec<ValueType>,
    height: usize,
    unreachable: bool,
}

pub(super) fn validate_expression(
    expr: &Expression,
    result: &ResultType,
    context: &Context,
) -> ValidationResult<()> {
    let mut stack = ValidationStack::new();

    let values = result.types.iter().copied().collect::<Vec<_>>();
    stack.push_frame(values.clone(), values);

    validate_instruction_sequence(&expr.instructions, context, &mut stack)?;

    stack.pop_operands(result.types.iter().copied())?;

    Ok(())
}

pub(super) fn validate_constant_expression(
    expr: &Expression,
    context: &Context,
) -> ValidationResult<()> {
    for instruction in &expr.instructions {
        match instruction.class() {
            InstructionClass::Constant(_) => (),
            InstructionClass::GlobalGet(index) if global_is_constant(index, context) => (),
            _ => return Err(ValidationError::InstructionNotConstant),
        }
    }

    Ok(())
}

fn global_is_constant(GlobalIndex(index): GlobalIndex, context: &Context) -> bool {
    context
        .globals
        .get(index as usize)
        .map(|global| global.mutability == Mutability::Constant)
        .unwrap_or(false)
}

pub(super) fn validate_instruction_sequence(
    sequence: &[Instruction],
    context: &Context,
    stack: &mut ValidationStack,
) -> ValidationResult<()> {
    use InstructionClass::*;

    for instruction in sequence {
        let class = instruction.class();

        match class {
            Constant(value) => {
                stack.push_operand(value);
            }
            Unary(value) => {
                stack.pop_operand(value)?;
                stack.push_operand(value);
            }
            Binary(value) => {
                stack.pop_operand(value)?;
                stack.pop_operand(value)?;
                stack.push_operand(value);
            }
            Test(value) => {
                stack.pop_operand(value)?;
                stack.push_operand(ValueType::I32);
            }
            Comparison(value) => {
                stack.pop_operand(value)?;
                stack.pop_operand(value)?;
                stack.push_operand(ValueType::I32);
            }
            Conversion { source, target } => {
                stack.pop_operand(source)?;
                stack.push_operand(target);
            }
            Drop => {
                stack.pop_any_operand()?;
            }
            Select => {
                stack.pop_operand(ValueType::I32)?;
                let v1 = stack.pop_any_operand()?;
                let v2 = stack.pop_operand(v1)?;
                let value = if v1 == Operand::Unknown { v2 } else { v1 };
                stack.push_operand(value);
            }
            LocalGet(LocalIndex(index)) => {
                let local = context
                    .locals
                    .get(index as usize)
                    .ok_or(ValidationError::LocalNotFound)?;
                stack.push_operand(*local);
            }
            LocalSet(LocalIndex(index)) => {
                let local = context
                    .locals
                    .get(index as usize)
                    .ok_or(ValidationError::LocalNotFound)?;
                stack.pop_operand(*local)?;
            }
            LocalTee(LocalIndex(index)) => {
                let local = context
                    .locals
                    .get(index as usize)
                    .ok_or(ValidationError::LocalNotFound)?;
                stack.pop_operand(*local)?;
                stack.push_operand(*local);
            }
            GlobalGet(GlobalIndex(index)) => {
                let global = context
                    .globals
                    .get(index as usize)
                    .ok_or(ValidationError::GlobalNotFound)?
                    .ty;
                stack.push_operand(global);
            }
            GlobalSet(GlobalIndex(index)) => {
                let global = context
                    .globals
                    .get(index as usize)
                    .ok_or(ValidationError::GlobalNotFound)?
                    .ty;
                stack.pop_operand(global)?;
            }

            Load {
                value,
                memory,
                bit_width,
            } => {
                context
                    .memories
                    .get(0)
                    .ok_or(ValidationError::MemoryNotFound)?;
                if 1 << memory.align > bit_width / 8 {
                    return Err(ValidationError::InvalidMemoryAlignment);
                } else {
                    stack.pop_operand(ValueType::I32)?;
                    stack.push_operand(value);
                }
            }

            Store {
                value,
                memory,
                bit_width,
            } => {
                context
                    .memories
                    .get(0)
                    .ok_or(ValidationError::MemoryNotFound)?;
                if 1 << memory.align > bit_width / 8 {
                    return Err(ValidationError::InvalidMemoryAlignment);
                } else {
                    stack.pop_operand(value)?;
                    stack.pop_operand(ValueType::I32)?;
                }
            }

            MemorySize => {
                context
                    .memories
                    .get(0)
                    .ok_or(ValidationError::MemoryNotFound)?;
                stack.push_operand(ValueType::I32);
            }

            MemoryGrow => {
                context
                    .memories
                    .get(0)
                    .ok_or(ValidationError::MemoryNotFound)?;
                stack.pop_operand(ValueType::I32)?;
                stack.push_operand(ValueType::I32);
            }

            Nop => {}
            Unreachable => {
                stack.mark_unreachable()?;
            }

            Block {
                result,
                instructions,
            } => {
                let types = result.types.clone().into_iter().collect::<Vec<_>>();
                stack.push_frame(types.clone(), types);
                validate_instruction_sequence(instructions, context, stack)?;
                for result in stack.pop_frame()? {
                    stack.push_operand(result);
                }
            }

            Loop {
                result,
                instructions,
            } => {
                let types = result.types.clone().into_iter().collect::<Vec<_>>();
                stack.push_frame(vec![], types);
                validate_instruction_sequence(instructions, context, stack)?;
                for result in stack.pop_frame()? {
                    stack.push_operand(result);
                }
            }

            Conditional {
                result,
                success,
                failure,
            } => {
                stack.pop_operand(ValueType::I32)?;

                let types = result.types.clone().into_iter().collect::<Vec<_>>();
                stack.push_frame(types.clone(), types);
                validate_instruction_sequence(success, context, stack)?;

                let types = stack.pop_frame()?;
                stack.push_frame(types.clone(), types);
                validate_instruction_sequence(failure, context, stack)?;

                for result in stack.pop_frame()? {
                    stack.push_operand(result);
                }
            }

            Branch(LabelIndex(index)) => {
                let label_types = stack.get_label(index as usize)?.to_vec();
                stack.pop_operands(label_types.into_iter())?;
                stack.mark_unreachable()?;
            }

            BranchConditional(LabelIndex(index)) => {
                let label_types = stack.get_label(index as usize)?.to_vec();
                stack.pop_operand(ValueType::I32)?;
                stack.pop_operands(label_types.iter().copied())?;
                label_types
                    .into_iter()
                    .for_each(|op| stack.push_operand(op));
            }

            BranchTable {
                targets,
                default: LabelIndex(default),
            } => {
                let label_types = stack.get_label(default as usize)?.to_vec();
                for LabelIndex(index) in targets {
                    if stack.get_label(*index as usize)? != label_types.as_slice() {
                        return Err(ValidationError::LabelTypeMismatch);
                    }
                }

                stack.pop_operand(ValueType::I32)?;
                stack.pop_operands(label_types.into_iter())?;
                stack.mark_unreachable()?;
            }

            Return => {
                let ret = context
                    .ret
                    .as_ref()
                    .ok_or(ValidationError::ContextDisallowReturn)?;
                stack.pop_operands(ret.types.iter().copied())?;
                stack.mark_unreachable()?;
            }

            Call(FunctionIndex(index)) => {
                let function = context
                    .functions
                    .get(index as usize)
                    .ok_or(ValidationError::FunctionNotFound)?;
                stack.pop_operands(function.parameters.iter().copied())?;
                function
                    .results
                    .iter()
                    .for_each(|op| stack.push_operand(*op));
            }

            CallIndirect(TypeIndex(index)) => {
                let table = context
                    .tables
                    .get(0)
                    .ok_or(ValidationError::TableNotFound)?;

                match table.element {
                    ElementType::FunctionReference => (),
                }

                let function = context
                    .types
                    .get(index as usize)
                    .ok_or(ValidationError::TypeNotFound)?;
                stack.pop_operand(ValueType::I32)?;
                stack.pop_operands(function.parameters.iter().copied())?;
                function
                    .results
                    .iter()
                    .for_each(|op| stack.push_operand(*op));
            }
        }
    }

    Ok(())
}

impl ValidationStack {
    pub fn new() -> ValidationStack {
        ValidationStack {
            operands: Vec::new(),
            frames: Vec::new(),
        }
    }

    pub fn push_operand(&mut self, operand: impl Into<Operand>) {
        self.operands.push(operand.into());
    }

    pub fn pop_any_operand(&mut self) -> ValidationResult<Operand> {
        let control = self
            .frames
            .last()
            .ok_or(ValidationError::ControlFrameUnderflow)?;

        if self.operands.len() == control.height {
            if control.unreachable {
                Ok(Operand::Unknown)
            } else {
                Err(ValidationError::OperandUnderflow)
            }
        } else {
            self.operands.pop().ok_or(ValidationError::OperandUnderflow)
        }
    }

    pub fn pop_operand(&mut self, expected: impl Into<Operand>) -> ValidationResult<Operand> {
        let expected = expected.into();
        let actual = self.pop_any_operand()?;
        match (actual, expected) {
            (Operand::Unknown, expected) => Ok(expected),
            (actual, Operand::Unknown) => Ok(actual),
            (actual, expected) if actual != expected => Err(ValidationError::UnexpectedOperand),
            (actual, _) => Ok(actual),
        }
    }

    pub fn pop_operands(
        &mut self,
        operands: impl Iterator<Item = impl Into<Operand>> + DoubleEndedIterator,
    ) -> ValidationResult<()> {
        for operand in operands.rev() {
            self.pop_operand(operand)?;
        }

        Ok(())
    }

    pub fn push_frame(&mut self, label: Vec<ValueType>, out: Vec<ValueType>) {
        let frame = ControlFrame {
            label_types: label,
            end_types: out,
            height: self.operands.len(),
            unreachable: false,
        };
        self.frames.push(frame);
    }

    pub fn pop_frame(&mut self) -> ValidationResult<Vec<ValueType>> {
        let types = self
            .frames
            .last()
            .ok_or(ValidationError::ControlFrameUnderflow)?
            .end_types
            .clone();
        self.pop_operands(types.into_iter())?;

        let frame = self
            .frames
            .pop()
            .ok_or(ValidationError::ControlFrameUnderflow)?;

        if self.operands.len() != frame.height {
            Err(ValidationError::OperandLeak)
        } else {
            Ok(frame.end_types)
        }
    }

    pub fn mark_unreachable(&mut self) -> ValidationResult<()> {
        let frame = self
            .frames
            .last_mut()
            .ok_or(ValidationError::ControlFrameUnderflow)?;

        self.operands.truncate(frame.height);
        frame.unreachable = true;
        Ok(())
    }

    pub fn get_label(&self, label: usize) -> ValidationResult<&[ValueType]> {
        let frame = self
            .frames
            .iter()
            .nth_back(label)
            .ok_or(ValidationError::LabelNotFound)?;
        Ok(&frame.label_types)
    }
}
