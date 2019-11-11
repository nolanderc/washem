use super::instruction::*;

#[derive(Debug, Clone)]
pub struct Expression {
    pub instructions: Vec<Instruction>,
}
