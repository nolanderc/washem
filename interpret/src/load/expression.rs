use super::error::*;
use super::instruction::*;
use nom::{bytes::complete::*, combinator::*, multi::*};

#[derive(Debug)]
pub struct Expression {
    instructions: Vec<Instruction>,
}

pub fn expression(bytes: &[u8]) -> ParseResult<Expression> {
    map(many_till(instruction, tag(&[END_BYTE])), |(instructions, _)| {
        Expression { instructions }
    })(bytes)
}
