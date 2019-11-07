use super::error::*;
use super::instruction::*;
use crate::ast::expression::*;
use nom::{bytes::complete::*, combinator::*, multi::*};

pub fn expression(bytes: &[u8]) -> ParseResult<Expression> {
    map(
        many_till(instruction, tag(&[END_BYTE])),
        |(instructions, _)| Expression { instructions },
    )(bytes)
}
