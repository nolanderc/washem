use nom::{
    error::{ErrorKind as NomErrorKind, ParseError as NomError},
    IResult,
};

use failure::Context;
pub use failure::{Fail, ResultExt};

use std::fmt::{Display, Formatter, Result as FmtResult};

use super::sections::SectionKind;
pub use crate::error::*;

#[derive(Debug)]
pub struct ParseError {
    context: Context<ParseErrorKind>,
}

#[derive(Debug, Copy, Clone, PartialEq, Fail)]
pub enum ParseErrorKind {
    #[fail(display = "nom parser error: `{:?}`", _0)]
    Nom(NomErrorKind),
    #[fail(display = "while parsing {}", _0)]
    While(ParseLocation),
    #[fail(display = "unknown section kind")]
    UnknownSectionKind,
    #[fail(display = "unexpected eof")]
    UnexpectedEof,
    #[fail(display = "invalid u32")]
    InvalidU32,
    #[fail(display = "invalid value type")]
    InvalidValueType,
    #[fail(display = "section kind mismatch")]
    SectionKindMismatch,
    #[fail(display = "invalid name")]
    InvalidName,
    #[fail(display = "trailing bytes")]
    TrailingBytes,
    #[fail(display = "invalid opcode `{:x}`", _0)]
    InvalidOpCode(u8),
}

#[derive(Debug, Copy, Clone, PartialEq, Fail)]
pub enum ParseLocation {
    #[fail(display = "`{}` section", _0)]
    Section(SectionKind),
    #[fail(display = "expression")]
    Expression,
    #[fail(display = "instruction")]
    Instruction,
}

pub type ParseResult<'a, T> = IResult<&'a [u8], T, ParseError>;

pub fn wrap_context<'a, T>(
    f: impl Fn(&'a [u8]) -> ParseResult<T> + 'a,
    kind: impl Into<ParseErrorKind>,
) -> impl Fn(&'a [u8]) -> ParseResult<T> + 'a {
    let kind = kind.into();
    move |bytes| {
        f(bytes).map_err(|e| match e {
            nom::Err::Error(e) => nom::Err::Error(e.context(kind).into()),
            nom::Err::Failure(e) => nom::Err::Failure(e.context(kind).into()),
            nom::Err::Incomplete(e) => nom::Err::Incomplete(e),
        })
    }
}

impl<'a> NomError<&'a [u8]> for ParseError {
    fn from_error_kind(_input: &[u8], kind: NomErrorKind) -> Self {
        ParseError {
            context: Context::new(ParseErrorKind::Nom(kind)),
        }
    }

    fn append(_input: &[u8], _kind: NomErrorKind, other: Self) -> Self {
        other
    }
}

impl From<ParseLocation> for ParseErrorKind {
    fn from(location: ParseLocation) -> Self { ParseErrorKind::While(location) }
}

impl From<ParseError> for nom::Err<ParseError> {
    fn from(err: ParseError) -> Self {
        nom::Err::Error(err)
    }
}

impl From<ParseErrorKind> for nom::Err<ParseError> {
    fn from(err: ParseErrorKind) -> Self {
        nom::Err::Error(err.into())
    }
}

impl From<ParseErrorKind> for ParseError {
    fn from(kind: ParseErrorKind) -> Self {
        ParseError {
            context: Context::new(kind),
        }
    }
}

impl From<Context<ParseErrorKind>> for ParseError {
    fn from(context: Context<ParseErrorKind>) -> Self {
        ParseError { context }
    }
}

impl Fail for ParseError {
    fn cause(&self) -> Option<&dyn Fail> {
        None
    }

    fn context<D>(self, context: D) -> Context<D>
    where
        D: Display + Send + Sync + 'static,
        Self: Sized,
    {
        self.context.context(context)
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let message = Fail::iter_chain(&self.context)
            .map(|cause| cause.to_string())
            .collect::<Vec<_>>()
            .join(": ");

        Display::fmt(&message, f)?;

        Ok(())
    }
}
