use nom::{
    error::{ErrorKind as NomErrorKind, ParseError as NomError},
    IResult,
};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ParseError {
    Nom(NomErrorKind),
    UnknownSectionKind,
    UnexpectedEof,
    InvalidU32,
    InvalidValueType,
    SectionKindMismatch,
}

pub type Error = Box<dyn std::error::Error>;
pub type Result<T, E = Error> = std::result::Result<T, E>;
pub type ParseResult<'a, T> = IResult<&'a [u8], T, ParseError>;

impl<'a> NomError<&'a [u8]> for ParseError {
    fn from_error_kind(_input: &[u8], kind: NomErrorKind) -> Self {
        ParseError::Nom(kind)
    }

    fn append(_input: &[u8], _kind: NomErrorKind, other: Self) -> Self {
        other
    }
}

impl From<ParseError> for nom::Err<ParseError> {
    fn from(err: ParseError) -> Self {
        nom::Err::Error(err)
    }
}
