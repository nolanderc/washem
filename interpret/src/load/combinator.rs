use super::error::*;

/// Always return a constant value
pub fn constant<T: Copy>(value: T) -> impl Fn(&[u8]) -> ParseResult<T> {
    move |bytes| Ok((bytes, value))
}

/// Fails if the remaining input after applying a parser is not empty
pub fn exact<'a, T>(
    f: impl Fn(&'a [u8]) -> ParseResult<'a, T> + 'a,
) -> impl Fn(&'a [u8]) -> ParseResult<'a, T> + 'a {
    move |bytes| {
        let (rest, value) = f(bytes)?;
        if rest.is_empty() {
            Ok((rest, value))
        } else {
            Err(ParseErrorKind::TrailingBytes.into())
        }
    }
}
