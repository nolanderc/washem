use super::error::*;
use super::values::*;
use nom::multi::*;

pub fn vec<T>(f: impl Fn(&[u8]) -> ParseResult<T>) -> impl Fn(&[u8]) -> ParseResult<Vec<T>> {
    move |bytes| {
        let (bytes, n) = leb_u32(bytes)?;
        count(&f, n as usize)(bytes)
    }
}
