use nom::bytes::complete::*;

use super::error::*;

pub fn leb_u32(bytes: &[u8]) -> ParseResult<u32> {
    // 5 bytes
    let mut bytes = bytes.iter();

    let mut value = 0u32;
    let mut n = 32u8;

    loop {
        let byte = *bytes.next().ok_or(ParseError::UnexpectedEof)?;

        value |= u32::from(byte & 0b0111_1111) << (32 - n);

        if byte > 127 {
            return Ok((bytes.as_slice(), value));
        }

        n = n.saturating_sub(7);

        if n == 0 {
            return Err(ParseError::InvalidU32.into());
        }
    }
}

pub fn leb128(bytes: &[u8], n: u32) -> ParseResult<&[u8]> {
    unimplemented!()
}
