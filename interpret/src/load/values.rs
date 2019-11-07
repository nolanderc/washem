use super::error::*;
use nom::multi::*;

pub fn byte(bytes: &[u8]) -> ParseResult<u8> {
    if bytes.is_empty() {
        Err(ParseErrorKind::UnexpectedEof.into())
    } else {
        Ok((&bytes[1..], bytes[0]))
    }
}

pub fn leb_u32(bytes: &[u8]) -> ParseResult<u32> {
    // 5 bytes
    let mut bytes = bytes.iter();

    let mut value = 0u32;
    let mut n = 32u8;

    loop {
        let byte = *bytes.next().ok_or(ParseErrorKind::UnexpectedEof)?;

        value |= u32::from(byte & 0b0111_1111) << (32 - n);

        if byte < 128 {
            return Ok((bytes.as_slice(), value));
        }

        n = n.saturating_sub(7);

        if n == 0 {
            return Err(ParseErrorKind::InvalidU32.into());
        }
    }
}

pub fn leb_s32(bytes: &[u8]) -> ParseResult<i32> {
    let mut bytes = bytes.iter();

    let mut value = 0i32;
    let size = 32;
    let mut shift = 0;

    loop {
        let byte = *bytes.next().ok_or(ParseErrorKind::UnexpectedEof)?;

        value |= i32::from(byte & 0b0111_1111) << shift;

        shift += 7;

        if byte < 128 {
            if shift < size && (byte & 0b0100_0000 != 0) {
                value |= !0 << shift
            }

            return Ok((bytes.as_slice(), value));
        }

        if shift >= size {
            return Err(ParseErrorKind::InvalidI32.into());
        }
    }
}

pub fn leb_s64(bytes: &[u8]) -> ParseResult<i64> {
    let mut bytes = bytes.iter();

    let mut value = 0i64;
    let size = 64;
    let mut shift = 0;

    loop {
        let byte = *bytes.next().ok_or(ParseErrorKind::UnexpectedEof)?;

        value |= i64::from(byte & 0b0111_1111) << shift;

        shift += 7;

        if byte < 128 {
            if shift < size && (byte & 0b0100_0000 != 0) {
                value |= !0 << shift
            }

            return Ok((bytes.as_slice(), value));
        }

        if shift >= size {
            return Err(ParseErrorKind::InvalidI64.into());
        }
    }
}

pub fn name(bytes: &[u8]) -> ParseResult<&str> {
    let (bytes, text) = length_data(leb_u32)(bytes)?;

    let text = std::str::from_utf8(text)
        .context(ParseErrorKind::InvalidName)
        .map_err(ParseError::from)?;

    Ok((bytes, text))
}
