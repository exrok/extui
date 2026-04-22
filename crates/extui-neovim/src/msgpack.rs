//! Minimal MessagePack codec tailored to Neovim's embedded RPC protocol.
//!
//! The decoder is a cursor-style [`Reader`] that borrows directly
//! from an input slice. It is not a general-purpose MessagePack
//! library and exposes only the primitives used by Neovim's redraw
//! notifications and the RPC replies this crate expects. Unknown
//! values inside a payload can be stepped over with [`Reader::skip`].
//!
//! The encoder is a matching [`Writer`] providing the primitives used
//! by the outbound RPC calls `nvim_ui_attach`, `nvim_ui_try_resize`,
//! `nvim_ui_detach`, `nvim_input`, and `nvim_input_mouse`.

use std::fmt;

/// Error returned by the [`Reader`] when the input is malformed or truncated.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    /// The input slice ended mid-value.
    UnexpectedEof,
    /// A tag byte did not correspond to any MessagePack format.
    InvalidTag(u8),
    /// An integer value did not fit in the requested Rust type.
    IntOverflow,
    /// A `read_*` helper was called for a type that did not match the next value.
    TypeMismatch,
    /// A string value was not valid UTF-8.
    InvalidUtf8,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnexpectedEof => f.write_str("unexpected end of input"),
            Error::InvalidTag(b) => write!(f, "invalid msgpack tag 0x{b:02x}"),
            Error::IntOverflow => f.write_str("integer does not fit in target type"),
            Error::TypeMismatch => f.write_str("msgpack type mismatch"),
            Error::InvalidUtf8 => f.write_str("invalid utf-8 in msgpack string"),
        }
    }
}

impl std::error::Error for Error {}

/// High-level classification of the next value in a [`Reader`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    /// A nil value (`0xc0`).
    Nil,
    /// A boolean value.
    Bool,
    /// Any integer format, positive or negative.
    Int,
    /// A string value (fixstr / str 8 / str 16 / str 32).
    Str,
    /// A binary blob (bin 8 / bin 16 / bin 32).
    Bin,
    /// An array header.
    Array,
    /// A map header.
    Map,
    /// An extension type (Neovim uses these for Buffer/Window/Tabpage handles).
    Ext,
    /// A float (32 or 64 bit).
    Float,
}

/// A cursor that reads MessagePack values from a borrowed byte slice.
///
/// The reader is zero-copy: [`Reader::read_str`] and [`Reader::read_bin`]
/// return references borrowed from the input.
///
/// # Examples
///
/// ```ignore
/// use extui_neovim::msgpack::Reader;
///
/// let bytes = &[0x93, 0x01, 0x02, 0x03];
/// let mut r = Reader::new(bytes);
/// let len = r.read_array_len().unwrap();
/// assert_eq!(len, 3);
/// ```
pub struct Reader<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Reader<'a> {
    /// Creates a new reader over `bytes`, starting at position zero.
    pub fn new(bytes: &'a [u8]) -> Self {
        Reader { bytes, pos: 0 }
    }

    /// Returns the number of bytes consumed so far.
    pub fn position(&self) -> usize {
        self.pos
    }

    /// Returns `true` when the cursor is at the end of the input.
    pub fn eof(&self) -> bool {
        self.pos >= self.bytes.len()
    }

    /// Returns the slice of bytes that remain unread.
    pub fn remaining(&self) -> &'a [u8] {
        &self.bytes[self.pos..]
    }

    fn need(&self, n: usize) -> Result<(), Error> {
        if self.pos + n > self.bytes.len() {
            return Err(Error::UnexpectedEof);
        }
        Ok(())
    }

    fn read_u8(&mut self) -> Result<u8, Error> {
        self.need(1)?;
        let b = self.bytes[self.pos];
        self.pos += 1;
        Ok(b)
    }

    fn peek_u8(&self) -> Result<u8, Error> {
        self.need(1)?;
        Ok(self.bytes[self.pos])
    }

    fn read_n(&mut self, n: usize) -> Result<&'a [u8], Error> {
        self.need(n)?;
        let slice = &self.bytes[self.pos..self.pos + n];
        self.pos += n;
        Ok(slice)
    }

    fn read_be_u16(&mut self) -> Result<u16, Error> {
        let bytes = self.read_n(2)?;
        Ok(u16::from_be_bytes([bytes[0], bytes[1]]))
    }

    fn read_be_u32(&mut self) -> Result<u32, Error> {
        let bytes = self.read_n(4)?;
        Ok(u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }

    fn read_be_u64(&mut self) -> Result<u64, Error> {
        let bytes = self.read_n(8)?;
        let mut arr = [0u8; 8];
        arr.copy_from_slice(bytes);
        Ok(u64::from_be_bytes(arr))
    }

    /// Returns the [`Kind`] of the next value without consuming it.
    pub fn peek_kind(&self) -> Result<Kind, Error> {
        let tag = self.peek_u8()?;
        kind_of(tag)
    }

    /// Reads a nil value.
    pub fn read_nil(&mut self) -> Result<(), Error> {
        let tag = self.read_u8()?;
        if tag == 0xc0 {
            Ok(())
        } else {
            Err(Error::TypeMismatch)
        }
    }

    /// Reads a boolean value.
    pub fn read_bool(&mut self) -> Result<bool, Error> {
        let tag = self.read_u8()?;
        match tag {
            0xc2 => Ok(false),
            0xc3 => Ok(true),
            _ => Err(Error::TypeMismatch),
        }
    }

    /// Reads any integer format as a signed 64-bit value.
    ///
    /// # Errors
    ///
    /// Returns [`Error::IntOverflow`] if the wire value is a `uint 64` that
    /// does not fit in `i64`.
    pub fn read_i64(&mut self) -> Result<i64, Error> {
        let tag = self.read_u8()?;
        match tag {
            t if t < 0x80 => Ok(t as i64),
            t if t >= 0xe0 => Ok((t as i8) as i64),
            0xcc => Ok(self.read_u8()? as i64),
            0xcd => Ok(self.read_be_u16()? as i64),
            0xce => Ok(self.read_be_u32()? as i64),
            0xcf => {
                let v = self.read_be_u64()?;
                if v > i64::MAX as u64 {
                    return Err(Error::IntOverflow);
                }
                Ok(v as i64)
            }
            0xd0 => Ok((self.read_u8()? as i8) as i64),
            0xd1 => Ok((self.read_be_u16()? as i16) as i64),
            0xd2 => Ok((self.read_be_u32()? as i32) as i64),
            0xd3 => Ok(self.read_be_u64()? as i64),
            _ => Err(Error::TypeMismatch),
        }
    }

    /// Reads any integer format as an unsigned 64-bit value.
    ///
    /// # Errors
    ///
    /// Returns [`Error::IntOverflow`] if the wire value is negative.
    pub fn read_u64(&mut self) -> Result<u64, Error> {
        let v = self.read_i64()?;
        if v < 0 {
            return Err(Error::IntOverflow);
        }
        Ok(v as u64)
    }

    /// Reads a string value, returning a slice borrowed from the input.
    pub fn read_str(&mut self) -> Result<&'a str, Error> {
        let tag = self.read_u8()?;
        let len = match tag {
            t if (0xa0..=0xbf).contains(&t) => (t & 0x1f) as usize,
            0xd9 => self.read_u8()? as usize,
            0xda => self.read_be_u16()? as usize,
            0xdb => self.read_be_u32()? as usize,
            _ => return Err(Error::TypeMismatch),
        };
        let bytes = self.read_n(len)?;
        std::str::from_utf8(bytes).map_err(|_| Error::InvalidUtf8)
    }

    /// Reads a binary blob, returning a slice borrowed from the input.
    pub fn read_bin(&mut self) -> Result<&'a [u8], Error> {
        let tag = self.read_u8()?;
        let len = match tag {
            0xc4 => self.read_u8()? as usize,
            0xc5 => self.read_be_u16()? as usize,
            0xc6 => self.read_be_u32()? as usize,
            _ => return Err(Error::TypeMismatch),
        };
        self.read_n(len)
    }

    /// Reads an array header and returns its element count.
    pub fn read_array_len(&mut self) -> Result<usize, Error> {
        let tag = self.read_u8()?;
        match tag {
            t if (0x90..=0x9f).contains(&t) => Ok((t & 0x0f) as usize),
            0xdc => Ok(self.read_be_u16()? as usize),
            0xdd => Ok(self.read_be_u32()? as usize),
            _ => Err(Error::TypeMismatch),
        }
    }

    /// Reads a map header and returns its key-value pair count.
    pub fn read_map_len(&mut self) -> Result<usize, Error> {
        let tag = self.read_u8()?;
        match tag {
            t if (0x80..=0x8f).contains(&t) => Ok((t & 0x0f) as usize),
            0xde => Ok(self.read_be_u16()? as usize),
            0xdf => Ok(self.read_be_u32()? as usize),
            _ => Err(Error::TypeMismatch),
        }
    }

    /// Skips exactly one value of any type, descending into arrays and maps.
    ///
    /// Use this to discard unknown RPC response arguments, unknown map keys
    /// in `hl_attr_define`, or trailing optional fields in `grid_line` cell
    /// tuples.
    pub fn skip(&mut self) -> Result<(), Error> {
        let tag = self.read_u8()?;
        match tag {
            t if t < 0x80 => Ok(()),
            t if (0x80..=0x8f).contains(&t) => {
                let n = (t & 0x0f) as usize;
                for _ in 0..n * 2 {
                    self.skip()?;
                }
                Ok(())
            }
            t if (0x90..=0x9f).contains(&t) => {
                let n = (t & 0x0f) as usize;
                for _ in 0..n {
                    self.skip()?;
                }
                Ok(())
            }
            t if (0xa0..=0xbf).contains(&t) => {
                let n = (t & 0x1f) as usize;
                self.read_n(n)?;
                Ok(())
            }
            0xc0 | 0xc2 | 0xc3 => Ok(()),
            0xc4 => {
                let n = self.read_u8()? as usize;
                self.read_n(n)?;
                Ok(())
            }
            0xc5 => {
                let n = self.read_be_u16()? as usize;
                self.read_n(n)?;
                Ok(())
            }
            0xc6 => {
                let n = self.read_be_u32()? as usize;
                self.read_n(n)?;
                Ok(())
            }
            0xc7 => {
                let n = self.read_u8()? as usize;
                self.read_n(1 + n)?;
                Ok(())
            }
            0xc8 => {
                let n = self.read_be_u16()? as usize;
                self.read_n(1 + n)?;
                Ok(())
            }
            0xc9 => {
                let n = self.read_be_u32()? as usize;
                self.read_n(1 + n)?;
                Ok(())
            }
            0xca => {
                self.read_n(4)?;
                Ok(())
            }
            0xcb => {
                self.read_n(8)?;
                Ok(())
            }
            0xcc | 0xd0 => {
                self.read_n(1)?;
                Ok(())
            }
            0xcd | 0xd1 => {
                self.read_n(2)?;
                Ok(())
            }
            0xce | 0xd2 => {
                self.read_n(4)?;
                Ok(())
            }
            0xcf | 0xd3 => {
                self.read_n(8)?;
                Ok(())
            }
            0xd4 => {
                self.read_n(2)?;
                Ok(())
            }
            0xd5 => {
                self.read_n(3)?;
                Ok(())
            }
            0xd6 => {
                self.read_n(5)?;
                Ok(())
            }
            0xd7 => {
                self.read_n(9)?;
                Ok(())
            }
            0xd8 => {
                self.read_n(17)?;
                Ok(())
            }
            0xd9 => {
                let n = self.read_u8()? as usize;
                self.read_n(n)?;
                Ok(())
            }
            0xda => {
                let n = self.read_be_u16()? as usize;
                self.read_n(n)?;
                Ok(())
            }
            0xdb => {
                let n = self.read_be_u32()? as usize;
                self.read_n(n)?;
                Ok(())
            }
            0xdc => {
                let n = self.read_be_u16()? as usize;
                for _ in 0..n {
                    self.skip()?;
                }
                Ok(())
            }
            0xdd => {
                let n = self.read_be_u32()? as usize;
                for _ in 0..n {
                    self.skip()?;
                }
                Ok(())
            }
            0xde => {
                let n = self.read_be_u16()? as usize;
                for _ in 0..n * 2 {
                    self.skip()?;
                }
                Ok(())
            }
            0xdf => {
                let n = self.read_be_u32()? as usize;
                for _ in 0..n * 2 {
                    self.skip()?;
                }
                Ok(())
            }
            t if t >= 0xe0 => Ok(()),
            _ => Err(Error::InvalidTag(tag)),
        }
    }
}

fn kind_of(tag: u8) -> Result<Kind, Error> {
    match tag {
        t if t < 0x80 => Ok(Kind::Int),
        t if (0x80..=0x8f).contains(&t) => Ok(Kind::Map),
        t if (0x90..=0x9f).contains(&t) => Ok(Kind::Array),
        t if (0xa0..=0xbf).contains(&t) => Ok(Kind::Str),
        0xc0 => Ok(Kind::Nil),
        0xc2 | 0xc3 => Ok(Kind::Bool),
        0xc4..=0xc6 => Ok(Kind::Bin),
        0xc7..=0xc9 => Ok(Kind::Ext),
        0xca | 0xcb => Ok(Kind::Float),
        0xcc..=0xcf => Ok(Kind::Int),
        0xd0..=0xd3 => Ok(Kind::Int),
        0xd4..=0xd8 => Ok(Kind::Ext),
        0xd9..=0xdb => Ok(Kind::Str),
        0xdc | 0xdd => Ok(Kind::Array),
        0xde | 0xdf => Ok(Kind::Map),
        t if t >= 0xe0 => Ok(Kind::Int),
        _ => Err(Error::InvalidTag(tag)),
    }
}

/// Returns the byte length of the first complete MessagePack value in
/// `bytes`, or `None` if the input is truncated or malformed.
///
/// Used by the reader thread to tell when its scratch buffer holds a
/// full RPC frame.
pub fn frame_len(bytes: &[u8]) -> Option<usize> {
    let mut pos = 0;
    if skip_value(bytes, &mut pos) {
        Some(pos)
    } else {
        None
    }
}

#[inline(always)]
fn skip_n(bytes: &[u8], pos: &mut usize, n: usize) -> bool {
    let Some(end) = pos.checked_add(n) else {
        return false;
    };
    if end > bytes.len() {
        return false;
    }
    *pos = end;
    true
}

#[inline(always)]
fn read_u8_at(bytes: &[u8], pos: &mut usize) -> Option<u8> {
    let b = *bytes.get(*pos)?;
    *pos += 1;
    Some(b)
}

#[inline(always)]
fn read_be_u16_at(bytes: &[u8], pos: &mut usize) -> Option<u16> {
    let end = pos.checked_add(2)?;
    if end > bytes.len() {
        return None;
    }
    let v = u16::from_be_bytes([bytes[*pos], bytes[*pos + 1]]);
    *pos = end;
    Some(v)
}

#[inline(always)]
fn read_be_u32_at(bytes: &[u8], pos: &mut usize) -> Option<u32> {
    let end = pos.checked_add(4)?;
    if end > bytes.len() {
        return None;
    }
    let v = u32::from_be_bytes([
        bytes[*pos],
        bytes[*pos + 1],
        bytes[*pos + 2],
        bytes[*pos + 3],
    ]);
    *pos = end;
    Some(v)
}

fn skip_value(bytes: &[u8], pos: &mut usize) -> bool {
    let Some(tag) = read_u8_at(bytes, pos) else {
        return false;
    };
    match tag {
        // positive fixint, negative fixint, nil, false, true
        0x00..=0x7f | 0xe0..=0xff | 0xc0 | 0xc2 | 0xc3 => true,
        // fixmap: N key-value pairs (2N values).
        0x80..=0x8f => {
            let n = (tag & 0x0f) as usize;
            for _ in 0..n * 2 {
                if !skip_value(bytes, pos) {
                    return false;
                }
            }
            true
        }
        // fixarray: N values.
        0x90..=0x9f => {
            let n = (tag & 0x0f) as usize;
            for _ in 0..n {
                if !skip_value(bytes, pos) {
                    return false;
                }
            }
            true
        }
        // fixstr.
        0xa0..=0xbf => skip_n(bytes, pos, (tag & 0x1f) as usize),
        // bin 8, str 8.
        0xc4 | 0xd9 => match read_u8_at(bytes, pos) {
            Some(n) => skip_n(bytes, pos, n as usize),
            None => false,
        },
        // bin 16, str 16.
        0xc5 | 0xda => match read_be_u16_at(bytes, pos) {
            Some(n) => skip_n(bytes, pos, n as usize),
            None => false,
        },
        // bin 32, str 32.
        0xc6 | 0xdb => match read_be_u32_at(bytes, pos) {
            Some(n) => skip_n(bytes, pos, n as usize),
            None => false,
        },
        // ext 8: size u8 + type u8 + data.
        0xc7 => match read_u8_at(bytes, pos) {
            Some(n) => skip_n(bytes, pos, 1 + n as usize),
            None => false,
        },
        // ext 16.
        0xc8 => match read_be_u16_at(bytes, pos) {
            Some(n) => skip_n(bytes, pos, 1 + n as usize),
            None => false,
        },
        // ext 32.
        0xc9 => match read_be_u32_at(bytes, pos) {
            Some(n) => skip_n(bytes, pos, 1 + n as usize),
            None => false,
        },
        // float 32.
        0xca => skip_n(bytes, pos, 4),
        // float 64.
        0xcb => skip_n(bytes, pos, 8),
        // uint 8, int 8.
        0xcc | 0xd0 => skip_n(bytes, pos, 1),
        // uint 16, int 16.
        0xcd | 0xd1 => skip_n(bytes, pos, 2),
        // uint 32, int 32.
        0xce | 0xd2 => skip_n(bytes, pos, 4),
        // uint 64, int 64.
        0xcf | 0xd3 => skip_n(bytes, pos, 8),
        // fixext 1..16: type u8 + N data bytes.
        0xd4 => skip_n(bytes, pos, 2),
        0xd5 => skip_n(bytes, pos, 3),
        0xd6 => skip_n(bytes, pos, 5),
        0xd7 => skip_n(bytes, pos, 9),
        0xd8 => skip_n(bytes, pos, 17),
        // array 16.
        0xdc => {
            let Some(n) = read_be_u16_at(bytes, pos) else {
                return false;
            };
            for _ in 0..n {
                if !skip_value(bytes, pos) {
                    return false;
                }
            }
            true
        }
        // array 32.
        0xdd => {
            let Some(n) = read_be_u32_at(bytes, pos) else {
                return false;
            };
            for _ in 0..n {
                if !skip_value(bytes, pos) {
                    return false;
                }
            }
            true
        }
        // map 16.
        0xde => {
            let Some(n) = read_be_u16_at(bytes, pos) else {
                return false;
            };
            for _ in 0..(n as usize) * 2 {
                if !skip_value(bytes, pos) {
                    return false;
                }
            }
            true
        }
        // map 32.
        0xdf => {
            let Some(n) = read_be_u32_at(bytes, pos) else {
                return false;
            };
            for _ in 0..(n as usize) * 2 {
                if !skip_value(bytes, pos) {
                    return false;
                }
            }
            true
        }
        // 0xc1 is reserved; any other tag is malformed.
        0xc1 => false,
    }
}

/// Byte-oriented MessagePack encoder.
///
/// Provides only the primitives used by extui-neovim's outbound RPC calls.
pub struct Writer {
    out: Vec<u8>,
}

impl Writer {
    /// Creates a new writer with an empty output buffer.
    pub fn new() -> Self {
        Writer { out: Vec::new() }
    }

    /// Creates a new writer backed by a buffer with the given capacity.
    pub fn with_capacity(cap: usize) -> Self {
        Writer {
            out: Vec::with_capacity(cap),
        }
    }

    /// Empties the output buffer without releasing its capacity.
    pub fn clear(&mut self) {
        self.out.clear();
    }

    /// Returns a view of the currently buffered bytes.
    pub fn bytes(&self) -> &[u8] {
        &self.out
    }

    /// Consumes the writer and returns the owned byte buffer.
    pub fn into_bytes(self) -> Vec<u8> {
        self.out
    }

    /// Writes a nil value.
    pub fn write_nil(&mut self) {
        self.out.push(0xc0);
    }

    /// Writes a boolean value.
    pub fn write_bool(&mut self, b: bool) {
        self.out.push(if b { 0xc3 } else { 0xc2 });
    }

    /// Writes an unsigned integer using the smallest format that fits.
    pub fn write_u64(&mut self, v: u64) {
        if v < 0x80 {
            self.out.push(v as u8);
        } else if v <= u8::MAX as u64 {
            self.out.push(0xcc);
            self.out.push(v as u8);
        } else if v <= u16::MAX as u64 {
            self.out.push(0xcd);
            self.out.extend_from_slice(&(v as u16).to_be_bytes());
        } else if v <= u32::MAX as u64 {
            self.out.push(0xce);
            self.out.extend_from_slice(&(v as u32).to_be_bytes());
        } else {
            self.out.push(0xcf);
            self.out.extend_from_slice(&v.to_be_bytes());
        }
    }

    /// Writes a signed integer using the smallest format that fits.
    pub fn write_i64(&mut self, v: i64) {
        if v >= 0 {
            self.write_u64(v as u64);
            return;
        }
        if v >= -32 {
            self.out.push(v as i8 as u8);
        } else if v >= i8::MIN as i64 {
            self.out.push(0xd0);
            self.out.push(v as i8 as u8);
        } else if v >= i16::MIN as i64 {
            self.out.push(0xd1);
            self.out.extend_from_slice(&(v as i16).to_be_bytes());
        } else if v >= i32::MIN as i64 {
            self.out.push(0xd2);
            self.out.extend_from_slice(&(v as i32).to_be_bytes());
        } else {
            self.out.push(0xd3);
            self.out.extend_from_slice(&v.to_be_bytes());
        }
    }

    /// Writes a string value.
    pub fn write_str(&mut self, s: &str) {
        let len = s.len();
        if len <= 31 {
            self.out.push(0xa0 | len as u8);
        } else if len <= u8::MAX as usize {
            self.out.push(0xd9);
            self.out.push(len as u8);
        } else if len <= u16::MAX as usize {
            self.out.push(0xda);
            self.out.extend_from_slice(&(len as u16).to_be_bytes());
        } else {
            self.out.push(0xdb);
            self.out.extend_from_slice(&(len as u32).to_be_bytes());
        }
        self.out.extend_from_slice(s.as_bytes());
    }

    /// Writes an array header for an array of `n` elements.
    pub fn write_array_header(&mut self, n: usize) {
        if n <= 15 {
            self.out.push(0x90 | n as u8);
        } else if n <= u16::MAX as usize {
            self.out.push(0xdc);
            self.out.extend_from_slice(&(n as u16).to_be_bytes());
        } else {
            self.out.push(0xdd);
            self.out.extend_from_slice(&(n as u32).to_be_bytes());
        }
    }

    /// Writes a map header for a map of `n` key-value pairs.
    pub fn write_map_header(&mut self, n: usize) {
        if n <= 15 {
            self.out.push(0x80 | n as u8);
        } else if n <= u16::MAX as usize {
            self.out.push(0xde);
            self.out.extend_from_slice(&(n as u16).to_be_bytes());
        } else {
            self.out.push(0xdf);
            self.out.extend_from_slice(&(n as u32).to_be_bytes());
        }
    }
}

impl Default for Writer {
    fn default() -> Self {
        Writer::new()
    }
}

/// Encodes a `nvim_ui_attach` request into `w`.
///
/// The options enable `ext_linegrid`, `ext_termcolors`, and `rgb=true`.
/// The first selects the modern line-grid protocol. `ext_termcolors`
/// tells Neovim to leave external default colors unset so the client can
/// inherit the host terminal defaults. `rgb=true` advertises that the UI
/// can display 24-bit colors once `termguicolors` is enabled inside Nvim.
///
/// Sent as MessagePack-RPC request type 0.
pub fn encode_ui_attach(
    w: &mut Writer,
    msgid: u32,
    width: u16,
    height: u16,
    term_name: Option<&str>,
    term_colors: u16,
) {
    w.write_array_header(4);
    w.write_u64(0);
    w.write_u64(msgid as u64);
    w.write_str("nvim_ui_attach");
    w.write_array_header(3);
    w.write_u64(width as u64);
    w.write_u64(height as u64);
    let opt_count = if term_name.is_some() { 6 } else { 5 };
    w.write_map_header(opt_count);
    w.write_str("rgb");
    w.write_bool(true);
    w.write_str("ext_linegrid");
    w.write_bool(true);
    w.write_str("ext_termcolors");
    w.write_bool(true);
    w.write_str("ext_multigrid");
    w.write_bool(false);
    if let Some(term_name) = term_name {
        w.write_str("term_name");
        w.write_str(term_name);
    }
    w.write_str("term_colors");
    w.write_u64(term_colors as u64);
}

/// Encodes a `nvim_ui_try_resize` notification into `w`.
pub fn encode_ui_try_resize(w: &mut Writer, width: u16, height: u16) {
    w.write_array_header(3);
    w.write_u64(2);
    w.write_str("nvim_ui_try_resize");
    w.write_array_header(2);
    w.write_u64(width as u64);
    w.write_u64(height as u64);
}

/// Encodes a `nvim_input` notification into `w`.
pub fn encode_input(w: &mut Writer, keys: &str) {
    w.write_array_header(3);
    w.write_u64(2);
    w.write_str("nvim_input");
    w.write_array_header(1);
    w.write_str(keys);
}

/// Encodes a `nvim_command` notification into `w`.
pub fn encode_command(w: &mut Writer, command: &str) {
    w.write_array_header(3);
    w.write_u64(2);
    w.write_str("nvim_command");
    w.write_array_header(1);
    w.write_str(command);
}

/// Encodes `nvim_set_option_value` for a boolean option as a notification.
pub fn encode_set_option_value_bool(w: &mut Writer, name: &str, value: bool) {
    w.write_array_header(3);
    w.write_u64(2);
    w.write_str("nvim_set_option_value");
    w.write_array_header(3);
    w.write_str(name);
    w.write_bool(value);
    w.write_map_header(0);
}

/// Encodes a `nvim_input_mouse` notification into `w`.
pub fn encode_input_mouse(
    w: &mut Writer,
    button: &str,
    action: &str,
    modifier: &str,
    row: u16,
    col: u16,
) {
    w.write_array_header(3);
    w.write_u64(2);
    w.write_str("nvim_input_mouse");
    w.write_array_header(6);
    w.write_str(button);
    w.write_str(action);
    w.write_str(modifier);
    w.write_u64(0);
    w.write_u64(row as u64);
    w.write_u64(col as u64);
}

/// Encodes a `nvim_buf_get_lines(0, 0, -1, true)` request into `w`.
pub fn encode_current_buffer_get_lines_request(w: &mut Writer, msgid: u32) {
    w.write_array_header(4);
    w.write_u64(0);
    w.write_u64(msgid as u64);
    w.write_str("nvim_buf_get_lines");
    w.write_array_header(4);
    w.write_u64(0);
    w.write_i64(0);
    w.write_i64(-1);
    w.write_bool(true);
}

/// Encodes a `nvim_buf_set_lines(0, 0, -1, true, lines)` notification into `w`.
pub fn encode_current_buffer_set_lines(w: &mut Writer, lines: &[&str]) {
    w.write_array_header(3);
    w.write_u64(2);
    w.write_str("nvim_buf_set_lines");
    w.write_array_header(5);
    w.write_u64(0);
    w.write_i64(0);
    w.write_i64(-1);
    w.write_bool(true);
    w.write_array_header(lines.len());
    for line in lines {
        w.write_str(line);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_fixint() {
        let mut w = Writer::new();
        w.write_u64(0);
        w.write_u64(127);
        w.write_i64(-1);
        w.write_i64(-32);
        let bytes = w.into_bytes();
        assert_eq!(bytes, vec![0x00, 0x7f, 0xff, 0xe0]);

        let mut r = Reader::new(&bytes);
        assert_eq!(r.read_u64().unwrap(), 0);
        assert_eq!(r.read_u64().unwrap(), 127);
        assert_eq!(r.read_i64().unwrap(), -1);
        assert_eq!(r.read_i64().unwrap(), -32);
        assert!(r.eof());
    }

    #[test]
    fn int_width_promotion() {
        let cases: &[i64] = &[
            0,
            127,
            128,
            255,
            256,
            65_535,
            65_536,
            -1,
            -32,
            -33,
            -128,
            -129,
            -32_768,
            -32_769,
            i32::MAX as i64,
            i32::MIN as i64,
            i64::MAX,
            i64::MIN,
        ];
        for &v in cases {
            let mut w = Writer::new();
            w.write_i64(v);
            let bytes = w.into_bytes();
            let mut r = Reader::new(&bytes);
            assert_eq!(r.read_i64().unwrap(), v, "roundtrip failed for {v}");
            assert!(r.eof());
        }
    }

    #[test]
    fn u64_overflow_into_i64() {
        let mut w = Writer::new();
        w.write_u64(u64::MAX);
        let bytes = w.into_bytes();
        let mut r = Reader::new(&bytes);
        assert_eq!(r.read_i64(), Err(Error::IntOverflow));
    }

    #[test]
    fn str_formats() {
        for len in [0usize, 1, 31, 32, 255, 256, 65_535, 65_536] {
            let s: String = std::iter::repeat_n('a', len).collect();
            let mut w = Writer::new();
            w.write_str(&s);
            let bytes = w.into_bytes();
            let mut r = Reader::new(&bytes);
            assert_eq!(r.read_str().unwrap(), s.as_str());
            assert!(r.eof());
        }
    }

    #[test]
    fn array_and_map_headers() {
        let mut w = Writer::new();
        w.write_array_header(3);
        w.write_u64(1);
        w.write_u64(2);
        w.write_u64(3);
        let bytes = w.into_bytes();
        let mut r = Reader::new(&bytes);
        assert_eq!(r.read_array_len().unwrap(), 3);
        assert_eq!(r.read_u64().unwrap(), 1);
        assert_eq!(r.read_u64().unwrap(), 2);
        assert_eq!(r.read_u64().unwrap(), 3);

        let mut w = Writer::new();
        w.write_map_header(2);
        w.write_str("a");
        w.write_u64(1);
        w.write_str("b");
        w.write_u64(2);
        let bytes = w.into_bytes();
        let mut r = Reader::new(&bytes);
        assert_eq!(r.read_map_len().unwrap(), 2);
        assert_eq!(r.read_str().unwrap(), "a");
        assert_eq!(r.read_u64().unwrap(), 1);
        assert_eq!(r.read_str().unwrap(), "b");
        assert_eq!(r.read_u64().unwrap(), 2);
    }

    #[test]
    fn skip_nested() {
        let mut w = Writer::new();
        w.write_array_header(3);
        w.write_u64(42);
        w.write_map_header(2);
        w.write_str("foo");
        w.write_array_header(2);
        w.write_u64(1);
        w.write_u64(2);
        w.write_str("bar");
        w.write_bool(true);
        w.write_str("tail");
        let bytes = w.into_bytes();

        let mut r = Reader::new(&bytes);
        assert_eq!(r.read_array_len().unwrap(), 3);
        assert_eq!(r.read_u64().unwrap(), 42);
        r.skip().unwrap();
        assert_eq!(r.read_str().unwrap(), "tail");
        assert!(r.eof());
    }

    #[test]
    fn frame_len_detects_truncation() {
        let mut w = Writer::new();
        w.write_array_header(3);
        w.write_u64(2);
        w.write_str("redraw");
        w.write_array_header(0);
        let bytes = w.into_bytes();

        for short in 0..bytes.len() {
            assert_eq!(frame_len(&bytes[..short]), None, "len={short}");
        }
        assert_eq!(frame_len(&bytes), Some(bytes.len()));

        let mut two = bytes.clone();
        two.extend_from_slice(&bytes);
        assert_eq!(frame_len(&two), Some(bytes.len()));
    }

    #[test]
    fn ui_attach_is_a_request() {
        let mut w = Writer::new();
        encode_ui_attach(&mut w, 7, 80, 24, Some("xterm-256color"), 256);
        let bytes = w.into_bytes();
        let mut r = Reader::new(&bytes);
        assert_eq!(r.read_array_len().unwrap(), 4);
        assert_eq!(r.read_u64().unwrap(), 0);
        assert_eq!(r.read_u64().unwrap(), 7);
        assert_eq!(r.read_str().unwrap(), "nvim_ui_attach");
        assert_eq!(r.read_array_len().unwrap(), 3);
        assert_eq!(r.read_u64().unwrap(), 80);
        assert_eq!(r.read_u64().unwrap(), 24);
        assert_eq!(r.read_map_len().unwrap(), 6);
        assert_eq!(r.read_str().unwrap(), "rgb");
        assert!(r.read_bool().unwrap());
        assert_eq!(r.read_str().unwrap(), "ext_linegrid");
        assert!(r.read_bool().unwrap());
        assert_eq!(r.read_str().unwrap(), "ext_termcolors");
        assert!(r.read_bool().unwrap());
        assert_eq!(r.read_str().unwrap(), "ext_multigrid");
        assert!(!r.read_bool().unwrap());
        assert_eq!(r.read_str().unwrap(), "term_name");
        assert_eq!(r.read_str().unwrap(), "xterm-256color");
        assert_eq!(r.read_str().unwrap(), "term_colors");
        assert_eq!(r.read_u64().unwrap(), 256);
        assert!(r.eof());
    }

    #[test]
    fn encode_set_option_value_bool_shape() {
        let mut w = Writer::new();
        encode_set_option_value_bool(&mut w, "termguicolors", true);
        let bytes = w.into_bytes();
        let mut r = Reader::new(&bytes);
        assert_eq!(r.read_array_len().unwrap(), 3);
        assert_eq!(r.read_u64().unwrap(), 2);
        assert_eq!(r.read_str().unwrap(), "nvim_set_option_value");
        assert_eq!(r.read_array_len().unwrap(), 3);
        assert_eq!(r.read_str().unwrap(), "termguicolors");
        assert!(r.read_bool().unwrap());
        assert_eq!(r.read_map_len().unwrap(), 0);
        assert!(r.eof());
    }

    #[test]
    fn encode_input_shape() {
        let mut w = Writer::new();
        encode_input(&mut w, "<C-c>");
        let bytes = w.into_bytes();
        let mut r = Reader::new(&bytes);
        assert_eq!(r.read_array_len().unwrap(), 3);
        assert_eq!(r.read_u64().unwrap(), 2);
        assert_eq!(r.read_str().unwrap(), "nvim_input");
        assert_eq!(r.read_array_len().unwrap(), 1);
        assert_eq!(r.read_str().unwrap(), "<C-c>");
    }
}
