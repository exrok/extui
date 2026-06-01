const TABLE: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

pub(crate) fn encoded_len(len: usize) -> usize {
    len.div_ceil(3) * 4
}

pub(crate) fn encode_to_buffer(input: &[u8], out: &mut Vec<u8>) {
    out.reserve(encoded_len(input.len()));

    let mut chunks = input.chunks_exact(3);
    for chunk in &mut chunks {
        let n = ((chunk[0] as u32) << 16) | ((chunk[1] as u32) << 8) | chunk[2] as u32;
        out.push(TABLE[((n >> 18) & 0x3f) as usize]);
        out.push(TABLE[((n >> 12) & 0x3f) as usize]);
        out.push(TABLE[((n >> 6) & 0x3f) as usize]);
        out.push(TABLE[(n & 0x3f) as usize]);
    }

    match chunks.remainder() {
        [a] => {
            let n = (*a as u32) << 16;
            out.push(TABLE[((n >> 18) & 0x3f) as usize]);
            out.push(TABLE[((n >> 12) & 0x3f) as usize]);
            out.extend_from_slice(b"==");
        }
        [a, b] => {
            let n = ((*a as u32) << 16) | ((*b as u32) << 8);
            out.push(TABLE[((n >> 18) & 0x3f) as usize]);
            out.push(TABLE[((n >> 12) & 0x3f) as usize]);
            out.push(TABLE[((n >> 6) & 0x3f) as usize]);
            out.push(b'=');
        }
        [] => {}
        _ => unreachable!(),
    }
}

pub(crate) fn decode(input: &[u8]) -> Result<Vec<u8>, ()> {
    let mut out = Vec::with_capacity(input.len() / 4 * 3);
    let mut quad = [0u8; 4];
    let mut len = 0usize;
    let mut padding = 0usize;
    let mut finished = false;

    for &byte in input {
        let Some(value) = decode_byte(byte)? else {
            continue;
        };
        if finished {
            return Err(());
        }
        if value == 64 {
            padding += 1;
        } else if padding != 0 {
            return Err(());
        }

        quad[len] = value;
        len += 1;

        if len == 4 {
            decode_quad(quad, padding, &mut out)?;
            finished = padding != 0;
            len = 0;
            padding = 0;
        }
    }

    match len {
        0 => Ok(out),
        1 => Err(()),
        2 => {
            if padding != 0 {
                return Err(());
            }
            out.push((quad[0] << 2) | (quad[1] >> 4));
            Ok(out)
        }
        3 => {
            if padding != 0 {
                return Err(());
            }
            out.push((quad[0] << 2) | (quad[1] >> 4));
            out.push((quad[1] << 4) | (quad[2] >> 2));
            Ok(out)
        }
        _ => unreachable!(),
    }
}

fn decode_quad(quad: [u8; 4], padding: usize, out: &mut Vec<u8>) -> Result<(), ()> {
    if padding > 2 {
        return Err(());
    }

    out.push((quad[0] << 2) | (quad[1] >> 4));
    if padding < 2 {
        out.push((quad[1] << 4) | (quad[2] >> 2));
    }
    if padding < 1 {
        out.push((quad[2] << 6) | quad[3]);
    }
    Ok(())
}

fn decode_byte(byte: u8) -> Result<Option<u8>, ()> {
    match byte {
        b'A'..=b'Z' => Ok(Some(byte - b'A')),
        b'a'..=b'z' => Ok(Some(byte - b'a' + 26)),
        b'0'..=b'9' => Ok(Some(byte - b'0' + 52)),
        b'+' => Ok(Some(62)),
        b'/' => Ok(Some(63)),
        b'=' => Ok(Some(64)),
        b' ' | b'\n' | b'\r' | b'\t' => Ok(None),
        _ => Err(()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encodes_padded_base64() {
        let mut out = Vec::new();
        encode_to_buffer(b"", &mut out);
        assert_eq!(out, b"");

        encode_to_buffer(b"f", &mut out);
        assert_eq!(out, b"Zg==");

        out.clear();
        encode_to_buffer(b"fo", &mut out);
        assert_eq!(out, b"Zm8=");

        out.clear();
        encode_to_buffer(b"foo", &mut out);
        assert_eq!(out, b"Zm9v");

        out.clear();
        encode_to_buffer(b"Hello World", &mut out);
        assert_eq!(out, b"SGVsbG8gV29ybGQ=");
    }

    #[test]
    fn decodes_padded_and_unpadded_base64() {
        assert_eq!(decode(b"").unwrap(), b"");
        assert_eq!(decode(b"Zg==").unwrap(), b"f");
        assert_eq!(decode(b"Zg").unwrap(), b"f");
        assert_eq!(decode(b"Zm8=").unwrap(), b"fo");
        assert_eq!(decode(b"Zm8").unwrap(), b"fo");
        assert_eq!(decode(b"Zm9v").unwrap(), b"foo");
        assert_eq!(decode(b"SGVsbG8gV29ybGQ=").unwrap(), b"Hello World");
    }

    #[test]
    fn rejects_invalid_base64() {
        assert!(decode(b"Z").is_err());
        assert!(decode(b"Z===").is_err());
        assert!(decode(b"Zg==x").is_err());
        assert!(decode(b"Zm$=").is_err());
    }
}
