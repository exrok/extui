//! Byte classification lookup shared by scanners.

/// Set on ASCII whitespace: `0x20`, `\t`, `\r`, `\n`, form feed, vertical tab.
pub const WS: u8 = 1 << 0;
/// Set on `[A-Za-z_]` — valid first byte of an ASCII identifier.
pub const IDENT_START: u8 = 1 << 1;
/// Set on `[A-Za-z0-9_]` — valid continuation byte of an ASCII identifier.
pub const IDENT_CONT: u8 = 1 << 2;
/// Set on `[0-9]`.
pub const DIGIT: u8 = 1 << 3;
/// Set on `[0-9A-Fa-f]`.
pub const HEX_DIGIT: u8 = 1 << 4;

const fn build_table() -> [u8; 256] {
    let mut table = [0u8; 256];
    let mut i = 0;
    while i < 256 {
        let b = i as u8;
        let mut bits = 0u8;

        if b == b' ' || b == b'\t' || b == b'\n' || b == b'\r' || b == 0x0B || b == 0x0C {
            bits |= WS;
        }

        let is_alpha = (b >= b'a' && b <= b'z') || (b >= b'A' && b <= b'Z');
        let is_digit = b >= b'0' && b <= b'9';
        let is_hex_letter = (b >= b'a' && b <= b'f') || (b >= b'A' && b <= b'F');

        if is_alpha || b == b'_' {
            bits |= IDENT_START;
        }
        if is_alpha || is_digit || b == b'_' {
            bits |= IDENT_CONT;
        }
        if is_digit {
            bits |= DIGIT | HEX_DIGIT;
        }
        if is_hex_letter {
            bits |= HEX_DIGIT;
        }

        table[i] = bits;
        i += 1;
    }
    table
}

/// The classification table.
pub const BYTE_CLASS: [u8; 256] = build_table();

/// Returns `true` if `b` belongs to every class set in `mask`.
#[inline]
pub const fn has(b: u8, mask: u8) -> bool {
    (BYTE_CLASS[b as usize] & mask) == mask
}

/// Returns `true` if `b` is ASCII whitespace.
#[inline]
pub const fn is_whitespace(b: u8) -> bool {
    has(b, WS)
}

/// Returns `true` if `b` can start an ASCII identifier.
#[inline]
pub const fn is_ident_start(b: u8) -> bool {
    has(b, IDENT_START)
}

/// Returns `true` if `b` can continue an ASCII identifier.
#[inline]
pub const fn is_ident_cont(b: u8) -> bool {
    has(b, IDENT_CONT)
}

/// Returns `true` if `b` is an ASCII decimal digit.
#[inline]
pub const fn is_digit(b: u8) -> bool {
    has(b, DIGIT)
}

/// Returns `true` if `b` is an ASCII hex digit.
#[inline]
pub const fn is_hex_digit(b: u8) -> bool {
    has(b, HEX_DIGIT)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn whitespace() {
        for b in [b' ', b'\t', b'\n', b'\r', 0x0B, 0x0C] {
            assert!(is_whitespace(b), "expected {b} to be ws");
        }
        for b in [b'a', b'0', b'.', 0] {
            assert!(!is_whitespace(b), "expected {b} to not be ws");
        }
    }

    #[test]
    fn ident_classes() {
        assert!(is_ident_start(b'_'));
        assert!(is_ident_start(b'A'));
        assert!(is_ident_start(b'z'));
        assert!(!is_ident_start(b'0'));
        assert!(is_ident_cont(b'0'));
        assert!(is_ident_cont(b'Z'));
        assert!(!is_ident_cont(b'-'));
    }

    #[test]
    fn digit_classes() {
        for d in b'0'..=b'9' {
            assert!(is_digit(d));
            assert!(is_hex_digit(d));
        }
        for d in b'a'..=b'f' {
            assert!(is_hex_digit(d));
            assert!(!is_digit(d));
        }
        for d in b'A'..=b'F' {
            assert!(is_hex_digit(d));
        }
        assert!(!is_hex_digit(b'g'));
    }
}
