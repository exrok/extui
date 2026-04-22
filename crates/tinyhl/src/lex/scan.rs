//! Scanners shared across language lexers.

use crate::SourceView;
use crate::lex::byteclass;

/// Result of a scan that can terminate either cleanly or via an error.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ScanResult {
    /// The new cursor after the scan.
    pub end: u32,
    /// Set when the scanner recovered past a malformed input.
    pub is_error: bool,
}

impl ScanResult {
    const fn ok(end: u32) -> Self {
        Self {
            end,
            is_error: false,
        }
    }

    const fn err(end: u32) -> Self {
        Self {
            end,
            is_error: true,
        }
    }
}

/// Advances past runs of ASCII whitespace starting at `cursor` and returns
/// the new cursor.
pub fn scan_whitespace(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let mut i = (cursor - base) as usize;
        while i < page.len() && byteclass::is_whitespace(page[i]) {
            i += 1;
        }
        let consumed = i - (cursor - base) as usize;
        cursor += consumed as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

/// Scans a run of ASCII decimal digits starting at `cursor`, returning the
/// new cursor. Returns `cursor` unchanged if the first byte is not a digit.
pub fn scan_digits(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let mut i = (cursor - base) as usize;
        while i < page.len() && byteclass::is_digit(page[i]) {
            i += 1;
        }
        let consumed = i - (cursor - base) as usize;
        cursor += consumed as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

/// Scans a JSON number starting at `cursor`.
///
/// Grammar: `-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?`. Returns the
/// cursor after the number. The caller must have verified the first byte
/// is `-` or an ASCII digit.
pub fn scan_json_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor;

    if view.byte_at(end) == Some(b'-') {
        end += 1;
    }

    let int_start = end;
    match view.byte_at(end) {
        Some(b'0') => {
            end += 1;
        }
        Some(b) if byteclass::is_digit(b) => {
            end = scan_digits(view, end);
        }
        _ => return ScanResult::err(end),
    }
    if end == int_start {
        return ScanResult::err(end);
    }

    if view.byte_at(end) == Some(b'.') {
        end += 1;
        let after_dot = end;
        end = scan_digits(view, end);
        if end == after_dot {
            return ScanResult::err(end);
        }
    }

    match view.byte_at(end) {
        Some(b'e') | Some(b'E') => {
            end += 1;
            match view.byte_at(end) {
                Some(b'+') | Some(b'-') => end += 1,
                _ => {}
            }
            let after_exp = end;
            end = scan_digits(view, end);
            if end == after_exp {
                return ScanResult::err(end);
            }
        }
        _ => {}
    }

    ScanResult::ok(end)
}

/// Scans a double-quoted JSON string starting at `cursor`, which must point
/// at the opening `"`.
///
/// Recognizes `\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, `\t`, and `\uXXXX`
/// escape sequences. Stops at the closing `"` (included in the returned
/// span). Recovers on unescaped control bytes and unterminated strings by
/// ending at the offending byte with `is_error` set.
pub fn scan_json_string(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut cursor = cursor + 1;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return ScanResult::err(cursor);
        }
        let rel = (cursor - base) as usize;
        let mut i = rel;
        while i < page.len() {
            let b = page[i];
            if b == b'"' {
                return ScanResult::ok(base + i as u32 + 1);
            }
            if b == b'\\' {
                if i + 1 >= page.len() {
                    cursor = base + i as u32;
                    break;
                }
                let esc = page[i + 1];
                match esc {
                    b'"' | b'\\' | b'/' | b'b' | b'f' | b'n' | b'r' | b't' => {
                        i += 2;
                        continue;
                    }
                    b'u' => {
                        if i + 6 > page.len() {
                            cursor = base + i as u32;
                            break;
                        }
                        let hex = &page[i + 2..i + 6];
                        if !hex.iter().all(|&x| byteclass::is_hex_digit(x)) {
                            return ScanResult::err(base + i as u32 + 2);
                        }
                        i += 6;
                        continue;
                    }
                    _ => return ScanResult::err(base + i as u32 + 2),
                }
            }
            if b < 0x20 {
                return ScanResult::err(base + i as u32);
            }
            i += 1;
        }
        if i == rel {
            return ScanResult::err(cursor);
        }
        cursor = base + i as u32;
    }
}

/// Scans a double-quoted C-style string starting at `cursor`, which must
/// point at the opening `"`.
///
/// Treats `\` followed by `"` or `\` as a two-byte escape; any other byte
/// after a `\` is consumed normally on the next iteration. Stops at the
/// closing `"` (included in the returned span) or at end-of-source with
/// `is_error` set. Does not consume any literal suffix — that's the caller's
/// concern.
pub fn scan_c_string(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    scan_c_quoted(view, cursor, b'"')
}

/// Scans a single-quoted C-style character literal starting at `cursor`,
/// which must point at the opening `'`.
///
/// Escape semantics are identical to [`scan_c_string`]: `\` followed by `'`
/// or `\` is a two-byte escape, any other byte after `\` is taken literally
/// on the next iteration. Stops at the closing `'` or errors on
/// end-of-source. Length validation (e.g. multi-byte constants) is not
/// enforced here.
pub fn scan_c_char(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    scan_c_quoted(view, cursor, b'\'')
}

fn scan_c_quoted(view: &mut SourceView<'_>, cursor: u32, quote: u8) -> ScanResult {
    let mut cursor = cursor + 1;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return ScanResult::err(cursor);
        }
        let rel = (cursor - base) as usize;
        let mut i = rel;
        let mut crossed_page_on_escape = false;
        while i < page.len() {
            let b = page[i];
            if b == quote {
                return ScanResult::ok(base + i as u32 + 1);
            }
            if b == b'\\' {
                if i + 1 < page.len() {
                    let esc = page[i + 1];
                    if esc == quote || esc == b'\\' {
                        i += 2;
                    } else {
                        i += 1;
                    }
                    continue;
                }
                // Backslash at end of page: advance past it and handle the
                // escape target across the page boundary.
                i += 1;
                crossed_page_on_escape = true;
                break;
            }
            i += 1;
        }
        cursor = base + i as u32;
        if crossed_page_on_escape {
            match view.byte_at(cursor) {
                None => return ScanResult::err(cursor),
                Some(b) if b == quote || b == b'\\' => cursor += 1,
                Some(_) => {}
            }
            continue;
        }
        if i == rel {
            return ScanResult::err(cursor);
        }
    }
}

/// Consumes a `//` line comment starting at `cursor` (which must point at
/// the leading `/`). Returns the cursor at the terminating `\n` or at
/// end-of-source. The newline itself is not consumed.
pub fn scan_line_comment(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut cursor = cursor + 2;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let rel = (cursor - base) as usize;
        let mut i = rel;
        while i < page.len() && page[i] != b'\n' {
            i += 1;
        }
        cursor = base + i as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

/// Scans a `/* ... */` block comment starting at `cursor` (which must point
/// at the leading `/`). Returns the cursor just past the closing `*/`, or an
/// error if end-of-source is reached first. C block comments don't nest.
pub fn scan_block_comment(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut cursor = cursor + 2;
    let mut saw_star = false;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return ScanResult::err(cursor);
        }
        let rel = (cursor - base) as usize;
        let mut i = rel;
        while i < page.len() {
            let b = page[i];
            if saw_star && b == b'/' {
                return ScanResult::ok(base + i as u32 + 1);
            }
            saw_star = b == b'*';
            i += 1;
        }
        cursor = base + i as u32;
    }
}

/// Scans a C numeric literal starting at `cursor`.
///
/// Recognises integer literals in hexadecimal (`0x`/`0X`), binary (`0b`/`0B`,
/// a GCC/C23 extension widely accepted in practice), and decimal/octal
/// (leading `0`) radices; decimal and hexadecimal floats (`0x1.fp3`); and
/// any trailing `u`/`U`/`l`/`L`/`f`/`F` suffix combination. Suffix ordering
/// is accepted leniently — final validation is the parser's job.
///
/// The caller must dispatch here only when the first byte is an ASCII digit,
/// or a `.` followed by an ASCII digit (for `.5`-style decimals).
pub fn scan_c_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    if view.byte_at(cursor) == Some(b'0') {
        match view.byte_at(cursor + 1) {
            Some(b'x') | Some(b'X') => return scan_c_hex_number(view, cursor + 2),
            Some(b'b') | Some(b'B') => return scan_c_binary_number(view, cursor + 2),
            _ => {}
        }
    }
    scan_c_decimal_number(view, cursor)
}

fn scan_c_hex_number(view: &mut SourceView<'_>, after_prefix: u32) -> ScanResult {
    let end_int = scan_hex_digits(view, after_prefix);
    let had_int_digits = end_int > after_prefix;

    let (body_end, saw_dot, had_frac_digits) = if view.byte_at(end_int) == Some(b'.') {
        let frac_start = end_int + 1;
        let end_frac = scan_hex_digits(view, frac_start);
        (end_frac, true, end_frac > frac_start)
    } else {
        (end_int, false, false)
    };

    if !had_int_digits && !had_frac_digits {
        return ScanResult::err(body_end);
    }

    match view.byte_at(body_end) {
        Some(b'p') | Some(b'P') => {
            let mut end = body_end + 1;
            if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
                end += 1;
            }
            let after_exp = end;
            end = scan_digits(view, end);
            if end == after_exp {
                return ScanResult::err(end);
            }
            ScanResult::ok(scan_float_suffix(view, end))
        }
        _ => {
            if saw_dot {
                return ScanResult::err(body_end);
            }
            ScanResult::ok(scan_int_suffix(view, body_end))
        }
    }
}

fn scan_c_binary_number(view: &mut SourceView<'_>, after_prefix: u32) -> ScanResult {
    let end_digits = scan_binary_digits(view, after_prefix);
    if end_digits == after_prefix {
        return ScanResult::err(end_digits);
    }
    ScanResult::ok(scan_int_suffix(view, end_digits))
}

fn scan_c_decimal_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = scan_digits(view, cursor);
    let had_int_digits = end > cursor;
    let mut is_float = false;
    let mut had_frac_digits = false;

    if view.byte_at(end) == Some(b'.') {
        end += 1;
        is_float = true;
        let before_frac = end;
        end = scan_digits(view, end);
        had_frac_digits = end > before_frac;
    }

    if !had_int_digits && !had_frac_digits {
        return ScanResult::err(end);
    }

    if matches!(view.byte_at(end), Some(b'e') | Some(b'E')) {
        end += 1;
        is_float = true;
        if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
            end += 1;
        }
        let after_exp = end;
        end = scan_digits(view, end);
        if end == after_exp {
            return ScanResult::err(end);
        }
    }

    end = if is_float {
        scan_float_suffix(view, end)
    } else {
        scan_int_suffix(view, end)
    };
    ScanResult::ok(end)
}

fn scan_hex_digits(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let mut i = (cursor - base) as usize;
        while i < page.len() && byteclass::is_hex_digit(page[i]) {
            i += 1;
        }
        let consumed = i - (cursor - base) as usize;
        cursor += consumed as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

fn scan_binary_digits(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let mut i = (cursor - base) as usize;
        while i < page.len() && (page[i] == b'0' || page[i] == b'1') {
            i += 1;
        }
        let consumed = i - (cursor - base) as usize;
        cursor += consumed as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

fn scan_int_suffix(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut end = cursor;
    let mut u_count = 0u8;
    let mut l_count = 0u8;
    for _ in 0..4 {
        match view.byte_at(end) {
            Some(b'u') | Some(b'U') if u_count == 0 => {
                u_count = 1;
                end += 1;
            }
            Some(b'l') | Some(b'L') if l_count < 2 => {
                l_count += 1;
                end += 1;
            }
            _ => break,
        }
    }
    end
}

fn scan_float_suffix(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    match view.byte_at(cursor) {
        Some(b'f') | Some(b'F') | Some(b'l') | Some(b'L') => cursor + 1,
        _ => cursor,
    }
}

/// Scans an ASCII identifier run starting at `cursor`. Assumes the first
/// byte is [`byteclass::is_ident_start`]. Returns the cursor after the run.
pub fn scan_ident_ascii(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let mut i = (cursor - base) as usize;
        while i < page.len() && byteclass::is_ident_cont(page[i]) {
            i += 1;
        }
        let consumed = i - (cursor - base) as usize;
        cursor += consumed as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

/// Fills `buf` with consecutive bytes from `cursor`, refreshing the page as
/// needed. Returns `true` on success; returns `false` the moment
/// end-of-source is reached before `buf` is fully populated, in which case
/// the partial contents of `buf` are unspecified.
pub fn copy_bytes(view: &mut SourceView<'_>, cursor: u32, buf: &mut [u8]) -> bool {
    for (i, slot) in buf.iter_mut().enumerate() {
        let Some(b) = view.byte_at(cursor + i as u32) else {
            return false;
        };
        *slot = b;
    }
    true
}

/// Decodes a single UTF-8 code point starting at `cursor`.
///
/// Returns `Some((char, byte_len))` on success, `None` at end-of-source or
/// when the bytes at `cursor` are not a valid UTF-8 sequence (including
/// overlong encodings and surrogate pairs, which `core::str::from_utf8`
/// rejects).
pub fn decode_char_at(view: &mut SourceView<'_>, cursor: u32) -> Option<(char, u32)> {
    let b0 = view.byte_at(cursor)?;
    if b0 < 0x80 {
        return Some((b0 as char, 1));
    }
    let len = match b0 {
        0xC2..=0xDF => 2u32,
        0xE0..=0xEF => 3,
        0xF0..=0xF4 => 4,
        _ => return None,
    };
    let mut bytes = [0u8; 4];
    for i in 0..len {
        bytes[i as usize] = view.byte_at(cursor + i)?;
    }
    let s = core::str::from_utf8(&bytes[..len as usize]).ok()?;
    Some((s.chars().next()?, len))
}

/// Scans a Unicode XID identifier starting at `cursor`.
///
/// Returns `cursor` unchanged if the bytes at `cursor` do not form a
/// valid identifier start. Otherwise returns the cursor after the last
/// consumed byte.
///
/// A valid start is one of:
///
/// - an ASCII byte satisfying [`byteclass::is_ident_start`] (`[A-Za-z_]`)
/// - the byte `$`, when `allow_dollar` is `true`
/// - a non-ASCII UTF-8 character accepted by [`unicode_ident::is_xid_start`]
///
/// Continuation bytes are either ASCII `[A-Za-z0-9_]` (plus `$` when
/// `allow_dollar` is set) or non-ASCII points accepted by
/// [`unicode_ident::is_xid_continue`].
pub fn scan_xid_ident(view: &mut SourceView<'_>, cursor: u32, allow_dollar: bool) -> u32 {
    let Some(b0) = view.byte_at(cursor) else {
        return cursor;
    };
    let start_len = if b0 < 0x80 {
        if byteclass::is_ident_start(b0) || (allow_dollar && b0 == b'$') {
            1
        } else {
            return cursor;
        }
    } else {
        let Some((c, len)) = decode_char_at(view, cursor) else {
            return cursor;
        };
        if !unicode_ident::is_xid_start(c) {
            return cursor;
        }
        len
    };

    let mut end = cursor + start_len;
    loop {
        let (base, page) = view.window_at(end);
        if page.is_empty() {
            return end;
        }
        let rel_start = (end - base) as usize;
        let mut i = rel_start;
        while i < page.len() {
            let b = page[i];
            if b >= 0x80 {
                break;
            }
            if byteclass::is_ident_cont(b) || (allow_dollar && b == b'$') {
                i += 1;
            } else {
                return base + i as u32;
            }
        }
        end = base + i as u32;
        if i == page.len() {
            continue;
        }
        let Some((c, len)) = decode_char_at(view, end) else {
            return end;
        };
        if !unicode_ident::is_xid_continue(c) {
            return end;
        }
        end += len;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Source;

    fn view<'a>(src: &'a &'a str) -> SourceView<'a> {
        let s: &'a dyn Source = src;
        SourceView::new(s, 0)
    }

    #[test]
    fn whitespace_run() {
        let s = "   hi";
        let mut v = view(&s);
        assert_eq!(scan_whitespace(&mut v, 0), 3);
    }

    #[test]
    fn digits_run() {
        let s = "12345abc";
        let mut v = view(&s);
        assert_eq!(scan_digits(&mut v, 0), 5);
    }

    #[test]
    fn json_numbers() {
        for (input, expected) in [
            ("0", 1),
            ("-0", 2),
            ("42", 2),
            ("-123", 4),
            ("1.5", 3),
            ("1.5e10", 6),
            ("1.5E-10", 7),
            ("0.0", 3),
        ] {
            let mut v = view(&input);
            let r = scan_json_number(&mut v, 0);
            assert!(!r.is_error, "unexpected error for {input:?}");
            assert_eq!(r.end, expected as u32, "wrong end for {input:?}");
        }
    }

    #[test]
    fn json_numbers_errors() {
        for input in ["-", "1.", "1e", "1e+"] {
            let mut v = view(&input);
            let r = scan_json_number(&mut v, 0);
            assert!(r.is_error, "expected error for {input:?}");
        }
    }

    #[test]
    fn json_strings() {
        for (input, expected) in [
            (r#""""#, 2),
            (r#""abc""#, 5),
            (r#""a\"b""#, 6),
            (r#""\u00AB""#, 8),
            (r#""\n""#, 4),
        ] {
            let mut v = view(&input);
            let r = scan_json_string(&mut v, 0);
            assert!(!r.is_error, "unexpected error for {input:?}");
            assert_eq!(r.end, expected as u32, "wrong end for {input:?}");
        }
    }

    #[test]
    fn json_strings_errors() {
        for input in [r#""abc"#, "\"a\nb\"", r#""\x""#] {
            let mut v = view(&input);
            let r = scan_json_string(&mut v, 0);
            assert!(r.is_error, "expected error for {input:?}");
        }
    }

    #[test]
    fn ident_ascii() {
        let s = "foo_bar123!";
        let mut v = view(&s);
        assert_eq!(scan_ident_ascii(&mut v, 0), 10);
    }

    #[test]
    fn c_string_basics() {
        for (input, expected) in [
            (r#""""#, 2),
            (r#""abc""#, 5),
            (r#""a\"b""#, 6),
            (r#""\\""#, 4),
            ("\"line\nbreak\"", 12),
        ] {
            let mut v = view(&input);
            let r = scan_c_string(&mut v, 0);
            assert!(!r.is_error, "unexpected error for {input:?}");
            assert_eq!(r.end, expected as u32, "wrong end for {input:?}");
        }
    }

    #[test]
    fn c_string_unterminated() {
        for input in [r#""abc"#, r#""a\"#] {
            let mut v = view(&input);
            let r = scan_c_string(&mut v, 0);
            assert!(r.is_error, "expected error for {input:?}");
        }
    }

    struct Paged<'a> {
        pages: &'a [&'a [u8]],
        total: u32,
    }

    impl<'a> Paged<'a> {
        fn new(pages: &'a [&'a [u8]]) -> Self {
            let mut total = 0u32;
            for p in pages {
                total += p.len() as u32;
            }
            Self { pages, total }
        }
    }

    impl Source for Paged<'_> {
        fn len(&self) -> u32 {
            self.total
        }

        fn page(&self, offset: u32) -> (u32, &[u8]) {
            let mut base = 0u32;
            for p in self.pages {
                let end = base + p.len() as u32;
                if offset < end {
                    return (base, p);
                }
                base = end;
            }
            (self.total, &[])
        }
    }

    #[test]
    fn c_string_handles_backslash_at_page_boundary() {
        // Page splits the escape pair `\"`: `\` ends page 1, `"` starts page 2.
        let p = Paged::new(&[b"\"abc\\", b"\"def\""]);
        let src: &dyn Source = &p;
        let mut v = SourceView::new(src, 0);
        let r = scan_c_string(&mut v, 0);
        assert!(!r.is_error, "escape at page boundary should not error");
        assert_eq!(r.end, p.len(), "should consume closing quote on page 2");

        // Same split, but next page starts with `\\`.
        let p = Paged::new(&[b"\"abc\\", b"\\def\""]);
        let src: &dyn Source = &p;
        let mut v = SourceView::new(src, 0);
        let r = scan_c_string(&mut v, 0);
        assert!(!r.is_error, "`\\\\` across page boundary should not error");
        assert_eq!(r.end, p.len());

        // Split where the byte after the tail `\` is not a special escape.
        let p = Paged::new(&[b"\"abc\\", b"ndef\""]);
        let src: &dyn Source = &p;
        let mut v = SourceView::new(src, 0);
        let r = scan_c_string(&mut v, 0);
        assert!(!r.is_error);
        assert_eq!(r.end, p.len());
    }

    #[test]
    fn c_string_spans_newlines() {
        let s = "\"line 1\nline 2\nline 3\"";
        let mut v = view(&s);
        let r = scan_c_string(&mut v, 0);
        assert!(!r.is_error);
        assert_eq!(r.end as usize, s.len());
    }

    #[test]
    fn c_char_basics() {
        for (input, expected) in [
            (r#"'x'"#, 3),
            (r#"'\''"#, 4),
            (r#"'\\'"#, 4),
            (r#"'\n'"#, 4),
            (r#"'\x41'"#, 6),
            (r#"'ab'"#, 4),
        ] {
            let mut v = view(&input);
            let r = scan_c_char(&mut v, 0);
            assert!(!r.is_error, "unexpected error for {input:?}");
            assert_eq!(r.end, expected as u32, "wrong end for {input:?}");
        }
    }

    #[test]
    fn c_char_unterminated() {
        for input in [r#"'x"#, r#"'\"#] {
            let mut v = view(&input);
            let r = scan_c_char(&mut v, 0);
            assert!(r.is_error, "expected error for {input:?}");
        }
    }

    #[test]
    fn line_comment_stops_at_newline() {
        let s = "// hello\nafter";
        let mut v = view(&s);
        let end = scan_line_comment(&mut v, 0);
        assert_eq!(end, 8);
        assert_eq!(&s.as_bytes()[8..9], b"\n");
    }

    #[test]
    fn line_comment_runs_to_eof() {
        let s = "// trailing no newline";
        let mut v = view(&s);
        let end = scan_line_comment(&mut v, 0);
        assert_eq!(end as usize, s.len());
    }

    #[test]
    fn block_comment_basics() {
        for (input, expected) in [
            ("/**/", 4),
            ("/* hi */", 8),
            ("/* line 1\nline 2 */", 19),
            ("/***/", 5),
            ("/*/*/", 5),
        ] {
            let mut v = view(&input);
            let r = scan_block_comment(&mut v, 0);
            assert!(!r.is_error, "unexpected error for {input:?}");
            assert_eq!(r.end, expected as u32, "wrong end for {input:?}");
        }
    }

    #[test]
    fn block_comment_unterminated() {
        for input in ["/*", "/* never closed", "/* almost *"] {
            let mut v = view(&input);
            let r = scan_block_comment(&mut v, 0);
            assert!(r.is_error, "expected error for {input:?}");
        }
    }

    #[test]
    fn decode_char_ascii() {
        let s = "A";
        let mut v = view(&s);
        assert_eq!(decode_char_at(&mut v, 0), Some(('A', 1)));
    }

    #[test]
    fn decode_char_multibyte() {
        let s = "αβ漢🎉";
        let mut v = view(&s);
        let (c, len) = decode_char_at(&mut v, 0).unwrap();
        assert_eq!(c, 'α');
        assert_eq!(len, 2);
        let (c, len) = decode_char_at(&mut v, 2).unwrap();
        assert_eq!(c, 'β');
        assert_eq!(len, 2);
        let (c, len) = decode_char_at(&mut v, 4).unwrap();
        assert_eq!(c, '漢');
        assert_eq!(len, 3);
        let (c, len) = decode_char_at(&mut v, 7).unwrap();
        assert_eq!(c, '🎉');
        assert_eq!(len, 4);
    }

    #[test]
    fn decode_char_invalid_bytes() {
        // Stray continuation byte without a valid leader.
        let bytes: &[u8] = &[0x80, 0x41];
        let src: &dyn Source = &bytes;
        let mut v = SourceView::new(src, 0);
        assert_eq!(decode_char_at(&mut v, 0), None);
    }

    #[test]
    fn xid_ident_ascii() {
        let s = "foo_bar123!";
        let mut v = view(&s);
        assert_eq!(scan_xid_ident(&mut v, 0, false), 10);
    }

    #[test]
    fn xid_ident_dollar_opt_in() {
        let s = "$foo";
        let mut v = view(&s);
        assert_eq!(
            scan_xid_ident(&mut v, 0, false),
            0,
            "$ is not a start without allow_dollar"
        );
        let mut v = view(&s);
        assert_eq!(scan_xid_ident(&mut v, 0, true), 4);
    }

    #[test]
    fn xid_ident_unicode() {
        let s = "αβγ_42!";
        let mut v = view(&s);
        let end = scan_xid_ident(&mut v, 0, false);
        assert_eq!(end as usize, "αβγ_42".len());
    }

    #[test]
    fn xid_ident_rejects_non_start() {
        // `§` is category Po — not XID_Start.
        let s = "§";
        let mut v = view(&s);
        assert_eq!(scan_xid_ident(&mut v, 0, false), 0);
    }

    #[test]
    fn xid_ident_stops_at_non_cont_unicode() {
        // `a` then `§` — scanner should stop before `§`.
        let s = "a§";
        let mut v = view(&s);
        let end = scan_xid_ident(&mut v, 0, false);
        assert_eq!(end, 1);
    }

    #[test]
    fn xid_ident_across_pages() {
        let p = Paged::new(&[b"foo", b"bar", b"!baz"]);
        let src: &dyn Source = &p;
        let mut v = SourceView::new(src, 0);
        assert_eq!(scan_xid_ident(&mut v, 0, false), 6);
    }

    #[test]
    fn copy_bytes_fills_buffer() {
        let s = "hello";
        let mut v = view(&s);
        let mut buf = [0u8; 5];
        assert!(copy_bytes(&mut v, 0, &mut buf));
        assert_eq!(&buf, b"hello");
    }

    #[test]
    fn copy_bytes_returns_false_past_eof() {
        let s = "hi";
        let mut v = view(&s);
        let mut buf = [0u8; 5];
        assert!(!copy_bytes(&mut v, 0, &mut buf));
    }

    #[test]
    fn block_comment_close_across_pages() {
        // The `*/` is split across pages: `*` at end of page 1, `/` on page 2.
        let p = Paged::new(&[b"/* abc *", b"/ trail"]);
        let src: &dyn Source = &p;
        let mut v = SourceView::new(src, 0);
        let r = scan_block_comment(&mut v, 0);
        assert!(!r.is_error);
        assert_eq!(r.end, 9);
    }

    #[test]
    fn c_number_ints() {
        for (input, expected) in [
            ("0", 1),
            ("42", 2),
            ("123ul", 5),
            ("0ULL", 4),
            ("0777", 4),
            ("0xFF", 4),
            ("0xDEADBEEFull", 13),
            ("0b1010", 6),
            ("0B1101LL", 8),
        ] {
            let mut v = view(&input);
            let r = scan_c_number(&mut v, 0);
            assert!(!r.is_error, "unexpected error for {input:?}");
            assert_eq!(r.end, expected as u32, "wrong end for {input:?}");
        }
    }

    #[test]
    fn c_number_floats() {
        for (input, expected) in [
            ("1.0", 3),
            ("1.", 2),
            (".5", 2),
            ("1e10", 4),
            ("1.5e-3", 6),
            ("1.0f", 4),
            ("2.5L", 4),
            ("0x1p3", 5),
            ("0x1.fp3", 7),
            ("0x.5p-2", 7),
            ("0x1.FFp+10f", 11),
        ] {
            let mut v = view(&input);
            let r = scan_c_number(&mut v, 0);
            assert!(!r.is_error, "unexpected error for {input:?}");
            assert_eq!(r.end, expected as u32, "wrong end for {input:?}");
        }
    }

    #[test]
    fn c_number_errors() {
        for input in ["0x", "0b", "0xG", "0x1.5", "1e", "1e+", "0b2"] {
            let mut v = view(&input);
            let r = scan_c_number(&mut v, 0);
            assert!(r.is_error, "expected error for {input:?}");
        }
    }
}
