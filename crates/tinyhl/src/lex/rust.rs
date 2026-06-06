//! Rust lexer.

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, Source, SourceView, Span};

const MAX_META_WORD_LEN: usize = 11; // "macro_rules"

pub(crate) const META_WORD_MASK: u16 = 0x00FF;
pub(crate) const META_READY: u16 = 1 << 14;
pub(crate) const META_UPPER: u16 = 1 << 15;

pub(crate) const WORD_SELF_UPPER: u16 = 1;
pub(crate) const WORD_SELF: u16 = 2;
pub(crate) const WORD_SUPER: u16 = 3;
pub(crate) const WORD_CRATE: u16 = 4;
pub(crate) const WORD_LET: u16 = 5;
pub(crate) const WORD_FOR: u16 = 6;
pub(crate) const WORD_FN: u16 = 7;
pub(crate) const WORD_STRUCT: u16 = 8;
pub(crate) const WORD_ENUM: u16 = 9;
pub(crate) const WORD_TRAIT: u16 = 10;
pub(crate) const WORD_TYPE: u16 = 11;
pub(crate) const WORD_UNION: u16 = 12;
pub(crate) const WORD_CONST: u16 = 13;
pub(crate) const WORD_STATIC: u16 = 14;
pub(crate) const WORD_AS: u16 = 15;
pub(crate) const WORD_IMPL: u16 = 16;
pub(crate) const WORD_DYN: u16 = 17;
pub(crate) const WORD_RETURN: u16 = 18;
pub(crate) const WORD_BREAK: u16 = 19;
pub(crate) const WORD_IN: u16 = 20;
pub(crate) const WORD_MATCH: u16 = 21;
pub(crate) const WORD_IF: u16 = 22;
pub(crate) const WORD_WHILE: u16 = 23;
pub(crate) const WORD_MUT: u16 = 24;
pub(crate) const WORD_UNSAFE: u16 = 25;
pub(crate) const WORD_EXTERN: u16 = 26;
pub(crate) const WORD_MACRO_RULES: u16 = 27;
pub(crate) const WORD_U8: u16 = 28;
pub(crate) const WORD_U16: u16 = 29;
pub(crate) const WORD_U32: u16 = 30;
pub(crate) const WORD_U64: u16 = 31;
pub(crate) const WORD_U128: u16 = 32;
pub(crate) const WORD_USIZE: u16 = 33;
pub(crate) const WORD_I8: u16 = 34;
pub(crate) const WORD_I16: u16 = 35;
pub(crate) const WORD_I32: u16 = 36;
pub(crate) const WORD_I64: u16 = 37;
pub(crate) const WORD_I128: u16 = 38;
pub(crate) const WORD_ISIZE: u16 = 39;
pub(crate) const WORD_F16: u16 = 40;
pub(crate) const WORD_F32: u16 = 41;
pub(crate) const WORD_F64: u16 = 42;
pub(crate) const WORD_F128: u16 = 43;
pub(crate) const WORD_BOOL: u16 = 44;
pub(crate) const WORD_CHAR: u16 = 45;
pub(crate) const WORD_STR: u16 = 46;

#[inline]
pub(crate) fn word_id(bytes: &[u8]) -> u16 {
    word_info(bytes).0
}

fn word_info(bytes: &[u8]) -> (u16, bool) {
    match bytes {
        b"Self" => (WORD_SELF_UPPER, true),
        b"self" => (WORD_SELF, true),
        b"super" => (WORD_SUPER, true),
        b"crate" => (WORD_CRATE, true),
        b"let" => (WORD_LET, true),
        b"for" => (WORD_FOR, true),
        b"fn" => (WORD_FN, true),
        b"struct" => (WORD_STRUCT, true),
        b"enum" => (WORD_ENUM, true),
        b"trait" => (WORD_TRAIT, true),
        b"type" => (WORD_TYPE, true),
        b"union" => (WORD_UNION, true),
        b"const" => (WORD_CONST, true),
        b"static" => (WORD_STATIC, true),
        b"as" => (WORD_AS, true),
        b"impl" => (WORD_IMPL, true),
        b"dyn" => (WORD_DYN, true),
        b"return" => (WORD_RETURN, true),
        b"break" => (WORD_BREAK, true),
        b"in" => (WORD_IN, true),
        b"match" => (WORD_MATCH, true),
        b"if" => (WORD_IF, true),
        b"while" => (WORD_WHILE, true),
        b"mut" => (WORD_MUT, true),
        b"unsafe" => (WORD_UNSAFE, true),
        b"extern" => (WORD_EXTERN, true),
        b"abstract" | b"async" | b"await" | b"become" | b"box" | b"continue" | b"do" | b"else"
        | b"false" | b"final" | b"loop" | b"macro" | b"mod" | b"move" | b"override" | b"priv"
        | b"pub" | b"ref" | b"true" | b"try" | b"typeof" | b"unsized" | b"use" | b"virtual"
        | b"where" | b"yield" => (0, true),
        b"macro_rules" => (WORD_MACRO_RULES, false),
        b"u8" => (WORD_U8, false),
        b"u16" => (WORD_U16, false),
        b"u32" => (WORD_U32, false),
        b"u64" => (WORD_U64, false),
        b"u128" => (WORD_U128, false),
        b"usize" => (WORD_USIZE, false),
        b"i8" => (WORD_I8, false),
        b"i16" => (WORD_I16, false),
        b"i32" => (WORD_I32, false),
        b"i64" => (WORD_I64, false),
        b"i128" => (WORD_I128, false),
        b"isize" => (WORD_ISIZE, false),
        b"f16" => (WORD_F16, false),
        b"f32" => (WORD_F32, false),
        b"f64" => (WORD_F64, false),
        b"f128" => (WORD_F128, false),
        b"bool" => (WORD_BOOL, false),
        b"char" => (WORD_CHAR, false),
        b"str" => (WORD_STR, false),
        _ => (0, false),
    }
}

#[inline]
pub(crate) fn meta_word(meta: u16) -> u16 {
    meta & META_WORD_MASK
}

#[inline]
pub(crate) fn meta_ready(meta: u16) -> bool {
    (meta & META_READY) != 0
}

#[inline]
pub(crate) fn meta_upper(meta: u16) -> bool {
    (meta & META_UPPER) != 0
}

#[inline]
pub(crate) fn word_is_primitive(word: u16) -> bool {
    matches!(
        word,
        WORD_U8
            | WORD_U16
            | WORD_U32
            | WORD_U64
            | WORD_U128
            | WORD_USIZE
            | WORD_I8
            | WORD_I16
            | WORD_I32
            | WORD_I64
            | WORD_I128
            | WORD_ISIZE
            | WORD_F16
            | WORD_F32
            | WORD_F64
            | WORD_F128
            | WORD_BOOL
            | WORD_CHAR
            | WORD_STR
    )
}

#[inline]
fn scan_rust_ident(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let Some(first) = view.byte_at(cursor) else {
        return cursor;
    };
    if first >= 0x80 {
        return scan::scan_xid_ident(view, cursor, false);
    }
    if !byteclass::is_ident_start(first) {
        return cursor;
    }

    let mut end = cursor + 1;
    loop {
        let (base, page) = view.window_at(end);
        if page.is_empty() {
            return end;
        }
        let mut i = (end - base) as usize;
        while i < page.len() {
            let b = page[i];
            if b >= 0x80 {
                return scan::scan_xid_ident(view, cursor, false);
            }
            if !byteclass::is_ident_cont(b) {
                return base + i as u32;
            }
            i += 1;
        }
        end = base + i as u32;
    }
}

pub(crate) struct Rust;

pub(crate) struct FlatLexer<'s> {
    view: SourceView<'s>,
    cursor: u32,
}

impl<'s> FlatLexer<'s> {
    pub(crate) fn new(src: &'s dyn Source) -> Self {
        Self {
            view: SourceView::new(src, 0),
            cursor: 0,
        }
    }

    #[inline]
    pub(crate) fn next_parts(&mut self) -> Option<(Span, u16, u16)> {
        let b = self.view.byte_at(self.cursor)?;
        let start = self.cursor;
        let (local_kind, end, _is_error, meta) = classify(&mut self.view, self.cursor, b);
        debug_assert!(end > start, "lexer must consume at least one byte");
        self.cursor = end;
        Some((Span::new(start, end - start), local_kind, meta))
    }
}

impl Lexer for Rust {
    fn step_batch(
        view: &mut SourceView<'_>,
        cursor: u32,
        _state: LexState,
        out: &mut StepBuf,
    ) -> (u32, LexState) {
        let mut cursor = cursor;
        while !out.is_full() {
            let Some(b) = view.byte_at(cursor) else {
                out.push(LexStep::Eof);
                return (cursor, LexState::INITIAL);
            };

            let start = cursor;
            let (local_kind, end, is_error, _) = classify(view, cursor, b);
            debug_assert!(end > start, "lexer must consume at least one byte");

            let mut bit_flags = flags::STATE_BREAKPOINT;
            if is_error {
                bit_flags |= flags::IS_ERROR;
            }

            out.push(LexStep::Token {
                len: end - start,
                local_kind,
                state_out: LexState::INITIAL,
                flags: bit_flags,
            });
            cursor = end;
        }
        (cursor, LexState::INITIAL)
    }

    fn is_safe_state(_state: LexState) -> bool {
        true
    }
}

#[inline]
fn no_meta(classified: (u16, u32, bool)) -> (u16, u32, bool, u16) {
    (classified.0, classified.1, classified.2, 0)
}

#[inline]
fn classify(view: &mut SourceView<'_>, cursor: u32, first: u8) -> (u16, u32, bool, u16) {
    match first {
        b' ' | b'\t' | b'\n' | b'\r' | 0x0B | 0x0C => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            false,
            0,
        ),
        b'/' => match view.byte_at(cursor + 1) {
            Some(b'/') => no_meta(scan_line_comment(view, cursor)),
            Some(b'*') => no_meta(scan_block_comment(view, cursor)),
            _ => (kinds::SLASH, cursor + 1, false, 0),
        },
        b'"' => {
            let r = scan::scan_c_string(view, cursor);
            let end = eat_suffix(view, r.end);
            (kinds::STRING, end, r.is_error, 0)
        }
        b'\'' => no_meta(scan_char_or_lifetime(view, cursor)),
        b'0'..=b'9' => {
            let r = scan_rust_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error, 0)
        }
        b'r' => scan_r_prefix(view, cursor),
        b'b' => scan_b_prefix(view, cursor),
        b'c' => scan_c_prefix(view, cursor),
        b if byteclass::is_ident_start(b) => scan_ident_or_keyword(view, cursor, b),
        b'{' => (kinds::OPEN_BRACE, cursor + 1, false, 0),
        b'}' => (kinds::CLOSE_BRACE, cursor + 1, false, 0),
        b'(' => (kinds::OPEN_PAREN, cursor + 1, false, 0),
        b')' => (kinds::CLOSE_PAREN, cursor + 1, false, 0),
        b'[' => (kinds::OPEN_BRACKET, cursor + 1, false, 0),
        b']' => (kinds::CLOSE_BRACKET, cursor + 1, false, 0),
        b',' => (kinds::COMMA, cursor + 1, false, 0),
        b';' => (kinds::SEMI, cursor + 1, false, 0),
        b':' => (kinds::COLON, cursor + 1, false, 0),
        b'.' => (kinds::DOT, cursor + 1, false, 0),
        b'?' => (kinds::QUESTION, cursor + 1, false, 0),
        b'@' => (kinds::AT, cursor + 1, false, 0),
        b'#' => (kinds::HASH, cursor + 1, false, 0),
        b'~' => (kinds::TILDE, cursor + 1, false, 0),
        b'$' => (kinds::DOLLAR, cursor + 1, false, 0),
        b'=' => (kinds::EQ, cursor + 1, false, 0),
        b'!' => (kinds::BANG, cursor + 1, false, 0),
        b'<' => (kinds::LT, cursor + 1, false, 0),
        b'>' => (kinds::GT, cursor + 1, false, 0),
        b'-' => (kinds::MINUS, cursor + 1, false, 0),
        b'&' => (kinds::AMP, cursor + 1, false, 0),
        b'|' => (kinds::PIPE, cursor + 1, false, 0),
        b'+' => (kinds::PLUS, cursor + 1, false, 0),
        b'*' => (kinds::STAR, cursor + 1, false, 0),
        b'^' => (kinds::CARET, cursor + 1, false, 0),
        b'%' => (kinds::PERCENT, cursor + 1, false, 0),
        b if b >= 0x80 => no_meta(classify_unicode_ident(view, cursor)),
        _ => (kinds::ERROR, cursor + 1, true, 0),
    }
}

fn classify_unicode_ident(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    // Non-ASCII start: try a Unicode XID identifier. Rust keywords are all
    // ASCII, so a Unicode-starting identifier cannot be a keyword and the
    // shared keyword lookup is skipped.
    let end = scan_rust_ident(view, cursor);
    if end > cursor {
        return (kinds::IDENT, end, false);
    }
    // Not a valid XID_Start — advance by one full UTF-8 char so the lexer
    // makes forward progress without stranding continuation bytes.
    let len = scan::decoded_len_or_one(view, cursor);
    (kinds::ERROR, cursor + len, true)
}

fn scan_line_comment(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let mut end = cursor + 2;
    let is_doc = match view.byte_at(end) {
        Some(b'!') => true,
        Some(b'/') => view.byte_at(end + 1) != Some(b'/'),
        _ => false,
    };
    loop {
        let (base, page) = view.window_at(end);
        if page.is_empty() {
            break;
        }
        let mut i = (end - base) as usize;
        let start = i;
        while i < page.len() && page[i] != b'\n' {
            i += 1;
        }
        end = base + i as u32;
        if i < page.len() || i == start {
            break;
        }
    }
    let kind = if is_doc {
        kinds::DOC_COMMENT
    } else {
        kinds::COMMENT
    };
    (kind, end, false)
}

fn scan_block_comment(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = cursor + 2;
    let is_doc = match view.byte_at(end) {
        Some(b'!') => true,
        Some(b'*') => !matches!(view.byte_at(end + 1), Some(b'*') | Some(b'/')),
        _ => false,
    };
    let kind = if is_doc {
        kinds::DOC_COMMENT
    } else {
        kinds::COMMENT
    };

    let r = scan::scan_nested_block_comment(view, cursor);
    (kind, r.end, r.is_error)
}

fn scan_char_or_lifetime(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let c1 = view.byte_at(cursor + 1);
    let c2 = view.byte_at(cursor + 2);

    let can_be_lifetime = match (c1, c2) {
        (_, Some(b'\'')) => false,
        (Some(b), _) if byteclass::is_ident_start(b) || byteclass::is_digit(b) => true,
        _ => false,
    };

    if !can_be_lifetime {
        return scan_char_literal(view, cursor);
    }

    // Raw lifetime: 'r#ident.
    if c1 == Some(b'r') && c2 == Some(b'#') {
        let end = scan_rust_ident(view, cursor + 3);
        if end > cursor + 3 {
            return (kinds::LIFETIME, end, false);
        }
    }

    let mut end = cursor + 1;
    // Consume first character unconditionally (may be a digit).
    end += 1;
    while let Some(b) = view.byte_at(end) {
        if byteclass::is_ident_cont(b) {
            end += 1;
        } else {
            break;
        }
    }
    if view.byte_at(end) == Some(b'\'') {
        end += 1;
        end = eat_suffix(view, end);
        return (kinds::CHAR, end, false);
    }
    (kinds::LIFETIME, end, false)
}

fn scan_char_literal(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let mut end = cursor + 1;
    let first = view.byte_at(end);
    let second = view.byte_at(end + 1);
    if matches!(second, Some(b'\'')) && !matches!(first, Some(b'\\')) && first.is_some() {
        end += 2;
        end = eat_suffix(view, end);
        return (kinds::CHAR, end, false);
    }
    loop {
        match view.byte_at(end) {
            Some(b'\'') => {
                end += 1;
                end = eat_suffix(view, end);
                return (kinds::CHAR, end, false);
            }
            Some(b'\n') => return (kinds::CHAR, end, true),
            Some(b'\\') => {
                end += 1;
                if view.byte_at(end).is_some() {
                    end += 1;
                }
            }
            Some(_) => end += 1,
            None => return (kinds::CHAR, end, true),
        }
    }
}

fn scan_r_prefix(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool, u16) {
    match (view.byte_at(cursor + 1), view.byte_at(cursor + 2)) {
        (Some(b'#'), _) => {
            let body = scan_rust_ident(view, cursor + 2);
            if body > cursor + 2 {
                return (kinds::IDENT, body, false, 0);
            }
            let r = scan_raw_string(view, cursor + 1);
            let end = eat_suffix(view, r.end);
            (kinds::STRING, end, r.is_error, 0)
        }
        (Some(b'"'), _) => {
            let r = scan_raw_string(view, cursor + 1);
            let end = eat_suffix(view, r.end);
            (kinds::STRING, end, r.is_error, 0)
        }
        _ => scan_ident_or_keyword(view, cursor, b'r'),
    }
}

fn scan_b_prefix(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool, u16) {
    match (view.byte_at(cursor + 1), view.byte_at(cursor + 2)) {
        (Some(b'\''), _) => {
            let (_, end, err) = scan_char_literal(view, cursor + 1);
            (kinds::STRING, end, err, 0)
        }
        (Some(b'"'), _) => {
            let r = scan::scan_c_string(view, cursor + 1);
            let end = eat_suffix(view, r.end);
            (kinds::STRING, end, r.is_error, 0)
        }
        (Some(b'r'), Some(b'"')) | (Some(b'r'), Some(b'#')) => {
            let r = scan_raw_string(view, cursor + 2);
            let end = eat_suffix(view, r.end);
            (kinds::STRING, end, r.is_error, 0)
        }
        _ => scan_ident_or_keyword(view, cursor, b'b'),
    }
}

fn scan_c_prefix(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool, u16) {
    match (view.byte_at(cursor + 1), view.byte_at(cursor + 2)) {
        (Some(b'"'), _) => {
            let r = scan::scan_c_string(view, cursor + 1);
            let end = eat_suffix(view, r.end);
            (kinds::STRING, end, r.is_error, 0)
        }
        (Some(b'r'), Some(b'"')) | (Some(b'r'), Some(b'#')) => {
            let r = scan_raw_string(view, cursor + 2);
            let end = eat_suffix(view, r.end);
            (kinds::STRING, end, r.is_error, 0)
        }
        _ => scan_ident_or_keyword(view, cursor, b'c'),
    }
}

fn scan_raw_string(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor;
    let mut n_hashes: u32 = 0;
    while view.byte_at(end) == Some(b'#') {
        n_hashes += 1;
        end += 1;
    }
    if view.byte_at(end) != Some(b'"') {
        return ScanResult {
            end,
            is_error: true,
        };
    }
    end += 1;
    loop {
        match view.byte_at(end) {
            None => {
                return ScanResult {
                    end,
                    is_error: true,
                };
            }
            Some(b'"') => {
                end += 1;
                let mut closing: u32 = 0;
                while closing < n_hashes && view.byte_at(end) == Some(b'#') {
                    closing += 1;
                    end += 1;
                }
                if closing == n_hashes {
                    return ScanResult {
                        end,
                        is_error: false,
                    };
                }
            }
            Some(_) => end += 1,
        }
    }
}

fn scan_rust_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor;
    let first = view.byte_at(end).unwrap();
    end += 1;
    let mut has_error = false;

    let mut consumed_body = true;
    if first == b'0' {
        match view.byte_at(end) {
            Some(b'b') | Some(b'o') => {
                end += 1;
                let before = end;
                end = eat_decimal_digits(view, end);
                if end == before {
                    has_error = true;
                }
                consumed_body = false;
            }
            Some(b'x') => {
                end += 1;
                let before = end;
                end = eat_hex_digits(view, end);
                if end == before {
                    has_error = true;
                }
                consumed_body = false;
            }
            Some(b) if b == b'_' || byteclass::is_digit(b) => {
                end = eat_decimal_digits(view, end);
            }
            _ => {}
        }
    } else {
        end = eat_decimal_digits(view, end);
    }

    if consumed_body {
        match view.byte_at(end) {
            Some(b'.') => {
                let next = view.byte_at(end + 1);
                let eat_dot = !matches!(next, Some(b'.'))
                    && !matches!(next, Some(b) if byteclass::is_ident_start(b));
                if eat_dot {
                    end += 1;
                    if matches!(view.byte_at(end), Some(b'0'..=b'9')) {
                        end = eat_decimal_digits(view, end);
                        if matches!(view.byte_at(end), Some(b'e') | Some(b'E')) {
                            end += 1;
                            if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
                                end += 1;
                            }
                            let before = end;
                            end = eat_decimal_digits(view, end);
                            if end == before {
                                has_error = true;
                            }
                        }
                    }
                }
            }
            Some(b'e') | Some(b'E') => {
                end += 1;
                if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
                    end += 1;
                }
                let before = end;
                end = eat_decimal_digits(view, end);
                if end == before {
                    has_error = true;
                }
            }
            _ => {}
        }
    }

    end = eat_suffix(view, end);
    ScanResult {
        end,
        is_error: has_error,
    }
}

fn eat_decimal_digits(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut end = cursor;
    loop {
        let (base, page) = view.window_at(end);
        if page.is_empty() {
            return end;
        }
        let mut i = (end - base) as usize;
        while i < page.len() && (page[i] == b'_' || byteclass::is_digit(page[i])) {
            i += 1;
        }
        end = base + i as u32;
        if i < page.len() {
            return end;
        }
    }
}

fn eat_hex_digits(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut end = cursor;
    loop {
        let (base, page) = view.window_at(end);
        if page.is_empty() {
            return end;
        }
        let mut i = (end - base) as usize;
        while i < page.len() && (page[i] == b'_' || byteclass::is_hex_digit(page[i])) {
            i += 1;
        }
        end = base + i as u32;
        if i < page.len() {
            return end;
        }
    }
}

fn eat_suffix(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    scan_rust_ident(view, cursor)
}

fn scan_ident_or_keyword(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
) -> (u16, u32, bool, u16) {
    let end = scan_rust_ident(view, cursor);
    let len = (end - cursor) as usize;

    let mut meta = META_READY;
    if first.is_ascii_uppercase() {
        meta |= META_UPPER;
    }

    let mut word = 0u16;
    let mut is_keyword = false;
    if len <= MAX_META_WORD_LEN {
        let (_, page) = view.window_at(cursor);
        if page.len() >= len {
            let bytes = &page[..len];
            (word, is_keyword) = word_info(bytes);
        } else {
            let mut buf = [0u8; MAX_META_WORD_LEN];
            if scan::copy_bytes(view, cursor, &mut buf[..len]) {
                let bytes = &buf[..len];
                (word, is_keyword) = word_info(bytes);
            }
        }
        meta |= word;
    }

    if is_keyword {
        return (kinds::KEYWORD, end, false, meta);
    }
    (kinds::IDENT, end, false, meta)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Source;

    fn run(s: &str) -> Vec<(u16, u32, u32, u8)> {
        let src: &dyn Source = &s;
        let mut view = SourceView::new(src, 0);
        let mut out = StepBuf::new();
        let mut cursor = 0u32;
        let mut state = LexState::INITIAL;
        let mut tokens = Vec::new();
        loop {
            out.clear();
            let before = cursor;
            (cursor, state) = Rust::step_batch(&mut view, cursor, state, &mut out);
            let mut pos = before;
            let mut saw_eof = false;
            for step in out.as_slice() {
                match *step {
                    LexStep::Token {
                        len,
                        local_kind,
                        flags,
                        ..
                    } => {
                        tokens.push((local_kind, pos, len, flags));
                        pos += len;
                    }
                    LexStep::Eof => saw_eof = true,
                    LexStep::Descend { .. } => unreachable!("Rust has no embedding"),
                }
            }
            if saw_eof {
                return tokens;
            }
        }
    }

    fn kinds_of(s: &str) -> Vec<u16> {
        run(s).iter().map(|t| t.0).collect()
    }

    #[test]
    fn coverage_is_contiguous() {
        let input = r#"fn main() { let x: i32 = 42; println!("hi {x}"); }"#;
        let tokens = run(input);
        let mut pos = 0u32;
        for (_k, off, len, _f) in &tokens {
            assert_eq!(*off, pos, "gap before {pos}");
            pos += len;
        }
        assert_eq!(pos as usize, input.len());
    }

    #[test]
    fn keyword_vs_ident() {
        assert_eq!(kinds_of("fn"), vec![kinds::KEYWORD]);
        assert_eq!(kinds_of("fna"), vec![kinds::IDENT]);
        assert_eq!(kinds_of("Self"), vec![kinds::KEYWORD]);
        assert_eq!(kinds_of("selfish"), vec![kinds::IDENT]);
    }

    #[test]
    fn line_and_doc_comments() {
        assert_eq!(kinds_of("// hi"), vec![kinds::COMMENT]);
        assert_eq!(kinds_of("//! inner"), vec![kinds::DOC_COMMENT]);
        assert_eq!(kinds_of("/// outer"), vec![kinds::DOC_COMMENT]);
        assert_eq!(kinds_of("//// four"), vec![kinds::COMMENT]);
    }

    #[test]
    fn block_comments_nested() {
        assert_eq!(kinds_of("/* a /* b */ c */"), vec![kinds::COMMENT]);
        assert_eq!(kinds_of("/** outer */"), vec![kinds::DOC_COMMENT]);
        assert_eq!(kinds_of("/*! inner */"), vec![kinds::DOC_COMMENT]);
        let t = run("/* unterminated");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::COMMENT);
        assert_ne!(t[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn strings_and_prefixes() {
        assert_eq!(kinds_of(r#""hi""#), vec![kinds::STRING]);
        assert_eq!(kinds_of(r#"b"hi""#), vec![kinds::STRING]);
        assert_eq!(kinds_of(r#"c"hi""#), vec![kinds::STRING]);
        assert_eq!(kinds_of(r##"r#"raw"#"##), vec![kinds::STRING]);
        assert_eq!(kinds_of(r##"br#"raw"#"##), vec![kinds::STRING]);
        assert_eq!(kinds_of(r#""a\"b""#), vec![kinds::STRING]);
    }

    #[test]
    fn multiline_strings_are_one_token() {
        // Regular string: newlines are literal content, not a terminator.
        let t = run("\"line 1\nline 2\nline 3\"");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].3 & flags::IS_ERROR, 0);

        // Line continuation: `\` followed by newline — the `\n` must not
        // terminate the string and must not be misread as an escape pair.
        let s = "\"a\\\nb\"";
        let t = run(s);
        assert_eq!(t.len(), 1, "tokens for {s:?}: {t:?}");
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].2 as usize, s.len());

        // Escaped closing quote spanning two lines.
        let s = "\"first\\\n\"second\"";
        let t = run(s);
        // `"first\<newline>"` then `second` ident then `"` unterminated.
        assert!(t.iter().any(|(k, _, _, _)| *k == kinds::STRING));

        // Raw string with embedded newlines.
        let t = run("r\"line 1\nline 2\"");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].3 & flags::IS_ERROR, 0);

        // Hashed raw string with embedded newlines and a bare `"`.
        let t = run("r#\"one\n\"two\n\"#");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].3 & flags::IS_ERROR, 0);

        // Unterminated multi-line string — whole tail becomes a single
        // error-flagged STRING, followed by Eof only.
        let s = "\"open\nmiddle\nstill open";
        let t = run(s);
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_ne!(t[0].3 & flags::IS_ERROR, 0);
        assert_eq!(t[0].2 as usize, s.len());
    }

    #[test]
    fn chars_and_lifetimes() {
        assert_eq!(kinds_of("'a'"), vec![kinds::CHAR]);
        assert_eq!(kinds_of("'\\n'"), vec![kinds::CHAR]);
        assert_eq!(kinds_of("'a"), vec![kinds::LIFETIME]);
        assert_eq!(kinds_of("'static"), vec![kinds::LIFETIME]);
        // Multi-char char literal (syntax error at parse time but lexes as CHAR).
        assert_eq!(kinds_of("'abc'"), vec![kinds::CHAR]);
    }

    #[test]
    fn raw_identifier() {
        let t = run("r#fn");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::IDENT);
    }

    #[test]
    fn numbers_basic() {
        assert_eq!(kinds_of("0"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("42"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("0x1F"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("0b1010"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("0o77"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("1.5"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("1_000_000u32"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("1e10"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("2.5E-3"), vec![kinds::NUMBER]);
    }

    #[test]
    fn number_dot_method_not_float() {
        // 1.foo() — the `1` should be a number, `.` DOT, `foo` ident.
        let ks = kinds_of("1.foo");
        assert_eq!(ks, vec![kinds::NUMBER, kinds::DOT, kinds::IDENT]);
    }

    #[test]
    fn range_not_float() {
        let ks = kinds_of("0..5");
        assert_eq!(ks.first(), Some(&kinds::NUMBER));
        assert!(ks.contains(&kinds::DOT));
    }

    #[test]
    fn error_on_non_ascii_byte() {
        let t = run("§");
        // `§` is Unicode category Po — not XID_Start, so it must error. The
        // whole 2-byte UTF-8 sequence is consumed as a single ERROR token.
        assert!(
            t.iter()
                .any(|(k, _, _, f)| *k == kinds::ERROR && *f & flags::IS_ERROR != 0)
        );
    }

    #[test]
    fn unicode_identifier() {
        let t = run("let αβγ = 1;");
        let idents: Vec<_> = t.iter().filter(|(k, _, _, _)| *k == kinds::IDENT).collect();
        assert_eq!(idents.len(), 1);
        assert_eq!(idents[0].2 as usize, "αβγ".len());
    }

    #[test]
    fn unicode_identifier_at_source_start() {
        // Starting with a non-ASCII XID_Start should not take the error path.
        let t = run("пример + 1");
        assert_eq!(t[0].0, kinds::IDENT);
        assert_eq!(t[0].2 as usize, "пример".len());
    }
}
