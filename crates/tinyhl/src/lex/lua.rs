//! Lua 5.5 lexer.
//!
//! Lua is lexed as a byte machine with one state bit:
//! `seen_token: u1`. [`LexState::INITIAL`] only occurs before the first token,
//! which keeps shebang recognition incremental-safe. Long strings/comments,
//! short strings, comments, and numerals are scanned to completion as single
//! tokens, so every emitted token can be a safe incremental breakpoint.
//!
//! Deliberate "good enough" choices:
//!
//! - Identifiers are ASCII `[A-Za-z_][A-Za-z0-9_]*`, matching the stock Lua
//!   lexer surface useful for highlighting. Non-ASCII bytes are local errors.
//! - Attribute names such as `const` and `close` are not reserved words in
//!   Lua 5.5, so they stay [`IDENT`] inside `<...>`.
//! - String escape validation catches malformed escapes and unescaped line
//!   breaks, but does not validate numeric escape ranges.
//!
//! [`IDENT`]: kinds::IDENT

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::kw::{self, kw_lookup};
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

kw::kw_table! {
    static LUA_KWS = [
        (b"and", kinds::KEYWORD),
        (b"break", kinds::KEYWORD),
        (b"do", kinds::KEYWORD),
        (b"else", kinds::KEYWORD),
        (b"elseif", kinds::KEYWORD),
        (b"end", kinds::KEYWORD),
        (b"false", kinds::KEYWORD),
        (b"for", kinds::KEYWORD),
        (b"function", kinds::KEYWORD),
        (b"global", kinds::KEYWORD),
        (b"goto", kinds::KEYWORD),
        (b"if", kinds::KEYWORD),
        (b"in", kinds::KEYWORD),
        (b"local", kinds::KEYWORD),
        (b"nil", kinds::KEYWORD),
        (b"not", kinds::KEYWORD),
        (b"or", kinds::KEYWORD),
        (b"repeat", kinds::KEYWORD),
        (b"return", kinds::KEYWORD),
        (b"then", kinds::KEYWORD),
        (b"true", kinds::KEYWORD),
        (b"until", kinds::KEYWORD),
        (b"while", kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 8; // "function"

mod state {
    pub const SEEN_TOKEN: u16 = 1 << 0;
}

pub(crate) struct Lua;

impl Lexer for Lua {
    fn step_batch(
        view: &mut SourceView<'_>,
        cursor: u32,
        state: LexState,
        out: &mut StepBuf,
    ) -> (u32, LexState) {
        let mut cursor = cursor;
        let mut state = canonical_state(state);
        while !out.is_full() {
            let Some(b) = view.byte_at(cursor) else {
                out.push(LexStep::Eof);
                return (cursor, state);
            };

            let start = cursor;
            let (local_kind, end, is_error) = classify(view, cursor, b, state);
            debug_assert!(end > start, "lexer must consume at least one byte");
            let state_out = LexState(state::SEEN_TOKEN);

            let mut bit_flags = flags::STATE_BREAKPOINT;
            if is_error {
                bit_flags |= flags::IS_ERROR;
            }

            out.push(LexStep::Token {
                len: end - start,
                local_kind,
                state_out,
                flags: bit_flags,
            });
            cursor = end;
            state = state_out;
        }
        (cursor, state)
    }

    fn is_safe_state(_state: LexState) -> bool {
        true
    }
}

fn canonical_state(state: LexState) -> LexState {
    LexState(state.bits() & state::SEEN_TOKEN)
}

fn classify(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
    state: LexState,
) -> (u16, u32, bool) {
    match first {
        b if byteclass::is_whitespace(b) => {
            let end = scan::scan_whitespace(view, cursor);
            (kinds::WHITESPACE, end, false)
        }
        b'#' if state.is_initial() && view.byte_at(cursor + 1) == Some(b'!') => {
            let end = scan_shebang(view, cursor);
            (kinds::COMMENT, end, false)
        }
        b'-' if view.byte_at(cursor + 1) == Some(b'-') => scan_comment(view, cursor),
        b'"' | b'\'' => {
            let r = scan_short_string(view, cursor, first);
            (kinds::STRING, r.end, r.is_error)
        }
        b'[' => {
            if let Some(level) = long_bracket_level(view, cursor) {
                let r = scan_long_body(view, cursor, level);
                (kinds::STRING, r.end, r.is_error)
            } else {
                (kinds::OPEN_BRACKET, cursor + 1, false)
            }
        }
        b'.' => match view.byte_at(cursor + 1) {
            Some(b) if byteclass::is_digit(b) => {
                let r = scan_lua_number(view, cursor);
                (kinds::NUMBER, r.end, r.is_error)
            }
            Some(b'.') if view.byte_at(cursor + 2) == Some(b'.') => {
                (kinds::ELLIPSIS, cursor + 3, false)
            }
            Some(b'.') => (kinds::DOT_DOT, cursor + 2, false),
            _ => (kinds::DOT, cursor + 1, false),
        },
        b'0'..=b'9' => {
            let r = scan_lua_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b if byteclass::is_ident_start(b) => classify_ident(view, cursor),
        b'+' | b'-' | b'*' | b'/' | b'%' | b'^' | b'#' | b'&' | b'~' | b'|' | b'<' | b'>'
        | b'=' | b':' | b',' | b';' | b'(' | b')' | b'{' | b'}' | b']' => {
            let (kind, end) = scan_operator(view, cursor, first);
            (kind, end, false)
        }
        b if b >= 0x80 => {
            let len = scan::decode_char_at(view, cursor)
                .map(|(_, n)| n)
                .unwrap_or(1);
            (kinds::ERROR, cursor + len, true)
        }
        _ => (kinds::ERROR, cursor + 1, true),
    }
}

fn classify_ident(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = scan::scan_ident_ascii(view, cursor);
    let len = (end - cursor) as usize;
    if len <= MAX_KW_LEN {
        let mut buf = [0u8; MAX_KW_LEN];
        if scan::copy_bytes(view, cursor, &mut buf[..len]) {
            if let Some(kind) = kw_lookup(LUA_KWS, &buf[..len]) {
                return (kind, end, false);
            }
        }
    }
    (kinds::IDENT, end, false)
}

fn scan_shebang(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut end = cursor + 2;
    loop {
        match view.byte_at(end) {
            Some(b'\n') | None => return end,
            Some(_) => end += 1,
        }
    }
}

fn scan_comment(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    if let Some(level) = long_bracket_level(view, cursor + 2) {
        let r = scan_long_body(view, cursor + 2, level);
        (kinds::COMMENT, r.end, r.is_error)
    } else {
        let mut end = cursor + 2;
        loop {
            match view.byte_at(end) {
                Some(b'\n') | None => return (kinds::COMMENT, end, false),
                Some(_) => end += 1,
            }
        }
    }
}

fn long_bracket_level(view: &mut SourceView<'_>, cursor: u32) -> Option<u32> {
    if view.byte_at(cursor) != Some(b'[') {
        return None;
    }
    let mut p = cursor + 1;
    let mut level = 0u32;
    while view.byte_at(p) == Some(b'=') {
        level += 1;
        p += 1;
    }
    if view.byte_at(p) == Some(b'[') {
        Some(level)
    } else {
        None
    }
}

fn scan_long_body(view: &mut SourceView<'_>, opener: u32, level: u32) -> ScanResult {
    let mut p = opener + level + 2;
    loop {
        match view.byte_at(p) {
            None => return err(p),
            Some(b']') if long_close_matches(view, p, level) => {
                return ok(p + level + 2);
            }
            Some(_) => p += 1,
        }
    }
}

fn long_close_matches(view: &mut SourceView<'_>, cursor: u32, level: u32) -> bool {
    let mut p = cursor + 1;
    for _ in 0..level {
        if view.byte_at(p) != Some(b'=') {
            return false;
        }
        p += 1;
    }
    view.byte_at(p) == Some(b']')
}

fn scan_short_string(view: &mut SourceView<'_>, cursor: u32, quote: u8) -> ScanResult {
    let mut end = cursor + 1;
    loop {
        match view.byte_at(end) {
            None => return err(end),
            Some(b) if b == quote => return ok(end + 1),
            Some(b'\n') | Some(b'\r') => return err(end),
            Some(b'\\') => {
                let r = scan_escape(view, end);
                end = r.end;
                if r.is_error {
                    return r;
                }
            }
            Some(_) => end += 1,
        }
    }
}

fn scan_escape(view: &mut SourceView<'_>, slash: u32) -> ScanResult {
    let Some(next) = view.byte_at(slash + 1) else {
        return err(slash + 1);
    };

    match next {
        b'a' | b'b' | b'f' | b'n' | b'r' | b't' | b'v' | b'\\' | b'"' | b'\'' => ok(slash + 2),
        b'\n' | b'\r' => ok(scan_escaped_line_break(view, slash + 1)),
        b'z' => {
            let mut end = slash + 2;
            while let Some(b) = view.byte_at(end) {
                if byteclass::is_whitespace(b) {
                    end += 1;
                } else {
                    break;
                }
            }
            ok(end)
        }
        b'x' => {
            let h1 = view.byte_at(slash + 2);
            let h2 = view.byte_at(slash + 3);
            if matches!(h1, Some(b) if byteclass::is_hex_digit(b))
                && matches!(h2, Some(b) if byteclass::is_hex_digit(b))
            {
                ok(slash + 4)
            } else {
                err(slash + 2)
            }
        }
        b'u' => scan_utf8_escape(view, slash),
        b'0'..=b'9' => {
            let mut end = slash + 2;
            for _ in 0..2 {
                match view.byte_at(end) {
                    Some(b) if byteclass::is_digit(b) => end += 1,
                    _ => break,
                }
            }
            ok(end)
        }
        _ => err(slash + 2),
    }
}

fn scan_escaped_line_break(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    match view.byte_at(cursor) {
        Some(b'\r') => {
            let end = cursor + 1;
            if view.byte_at(end) == Some(b'\n') {
                end + 1
            } else {
                end
            }
        }
        Some(b'\n') => {
            let end = cursor + 1;
            if view.byte_at(end) == Some(b'\r') {
                end + 1
            } else {
                end
            }
        }
        _ => cursor,
    }
}

fn scan_utf8_escape(view: &mut SourceView<'_>, slash: u32) -> ScanResult {
    if view.byte_at(slash + 2) != Some(b'{') {
        return err(slash + 2);
    }
    let mut end = slash + 3;
    let mut had_digit = false;
    loop {
        match view.byte_at(end) {
            Some(b) if byteclass::is_hex_digit(b) => {
                had_digit = true;
                end += 1;
            }
            Some(b'}') if had_digit => return ok(end + 1),
            Some(_) => return err(end + 1),
            None => return err(end),
        }
    }
}

fn scan_lua_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    if view.byte_at(cursor) == Some(b'0') {
        match view.byte_at(cursor + 1) {
            Some(b'x') | Some(b'X') => return scan_hex_number(view, cursor + 2),
            _ => {}
        }
    }
    scan_decimal_number(view, cursor)
}

fn scan_decimal_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor;
    let mut had_int_digits = false;
    if view.byte_at(cursor) != Some(b'.') {
        let (int_end, had) = scan_digit_run(view, cursor, byteclass::is_digit);
        end = int_end;
        had_int_digits = had;
    }

    let mut had_frac_digits = false;
    if view.byte_at(end) == Some(b'.') && view.byte_at(end + 1) != Some(b'.') {
        let (frac_end, had) = scan_digit_run(view, end + 1, byteclass::is_digit);
        end = frac_end;
        had_frac_digits = had;
    }

    if !had_int_digits && !had_frac_digits {
        return err(end);
    }

    match view.byte_at(end) {
        Some(b'e') | Some(b'E') => {
            end += 1;
            if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
                end += 1;
            }
            let (exp_end, had_exp_digits) = scan_digit_run(view, end, byteclass::is_digit);
            if !had_exp_digits {
                return err(exp_end);
            }
            ok(exp_end)
        }
        _ => ok(end),
    }
}

fn scan_hex_number(view: &mut SourceView<'_>, after_prefix: u32) -> ScanResult {
    let (int_end, had_int_digits) = scan_digit_run(view, after_prefix, byteclass::is_hex_digit);
    let mut end = int_end;
    let mut had_frac_digits = false;

    if view.byte_at(end) == Some(b'.') && view.byte_at(end + 1) != Some(b'.') {
        let (frac_end, had) = scan_digit_run(view, end + 1, byteclass::is_hex_digit);
        end = frac_end;
        had_frac_digits = had;
    }

    if !had_int_digits && !had_frac_digits {
        return err(end);
    }

    match view.byte_at(end) {
        Some(b'p') | Some(b'P') => {
            end += 1;
            if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
                end += 1;
            }
            let (exp_end, had_exp_digits) = scan_digit_run(view, end, byteclass::is_digit);
            if !had_exp_digits {
                return err(exp_end);
            }
            ok(exp_end)
        }
        _ => ok(end),
    }
}

fn scan_digit_run(view: &mut SourceView<'_>, cursor: u32, pred: fn(u8) -> bool) -> (u32, bool) {
    let mut cursor = cursor;
    let mut saw_digit = false;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return (cursor, saw_digit);
        }
        let start = (cursor - base) as usize;
        let mut i = start;
        while i < page.len() && pred(page[i]) {
            saw_digit = true;
            i += 1;
        }
        cursor += (i - start) as u32;
        if i < page.len() {
            return (cursor, saw_digit);
        }
    }
}

fn scan_operator(view: &mut SourceView<'_>, cursor: u32, b0: u8) -> (u16, u32) {
    let b1 = view.byte_at(cursor + 1);
    let b2 = view.byte_at(cursor + 2);

    let three = match (b0, b1, b2) {
        (b'.', Some(b'.'), Some(b'.')) => Some(kinds::ELLIPSIS),
        _ => None,
    };
    if let Some(k) = three {
        return (k, cursor + 3);
    }

    if let Some(b1) = b1 {
        let two = match (b0, b1) {
            (b'=', b'=') => Some(kinds::EQ_EQ),
            (b'~', b'=') => Some(kinds::BANG_EQ),
            (b'<', b'=') => Some(kinds::LT_EQ),
            (b'>', b'=') => Some(kinds::GT_EQ),
            (b'<', b'<') => Some(kinds::SHL),
            (b'>', b'>') => Some(kinds::SHR),
            (b'/', b'/') => Some(kinds::SLASH_SLASH),
            (b':', b':') => Some(kinds::COLON_COLON),
            (b'.', b'.') => Some(kinds::DOT_DOT),
            _ => None,
        };
        if let Some(k) = two {
            return (k, cursor + 2);
        }
    }

    let one = match b0 {
        b'+' => kinds::PLUS,
        b'-' => kinds::MINUS,
        b'*' => kinds::STAR,
        b'/' => kinds::SLASH,
        b'%' => kinds::PERCENT,
        b'^' => kinds::CARET,
        b'#' => kinds::HASH,
        b'&' => kinds::AMP,
        b'~' => kinds::TILDE,
        b'|' => kinds::PIPE,
        b'<' => kinds::LT,
        b'>' => kinds::GT,
        b'=' => kinds::EQ,
        b':' => kinds::COLON,
        b',' => kinds::COMMA,
        b';' => kinds::SEMI,
        b'(' => kinds::OPEN_PAREN,
        b')' => kinds::CLOSE_PAREN,
        b'{' => kinds::OPEN_BRACE,
        b'}' => kinds::CLOSE_BRACE,
        b']' => kinds::CLOSE_BRACKET,
        _ => kinds::ERROR,
    };
    (one, cursor + 1)
}

fn ok(end: u32) -> ScanResult {
    ScanResult {
        end,
        is_error: false,
    }
}

fn err(end: u32) -> ScanResult {
    ScanResult {
        end,
        is_error: true,
    }
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
            (cursor, state) = Lua::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("Lua has no embedding"),
                }
            }
            if saw_eof {
                return tokens;
            }
        }
    }

    fn kinds_of(tokens: &[(u16, u32, u32, u8)]) -> Vec<u16> {
        tokens.iter().map(|t| t.0).collect()
    }

    #[test]
    fn keywords_covering_the_table() {
        for kw in [
            "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "global",
            "goto", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then", "true",
            "until", "while",
        ] {
            let tokens = run(kw);
            assert_eq!(tokens.len(), 1, "{kw:?}");
            assert_eq!(tokens[0].0, kinds::KEYWORD, "{kw:?} should be KEYWORD");
        }
    }

    #[test]
    fn attributes_are_identifiers() {
        let tokens = run("local x <const>, y <close>");
        let ident_count = tokens.iter().filter(|t| t.0 == kinds::IDENT).count();
        assert_eq!(ident_count, 4);
    }

    #[test]
    fn comments() {
        assert_eq!(
            kinds_of(&run("-- hello\nlocal")),
            vec![kinds::COMMENT, kinds::WHITESPACE, kinds::KEYWORD]
        );
        let block = run("--[=[ block\ncomment ]=] local");
        assert_eq!(
            kinds_of(&block),
            vec![kinds::COMMENT, kinds::WHITESPACE, kinds::KEYWORD]
        );
        assert_eq!(block[0].2, "--[=[ block\ncomment ]=]".len() as u32);
    }

    #[test]
    fn strings() {
        for input in [
            r#""hi\n""#,
            r#"'hi\z
            there'"#,
            "[==[long\nstring]==]",
        ] {
            let tokens = run(input);
            assert_eq!(tokens.len(), 1, "{input:?}");
            assert_eq!(tokens[0].0, kinds::STRING, "{input:?}");
            assert_eq!(tokens[0].3 & flags::IS_ERROR, 0, "{input:?}");
        }
    }

    #[test]
    fn malformed_strings_flag_error() {
        for input in [r#""oops"#, "\"line\nbreak\"", "[=[long"] {
            let tokens = run(input);
            assert_eq!(tokens[0].0, kinds::STRING, "{input:?}");
            assert_ne!(tokens[0].3 & flags::IS_ERROR, 0, "{input:?}");
        }
    }

    #[test]
    fn numbers_mixed() {
        let tokens = run("0 42 3.14 .5 1e-3 0xff 0x1.fp3 0x0.1E 1..2");
        let numbers: Vec<_> = tokens.iter().filter(|t| t.0 == kinds::NUMBER).collect();
        assert_eq!(numbers.len(), 10);
        assert!(tokens.iter().any(|t| t.0 == kinds::DOT_DOT));
        for t in numbers {
            assert_eq!(t.3 & flags::IS_ERROR, 0, "{t:?}");
        }
    }

    #[test]
    fn bad_numbers_flag_error() {
        for input in ["0x", "0x.", "1e", "."] {
            let tokens = run(input);
            if input == "." {
                assert_eq!(tokens[0].0, kinds::DOT);
            } else {
                assert_eq!(tokens[0].0, kinds::NUMBER, "{input:?}");
                assert_ne!(tokens[0].3 & flags::IS_ERROR, 0, "{input:?}");
            }
        }
    }

    #[test]
    fn operators_greedy() {
        let src = "a==b ~=c <=d >=e <<f >>g //h ::i ..j ...k &l |m ~n #o";
        let tokens = run(src);
        for required in [
            kinds::EQ_EQ,
            kinds::BANG_EQ,
            kinds::LT_EQ,
            kinds::GT_EQ,
            kinds::SHL,
            kinds::SHR,
            kinds::SLASH_SLASH,
            kinds::COLON_COLON,
            kinds::DOT_DOT,
            kinds::ELLIPSIS,
            kinds::AMP,
            kinds::PIPE,
            kinds::TILDE,
            kinds::HASH,
        ] {
            assert!(
                tokens.iter().any(|t| t.0 == required),
                "missing kind {required}"
            );
        }
    }

    #[test]
    fn shebang_only_at_start() {
        assert_eq!(run("#!/usr/bin/env lua\n")[0].0, kinds::COMMENT);
        assert_eq!(run("x # y")[2].0, kinds::HASH);
    }
}
