//! Minimal CMake lexer.
//!
//! State encoding: stateless. Every emitted token returns
//! [`LexState::INITIAL`] and carries [`STATE_BREAKPOINT`].
//!
//! Recognized syntax covers the CMake shapes that most affect highlighting:
//! commands/keywords, comments including bracket comments, bracket arguments,
//! quoted strings, variable and generator-expression references, numbers,
//! parentheses, and list separators.
//!
//! [`STATE_BREAKPOINT`]: flags::STATE_BREAKPOINT

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::kw::{self, kw_lookup};
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

kw::kw_table! {
    static CMAKE_KWS = [
        (b"add_executable", kinds::KEYWORD),
        (b"add_library", kinds::KEYWORD),
        (b"and", kinds::KEYWORD),
        (b"cmake_minimum_required", kinds::KEYWORD),
        (b"else", kinds::KEYWORD),
        (b"elseif", kinds::KEYWORD),
        (b"endforeach", kinds::KEYWORD),
        (b"endfunction", kinds::KEYWORD),
        (b"endif", kinds::KEYWORD),
        (b"endmacro", kinds::KEYWORD),
        (b"endwhile", kinds::KEYWORD),
        (b"false", kinds::KEYWORD),
        (b"find_package", kinds::KEYWORD),
        (b"foreach", kinds::KEYWORD),
        (b"function", kinds::KEYWORD),
        (b"if", kinds::KEYWORD),
        (b"include", kinds::KEYWORD),
        (b"macro", kinds::KEYWORD),
        (b"message", kinds::KEYWORD),
        (b"not", kinds::KEYWORD),
        (b"off", kinds::KEYWORD),
        (b"on", kinds::KEYWORD),
        (b"option", kinds::KEYWORD),
        (b"or", kinds::KEYWORD),
        (b"project", kinds::KEYWORD),
        (b"return", kinds::KEYWORD),
        (b"set", kinds::KEYWORD),
        (b"true", kinds::KEYWORD),
        (b"while", kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 22; // "cmake_minimum_required"

pub(crate) struct Cmake;

impl Lexer for Cmake {
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
            let (local_kind, end, is_error) = classify(view, cursor, b);
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

fn classify(view: &mut SourceView<'_>, cursor: u32, first: u8) -> (u16, u32, bool) {
    match first {
        b if byteclass::is_whitespace(b) => {
            let end = scan::scan_whitespace(view, cursor);
            (kinds::WHITESPACE, end, false)
        }
        b'#' => {
            if let Some(level) = bracket_level(view, cursor + 1) {
                let r = scan_bracket_body(view, cursor + 1, level);
                (kinds::COMMENT, r.end, r.is_error)
            } else {
                let end = scan::scan_hash_comment(view, cursor);
                (kinds::COMMENT, end, false)
            }
        }
        b'[' => {
            if let Some(level) = bracket_level(view, cursor) {
                let r = scan_bracket_body(view, cursor, level);
                (kinds::STRING, r.end, r.is_error)
            } else {
                (kinds::OPEN_BRACKET, cursor + 1, false)
            }
        }
        b']' => (kinds::CLOSE_BRACKET, cursor + 1, false),
        b'"' => {
            let r = scan_quoted_string(view, cursor);
            (kinds::STRING, r.end, r.is_error)
        }
        b'$' => match scan_variable_ref(view, cursor) {
            Some(end) => (kinds::IDENT, end, false),
            None => (kinds::DOLLAR, cursor + 1, false),
        },
        b'(' => (kinds::OPEN_PAREN, cursor + 1, false),
        b')' => (kinds::CLOSE_PAREN, cursor + 1, false),
        b'{' => (kinds::OPEN_BRACE, cursor + 1, false),
        b'}' => (kinds::CLOSE_BRACE, cursor + 1, false),
        b',' => (kinds::COMMA, cursor + 1, false),
        b';' => (kinds::SEMI, cursor + 1, false),
        b':' => (kinds::COLON, cursor + 1, false),
        b'.' if matches!(view.byte_at(cursor + 1), Some(b'0'..=b'9')) => {
            let r = scan::scan_c_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b'.' => (kinds::DOT, cursor + 1, false),
        b'=' => (kinds::EQ, cursor + 1, false),
        b'<' => (kinds::LT, cursor + 1, false),
        b'>' => (kinds::GT, cursor + 1, false),
        b'+' => (kinds::PLUS, cursor + 1, false),
        b'-' => (kinds::MINUS, cursor + 1, false),
        b'*' => (kinds::STAR, cursor + 1, false),
        b'/' => (kinds::SLASH, cursor + 1, false),
        b'0'..=b'9' => {
            let r = scan::scan_c_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        _ => classify_word(view, cursor),
    }
}

fn classify_word(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = scan_cmake_word(view, cursor);
    if end == cursor {
        if let Some((_, len)) = scan::decode_char_at(view, cursor) {
            return (kinds::ERROR, cursor + len, true);
        }
        return (kinds::ERROR, cursor + 1, true);
    }

    let len = (end - cursor) as usize;
    if len <= MAX_KW_LEN && is_ascii_identish(view, cursor, end) {
        let mut buf = [0u8; MAX_KW_LEN];
        if scan::copy_bytes(view, cursor, &mut buf[..len]) {
            for b in &mut buf[..len] {
                *b = b.to_ascii_lowercase();
            }
            if let Some(kind) = kw_lookup(CMAKE_KWS, &buf[..len]) {
                return (kind, end, false);
            }
        }
    }

    let kind = if is_ascii_identish(view, cursor, end) {
        kinds::IDENT
    } else {
        kinds::TEXT
    };
    (kind, end, false)
}

fn scan_cmake_word(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let rel = (cursor - base) as usize;
        let mut i = rel;
        while i < page.len() && !is_word_break(page[i]) {
            i += 1;
        }
        cursor = base + i as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

fn is_word_break(b: u8) -> bool {
    matches!(
        b,
        b' ' | b'\t'
            | b'\n'
            | b'\r'
            | b'#'
            | b'"'
            | b'$'
            | b'('
            | b')'
            | b'['
            | b']'
            | b'{'
            | b'}'
            | b','
            | b';'
            | b':'
            | b'='
            | b'<'
            | b'>'
            | b'+'
            | b'*'
    )
}

fn is_ascii_identish(view: &mut SourceView<'_>, start: u32, end: u32) -> bool {
    let Some(first) = view.byte_at(start) else {
        return false;
    };
    if !byteclass::is_ident_start(first) {
        return false;
    }
    let mut cursor = start + 1;
    while cursor < end {
        match view.byte_at(cursor) {
            Some(b) if byteclass::is_ident_cont(b) => cursor += 1,
            _ => return false,
        }
    }
    true
}

fn scan_variable_ref(view: &mut SourceView<'_>, cursor: u32) -> Option<u32> {
    match view.byte_at(cursor + 1) {
        Some(b'{') => scan_until_before_newline(view, cursor + 2, b'}'),
        Some(b'<') => scan_generator_expr(view, cursor + 2),
        Some(b'E') if matches_word_then_open_brace(view, cursor + 1, b"ENV") => {
            scan_until_before_newline(view, cursor + 5, b'}')
        }
        Some(b'C') if matches_word_then_open_brace(view, cursor + 1, b"CACHE") => {
            scan_until_before_newline(view, cursor + 7, b'}')
        }
        _ => None,
    }
}

fn matches_word_then_open_brace(view: &mut SourceView<'_>, cursor: u32, word: &[u8]) -> bool {
    for (i, &b) in word.iter().enumerate() {
        if view.byte_at(cursor + i as u32) != Some(b) {
            return false;
        }
    }
    view.byte_at(cursor + word.len() as u32) == Some(b'{')
}

fn scan_until_before_newline(view: &mut SourceView<'_>, mut cursor: u32, close: u8) -> Option<u32> {
    while let Some(b) = view.byte_at(cursor) {
        match b {
            b'\n' | b'\r' => return None,
            b if b == close => return Some(cursor + 1),
            _ => cursor += 1,
        }
    }
    None
}

fn scan_generator_expr(view: &mut SourceView<'_>, mut cursor: u32) -> Option<u32> {
    let mut depth = 1u8;
    while let Some(b) = view.byte_at(cursor) {
        match b {
            b'\n' | b'\r' => return None,
            b'$' if view.byte_at(cursor + 1) == Some(b'<') && depth < u8::MAX => {
                depth += 1;
                cursor += 2;
            }
            b'>' => {
                depth -= 1;
                cursor += 1;
                if depth == 0 {
                    return Some(cursor);
                }
            }
            _ => cursor += 1,
        }
    }
    None
}

fn bracket_level(view: &mut SourceView<'_>, cursor: u32) -> Option<u32> {
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

fn scan_bracket_body(view: &mut SourceView<'_>, cursor: u32, level: u32) -> ScanResult {
    let mut p = cursor + level + 2;
    while let Some(b) = view.byte_at(p) {
        if b == b']' && bracket_close_matches(view, p + 1, level) {
            return ScanResult {
                end: p + level + 2,
                is_error: false,
            };
        }
        p += 1;
    }
    ScanResult {
        end: p,
        is_error: true,
    }
}

fn bracket_close_matches(view: &mut SourceView<'_>, cursor: u32, level: u32) -> bool {
    let mut p = cursor;
    let mut seen = 0u32;
    while seen < level {
        if view.byte_at(p) != Some(b'=') {
            return false;
        }
        seen += 1;
        p += 1;
    }
    view.byte_at(p) == Some(b']')
}

fn scan_quoted_string(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut cursor = cursor + 1;
    loop {
        match view.byte_at(cursor) {
            Some(b'"') => {
                return ScanResult {
                    end: cursor + 1,
                    is_error: false,
                };
            }
            Some(b'\\') => match view.byte_at(cursor + 1) {
                Some(b'\n') | Some(b'\r') | None => {
                    return ScanResult {
                        end: cursor,
                        is_error: true,
                    };
                }
                Some(_) => cursor += 2,
            },
            Some(b'\n') | Some(b'\r') | None => {
                return ScanResult {
                    end: cursor,
                    is_error: true,
                };
            }
            Some(_) => cursor += 1,
        }
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
            (cursor, state) = Cmake::step_batch(&mut view, cursor, state, &mut out);
            let mut saw_eof = false;
            let mut pos = cursor
                - out
                    .as_slice()
                    .iter()
                    .map(|s| match s {
                        LexStep::Token { len, .. } => *len,
                        _ => 0,
                    })
                    .sum::<u32>();
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
                    LexStep::Descend { .. } => unreachable!("CMake has no embedding"),
                }
            }
            if saw_eof {
                return tokens;
            }
        }
    }

    #[test]
    fn common_mapping_tokens() {
        let tokens = run(
            "cmake_minimum_required(VERSION 3.24)\nset(NAME \"tiny\")\n#[=[comment]=]\nmessage(${NAME})\n",
        );
        let kinds: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert!(kinds.contains(&kinds::KEYWORD));
        assert!(kinds.contains(&kinds::IDENT));
        assert!(kinds.contains(&kinds::OPEN_PAREN));
        assert!(kinds.contains(&kinds::CLOSE_PAREN));
        assert!(kinds.contains(&kinds::NUMBER));
        assert!(kinds.contains(&kinds::STRING));
        assert!(kinds.contains(&kinds::COMMENT));
    }

    #[test]
    fn coverage_is_contiguous() {
        let input = "project(tinyhl LANGUAGES C)\nadd_executable(app main.c)\nif(ON AND NOT OFF)\nendif()\n";
        let tokens = run(input);
        let mut pos = 0u32;
        for (_kind, offset, len, _flags) in &tokens {
            assert_eq!(*offset, pos, "gap or overlap before offset {pos}");
            pos += len;
        }
        assert_eq!(pos as usize, input.len());
    }

    #[test]
    fn bracket_arguments_span_lines() {
        let tokens = run("set(TEXT [=[hello\nworld]=])");
        assert!(tokens.iter().any(|t| t.0 == kinds::STRING && t.2 > 10));
    }

    #[test]
    fn unterminated_quote_is_line_local_error() {
        let tokens = run("set(A \"abc\nset(B ok)\n");
        assert!(
            tokens
                .iter()
                .any(|t| t.0 == kinds::STRING && (t.3 & flags::IS_ERROR) != 0)
        );
        assert!(tokens.iter().filter(|t| t.0 == kinds::KEYWORD).count() >= 2);
    }
}
