//! SQL lexer.
//!
//! State encoding: stateless. Every emitted token returns
//! [`LexState::INITIAL`] and carries [`STATE_BREAKPOINT`].
//!
//! This is a dialect-neutral highlighter, not a SQL parser. It recognizes the
//! common token shapes shared by ANSI SQL, PostgreSQL, MySQL, SQLite, and SQL
//! Server while keeping recovery local and byte-driven.

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::kw::{self, kw_lookup};
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

kw::kw_table! {
    static SQL_KWS = [
        (b"ADD", kinds::KEYWORD),
        (b"ALL", kinds::KEYWORD),
        (b"ALTER", kinds::KEYWORD),
        (b"AND", kinds::KEYWORD),
        (b"ANY", kinds::KEYWORD),
        (b"AS", kinds::KEYWORD),
        (b"ASC", kinds::KEYWORD),
        (b"AVG", kinds::KEYWORD),
        (b"BETWEEN", kinds::KEYWORD),
        (b"BIGINT", kinds::KEYWORD),
        (b"BINARY", kinds::KEYWORD),
        (b"BIT", kinds::KEYWORD),
        (b"BLOB", kinds::KEYWORD),
        (b"BOOL", kinds::KEYWORD),
        (b"BOOLEAN", kinds::KEYWORD),
        (b"BY", kinds::KEYWORD),
        (b"CASE", kinds::KEYWORD),
        (b"CAST", kinds::KEYWORD),
        (b"CHAR", kinds::KEYWORD),
        (b"CHECK", kinds::KEYWORD),
        (b"CLOB", kinds::KEYWORD),
        (b"COLUMN", kinds::KEYWORD),
        (b"COMMIT", kinds::KEYWORD),
        (b"CONSTRAINT", kinds::KEYWORD),
        (b"COUNT", kinds::KEYWORD),
        (b"CREATE", kinds::KEYWORD),
        (b"CROSS", kinds::KEYWORD),
        (b"CURRENT_DATE", kinds::KEYWORD),
        (b"CURRENT_TIME", kinds::KEYWORD),
        (b"CURRENT_TIMESTAMP", kinds::KEYWORD),
        (b"DATABASE", kinds::KEYWORD),
        (b"DATE", kinds::KEYWORD),
        (b"DECIMAL", kinds::KEYWORD),
        (b"DEFAULT", kinds::KEYWORD),
        (b"DELETE", kinds::KEYWORD),
        (b"DESC", kinds::KEYWORD),
        (b"DISTINCT", kinds::KEYWORD),
        (b"DOUBLE", kinds::KEYWORD),
        (b"DROP", kinds::KEYWORD),
        (b"ELSE", kinds::KEYWORD),
        (b"END", kinds::KEYWORD),
        (b"EXCEPT", kinds::KEYWORD),
        (b"EXISTS", kinds::KEYWORD),
        (b"FALSE", kinds::KEYWORD),
        (b"FLOAT", kinds::KEYWORD),
        (b"FOREIGN", kinds::KEYWORD),
        (b"FROM", kinds::KEYWORD),
        (b"FULL", kinds::KEYWORD),
        (b"GROUP", kinds::KEYWORD),
        (b"HAVING", kinds::KEYWORD),
        (b"IF", kinds::KEYWORD),
        (b"IN", kinds::KEYWORD),
        (b"INDEX", kinds::KEYWORD),
        (b"INNER", kinds::KEYWORD),
        (b"INSERT", kinds::KEYWORD),
        (b"INT", kinds::KEYWORD),
        (b"INTEGER", kinds::KEYWORD),
        (b"INTERSECT", kinds::KEYWORD),
        (b"INTERVAL", kinds::KEYWORD),
        (b"INTO", kinds::KEYWORD),
        (b"IS", kinds::KEYWORD),
        (b"JOIN", kinds::KEYWORD),
        (b"JSON", kinds::KEYWORD),
        (b"KEY", kinds::KEYWORD),
        (b"LEFT", kinds::KEYWORD),
        (b"LIKE", kinds::KEYWORD),
        (b"LIMIT", kinds::KEYWORD),
        (b"MAX", kinds::KEYWORD),
        (b"MIN", kinds::KEYWORD),
        (b"NOT", kinds::KEYWORD),
        (b"NULL", kinds::KEYWORD),
        (b"NUMERIC", kinds::KEYWORD),
        (b"OFFSET", kinds::KEYWORD),
        (b"ON", kinds::KEYWORD),
        (b"OR", kinds::KEYWORD),
        (b"ORDER", kinds::KEYWORD),
        (b"OUTER", kinds::KEYWORD),
        (b"PRIMARY", kinds::KEYWORD),
        (b"REAL", kinds::KEYWORD),
        (b"REFERENCES", kinds::KEYWORD),
        (b"RETURNING", kinds::KEYWORD),
        (b"RIGHT", kinds::KEYWORD),
        (b"ROLLBACK", kinds::KEYWORD),
        (b"SELECT", kinds::KEYWORD),
        (b"SET", kinds::KEYWORD),
        (b"SMALLINT", kinds::KEYWORD),
        (b"SUM", kinds::KEYWORD),
        (b"TABLE", kinds::KEYWORD),
        (b"TEXT", kinds::KEYWORD),
        (b"THEN", kinds::KEYWORD),
        (b"TIME", kinds::KEYWORD),
        (b"TIMESTAMP", kinds::KEYWORD),
        (b"TRUE", kinds::KEYWORD),
        (b"UNION", kinds::KEYWORD),
        (b"UNIQUE", kinds::KEYWORD),
        (b"UPDATE", kinds::KEYWORD),
        (b"VALUES", kinds::KEYWORD),
        (b"VARCHAR", kinds::KEYWORD),
        (b"VIEW", kinds::KEYWORD),
        (b"WHEN", kinds::KEYWORD),
        (b"WHERE", kinds::KEYWORD),
        (b"WITH", kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 17; // "CURRENT_TIMESTAMP"

pub(crate) struct Sql;

impl Lexer for Sql {
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
        b'-' if view.byte_at(cursor + 1) == Some(b'-') => {
            let end = scan::scan_line_comment(view, cursor);
            (kinds::COMMENT, end, false)
        }
        b'/' => match view.byte_at(cursor + 1) {
            Some(b'*') => {
                let r = scan::scan_block_comment(view, cursor);
                (kinds::COMMENT, r.end, r.is_error)
            }
            _ => {
                let (kind, end) = scan_operator(view, cursor, first);
                (kind, end, false)
            }
        },
        b'\'' => {
            let r = scan_sql_string(view, cursor);
            (kinds::STRING, r.end, r.is_error)
        }
        b'$' => {
            if let Some(r) = scan_dollar_quoted_string(view, cursor) {
                (kinds::STRING, r.end, r.is_error)
            } else {
                let (kind, end) = scan_operator(view, cursor, first);
                (kind, end, false)
            }
        }
        b'"' | b'`' => {
            let r = scan_quoted_ident(view, cursor, first);
            (kinds::IDENT, r.end, r.is_error)
        }
        b'[' => {
            if let Some(r) = scan_simple_bracket_ident(view, cursor) {
                (kinds::IDENT, r.end, r.is_error)
            } else {
                (kinds::OPEN_BRACKET, cursor + 1, false)
            }
        }
        b']' => (kinds::CLOSE_BRACKET, cursor + 1, false),
        b'0'..=b'9' => {
            let r = scan_sql_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b'.' if matches!(view.byte_at(cursor + 1), Some(b'0'..=b'9')) => {
            let r = scan_sql_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b if byteclass::is_ident_start(b) => {
            if let Some(r) = scan_prefixed_string(view, cursor, b) {
                (kinds::STRING, r.end, r.is_error)
            } else {
                classify_ident(view, cursor)
            }
        }
        b if b >= 0x80 => classify_unicode_ident(view, cursor),
        b'{' | b'}' | b'(' | b')' | b',' | b';' | b':' | b'.' | b'?' | b'@' | b'#' | b'~'
        | b'=' | b'!' | b'<' | b'>' | b'+' | b'-' | b'*' | b'%' | b'&' | b'|' | b'^' => {
            let (kind, end) = scan_operator(view, cursor, first);
            (kind, end, false)
        }
        _ => (kinds::ERROR, cursor + 1, true),
    }
}

fn classify_ident(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = scan_sql_ident_ascii(view, cursor);
    let len = (end - cursor) as usize;
    if len <= MAX_KW_LEN {
        let mut buf = [0u8; MAX_KW_LEN];
        if scan::copy_bytes(view, cursor, &mut buf[..len]) {
            for b in &mut buf[..len] {
                *b = b.to_ascii_uppercase();
            }
            if let Some(kind) = kw_lookup(SQL_KWS, &buf[..len]) {
                return (kind, end, false);
            }
        }
    }
    (kinds::IDENT, end, false)
}

fn classify_unicode_ident(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = scan::scan_xid_ident(view, cursor, true);
    if end > cursor {
        return (kinds::IDENT, end, false);
    }
    let len = scan::decoded_len_or_one(view, cursor);
    (kinds::ERROR, cursor + len, true)
}

fn scan_prefixed_string(view: &mut SourceView<'_>, cursor: u32, first: u8) -> Option<ScanResult> {
    let first = first.to_ascii_uppercase();
    match first {
        b'E' | b'N' | b'X' => {
            if view.byte_at(cursor + 1) == Some(b'\'') {
                return Some(scan_sql_string(view, cursor + 1));
            }
        }
        b'B' | b'R' => match view.byte_at(cursor + 1) {
            Some(b'\'') => return Some(scan_sql_string(view, cursor + 1)),
            Some(b'"') => {
                return Some(scan_quoted_with_doubled_close(view, cursor + 1, b'"', true));
            }
            _ => {}
        },
        b'U' => {
            if view.byte_at(cursor + 1) == Some(b'&') && view.byte_at(cursor + 2) == Some(b'\'') {
                return Some(scan_sql_string(view, cursor + 2));
            }
        }
        b'Q' => {
            if view.byte_at(cursor + 1) == Some(b'\'') {
                return Some(scan_quote_delimited_string(view, cursor + 2));
            }
        }
        _ => {}
    }

    if first == b'N'
        && matches!(view.byte_at(cursor + 1), Some(b'q' | b'Q'))
        && view.byte_at(cursor + 2) == Some(b'\'')
    {
        return Some(scan_quote_delimited_string(view, cursor + 3));
    }

    None
}

fn scan_sql_string(view: &mut SourceView<'_>, quote_cursor: u32) -> ScanResult {
    scan_quoted_with_doubled_close(view, quote_cursor, b'\'', true)
}

fn scan_quoted_ident(view: &mut SourceView<'_>, cursor: u32, open: u8) -> ScanResult {
    let close = if open == b'[' { b']' } else { open };
    scan_quoted_with_doubled_close(view, cursor, close, false)
}

fn scan_quoted_with_doubled_close(
    view: &mut SourceView<'_>,
    cursor: u32,
    close: u8,
    backslash_escape: bool,
) -> ScanResult {
    let mut end = cursor + 1;
    loop {
        match view.byte_at(end) {
            None => return scan_err(end),
            Some(b) if b == close => {
                if view.byte_at(end + 1) == Some(close) {
                    end += 2;
                } else {
                    return scan_ok(end + 1);
                }
            }
            Some(b'\\') if backslash_escape => match view.byte_at(end + 1) {
                Some(_) => end += 2,
                None => return scan_err(end + 1),
            },
            Some(_) => end += 1,
        }
    }
}

fn scan_quote_delimited_string(view: &mut SourceView<'_>, delimiter_cursor: u32) -> ScanResult {
    let Some(open) = view.byte_at(delimiter_cursor) else {
        return scan_err(delimiter_cursor);
    };
    if byteclass::is_whitespace(open) {
        return scan_err(delimiter_cursor + 1);
    }
    let close = match open {
        b'(' => b')',
        b'[' => b']',
        b'{' => b'}',
        b'<' => b'>',
        _ => open,
    };
    let mut end = delimiter_cursor + 1;
    loop {
        match view.byte_at(end) {
            None => return scan_err(end),
            Some(b) if b == close && view.byte_at(end + 1) == Some(b'\'') => {
                return scan_ok(end + 2);
            }
            Some(_) => end += 1,
        }
    }
}

fn scan_dollar_quoted_string(view: &mut SourceView<'_>, cursor: u32) -> Option<ScanResult> {
    let mut delimiter = [0u8; 34];
    delimiter[0] = b'$';
    let mut len = 1usize;
    let mut open_end = cursor + 1;

    match view.byte_at(open_end) {
        Some(b'$') => {
            delimiter[len] = b'$';
            len += 1;
            open_end += 1;
        }
        Some(b) if byteclass::is_ident_start(b) => {
            loop {
                let Some(b) = view.byte_at(open_end) else {
                    return None;
                };
                if byteclass::is_ident_cont(b) {
                    if len + 1 >= delimiter.len() {
                        return None;
                    }
                    delimiter[len] = b;
                    len += 1;
                    open_end += 1;
                } else {
                    break;
                }
            }
            if view.byte_at(open_end) != Some(b'$') {
                return None;
            }
            delimiter[len] = b'$';
            len += 1;
            open_end += 1;
        }
        _ => return None,
    }

    let delimiter = &delimiter[..len];
    let mut end = open_end;
    loop {
        match view.byte_at(end) {
            None => return Some(scan_err(end)),
            Some(b'$') if bytes_at(view, end, delimiter) => {
                return Some(scan_ok(end + len as u32));
            }
            Some(_) => end += 1,
        }
    }
}

fn bytes_at(view: &mut SourceView<'_>, cursor: u32, expected: &[u8]) -> bool {
    for (i, &b) in expected.iter().enumerate() {
        if view.byte_at(cursor + i as u32) != Some(b) {
            return false;
        }
    }
    true
}

fn scan_simple_bracket_ident(view: &mut SourceView<'_>, cursor: u32) -> Option<ScanResult> {
    let first = view.byte_at(cursor + 1)?;
    if !byteclass::is_ident_start(first) {
        return None;
    }
    let end = scan_sql_ident_ascii(view, cursor + 1);
    if view.byte_at(end) == Some(b']') {
        Some(scan_ok(end + 1))
    } else {
        None
    }
}

fn scan_sql_ident_ascii(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let mut i = (cursor - base) as usize;
        while i < page.len() && (byteclass::is_ident_cont(page[i]) || page[i] == b'$') {
            i += 1;
        }
        let consumed = i - (cursor - base) as usize;
        cursor += consumed as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

fn scan_sql_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    if view.byte_at(cursor) == Some(b'0') {
        match view.byte_at(cursor + 1) {
            Some(b'x') | Some(b'X') => {
                let end = scan_hex_digits_sql(view, cursor + 2);
                if end == cursor + 2 {
                    return scan_err(end);
                }
                return scan_ok(end);
            }
            _ => {}
        }
    }

    let mut end = cursor;
    let mut had_digits = false;

    if view.byte_at(end) == Some(b'.') {
        end += 1;
    } else {
        let before = end;
        end = scan_digits_sql(view, end);
        had_digits = end > before;
    }

    if view.byte_at(end) == Some(b'.') {
        end += 1;
    }

    let before_frac = end;
    end = scan_digits_sql(view, end);
    let had_frac_digits = end > before_frac;

    if !had_digits && !had_frac_digits {
        return scan_err(end.max(cursor + 1));
    }

    if matches!(view.byte_at(end), Some(b'e') | Some(b'E')) {
        let exp = end;
        end += 1;
        if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
            end += 1;
        }
        let digits = end;
        end = scan_digits_sql(view, end);
        if end == digits {
            return scan_err((exp + 1).max(end));
        }
    }

    scan_ok(end)
}

fn scan_ok(end: u32) -> ScanResult {
    ScanResult {
        end,
        is_error: false,
    }
}

fn scan_err(end: u32) -> ScanResult {
    ScanResult {
        end,
        is_error: true,
    }
}

fn scan_digits_sql(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    scan_while(view, cursor, |b| byteclass::is_digit(b) || b == b'_')
}

fn scan_hex_digits_sql(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    scan_while(view, cursor, |b| byteclass::is_hex_digit(b) || b == b'_')
}

fn scan_while(view: &mut SourceView<'_>, cursor: u32, pred: fn(u8) -> bool) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let mut i = (cursor - base) as usize;
        while i < page.len() && pred(page[i]) {
            i += 1;
        }
        let consumed = i - (cursor - base) as usize;
        cursor += consumed as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

fn scan_operator(view: &mut SourceView<'_>, cursor: u32, b0: u8) -> (u16, u32) {
    let b1 = view.byte_at(cursor + 1);
    let (kind, len) = match b0 {
        b'{' => (kinds::OPEN_BRACE, 1),
        b'}' => (kinds::CLOSE_BRACE, 1),
        b'(' => (kinds::OPEN_PAREN, 1),
        b')' => (kinds::CLOSE_PAREN, 1),
        b',' => (kinds::COMMA, 1),
        b';' => (kinds::SEMI, 1),
        b':' => match b1 {
            Some(b'=') => (kinds::COLON_EQ, 2),
            Some(b':') => (kinds::COLON, 2),
            _ => (kinds::COLON, 1),
        },
        b'.' => (kinds::DOT, 1),
        b'?' => match b1 {
            Some(b'&' | b'|' | b'-' | b'#') => (kinds::QUESTION, 2),
            _ => (kinds::QUESTION, 1),
        },
        b'@' => match b1 {
            Some(b'>' | b'@' | b'?') => (kinds::AT, 2),
            _ => (kinds::AT, 1),
        },
        b'#' => match b1 {
            Some(b'>') if view.byte_at(cursor + 2) == Some(b'>') => (kinds::HASH, 3),
            Some(b'#') => (kinds::HASH_HASH, 2),
            Some(b'>' | b'-') => (kinds::HASH, 2),
            _ => (kinds::HASH, 1),
        },
        b'~' => match b1 {
            Some(b'~') if view.byte_at(cursor + 2) == Some(b'*') => (kinds::TILDE, 3),
            Some(b'*' | b'=' | b'~') => (kinds::TILDE, 2),
            _ => (kinds::TILDE, 1),
        },
        b'=' => match b1 {
            Some(b'=') => (kinds::EQ_EQ, 2),
            Some(b'>') => (kinds::FAT_ARROW, 2),
            _ => (kinds::EQ, 1),
        },
        b'!' => match b1 {
            Some(b'~') if view.byte_at(cursor + 2) == Some(b'*') => (kinds::BANG, 3),
            Some(b'=') => (kinds::BANG_EQ, 2),
            Some(b'~') => (kinds::BANG, 2),
            _ => (kinds::BANG, 1),
        },
        b'<' => match b1 {
            Some(b'>') => (kinds::BANG_EQ, 2),
            Some(b'=') => (kinds::LT_EQ, 2),
            Some(b'<') => (kinds::SHL, 2),
            Some(b'@') => (kinds::LT, 2),
            _ => (kinds::LT, 1),
        },
        b'>' => match b1 {
            Some(b'=') => (kinds::GT_EQ, 2),
            Some(b'>') => (kinds::SHR, 2),
            _ => (kinds::GT, 1),
        },
        b'+' => match b1 {
            Some(b'=') => (kinds::PLUS_EQ, 2),
            _ => (kinds::PLUS, 1),
        },
        b'-' => match b1 {
            Some(b'>') if view.byte_at(cursor + 2) == Some(b'>') => (kinds::THIN_ARROW, 3),
            Some(b'=') => (kinds::MINUS_EQ, 2),
            Some(b'>') => (kinds::THIN_ARROW, 2),
            _ => (kinds::MINUS, 1),
        },
        b'*' => match b1 {
            Some(b'=') => (kinds::STAR_EQ, 2),
            _ => (kinds::STAR, 1),
        },
        b'/' => match b1 {
            Some(b'=') => (kinds::SLASH_EQ, 2),
            _ => (kinds::SLASH, 1),
        },
        b'%' => match b1 {
            Some(b'=') => (kinds::PERCENT_EQ, 2),
            _ => (kinds::PERCENT, 1),
        },
        b'&' => match b1 {
            Some(b'&') => (kinds::AMP_AMP, 2),
            Some(b'=') => (kinds::AMP_EQ, 2),
            _ => (kinds::AMP, 1),
        },
        b'|' => match b1 {
            Some(b'|') => (kinds::PIPE_PIPE, 2),
            Some(b'=') => (kinds::PIPE_EQ, 2),
            _ => (kinds::PIPE, 1),
        },
        b'^' => match b1 {
            Some(b'=') => (kinds::CARET_EQ, 2),
            _ => (kinds::CARET, 1),
        },
        b'$' => (kinds::DOLLAR, 1),
        _ => (kinds::ERROR, 1),
    };
    (kind, cursor + len)
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
            let prev = cursor;
            (cursor, state) = Sql::step_batch(&mut view, cursor, state, &mut out);
            let mut pos = prev;
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
                    LexStep::Descend { .. } => unreachable!("SQL has no embedding"),
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
    fn simple_select() {
        let tokens = run("select id, name from users where active = true and score >= 10;");
        assert_eq!(
            kinds_of(&tokens),
            vec![
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::IDENT,
                kinds::COMMA,
                kinds::WHITESPACE,
                kinds::IDENT,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::IDENT,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::IDENT,
                kinds::WHITESPACE,
                kinds::EQ,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::IDENT,
                kinds::WHITESPACE,
                kinds::GT_EQ,
                kinds::WHITESPACE,
                kinds::NUMBER,
                kinds::SEMI,
            ]
        );
    }

    #[test]
    fn comments_strings_and_quoted_identifiers() {
        let tokens = run("-- note\nSELECT 'it''s', E'a\\'b', \"odd name\", `mysql`, [mssql]");
        assert_eq!(
            kinds_of(&tokens),
            vec![
                kinds::COMMENT,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::STRING,
                kinds::COMMA,
                kinds::WHITESPACE,
                kinds::STRING,
                kinds::COMMA,
                kinds::WHITESPACE,
                kinds::IDENT,
                kinds::COMMA,
                kinds::WHITESPACE,
                kinds::IDENT,
                kinds::COMMA,
                kinds::WHITESPACE,
                kinds::IDENT,
            ]
        );
    }

    #[test]
    fn dollar_quoted_strings() {
        let tokens = run("$$a'b$$ $tag$body$tag$ $x$unterminated");
        assert_eq!(
            kinds_of(&tokens),
            vec![
                kinds::STRING,
                kinds::WHITESPACE,
                kinds::STRING,
                kinds::WHITESPACE,
                kinds::STRING,
            ]
        );
        assert_eq!(tokens[4].0, kinds::STRING);
        assert_ne!(tokens[4].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn operators_and_numbers() {
        let tokens = run("a<>b and c!=d or x->>'k' = 0x2a and y::int := .5e+2");
        let kinds = kinds_of(&tokens);
        assert!(kinds.contains(&kinds::BANG_EQ));
        assert!(kinds.contains(&kinds::THIN_ARROW));
        assert!(kinds.contains(&kinds::COLON_EQ));
        assert!(kinds.contains(&kinds::NUMBER));
    }

    #[test]
    fn coverage_is_contiguous_and_breakpointed() {
        let input = "WITH q AS (SELECT 1. FROM t /* c */ WHERE x LIKE '%a%') SELECT * FROM q";
        let tokens = run(input);
        let mut pos = 0u32;
        for (_kind, offset, len, flags) in &tokens {
            assert_eq!(*offset, pos, "gap or overlap before offset {pos}");
            assert_ne!(flags & flags::STATE_BREAKPOINT, 0, "missing breakpoint");
            pos += len;
        }
        assert_eq!(pos as usize, input.len());
    }
}
