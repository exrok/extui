//! C lexer.

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::kw::{self, kw_lookup};
use crate::lex::scan;
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

kw::kw_table! {
    static C_KWS = [
        (b"_Alignas", kinds::KEYWORD),
        (b"_Alignof", kinds::KEYWORD),
        (b"_Atomic", kinds::KEYWORD),
        (b"_Bool", kinds::KEYWORD),
        (b"_Complex", kinds::KEYWORD),
        (b"_Generic", kinds::KEYWORD),
        (b"_Imaginary", kinds::KEYWORD),
        (b"_Noreturn", kinds::KEYWORD),
        (b"_Static_assert", kinds::KEYWORD),
        (b"_Thread_local", kinds::KEYWORD),
        (b"__func__", kinds::KEYWORD),
        (b"auto", kinds::KEYWORD),
        (b"break", kinds::KEYWORD),
        (b"case", kinds::KEYWORD),
        (b"char", kinds::KEYWORD),
        (b"const", kinds::KEYWORD),
        (b"continue", kinds::KEYWORD),
        (b"default", kinds::KEYWORD),
        (b"do", kinds::KEYWORD),
        (b"double", kinds::KEYWORD),
        (b"else", kinds::KEYWORD),
        (b"enum", kinds::KEYWORD),
        (b"extern", kinds::KEYWORD),
        (b"float", kinds::KEYWORD),
        (b"for", kinds::KEYWORD),
        (b"goto", kinds::KEYWORD),
        (b"if", kinds::KEYWORD),
        (b"inline", kinds::KEYWORD),
        (b"int", kinds::KEYWORD),
        (b"long", kinds::KEYWORD),
        (b"register", kinds::KEYWORD),
        (b"restrict", kinds::KEYWORD),
        (b"return", kinds::KEYWORD),
        (b"short", kinds::KEYWORD),
        (b"signed", kinds::KEYWORD),
        (b"sizeof", kinds::KEYWORD),
        (b"static", kinds::KEYWORD),
        (b"struct", kinds::KEYWORD),
        (b"switch", kinds::KEYWORD),
        (b"typedef", kinds::KEYWORD),
        (b"union", kinds::KEYWORD),
        (b"unsigned", kinds::KEYWORD),
        (b"void", kinds::KEYWORD),
        (b"volatile", kinds::KEYWORD),
        (b"while", kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 14; // "_Static_assert"

pub(crate) struct C;

impl Lexer for C {
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
        b'/' => match view.byte_at(cursor + 1) {
            Some(b'/') => {
                let end = scan::scan_line_comment(view, cursor);
                (kinds::COMMENT, end, false)
            }
            Some(b'*') => {
                let r = scan::scan_block_comment(view, cursor);
                (kinds::COMMENT, r.end, r.is_error)
            }
            _ => {
                let (kind, end) = scan_operator(view, cursor, b'/');
                (kind, end, false)
            }
        },
        b'"' => {
            let r = scan::scan_c_string(view, cursor);
            (kinds::STRING, r.end, r.is_error)
        }
        b'\'' => {
            let r = scan::scan_c_char(view, cursor);
            (kinds::CHAR, r.end, r.is_error)
        }
        b'L' | b'U' => match view.byte_at(cursor + 1) {
            Some(b'"') => {
                let r = scan::scan_c_string(view, cursor + 1);
                (kinds::STRING, r.end, r.is_error)
            }
            Some(b'\'') => {
                let r = scan::scan_c_char(view, cursor + 1);
                (kinds::CHAR, r.end, r.is_error)
            }
            _ => classify_ident(view, cursor),
        },
        b'u' => match view.byte_at(cursor + 1) {
            Some(b'"') => {
                let r = scan::scan_c_string(view, cursor + 1);
                (kinds::STRING, r.end, r.is_error)
            }
            Some(b'\'') => {
                let r = scan::scan_c_char(view, cursor + 1);
                (kinds::CHAR, r.end, r.is_error)
            }
            Some(b'8') if view.byte_at(cursor + 2) == Some(b'"') => {
                let r = scan::scan_c_string(view, cursor + 2);
                (kinds::STRING, r.end, r.is_error)
            }
            _ => classify_ident(view, cursor),
        },
        b'.' => match view.byte_at(cursor + 1) {
            Some(b) if byteclass::is_digit(b) => {
                let r = scan::scan_c_number(view, cursor);
                (kinds::NUMBER, r.end, r.is_error)
            }
            _ => {
                let (kind, end) = scan_operator(view, cursor, b'.');
                (kind, end, false)
            }
        },
        b'0'..=b'9' => {
            let r = scan::scan_c_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b if byteclass::is_ident_start(b) => classify_ident(view, cursor),
        b'+' | b'-' | b'*' | b'%' | b'=' | b'<' | b'>' | b'!' | b'&' | b'|' | b'^' | b'~'
        | b'?' | b':' | b',' | b';' | b'(' | b')' | b'{' | b'}' | b'[' | b']' | b'#' => {
            let (kind, end) = scan_operator(view, cursor, first);
            (kind, end, false)
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
            if let Some(kind) = kw_lookup(C_KWS, &buf[..len]) {
                return (kind, end, false);
            }
        }
    }
    (kinds::IDENT, end, false)
}

fn scan_operator(view: &mut SourceView<'_>, cursor: u32, b0: u8) -> (u16, u32) {
    let b1 = view.byte_at(cursor + 1);
    let b2 = view.byte_at(cursor + 2);

    let three = match (b0, b1, b2) {
        (b'<', Some(b'<'), Some(b'=')) => Some(kinds::SHL_EQ),
        (b'>', Some(b'>'), Some(b'=')) => Some(kinds::SHR_EQ),
        (b'.', Some(b'.'), Some(b'.')) => Some(kinds::ELLIPSIS),
        _ => None,
    };
    if let Some(k) = three {
        return (k, cursor + 3);
    }

    if let Some(b1) = b1 {
        let two = match (b0, b1) {
            (b'=', b'=') => Some(kinds::EQ_EQ),
            (b'!', b'=') => Some(kinds::BANG_EQ),
            (b'<', b'=') => Some(kinds::LT_EQ),
            (b'>', b'=') => Some(kinds::GT_EQ),
            (b'&', b'&') => Some(kinds::AMP_AMP),
            (b'|', b'|') => Some(kinds::PIPE_PIPE),
            (b'<', b'<') => Some(kinds::SHL),
            (b'>', b'>') => Some(kinds::SHR),
            (b'+', b'+') => Some(kinds::PLUS_PLUS),
            (b'-', b'-') => Some(kinds::MINUS_MINUS),
            (b'+', b'=') => Some(kinds::PLUS_EQ),
            (b'-', b'=') => Some(kinds::MINUS_EQ),
            (b'*', b'=') => Some(kinds::STAR_EQ),
            (b'/', b'=') => Some(kinds::SLASH_EQ),
            (b'%', b'=') => Some(kinds::PERCENT_EQ),
            (b'&', b'=') => Some(kinds::AMP_EQ),
            (b'|', b'=') => Some(kinds::PIPE_EQ),
            (b'^', b'=') => Some(kinds::CARET_EQ),
            (b'-', b'>') => Some(kinds::THIN_ARROW),
            (b'#', b'#') => Some(kinds::HASH_HASH),
            (b'<', b':') => Some(kinds::LT_COLON),
            (b':', b'>') => Some(kinds::COLON_GT),
            (b'<', b'%') => Some(kinds::LT_PERCENT),
            (b'%', b'>') => Some(kinds::PERCENT_GT),
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
        b'=' => kinds::EQ,
        b'<' => kinds::LT,
        b'>' => kinds::GT,
        b'!' => kinds::BANG,
        b'&' => kinds::AMP,
        b'|' => kinds::PIPE,
        b'^' => kinds::CARET,
        b'~' => kinds::TILDE,
        b'?' => kinds::QUESTION,
        b':' => kinds::COLON,
        b',' => kinds::COMMA,
        b';' => kinds::SEMI,
        b'(' => kinds::OPEN_PAREN,
        b')' => kinds::CLOSE_PAREN,
        b'{' => kinds::OPEN_BRACE,
        b'}' => kinds::CLOSE_BRACE,
        b'[' => kinds::OPEN_BRACKET,
        b']' => kinds::CLOSE_BRACKET,
        b'#' => kinds::HASH,
        b'.' => kinds::DOT,
        _ => kinds::ERROR,
    };
    (one, cursor + 1)
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
            (cursor, state) = C::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Eof => {
                        saw_eof = true;
                    }
                    LexStep::Descend { .. } => unreachable!("C has no embedding"),
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
    fn simple_function() {
        let tokens = run("int main() { return 0; }");
        assert_eq!(
            kinds_of(&tokens),
            vec![
                kinds::KEYWORD,     // int
                kinds::WHITESPACE,  //
                kinds::IDENT,       // main
                kinds::OPEN_PAREN,  // (
                kinds::CLOSE_PAREN, // )
                kinds::WHITESPACE,  //
                kinds::OPEN_BRACE,  // {
                kinds::WHITESPACE,  //
                kinds::KEYWORD,     // return
                kinds::WHITESPACE,  //
                kinds::NUMBER,      // 0
                kinds::SEMI,        // ;
                kinds::WHITESPACE,  //
                kinds::CLOSE_BRACE, // }
            ]
        );
    }

    #[test]
    fn keywords_covering_the_table() {
        for kw in [
            "_Alignas",
            "_Alignof",
            "_Atomic",
            "_Bool",
            "_Complex",
            "_Generic",
            "_Imaginary",
            "_Noreturn",
            "_Static_assert",
            "_Thread_local",
            "__func__",
            "auto",
            "break",
            "case",
            "char",
            "const",
            "continue",
            "default",
            "do",
            "double",
            "else",
            "enum",
            "extern",
            "float",
            "for",
            "goto",
            "if",
            "inline",
            "int",
            "long",
            "register",
            "restrict",
            "return",
            "short",
            "signed",
            "sizeof",
            "static",
            "struct",
            "switch",
            "typedef",
            "union",
            "unsigned",
            "void",
            "volatile",
            "while",
        ] {
            let tokens = run(kw);
            assert_eq!(tokens.len(), 1, "{kw:?}");
            assert_eq!(tokens[0].0, kinds::KEYWORD, "{kw:?} should be KEYWORD");
        }
    }

    #[test]
    fn non_keyword_idents() {
        for ident in [
            "foo", "_bar", "baz123", "Long", "Intx", "uU", "u9", "u8x", "L9",
        ] {
            let tokens = run(ident);
            assert_eq!(tokens.len(), 1, "{ident:?}");
            assert_eq!(tokens[0].0, kinds::IDENT, "{ident:?}");
        }
    }

    #[test]
    fn strings_with_prefixes() {
        for (input, expected_len) in [
            (r#""hi""#, 4),
            (r#"L"wide""#, 7),
            (r#"u"u16""#, 6),
            (r#"U"u32""#, 6),
            (r#"u8"utf8""#, 8),
        ] {
            let tokens = run(input);
            assert_eq!(tokens.len(), 1, "{input:?}");
            assert_eq!(tokens[0].0, kinds::STRING, "{input:?}");
            assert_eq!(tokens[0].2, expected_len as u32, "{input:?}");
            assert_eq!(tokens[0].3 & flags::IS_ERROR, 0, "{input:?}");
        }
    }

    #[test]
    fn unterminated_string_flags_error() {
        let tokens = run(r#""oops"#);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, kinds::STRING);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn char_literals() {
        for (input, expected_kind) in [
            (r#"'x'"#, kinds::CHAR),
            (r#"'\n'"#, kinds::CHAR),
            (r#"L'w'"#, kinds::CHAR),
            (r#"u'c'"#, kinds::CHAR),
            (r#"U'C'"#, kinds::CHAR),
        ] {
            let tokens = run(input);
            assert_eq!(tokens.len(), 1, "{input:?}");
            assert_eq!(tokens[0].0, expected_kind, "{input:?}");
        }
    }

    #[test]
    fn numbers_mixed() {
        let tokens = run("0 42 0xFF 0b1010 0777 1.5 1e10 0x1.fp3 1.0f 123ull");
        let kinds = kinds_of(&tokens);
        let numbers: Vec<u16> = kinds
            .iter()
            .copied()
            .filter(|k| *k == kinds::NUMBER)
            .collect();
        assert_eq!(numbers.len(), 10);
    }

    #[test]
    fn bad_number_is_error() {
        let tokens = run("0x");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, kinds::NUMBER);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn line_comment() {
        let tokens = run("// hello\nint");
        assert_eq!(
            kinds_of(&tokens),
            vec![kinds::COMMENT, kinds::WHITESPACE, kinds::KEYWORD]
        );
    }

    #[test]
    fn block_comment_single_token() {
        let tokens = run("/* line 1\nline 2 */ int");
        assert_eq!(
            kinds_of(&tokens),
            vec![kinds::COMMENT, kinds::WHITESPACE, kinds::KEYWORD]
        );
        assert_eq!(tokens[0].2, "/* line 1\nline 2 */".len() as u32);
    }

    #[test]
    fn unterminated_block_comment_flags_error() {
        let tokens = run("/* never ends");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, kinds::COMMENT);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn operators_greedy() {
        let src = "a<<=b >>=c ->d ==e !=f ...g ##h";
        let tokens = run(src);
        let three = [kinds::SHL_EQ, kinds::SHR_EQ, kinds::ELLIPSIS];
        let two = [
            kinds::THIN_ARROW,
            kinds::EQ_EQ,
            kinds::BANG_EQ,
            kinds::HASH_HASH,
        ];
        let three_count = tokens
            .iter()
            .filter(|t| three.contains(&t.0))
            .inspect(|t| assert_eq!(t.2, 3))
            .count();
        let two_count = tokens
            .iter()
            .filter(|t| two.contains(&t.0))
            .inspect(|t| assert_eq!(t.2, 2))
            .count();
        assert_eq!(three_count, 3, "expected <<= >>= ... as 3-char ops");
        assert_eq!(two_count, 4, "expected -> == != ## as 2-char ops");
    }

    #[test]
    fn digraphs() {
        let tokens = run("<: :> <% %>");
        let kinds = kinds_of(&tokens);
        assert_eq!(
            kinds,
            vec![
                kinds::LT_COLON,
                kinds::WHITESPACE,
                kinds::COLON_GT,
                kinds::WHITESPACE,
                kinds::LT_PERCENT,
                kinds::WHITESPACE,
                kinds::PERCENT_GT,
            ]
        );
        let digraphs = [
            kinds::LT_COLON,
            kinds::COLON_GT,
            kinds::LT_PERCENT,
            kinds::PERCENT_GT,
        ];
        for t in tokens.iter().filter(|t| digraphs.contains(&t.0)) {
            assert_eq!(t.2, 2, "digraph should be 2 bytes");
        }
    }

    #[test]
    fn hash_as_punctuation() {
        let tokens = run("#include");
        assert_eq!(kinds_of(&tokens), vec![kinds::HASH, kinds::IDENT]);
    }

    #[test]
    fn error_on_unknown_byte() {
        let tokens = run("@");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, kinds::ERROR);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn coverage_is_contiguous() {
        let input = r#"int main() { char *s = "hi"; /* c */ return 0; }"#;
        let tokens = run(input);
        let mut pos = 0u32;
        for (_kind, offset, len, _flags) in &tokens {
            assert_eq!(*offset, pos, "gap or overlap before offset {pos}");
            pos += len;
        }
        assert_eq!(pos as usize, input.len());
    }

    #[test]
    fn every_token_has_state_breakpoint() {
        let input = r#"int x = 0x1p3; /* comment */ "str"; 'c';"#;
        let tokens = run(input);
        for t in &tokens {
            assert_ne!(t.3 & flags::STATE_BREAKPOINT, 0, "missing STATE_BREAKPOINT");
        }
    }
}
