//! Protocol Buffers schema lexer.
//!
//! State encoding: stateless. Every emitted token returns
//! [`LexState::INITIAL`] and carries [`STATE_BREAKPOINT`].
//!
//! This highlights `.proto` definitions, not protobuf text-format data. The
//! lexer recognizes proto2/proto3 declaration keywords, scalar field types,
//! identifiers, C-style comments, quoted strings, numeric literals, and the
//! punctuation used by messages, enums, options, maps, services, and custom
//! options.
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
    static PROTOBUF_KWS = [
        (b"bool", kinds::KEYWORD),
        (b"bytes", kinds::KEYWORD),
        (b"double", kinds::KEYWORD),
        (b"edition", kinds::KEYWORD),
        (b"enum", kinds::KEYWORD),
        (b"extend", kinds::KEYWORD),
        (b"extensions", kinds::KEYWORD),
        (b"false", kinds::KEYWORD),
        (b"fixed32", kinds::KEYWORD),
        (b"fixed64", kinds::KEYWORD),
        (b"float", kinds::KEYWORD),
        (b"group", kinds::KEYWORD),
        (b"import", kinds::KEYWORD),
        (b"inf", kinds::KEYWORD),
        (b"int32", kinds::KEYWORD),
        (b"int64", kinds::KEYWORD),
        (b"map", kinds::KEYWORD),
        (b"max", kinds::KEYWORD),
        (b"message", kinds::KEYWORD),
        (b"nan", kinds::KEYWORD),
        (b"oneof", kinds::KEYWORD),
        (b"option", kinds::KEYWORD),
        (b"optional", kinds::KEYWORD),
        (b"package", kinds::KEYWORD),
        (b"proto2", kinds::KEYWORD),
        (b"proto3", kinds::KEYWORD),
        (b"public", kinds::KEYWORD),
        (b"repeated", kinds::KEYWORD),
        (b"required", kinds::KEYWORD),
        (b"reserved", kinds::KEYWORD),
        (b"returns", kinds::KEYWORD),
        (b"rpc", kinds::KEYWORD),
        (b"service", kinds::KEYWORD),
        (b"sfixed32", kinds::KEYWORD),
        (b"sfixed64", kinds::KEYWORD),
        (b"sint32", kinds::KEYWORD),
        (b"sint64", kinds::KEYWORD),
        (b"stream", kinds::KEYWORD),
        (b"string", kinds::KEYWORD),
        (b"syntax", kinds::KEYWORD),
        (b"to", kinds::KEYWORD),
        (b"true", kinds::KEYWORD),
        (b"uint32", kinds::KEYWORD),
        (b"uint64", kinds::KEYWORD),
        (b"weak", kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 10; // "extensions"

pub(crate) struct Protobuf;

impl Lexer for Protobuf {
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
        b'/' => match view.byte_at(cursor + 1) {
            Some(b'/') => {
                let end = scan::scan_line_comment(view, cursor);
                (kinds::COMMENT, end, false)
            }
            Some(b'*') => {
                let r = scan::scan_block_comment(view, cursor);
                (kinds::COMMENT, r.end, r.is_error)
            }
            _ => (kinds::SLASH, cursor + 1, false),
        },
        b'"' | b'\'' => {
            let r = scan_proto_string(view, cursor, first);
            (kinds::STRING, r.end, r.is_error)
        }
        b'.' if matches!(view.byte_at(cursor + 1), Some(b'0'..=b'9')) => {
            let r = scan::scan_c_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b'.' => (kinds::DOT, cursor + 1, false),
        b'0'..=b'9' => {
            let r = scan::scan_c_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b if byteclass::is_ident_start(b) => classify_ident(view, cursor),
        b if b >= 0x80 => classify_unicode_ident(view, cursor),
        b'{' => (kinds::OPEN_BRACE, cursor + 1, false),
        b'}' => (kinds::CLOSE_BRACE, cursor + 1, false),
        b'(' => (kinds::OPEN_PAREN, cursor + 1, false),
        b')' => (kinds::CLOSE_PAREN, cursor + 1, false),
        b'[' => (kinds::OPEN_BRACKET, cursor + 1, false),
        b']' => (kinds::CLOSE_BRACKET, cursor + 1, false),
        b',' => (kinds::COMMA, cursor + 1, false),
        b';' => (kinds::SEMI, cursor + 1, false),
        b':' => (kinds::COLON, cursor + 1, false),
        b'=' => (kinds::EQ, cursor + 1, false),
        b'<' => (kinds::LT, cursor + 1, false),
        b'>' => (kinds::GT, cursor + 1, false),
        b'+' => (kinds::PLUS, cursor + 1, false),
        b'-' => (kinds::MINUS, cursor + 1, false),
        b'*' => (kinds::STAR, cursor + 1, false),
        b'?' => (kinds::QUESTION, cursor + 1, false),
        b'!' => (kinds::BANG, cursor + 1, false),
        _ => (kinds::ERROR, cursor + 1, true),
    }
}

fn classify_ident(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = scan::scan_ident_ascii(view, cursor);
    let len = (end - cursor) as usize;
    if len <= MAX_KW_LEN {
        let mut buf = [0u8; MAX_KW_LEN];
        if scan::copy_bytes(view, cursor, &mut buf[..len]) {
            if let Some(kind) = kw_lookup(PROTOBUF_KWS, &buf[..len]) {
                return (kind, end, false);
            }
        }
    }
    (kinds::IDENT, end, false)
}

fn classify_unicode_ident(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let len = scan::decoded_len_or_one(view, cursor);
    (kinds::ERROR, cursor + len, true)
}

fn scan_proto_string(view: &mut SourceView<'_>, cursor: u32, quote: u8) -> ScanResult {
    let mut end = cursor + 1;
    loop {
        match view.byte_at(end) {
            None => return err(end),
            Some(b) if b == quote => return ok(end + 1),
            Some(b'\\') => match view.byte_at(end + 1) {
                Some(b'\n') | Some(b'\r') | None => return err(end + 1),
                Some(_) => end += 2,
            },
            Some(b'\n') | Some(b'\r') => return err(end),
            Some(_) => end += 1,
        }
    }
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
            (cursor, state) = Protobuf::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("Protobuf has no embedding"),
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
    fn simple_schema() {
        let tokens = run(r#"syntax = "proto3"; message User { string name = 1; }"#);
        assert_eq!(
            kinds_of(&tokens),
            vec![
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::EQ,
                kinds::WHITESPACE,
                kinds::STRING,
                kinds::SEMI,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::IDENT,
                kinds::WHITESPACE,
                kinds::OPEN_BRACE,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::IDENT,
                kinds::WHITESPACE,
                kinds::EQ,
                kinds::WHITESPACE,
                kinds::NUMBER,
                kinds::SEMI,
                kinds::WHITESPACE,
                kinds::CLOSE_BRACE,
            ]
        );
    }

    #[test]
    fn keywords_cover_proto2_and_proto3() {
        for kw in [
            "bool",
            "bytes",
            "double",
            "edition",
            "enum",
            "extend",
            "extensions",
            "false",
            "fixed32",
            "fixed64",
            "float",
            "group",
            "import",
            "inf",
            "int32",
            "int64",
            "map",
            "max",
            "message",
            "nan",
            "oneof",
            "option",
            "optional",
            "package",
            "proto2",
            "proto3",
            "public",
            "repeated",
            "required",
            "reserved",
            "returns",
            "rpc",
            "service",
            "sfixed32",
            "sfixed64",
            "sint32",
            "sint64",
            "stream",
            "string",
            "syntax",
            "to",
            "true",
            "uint32",
            "uint64",
            "weak",
        ] {
            let tokens = run(kw);
            assert_eq!(tokens.len(), 1, "{kw:?}");
            assert_eq!(tokens[0].0, kinds::KEYWORD, "{kw:?} should be KEYWORD");
        }
    }

    #[test]
    fn comments_strings_and_numbers() {
        let tokens = run("// c\n/* block */ 's\\'x' \"path\" 0x10 .5 1.5e-2");
        assert_eq!(
            kinds_of(&tokens),
            vec![
                kinds::COMMENT,
                kinds::WHITESPACE,
                kinds::COMMENT,
                kinds::WHITESPACE,
                kinds::STRING,
                kinds::WHITESPACE,
                kinds::STRING,
                kinds::WHITESPACE,
                kinds::NUMBER,
                kinds::WHITESPACE,
                kinds::NUMBER,
                kinds::WHITESPACE,
                kinds::NUMBER,
            ]
        );
        for t in tokens
            .iter()
            .filter(|t| t.0 == kinds::STRING || t.0 == kinds::NUMBER)
        {
            assert_eq!(t.3 & flags::IS_ERROR, 0, "{t:?}");
        }
    }

    #[test]
    fn options_maps_and_rpc_punctuation() {
        let tokens =
            run("map<string, bytes> labels = 1 [(demo.opt).x = true]; rpc Get (A) returns (B);");
        let kinds = kinds_of(&tokens);
        for required in [
            kinds::LT,
            kinds::GT,
            kinds::COMMA,
            kinds::OPEN_BRACKET,
            kinds::CLOSE_BRACKET,
            kinds::OPEN_PAREN,
            kinds::CLOSE_PAREN,
            kinds::DOT,
            kinds::EQ,
            kinds::SEMI,
        ] {
            assert!(kinds.contains(&required), "missing kind {required}");
        }
    }

    #[test]
    fn unterminated_string_flags_error_without_swallowing_line() {
        let tokens = run("\"bad\nmessage M {}");
        assert_eq!(tokens[0].0, kinds::STRING);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
        assert_eq!(tokens[0].2, 4);
        assert!(tokens.iter().any(|t| t.0 == kinds::KEYWORD));
    }

    #[test]
    fn non_ascii_identifier_start_is_local_error() {
        let tokens = run("café");
        assert_eq!(tokens[0].0, kinds::IDENT);
        assert_eq!(tokens[1].0, kinds::ERROR);
        assert_ne!(tokens[1].3 & flags::IS_ERROR, 0);
    }
}
