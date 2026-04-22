//! Local-kind constants used by [`crate::Token::local_kind`].
//!
//! A single shared namespace lets themes match a token without branching
//! on language first. Every language that emits the same shape of token
//! reuses the same constant. Punctuation and operators each have their
//! own constant so a theme can style them without re-reading source
//! bytes.
//!
//! C's `->` is [`THIN_ARROW`] and TypeScript's `=>` is [`FAT_ARROW`].

// Trivia.

/// Run of ASCII whitespace between meaningful tokens.
pub const WHITESPACE: u16 = 0;
/// Line or block comment.
pub const COMMENT: u16 = 1;
/// Documentation comment (e.g. Rust `///`, `//!`, `/** */`).
pub const DOC_COMMENT: u16 = 2;

// Literals.

/// Quoted string literal.
pub const STRING: u16 = 10;
/// Backtick-quoted template literal, including any embedded `${…}` expressions.
pub const TEMPLATE_STRING: u16 = 11;
/// Numeric literal, including any radix prefix and type suffix.
pub const NUMBER: u16 = 12;
/// Character literal.
pub const CHAR: u16 = 13;
/// Regular expression literal `/…/flags`.
pub const REGEX: u16 = 14;

// Identifiers and keywords.

/// Non-keyword identifier.
pub const IDENT: u16 = 20;
/// Reserved keyword.
pub const KEYWORD: u16 = 21;
/// Lifetime or label, e.g. `'a`, `'static`.
pub const LIFETIME: u16 = 22;

/// Byte the lexer could not classify, or literal that failed recovery.
pub const ERROR: u16 = 30;

// Grouping delimiters.

/// `{`
pub const OPEN_BRACE: u16 = 40;
/// `}`
pub const CLOSE_BRACE: u16 = 41;
/// `(`
pub const OPEN_PAREN: u16 = 42;
/// `)`
pub const CLOSE_PAREN: u16 = 43;
/// `[`
pub const OPEN_BRACKET: u16 = 44;
/// `]`
pub const CLOSE_BRACKET: u16 = 45;

// Single-byte punctuation.

/// `,`
pub const COMMA: u16 = 50;
/// `;`
pub const SEMI: u16 = 51;
/// `:`
pub const COLON: u16 = 52;
/// `.`
pub const DOT: u16 = 53;
/// `?`
pub const QUESTION: u16 = 54;
/// `@`
pub const AT: u16 = 55;
/// `#`
pub const HASH: u16 = 56;
/// `~`
pub const TILDE: u16 = 57;
/// `$`
pub const DOLLAR: u16 = 58;
/// `!`
pub const BANG: u16 = 59;
/// `` ` `` (Markdown inline-code run that did not form a valid code span).
pub const BACKTICK: u16 = 60;

// Single-byte operators.

/// `=`
pub const EQ: u16 = 70;
/// `<`
pub const LT: u16 = 71;
/// `>`
pub const GT: u16 = 72;
/// `+`
pub const PLUS: u16 = 73;
/// `-`
pub const MINUS: u16 = 74;
/// `*`
pub const STAR: u16 = 75;
/// `/`
pub const SLASH: u16 = 76;
/// `%`
pub const PERCENT: u16 = 77;
/// `&`
pub const AMP: u16 = 78;
/// `|`
pub const PIPE: u16 = 79;
/// `^`
pub const CARET: u16 = 80;

// Multi-byte operators.

/// `==`
pub const EQ_EQ: u16 = 100;
/// `===`
pub const EQ_EQ_EQ: u16 = 101;
/// `!=`
pub const BANG_EQ: u16 = 102;
/// `!==`
pub const BANG_EQ_EQ: u16 = 103;
/// `<=`
pub const LT_EQ: u16 = 104;
/// `>=`
pub const GT_EQ: u16 = 105;
/// `&&`
pub const AMP_AMP: u16 = 106;
/// `||`
pub const PIPE_PIPE: u16 = 107;
/// `??` (nullish coalescing).
pub const QUESTION_QUESTION: u16 = 108;
/// `?.` (optional chaining).
pub const OPTIONAL_CHAIN: u16 = 109;
/// `->` (C struct-member-through-pointer, Rust return-type arrow).
pub const THIN_ARROW: u16 = 110;
/// `=>` (TypeScript arrow function).
pub const FAT_ARROW: u16 = 111;
/// `...`
pub const ELLIPSIS: u16 = 112;
/// `<<`
pub const SHL: u16 = 113;
/// `>>`
pub const SHR: u16 = 114;
/// `>>>` (unsigned right shift).
pub const USHR: u16 = 115;
/// `++`
pub const PLUS_PLUS: u16 = 116;
/// `--`
pub const MINUS_MINUS: u16 = 117;
/// `**`
pub const STAR_STAR: u16 = 118;

// Compound assignment.

/// `+=`
pub const PLUS_EQ: u16 = 130;
/// `-=`
pub const MINUS_EQ: u16 = 131;
/// `*=`
pub const STAR_EQ: u16 = 132;
/// `/=`
pub const SLASH_EQ: u16 = 133;
/// `%=`
pub const PERCENT_EQ: u16 = 134;
/// `**=`
pub const STAR_STAR_EQ: u16 = 135;
/// `<<=`
pub const SHL_EQ: u16 = 136;
/// `>>=`
pub const SHR_EQ: u16 = 137;
/// `>>>=`
pub const USHR_EQ: u16 = 138;
/// `&=`
pub const AMP_EQ: u16 = 139;
/// `|=`
pub const PIPE_EQ: u16 = 140;
/// `^=`
pub const CARET_EQ: u16 = 141;
/// `&&=`
pub const AMP_AMP_EQ: u16 = 142;
/// `||=`
pub const PIPE_PIPE_EQ: u16 = 143;
/// `??=`
pub const QUESTION_QUESTION_EQ: u16 = 144;

// C-specific punctuation / digraphs.

/// `##` preprocessor token-paste.
pub const HASH_HASH: u16 = 160;
/// `<:` digraph for `[`.
pub const LT_COLON: u16 = 161;
/// `:>` digraph for `]`.
pub const COLON_GT: u16 = 162;
/// `<%` digraph for `{`.
pub const LT_PERCENT: u16 = 163;
/// `%>` digraph for `}`.
pub const PERCENT_GT: u16 = 164;

// Markdown-specific structure.

/// Plain text run between meaningful syntactic bytes.
pub const TEXT: u16 = 200;
/// Leading run of `#` characters introducing an ATX heading.
pub const HEADING_MARKER: u16 = 201;
/// Remainder of an ATX heading line, excluding the trailing newline.
pub const HEADING_TEXT: u16 = 202;
/// A run of `*` or `_` used as an emphasis or strong delimiter.
pub const EMPHASIS: u16 = 203;
/// A `` `…` `` inline code span, delimiters included.
pub const CODE_INLINE: u16 = 204;
/// Opening or closing fence line of a fenced code block, including the
/// trailing newline if present.
pub const CODE_FENCE: u16 = 205;
/// Body of a fenced code block when the info string did not select an
/// embedded lexer.
pub const CODE_BLOCK: u16 = 206;
/// `[…]` or `![…]` link/image label, brackets and bang included.
pub const LINK_TEXT: u16 = 207;
/// `(…)` link destination, parentheses included.
pub const LINK_URL: u16 = 208;
/// A `>` blockquote line prefix and the optional space that follows it.
pub const BLOCKQUOTE: u16 = 209;
/// A list-item marker (`-` / `*` / `+` / `<digits>.` / `<digits>)`) together
/// with the trailing space.
pub const LIST_MARKER: u16 = 210;
