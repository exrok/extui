//! Token and span types.
//!
//! A [`Token`] identifies a single lexical unit: its [`Span`] in the source,
//! the [`crate::Language`] that produced it, and a language-specific local
//! kind (see [`crate::kind`]). Tokens are [`Copy`].

use crate::Language;

/// Half-open byte range in source, identified by absolute offset and length.
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    /// Absolute byte offset of the first byte.
    pub offset: u32,
    /// Length in bytes.
    pub len: u32,
}

impl Span {
    /// Returns a span of `len` bytes starting at `offset`.
    pub const fn new(offset: u32, len: u32) -> Self {
        Self { offset, len }
    }

    /// Returns an empty span at `offset`.
    pub const fn empty_at(offset: u32) -> Self {
        Self { offset, len: 0 }
    }

    /// Returns the offset one past the last byte.
    pub const fn end(&self) -> u32 {
        self.offset + self.len
    }

    /// Returns `true` if the span has zero length.
    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Returns `true` if `offset` lies in `[self.offset, self.end())`.
    pub const fn contains(&self, offset: u32) -> bool {
        offset >= self.offset && offset < self.end()
    }

    /// Returns `true` if the two spans share at least one byte.
    pub const fn overlaps(&self, other: &Span) -> bool {
        self.offset < other.end() && other.offset < self.end()
    }
}

/// Bits that may be set in [`Token::flags`].
pub mod flags {
    /// Set on tokens continued from a previous chunk boundary.
    pub const IS_CONTINUATION: u8 = 1 << 0;
    /// Set when the token was emitted by error recovery.
    pub const IS_ERROR: u8 = 1 << 1;
    /// Set on the token that opens an embedded-language region.
    pub const EMBED_OPEN: u8 = 1 << 2;
    /// Set on the token that closes an embedded-language region.
    pub const EMBED_CLOSE: u8 = 1 << 3;
    /// Set when the lexer state after this token is a safe incremental
    /// resume point.
    pub const STATE_BREAKPOINT: u8 = 1 << 4;
}

const LANG_BITS: u16 = 4;
const LOCAL_MASK: u16 = (1 << (16 - LANG_BITS)) - 1;

#[inline]
pub(crate) const fn pack_kind(lang_tag: u8, local: u16) -> u16 {
    debug_assert!(lang_tag < 16);
    debug_assert!(local <= LOCAL_MASK);
    ((lang_tag as u16) << 12) | (local & LOCAL_MASK)
}

/// Returns `(language_tag, local_kind)` for a [`Token::kind`] value.
///
/// `language_tag` matches [`crate::Language::tag`]. `local_kind` matches
/// the constants in [`crate::kind`].
#[inline]
pub const fn unpack_kind(kind: u16) -> (u8, u16) {
    ((kind >> 12) as u8, kind & LOCAL_MASK)
}

#[inline]
pub(crate) const fn kind_lang_tag(kind: u16) -> u8 {
    (kind >> 12) as u8
}

#[inline]
pub(crate) const fn kind_local(kind: u16) -> u16 {
    kind & LOCAL_MASK
}

#[inline]
pub(crate) fn kind_language(kind: u16) -> Language {
    Language::from_tag(kind_lang_tag(kind))
}

/// A single lexical token.
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Token {
    /// Absolute position and length in the source.
    pub span: Span,
    /// Opaque identifier for this token's language and kind. Inspect via
    /// [`Token::language`] and [`Token::local_kind`], or split with
    /// [`unpack_kind`].
    pub kind: u16,
    pub(crate) state_out: u16,
    /// Bitmask of [`flags`] values.
    pub flags: u8,
    /// Embedded-language nesting depth. Zero at the outermost language.
    pub nest: u8,
    pub(crate) _reserved: u16,
}

impl Token {
    /// Returns the [`Language`] that produced this token.
    #[inline]
    pub fn language(&self) -> Language {
        kind_language(self.kind)
    }

    /// Returns the language-local kind.
    ///
    /// Compare against the constants in [`crate::kind`], for example
    /// [`crate::kind::STRING`] or [`crate::kind::KEYWORD`].
    #[inline]
    pub const fn local_kind(&self) -> u16 {
        kind_local(self.kind)
    }

    /// Returns the raw language tag matching [`Language::tag`].
    ///
    /// Prefer [`Token::language`] for most use cases.
    #[inline]
    pub const fn lang_tag(&self) -> u8 {
        kind_lang_tag(self.kind)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_is_sixteen_bytes() {
        assert_eq!(core::mem::size_of::<Token>(), 16);
        assert_eq!(core::mem::align_of::<Token>(), 4);
    }

    #[test]
    fn span_basics() {
        let s = Span::new(5, 3);
        assert_eq!(s.end(), 8);
        assert!(s.contains(5));
        assert!(s.contains(7));
        assert!(!s.contains(8));
        assert!(s.overlaps(&Span::new(7, 10)));
        assert!(!s.overlaps(&Span::new(8, 1)));
    }

    #[test]
    fn kind_roundtrip() {
        for lang in 0..16u8 {
            for local in [0u16, 1, 0x0FFF, 0x0AAA] {
                let k = pack_kind(lang, local);
                assert_eq!(unpack_kind(k), (lang, local));
            }
        }
    }
}
