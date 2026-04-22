//! Per-language lexer state.
//!
//! Each language encodes its own mid-scan state into a [`LexState`]. The
//! driver treats the value as opaque and compares instances bitwise.

/// Opaque lexer-state value threaded between lexer invocations.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
pub struct LexState(pub u16);

impl LexState {
    /// The canonical "nothing in progress" state.
    ///
    /// All lexers start at [`LexState::INITIAL`], and stateless lexers
    /// never leave it.
    pub const INITIAL: Self = Self(0);

    /// Returns the raw encoding.
    #[inline]
    pub const fn bits(self) -> u16 {
        self.0
    }

    /// Returns `true` if this is [`LexState::INITIAL`].
    #[inline]
    pub const fn is_initial(self) -> bool {
        self.0 == 0
    }
}

impl From<u16> for LexState {
    #[inline]
    fn from(bits: u16) -> Self {
        Self(bits)
    }
}

impl From<LexState> for u16 {
    #[inline]
    fn from(state: LexState) -> Self {
        state.0
    }
}
