//! Per-language lexer implementations and shared scanner primitives.
//!
//! Nothing in this module is part of the public API except the
//! [`kw::kw_table!`] macro, which declares a sorted keyword-lookup table.
//! All other items are re-exported here for the in-crate driver.

pub(crate) mod byteclass;
pub(crate) mod c;
pub(crate) mod css;
pub(crate) mod csv;
pub(crate) mod html;
pub(crate) mod json;
pub(crate) mod jsx;
pub mod kw;
pub(crate) mod markdown;
pub(crate) mod rust;
pub(crate) mod scan;
pub(crate) mod toml;
pub(crate) mod ts;
pub(crate) mod tsx;
pub(crate) mod xml;

use crate::{Language, LexState};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) enum LexStep {
    #[default]
    Eof,
    Token {
        len: u32,
        local_kind: u16,
        state_out: LexState,
        flags: u8,
    },
    Descend {
        language: Language,
        inner_len: u32,
        inner_state_in: LexState,
        outer_state_after: LexState,
    },
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct StepBuf {
    steps: [LexStep; Self::CAP],
    len: u8,
}

impl StepBuf {
    pub(crate) const CAP: usize = 32;

    pub(crate) const fn new() -> Self {
        Self {
            steps: [LexStep::Eof; Self::CAP],
            len: 0,
        }
    }

    #[inline]
    pub(crate) fn clear(&mut self) {
        self.len = 0;
    }

    #[inline]
    pub(crate) fn is_full(&self) -> bool {
        (self.len as usize) >= Self::CAP
    }

    #[inline]
    #[allow(dead_code)]
    pub(crate) fn remaining(&self) -> usize {
        Self::CAP - self.len as usize
    }

    #[inline]
    pub(crate) fn push(&mut self, step: LexStep) -> bool {
        let i = self.len as usize;
        if i >= Self::CAP {
            return false;
        }
        self.steps[i] = step;
        self.len += 1;
        true
    }

    #[inline]
    pub(crate) fn as_slice(&self) -> &[LexStep] {
        &self.steps[..self.len as usize]
    }
}

impl Default for StepBuf {
    fn default() -> Self {
        Self::new()
    }
}

pub(crate) trait Lexer {
    fn step_batch(
        view: &mut crate::SourceView<'_>,
        cursor: u32,
        state: LexState,
        out: &mut StepBuf,
    ) -> (u32, LexState);

    #[allow(dead_code)]
    fn is_safe_state(state: LexState) -> bool;
}
