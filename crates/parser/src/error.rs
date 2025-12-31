use lalrpop_util::{ErrorRecovery, ParseError};
use thiserror::Error;

use crate::lexer::Tok;

/// Compact byte-span used across the compiler.
///
/// LALRPOP provides `usize` locations; we convert to `u32` for compactness.
/// If you need >4GiB inputs, change to `u64`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: u32,
    pub end: u32, // exclusive
}

impl Span {
    #[inline]
    pub const fn new(start: usize, end: usize) -> Self {
        // Production choice: clamp rather than panic.
        // Alternatively: return Result<Span, SpanOverflow>.
        let s = if start > u32::MAX as usize {
            u32::MAX
        } else {
            start as u32
        };
        let e = if end > u32::MAX as usize {
            u32::MAX
        } else {
            end as u32
        };
        Self { start: s, end: e }
    }

    #[inline]
    pub const fn empty_at(pos: usize) -> Self {
        let p = if pos > u32::MAX as usize {
            u32::MAX
        } else {
            pos as u32
        };
        Self { start: p, end: p }
    }

    #[inline]
    pub const fn single_at(pos: usize) -> Self {
        // A one-byte span at pos (best-effort; used for InvalidToken).
        let p = if pos > u32::MAX as usize {
            u32::MAX
        } else {
            pos as u32
        };
        let e = p.saturating_add(1);
        Self { start: p, end: e }
    }

    pub const fn from_range(range: std::ops::Range<usize>) -> Self {
        Self::new(range.start, range.end)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagKind {
    Lex,
    Parse,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diag {
    pub kind: DiagKind,
    pub span: Span,
    pub message: String,
}

#[derive(Debug)]
pub struct ParseFailure {
    pub partial: Option<crate::ast::SourceFile>,
    pub diags: Vec<Diag>,
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum LexErrorKind {
    #[error("invalid token")]
    InvalidToken,
    #[error("invalid numeric literal")]
    InvalidNumber,
    #[error("invalid escape")]
    InvalidEscape,
    #[error("unterminated string")]
    UnterminatedString,
    #[error("unterminated comment")]
    UnterminatedComment,
}

impl Default for LexErrorKind {
    #[inline]
    fn default() -> Self {
        LexErrorKind::InvalidToken
    }
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
#[error("{kind}: {span:?}")]
pub struct LexError {
    pub kind: LexErrorKind,
    pub span: Span,
}

impl Default for LexError {
    #[inline]
    fn default() -> Self {
        Self {
            kind: LexErrorKind::InvalidToken,
            span: Span::default(),
        }
    }
}

impl LexError {
    #[inline]
    pub fn diag(&self) -> Diag {
        Diag {
            kind: DiagKind::Lex,
            span: self.span,
            message: self.kind.to_string(),
        }
    }
}

#[inline]
pub fn from_recoveries(recoveries: &[ErrorRecovery<usize, Tok, &'static str>]) -> Vec<Diag> {
    recoveries
        .iter()
        .map(|r| from_parse_error_ref(&r.error))
        .collect()
}

#[inline]
pub fn from_parse_error(e: ParseError<usize, Tok, &'static str>) -> Diag {
    from_parse_error_ref(&e)
}

fn from_parse_error_ref(e: &ParseError<usize, Tok, &'static str>) -> Diag {
    match e {
        ParseError::InvalidToken { location } => Diag {
            kind: DiagKind::Parse,
            span: Span::single_at(*location),
            message: "invalid token".to_string(),
        },

        ParseError::UnrecognizedEof { location, expected } => Diag {
            kind: DiagKind::Parse,
            span: Span::empty_at(*location),
            message: format!("unexpected EOF; expected: {}", expected.join(", ")),
        },

        ParseError::UnrecognizedToken { token, expected } => {
            let (l, _t, r) = token;
            Diag {
                kind: DiagKind::Parse,
                span: Span::new(*l, *r),
                message: format!("unexpected token; expected: {}", expected.join(", ")),
            }
        }

        ParseError::ExtraToken { token } => {
            let (l, _t, r) = token;
            Diag {
                kind: DiagKind::Parse,
                span: Span::new(*l, *r),
                message: "extra token".to_string(),
            }
        }

        ParseError::User { error } => Diag {
            kind: DiagKind::Parse,
            span: Span::default(),
            message: format!("user error: {error}"),
        },
    }
}
