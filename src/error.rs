use std::ops::Range;

use lalrpop_util::{ErrorRecovery, ParseError};
use thiserror::Error;

use crate::lexer::Tok;

pub type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone)]
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
    fn default() -> Self {
        Self {
            kind: LexErrorKind::InvalidToken,
            span: 0..0,
        }
    }
}

impl LexError {
    pub fn diag(&self) -> Diag {
        Diag {
            kind: DiagKind::Lex,
            span: self.span.clone(),
            message: self.kind.to_string(),
        }
    }
}

pub fn from_recoveries(recoveries: &[ErrorRecovery<usize, Tok, &'static str>]) -> Vec<Diag> {
    recoveries
        .iter()
        .map(|r| from_parse_error_ref(&r.error))
        .collect()
}

pub fn from_parse_error(e: ParseError<usize, Tok, &'static str>) -> Diag {
    from_parse_error_ref(&e)
}

fn from_parse_error_ref(e: &ParseError<usize, Tok, &'static str>) -> Diag {
    match e {
        ParseError::InvalidToken { location } => Diag {
            kind: DiagKind::Parse,
            span: (*location)..(*location + 1),
            message: "invalid token".to_string(),
        },
        ParseError::UnrecognizedEof { location, expected } => Diag {
            kind: DiagKind::Parse,
            span: (*location)..(*location),
            message: format!("unexpected EOF; expected: {}", expected.join(", ")),
        },
        ParseError::UnrecognizedToken { token, expected } => {
            let (l, _t, r) = token;
            Diag {
                kind: DiagKind::Parse,
                span: (*l)..(*r),
                message: format!("unexpected token; expected: {}", expected.join(", ")),
            }
        }
        ParseError::ExtraToken { token } => {
            let (l, _t, r) = token;
            Diag {
                kind: DiagKind::Parse,
                span: (*l)..(*r),
                message: "extra token".to_string(),
            }
        }
        ParseError::User { error } => Diag {
            kind: DiagKind::Parse,
            span: 0..0,
            message: format!("user error: {error}"),
        },
    }
}
