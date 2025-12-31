//! Go 1.25 lexer + parser (Logos + LALRPOP).
//!
//! - Lexer uses Logos (modern error type API) and implements Go semicolon insertion.
//! - Parser uses LALRPOP and returns a syntax tree.

pub mod ast;
pub mod error;
pub mod lexer;
pub mod walk;

// Re-exports for convenience
pub use lexer::Lexer;

// lalrpop_util::lalrpop_mod!(pub grammar);
