//! Go 1.25 lexer + parser (Logos + LALRPOP).
//!
//! - Lexer uses Logos (modern error type API) and implements Go semicolon insertion.
//! - Parser uses LALRPOP and returns a syntax tree.

pub mod ast;
pub mod ast2;
pub mod error;
pub mod lexer;

// Re-exports for convenience
pub use lexer::Lexer;

// lalrpop_util::lalrpop_mod!(pub grammar);

use crate::ast::SourceFile;
use crate::error::{Diag, ParseFailure};

//// Parse a Go source file.
////
///// Returns either a `SourceFile` or a `ParseFailure` with collected diagnostics.
// pub fn parse_source(input: &str) -> Result<SourceFile, ParseFailure> {
//     let mut lex = Lexer::new(input);
//     let mut recoveries = Vec::new();

//     let parsed = grammar::SourceFileParser::new().parse(&mut recoveries, &mut lex);

//     let mut diags: Vec<Diag> = Vec::new();
//     diags.extend(lex.take_diags());
//     diags.extend(crate::error::from_recoveries(&recoveries));

//     match parsed {
//         Ok(sf) if diags.is_empty() => Ok(sf),
//         Ok(sf) => Err(ParseFailure {
//             partial: Some(sf),
//             diags,
//         }),
//         Err(e) => {
//             diags.push(crate::error::from_parse_error(e));
//             Err(ParseFailure {
//                 partial: None,
//                 diags,
//             })
//         }
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::parse_source;

//     #[test]
//     fn smoke_parse_package() {
//         let src = "package p\n\nfunc main() {}\n";
//         let out = parse_source(src);
//         assert!(out.is_ok(), "{out:?}");
//     }
// }
