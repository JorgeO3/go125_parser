// crates/parser/tests/lexer_whitespace.rs
use go125_parser::error::Diag;
use go125_parser::lexer::{Lexer, Tok};

fn lex_all(input: &str) -> (Vec<(usize, Tok<'_>, usize)>, Vec<Diag>) {
    let mut lx = Lexer::new(input);
    let toks: Vec<_> = lx.by_ref().collect();
    let diags = lx.take_diags();
    (toks, diags)
}

fn has_error_token(toks: &[(usize, Tok<'_>, usize)]) -> bool {
    toks.iter().any(|(_, t, _)| matches!(t, Tok::Error))
}

/// Go spec whitespace: space, \t, \n, \r (NO incluye \v ni \f).
/// Estos tests deben activarse cuando el lexer deje de "skippear" \v/\f.
// #[ignore = "Enable once lexer enforces Go whitespace (rejects \\v/\\f)."]
#[test]
fn vertical_tab_is_not_whitespace() {
    // VT = U+000B
    let src = format!("package{}\u{0020}p", "\u{000B}");
    let (toks, diags) = lex_all(&src);
    assert!(has_error_token(&toks), "expected Tok::Error, toks={toks:?}");
    assert!(!diags.is_empty(), "expected diag for InvalidToken");
}

// #[ignore = "Enable once lexer enforces Go whitespace (rejects \\v/\\f)."]
#[test]
fn form_feed_is_not_whitespace() {
    // FF = U+000C
    let src = format!("package{}\u{0020}p", "\u{000C}");
    let (toks, diags) = lex_all(&src);
    assert!(has_error_token(&toks), "expected Tok::Error, toks={toks:?}");
    assert!(!diags.is_empty(), "expected diag for InvalidToken");
}
