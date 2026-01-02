// crates/parser/tests/lexer_unicode.rs
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

fn assert_error_at_char(input: &str, ch: char) {
    let (toks, diags) = lex_all(input);
    assert!(has_error_token(&toks), "expected Tok::Error, toks={toks:?}");
    assert!(
        !diags.is_empty(),
        "expected diag(s) for InvalidToken/Invalid*; toks={toks:?}"
    );

    let at = input
        .find(ch)
        .unwrap_or_else(|| panic!("input does not contain expected char U+{:04X}", ch as u32));
    let end = at + ch.len_utf8();

    assert!(
        toks.iter()
            .any(|(s, t, e)| *s == at && *e == end && matches!(t, Tok::Error)),
        "expected Error token exactly at [{at},{end}) for U+{:04X}, got toks={toks:?}",
        ch as u32
    );
}

#[test]
fn unicode_identifiers_letters_and_nd_digits() {
    // Greek letters
    let (toks, diags) = lex_all("αβγ");
    assert!(diags.is_empty(), "{diags:?}");
    assert!(matches!(toks[0].1, Tok::Ident(_)), "toks={toks:?}");

    // underscores ok
    let (toks, diags) = lex_all("_x1");
    assert!(diags.is_empty(), "{diags:?}");
    assert!(matches!(toks[0].1, Tok::Ident("_x1")), "toks={toks:?}");
}

#[test]
fn unicode_nd_digits_allowed_after_start() {
    // Arabic-Indic digits are Nd (allowed in identifier body per spec),
    // but NOT valid for numeric literals (Go numerals are ASCII only).
    let (toks, diags) = lex_all("x\u{0661}\u{0662}");
    assert!(diags.is_empty(), "{diags:?}");
    assert!(
        matches!(toks[0].1, Tok::Ident("x\u{0661}\u{0662}")),
        "toks={toks:?}"
    );
}

#[test]
fn unicode_bom_not_at_start_is_error_even_with_unicode() {
    // BOM is only allowed at absolute start of file.
    // Here it appears after a valid identifier start, so we must emit Error at BOM.
    let src = "π\u{FEFF}σ";
    assert_error_at_char(src, '\u{FEFF}');
}

#[test]
fn unicode_space_not_allowed_inside_ident() {
    // Go whitespace is only: space, \t, \n, \r. Other Unicode spaces are illegal chars.
    let src = "x\u{2003}y"; // EM SPACE
    assert_error_at_char(src, '\u{2003}');
}

#[test]
fn unicode_zero_width_joiner_not_allowed() {
    // ZWJ must be illegal (not letter, not Nd, not underscore).
    let src = "x\u{200D}y";
    assert_error_at_char(src, '\u{200D}');
}

#[test]
fn unicode_connector_punctuation_not_allowed_in_ident_body() {
    // Connector punctuation (Pc) is NOT allowed in Go identifiers (only '_' is allowed).
    // Example: U+203F UNDERTIE is Pc.
    let src = "x\u{203F}y";
    assert_error_at_char(src, '\u{203F}');
}

#[test]
fn unicode_non_nd_digit_not_allowed_in_ident_body() {
    // Non-Nd “digits” (like superscripts) are not Unicode category Nd.
    // Example: U+00B2 SUPERSCRIPT TWO (category No) must be illegal in identifier body.
    let src = "x\u{00B2}y";
    assert_error_at_char(src, '\u{00B2}');
}

#[test]
fn unicode_combining_mark_not_allowed_anywhere_in_ident() {
    // Go identifiers allow letters + Nd digits (and '_'), but NOT combining marks.
    // Example: U+0301 COMBINING ACUTE ACCENT.
    // "x◌́y" should produce Ident("x"), Error(◌́), Ident("y").
    let src = "x\u{0301}y";
    assert_error_at_char(src, '\u{0301}');
}

#[test]
fn unicode_ident_start_must_be_letter_or_underscore() {
    // If you want a "digit-like" start that is NOT a Go number literal,
    // use a Unicode Nd digit (Arabic-Indic '١' U+0661).
    // It is NOT a valid number literal (Go numerals are ASCII), and it can't start an identifier.
    let src = "\u{0661}x";
    let (toks, diags) = lex_all(src);

    assert!(
        matches!(toks.first().map(|t| &t.1), Some(Tok::Error)),
        "expected first token Error for leading Unicode digit; toks={toks:?}"
    );
    assert!(!diags.is_empty(), "expected diag; toks={toks:?}");
}
