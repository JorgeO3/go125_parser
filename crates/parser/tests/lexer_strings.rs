use go125_parser::error::Diag;
use go125_parser::lexer::{Lexer, Tok};

fn lex_all(input: &str) -> (Vec<(usize, Tok<'_>, usize)>, Vec<Diag>) {
    let mut lx = Lexer::new(input);
    let toks: Vec<_> = lx.by_ref().collect();
    let diags = lx.take_diags();
    (toks, diags)
}

fn has_error(toks: &[(usize, Tok<'_>, usize)]) -> bool {
    toks.iter().any(|(_, t, _)| matches!(t, Tok::Error))
}

fn lex1(input: &str) -> (Tok<'_>, Vec<Diag>) {
    let mut lx = Lexer::new(input);
    let t = lx.next().unwrap().1;
    let diags = lx.take_diags();
    (t, diags)
}

#[test]
fn raw_string_basic() {
    let (t, diags) = lex1("`abc`");
    assert!(diags.is_empty(), "{diags:?}");
    assert!(matches!(t, Tok::RawStringLit(_)));
}

#[test]
fn interpreted_string_basic_escapes_ok() {
    for s in [
        r#""\n""#,
        r#""\t""#,
        r#""\\\"""#,
        r#""\x41""#,
        r#""\u0041""#,
        r#""\U00000041""#,
    ] {
        let (t, diags) = lex1(s);
        assert!(diags.is_empty(), "{s} produced diags: {diags:?}");
        assert!(matches!(t, Tok::StringLit(_)), "{s} -> {t:?}");
    }
}

#[test]
fn rune_basic() {
    for s in [
        "'a'",
        r"'\n'",
        r"'\x41'",
        r"'\u0041'",
        r"'\U00000041'",
        r"'\141'",
    ] {
        let (t, diags) = lex1(s);
        assert!(diags.is_empty(), "{s} produced diags: {diags:?}");
        assert!(matches!(t, Tok::RuneLit(_)), "{s} -> {t:?}");
    }
}

#[test]
fn invalid_unicode_scalar_in_escape_rejected() {
    let (t, _diags) = lex1(r"'\uD800'");
    assert!(matches!(t, Tok::Error), "surrogate should error, got {t:?}");
}

/*
  IMPORTANT:
  Según spec:
  - en STRING: \' es ilegal
  - en RUNE:   \" es ilegal
*/
#[test]
fn spec_string_rejects_escaped_single_quote() {
    let (t, _diags) = lex1(r#""\'""#);
    assert!(
        matches!(t, Tok::Error),
        r#""\'"" should be Error per spec, got {t:?}"#
    );
}

#[test]
fn spec_rune_rejects_escaped_double_quote() {
    let (t, _diags) = lex1(r#"'\"'"#);
    assert!(
        matches!(t, Tok::Error),
        r#"'\"' should be Error per spec, got {t:?}"#
    );
}

#[test]
fn raw_string_allows_newline() {
    let (t, diags) = lex1("`a\nb`");
    assert!(diags.is_empty(), "{diags:?}");
    assert!(matches!(t, Tok::RawStringLit(_)), "{t:?}");
}

#[test]
fn raw_string_allows_crlf() {
    let (t, diags) = lex1("`a\r\nb`");
    assert!(diags.is_empty(), "{diags:?}");
    assert!(matches!(t, Tok::RawStringLit(_)), "{t:?}");
}

#[test]
fn raw_string_unterminated_is_error() {
    let (t, diags) = lex1("`unterminated");
    assert!(matches!(t, Tok::Error), "{t:?}");
    assert!(!diags.is_empty());
}

#[test]
fn unterminated_interpreted_string_emits_error_and_diags() {
    // Nota: hoy tu lexer probablemente emite varios Tok::Error (1 byte por token) en este caso.
    // Este test solo exige que exista al menos un Error y que haya diags.
    let (toks, diags) = lex_all(r#""abc"#);
    assert!(!diags.is_empty(), "expected diag(s), toks={toks:?}");
    assert!(
        toks.iter().any(|(_, t, _)| matches!(t, Tok::Error)),
        "expected at least one Tok::Error, toks={toks:?}"
    );
}

#[test]
fn unterminated_rune_emits_error_and_diags() {
    let (toks, diags) = lex_all("'a");
    assert!(!diags.is_empty(), "expected diag(s), toks={toks:?}");
    assert!(
        toks.iter().any(|(_, t, _)| matches!(t, Tok::Error)),
        "expected at least one Tok::Error, toks={toks:?}"
    );
}

#[test]
fn unterminated_string_incomplete_escape_emits_error_and_diags() {
    // Bordes de escapes incompletos: "\u12  (sin cierre y escape incompleto)
    let (toks, diags) = lex_all("\"\\u12");
    assert!(!diags.is_empty(), "expected diag(s), toks={toks:?}");
    assert!(
        toks.iter().any(|(_, t, _)| matches!(t, Tok::Error)),
        "expected at least one Tok::Error, toks={toks:?}"
    );
}

#[test]
fn comment_markers_inside_string_do_not_start_comment() {
    // "/* */" dentro de un string NO inicia comentario.
    let (t, diags) = lex1(r#""/* not a comment */""#);
    assert!(diags.is_empty(), "{diags:?}");
    assert!(matches!(t, Tok::StringLit(_)), "{t:?}");
}

#[test]
fn comment_markers_inside_raw_string_do_not_start_comment() {
    let (t, diags) = lex1(r#"`/* not a comment */`"#);
    assert!(diags.is_empty(), "{diags:?}");
    assert!(matches!(t, Tok::RawStringLit(_)), "{t:?}");
}

#[test]
fn comment_markers_inside_rune_do_not_start_comment() {
    // Dentro de rune literal, "/*" es solo bytes. Aquí usamos escape para meter '/' y '*'.
    let (t, diags) = lex1(r"'\x2F'");
    assert!(diags.is_empty(), "{diags:?}");
    assert!(matches!(t, Tok::RuneLit(_)), "{t:?}");
}

#[test]
fn rune_octal_escape_over_255_is_rejected() {
    // '\400' => 256 decimal, inválido (byte escape max 255)
    let (t, diags) = lex1(r"'\400'");
    assert!(matches!(t, Tok::Error), "t={t:?}");
    assert!(!diags.is_empty(), "expected diag");
}

#[test]
fn rune_unicode_escape_out_of_range_is_rejected() {
    let (t, diags) = lex1(r"'\U00110000'"); // > 0x10FFFF
    assert!(matches!(t, Tok::Error), "t={t:?}");
    assert!(!diags.is_empty(), "expected diag");
}

#[test]
fn string_surrogate_in_unicode_escape_is_rejected() {
    let (t, diags) = lex1(r#""\uD800""#);
    assert!(matches!(t, Tok::Error), "t={t:?}");
    assert!(!diags.is_empty(), "expected diag");
}

#[test]
fn invalid_escape_lengths_are_rejected() {
    for s in [
        r"'\x4'",         // \x requiere 2 hex
        r"'\u123'",       // \u requiere 4 hex
        r"'\U0000FFF'",   // \U requiere 8 hex (aquí hay 7)
        r#""\x4""#,       // idem en string
        r#""\u123""#,     // idem
        r#""\U0000FFF""#, // \U incompleto (7 hex) -> debe fallar
    ] {
        let (t, diags) = lex1(s);
        assert!(matches!(t, Tok::Error), "{s} should Error, got {t:?}");
        assert!(!diags.is_empty(), "{s} should emit diag");
    }
}

#[test]
fn rune_must_be_exactly_one_rune() {
    // 'ab' tiene 2 runes ASCII => inválido
    let (t, diags) = lex1("'ab'");
    assert!(matches!(t, Tok::Error), "t={t:?}");
    assert!(!diags.is_empty(), "expected diag");
}

#[test]
fn interpreted_string_cannot_contain_raw_newline() {
    // No es "\n" escape; es newline real dentro de comillas.
    let (toks, diags) = lex_all("\"a\nb\"");
    assert!(!diags.is_empty(), "expected diags, toks={toks:?}");
    assert!(has_error(&toks), "expected Error token(s), toks={toks:?}");
}

#[test]
fn rune_cannot_contain_raw_newline() {
    let (toks, diags) = lex_all("'a\n'");
    assert!(!diags.is_empty(), "expected diags, toks={toks:?}");
    assert!(has_error(&toks), "expected Error token(s), toks={toks:?}");
}

#[test]
fn raw_string_allows_cr_and_lexer_should_accept_it() {
    // Lexer: ok. (El “discard \r” es semántica del value, no del token slice.)
    let (t, diags) = lex1("`a\rb`");
    assert!(diags.is_empty(), "diags={diags:?}");
    assert!(matches!(t, Tok::RawStringLit(_)), "t={t:?}");
}

#[test]
fn u_escape_with_extra_hex_digit_is_still_valid_string() {
    let (t, diags) = lex1(r#""\U0000FFFFF""#); // 8 dígitos + 'F' literal
    assert!(diags.is_empty(), "{diags:?}");
    assert!(matches!(t, Tok::StringLit(_)), "t={t:?}");
}
