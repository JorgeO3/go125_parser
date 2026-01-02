// crates/parser/tests/lexer_comments_cr.rs
use go125_parser::error::Diag;
use go125_parser::lexer::{Lexer, Tok};

fn lex_all(input: &str) -> (Vec<(usize, Tok<'_>, usize)>, Vec<Diag>) {
    let mut lx = Lexer::new(input);
    let mut toks = Vec::new();
    for t in lx.by_ref() {
        toks.push(t);
    }
    let diags = lx.take_diags();
    (toks, diags)
}

#[inline]
fn is_injected_eof_semi(input: &str, s: usize, t: &Tok<'_>, e: usize) -> bool {
    matches!(t, Tok::Semi) && s == e && s == input.len()
}

// Devuelve una copia de toks sin el ';' inyectado en EOF.
// (No afecta semis reales ';' del código, ni semis inyectados por newline.)
fn strip_eof_semi<'src>(
    toks: &[(usize, Tok<'src>, usize)],
    input: &str,
) -> Vec<(usize, Tok<'src>, usize)> {
    toks.iter()
        .filter(|&(s, t, e)| !is_injected_eof_semi(input, *s, t, *e))
        .cloned()
        .collect()
}

fn kinds_no_eof_semi(toks: &[(usize, Tok<'_>, usize)], input: &str) -> Vec<&'static str> {
    toks.iter()
        .filter(|(s, t, e)| !is_injected_eof_semi(input, *s, t, *e))
        .map(|(_, t, _)| match t {
            Tok::Ident(_) => "Ident",
            Tok::IntLit(_) => "IntLit",
            Tok::FloatLit(_) => "FloatLit",
            Tok::ImagLit(_) => "ImagLit",
            Tok::RuneLit(_) => "RuneLit",
            Tok::StringLit(_) => "StringLit",
            Tok::RawStringLit(_) => "RawStringLit",

            Tok::KwBreak => "KwBreak",
            Tok::KwContinue => "KwContinue",
            Tok::KwFallthrough => "KwFallthrough",
            Tok::KwReturn => "KwReturn",
            Tok::KwIf => "KwIf",

            Tok::Semi => "Semi",
            Tok::LBrace => "LBrace",
            Tok::RBrace => "RBrace",
            Tok::LParen => "LParen",
            Tok::RParen => "RParen",
            Tok::Dot => "Dot",

            Tok::Error => "Error",

            // resto no usado aquí
            _ => "Other",
        })
        .collect()
}

#[test]
fn block_comment_with_cr_does_not_error() {
    let (_toks, diags) = lex_all("/*\r*/");
    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn block_comment_with_crlf_does_not_error() {
    let (_toks, diags) = lex_all("/*\r\n*/");
    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn block_comment_unterminated_is_error() {
    let (toks, diags) = lex_all("/*\r");
    assert!(!diags.is_empty(), "expected diag");
    assert!(
        toks.iter().any(|(_, t, _)| matches!(t, Tok::Error)),
        "toks={toks:?}"
    );
}

#[test]
fn semicolon_is_inserted_at_newline_inside_block_comment_lf() {
    // foo /*\n*/ bar  => foo ; bar  (ignorando el ';' inyectado en EOF)
    let src = "foo/*\n*/bar";
    let (toks, diags) = lex_all(src);
    assert!(diags.is_empty(), "{diags:?}");
    assert_eq!(
        kinds_no_eof_semi(&toks, src),
        ["Ident", "Semi", "Ident"],
        "toks={toks:?}"
    );
}

#[test]
fn semicolon_is_inserted_at_newline_inside_block_comment_cr() {
    // foo /*\r*/ bar  => foo ; bar  (ignorando el ';' inyectado en EOF)
    let src = "foo/*\r*/bar";
    let (toks, diags) = lex_all(src);
    assert!(diags.is_empty(), "{diags:?}");
    assert_eq!(
        kinds_no_eof_semi(&toks, src),
        ["Ident", "Semi", "Ident"],
        "toks={toks:?}"
    );
}

#[test]
fn semicolon_is_inserted_at_newline_inside_block_comment_crlf() {
    // foo /*\r\n*/ bar  => foo ; bar  (ignorando el ';' inyectado en EOF)
    let src = "foo/*\r\n*/bar";
    let (toks, diags) = lex_all(src);
    assert!(diags.is_empty(), "{diags:?}");
    assert_eq!(
        kinds_no_eof_semi(&toks, src),
        ["Ident", "Semi", "Ident"],
        "toks={toks:?}"
    );
}

#[test]
fn no_semicolon_insertion_if_prev_token_cannot_insert() {
    // "if" NO inserta semi, aunque el comentario tenga newline.
    // Pero OJO: al final del archivo sí puede insertarse ';' después de Ident.
    let src = "if/*\n*/x";
    let (toks, diags) = lex_all(src);
    assert!(diags.is_empty(), "{diags:?}");

    // Debe ser: if Ident (sin Semi intermedio). Ignoramos el ';' del EOF.
    let ks = kinds_no_eof_semi(&toks, src);
    assert_eq!(ks, ["KwIf", "Ident"], "toks={toks:?}");
}

#[test]
fn line_comment_crlf_triggers_newline_token_and_semi_insertion() {
    // foo//...\r\nbar  => foo ; bar  (ignorando el ';' inyectado en EOF)
    // (tu line comment se skippea, el Newline se emite y dispara el ';')
    let src = "foo//c\r\nbar";
    let (toks, diags) = lex_all(src);
    assert!(diags.is_empty(), "{diags:?}");
    assert_eq!(
        kinds_no_eof_semi(&toks, src),
        ["Ident", "Semi", "Ident"],
        "toks={toks:?}"
    );
}

// Protecciones tipo “issue 11151 family”: comentarios con \r dentro
// (en tu lexer esto es más sobre NO romper el escaneo / no terminar antes de tiempo)
#[test]
fn block_comment_issue11151_family_variants_do_not_error() {
    for src in [
        "/**\r/*/x",
        "/**\r\r/*/x",
        "/*\r/*/x",
        "/*\r*/x",
        "/*\r\r\r\r*/x",
    ] {
        let (toks, diags) = lex_all(src);
        assert!(
            diags.is_empty(),
            "src={src:?} diags={diags:?} toks={toks:?}"
        );

        // Ignorar el ';' inyectado al EOF para esta aserción
        let toks2 = strip_eof_semi(&toks, src);

        // el sufijo 'x' debe lexear como Ident al final
        assert!(
            matches!(toks2.last().map(|x| &x.1), Some(Tok::Ident("x"))),
            "src={src:?} toks={toks:?}"
        );
    }
}
