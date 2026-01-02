// crates/parser/tests/lexer_errors_corpus.rs
//
// Port directo del `var errors = []struct{...}` de go/scanner (TestScanErrors),
// adaptado a tu lexer:
// - Tu lexer NO emite COMMENT tokens (skip line comments y trata block comments como trivia).
// - Tu lexer SI inserta semicolons automáticamente.
// - Tu API de Diag no expone mensaje estable en tests => verificamos invariantes.
//
// Las filas no portables (o que requieren cambios de lexer) quedan en la tabla con portable=false.

use go125_parser::error::Diag;
use go125_parser::lexer::{Lexer, Tok};

fn lex_all(input: &str) -> (Vec<(usize, Tok<'_>, usize)>, Vec<Diag>) {
    let mut lx = Lexer::new(input);
    let toks: Vec<_> = lx.by_ref().collect();
    let diags = lx.take_diags();
    (toks, diags)
}

// Filtra semicolons inyectados (los tuyos tienen s==e).
fn strip_injected_semis<'a>(toks: &'a [(usize, Tok<'a>, usize)]) -> Vec<&'a Tok<'a>> {
    toks.iter()
        .filter_map(|(s, t, e)| {
            if matches!(t, Tok::Semi) && _toggle_injected(*s, *e) {
                None
            } else {
                Some(t)
            }
        })
        .collect()
}

#[inline(always)]
fn _toggle_injected(s: usize, e: usize) -> bool {
    s == e
}

fn kind_name(t: &Tok<'_>) -> &'static str {
    match t {
        Tok::Ident(_) => "Ident",
        Tok::IntLit(_) => "IntLit",
        Tok::FloatLit(_) => "FloatLit",
        Tok::ImagLit(_) => "ImagLit",
        Tok::RuneLit(_) => "RuneLit",
        Tok::StringLit(_) => "StringLit",
        Tok::RawStringLit(_) => "RawStringLit",
        Tok::Semi => "Semi",
        Tok::Dot => "Dot",
        Tok::Error => "Error",
        // Para este corpus no necesitamos el resto, pero dejamos fallback:
        _ => "Other",
    }
}

fn kinds_no_injected_semis(toks: &[(usize, Tok<'_>, usize)]) -> Vec<&'static str> {
    strip_injected_semis(toks)
        .into_iter()
        .map(kind_name)
        .collect()
}

#[derive(Clone, Copy)]
enum Expect {
    /// Espera exactamente esta secuencia de kinds (ignorando semis inyectados).
    Kinds(&'static [&'static str]),
    /// Espera que haya al menos un Error token y al menos un diag.
    ErrorWithDiag,
}

#[derive(Clone, Copy)]
struct GoRow {
    // originales de Go (referencia documental)
    src: &'static str,
    tok: &'static str,
    pos: usize,
    lit: &'static str,
    err: &'static str,

    // expectativa adaptada a tu lexer
    expect: Expect,

    // Si false: no se puede portar con tu lexer actual (&str / skip comments / sin mensajes)
    // o requiere feature explícito (BOM/NUL dentro de literales, UTF-8 inválido real, etc.)
    portable: bool,

    // nota corta de por qué no es portable o qué falta
    note: &'static str,
}

#[rustfmt::skip]
static GO_ERRORS_TABLE: &[GoRow] = &[
    // Illegal chars
    GoRow { src: "\u{0007}", tok: "ILLEGAL", pos: 0, lit: "", err: "illegal character U+0007",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: "#", tok: "ILLEGAL", pos: 0, lit: "", err: "illegal character U+0023 '#'",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: "…", tok: "ILLEGAL", pos: 0, lit: "", err: "illegal character U+2026 '…'",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    // two periods, not invalid token (issue #28112)
    GoRow { src: "..", tok: "PERIOD", pos: 0, lit: "", err: "",
        expect: Expect::Kinds(&["Dot", "Dot"]), portable: true, note: "" },

    // Rune literals (valid / invalid)
    GoRow { src: "' '", tok: "CHAR", pos: 0, lit: "' '", err: "",
        expect: Expect::Kinds(&["RuneLit"]), portable: true, note: "" },

    GoRow { src: "''", tok: "CHAR", pos: 0, lit: "''", err: "illegal rune literal",
        expect: Expect::ErrorWithDiag, portable: true, note: "tu regex de Rune no matchea vacío => caerá a Error token(s)" },
    GoRow { src: "'12'", tok: "CHAR", pos: 0, lit: "'12'", err: "illegal rune literal",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: "'123'", tok: "CHAR", pos: 0, lit: "'123'", err: "illegal rune literal",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    GoRow { src: r"'\0'", tok: "CHAR", pos: 3, lit: r"'\0'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: r"\0 no es escape válido en Go (octal requiere 3 dígitos)" },
    GoRow { src: r"'\07'", tok: "CHAR", pos: 4, lit: r"'\07'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: r"octal incompleto" },
    GoRow { src: r"'\8'", tok: "CHAR", pos: 2, lit: r"'\8'", err: "unknown escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: r"'\08'", tok: "CHAR", pos: 3, lit: r"'\08'", err: "illegal ... '8' in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    GoRow { src: r"'\x'", tok: "CHAR", pos: 3, lit: r"'\x'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: r"'\x0'", tok: "CHAR", pos: 4, lit: r"'\x0'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: r"'\x0g'", tok: "CHAR", pos: 4, lit: r"'\x0g'", err: "illegal ... 'g' in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    GoRow { src: r"'\u'", tok: "CHAR", pos: 3, lit: r"'\u'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: r"'\u0'", tok: "CHAR", pos: 4, lit: r"'\u0'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: r"'\u00'", tok: "CHAR", pos: 5, lit: r"'\u00'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: r"'\u000'", tok: "CHAR", pos: 6, lit: r"'\u000'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    // escape sequence not terminated (no closing quote)
    GoRow { src: r"'\u000", tok: "CHAR", pos: 6, lit: r"'\u000", err: "escape sequence not terminated",
        expect: Expect::ErrorWithDiag, portable: true, note: "tu lexer verá Error token(s); no matchea Rune" },

    GoRow { src: r"'\u0000'", tok: "CHAR", pos: 0, lit: r"'\u0000'", err: "",
        expect: Expect::Kinds(&["RuneLit"]), portable: true, note: "" },

    GoRow { src: r"'\U'", tok: "CHAR", pos: 3, lit: r"'\U'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: r"'\U0'", tok: "CHAR", pos: 4, lit: r"'\U0'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: r"'\U00'", tok: "CHAR", pos: 5, lit: r"'\U00'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: r"'\U000'", tok: "CHAR", pos: 6, lit: r"'\U000'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: r"'\U0000'", tok: "CHAR", pos: 7, lit: r"'\U0000'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: r"'\U00000'", tok: "CHAR", pos: 8, lit: r"'\U00000'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: r"'\U000000'", tok: "CHAR", pos: 9, lit: r"'\U000000'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: r"'\U0000000'", tok: "CHAR", pos: 10, lit: r"'\U0000000'", err: "illegal ... in escape sequence",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    // escape sequence not terminated (no closing quote)
    GoRow { src: r"'\U0000000", tok: "CHAR", pos: 10, lit: r"'\U0000000", err: "escape sequence not terminated",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    GoRow { src: r"'\U00000000'", tok: "CHAR", pos: 0, lit: r"'\U00000000'", err: "",
        expect: Expect::Kinds(&["RuneLit"]), portable: true, note: "" },

    GoRow { src: r"'\Uffffffff'", tok: "CHAR", pos: 2, lit: r"'\Uffffffff'", err: "escape sequence is invalid Unicode code point",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    GoRow { src: "'", tok: "CHAR", pos: 0, lit: "'", err: "rune literal not terminated",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: r"'\", tok: "CHAR", pos: 2, lit: r"'\", err: "escape sequence not terminated",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    GoRow { src: "'\n", tok: "CHAR", pos: 0, lit: "'", err: "rune literal not terminated",
        expect: Expect::ErrorWithDiag, portable: true, note: "la newline es trivia; igual debe haber Error+diag" },
    GoRow { src: "'\n   ", tok: "CHAR", pos: 0, lit: "'", err: "rune literal not terminated",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    // Strings
    GoRow { src: r#""""#, tok: "STRING", pos: 0, lit: r#""""#, err: "",
        expect: Expect::Kinds(&["StringLit"]), portable: true, note: "" },

    GoRow { src: r#""abc"#, tok: "STRING", pos: 0, lit: r#""abc"#, err: "string literal not terminated",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: "\"abc\n", tok: "STRING", pos: 0, lit: "\"abc", err: "string literal not terminated",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },
    GoRow { src: "\"abc\n   ", tok: "STRING", pos: 0, lit: "\"abc", err: "string literal not terminated",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    // Raw strings
    GoRow { src: "``", tok: "STRING", pos: 0, lit: "``", err: "",
        expect: Expect::Kinds(&["RawStringLit"]), portable: true, note: "" },
    GoRow { src: "`", tok: "STRING", pos: 0, lit: "`", err: "raw string literal not terminated",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    // Comments
    GoRow { src: "/**/", tok: "COMMENT", pos: 0, lit: "/**/", err: "",
        expect: Expect::Kinds(&[]), portable: true, note: "tu lexer descarta block comments (trivia) => no tokens" },

    GoRow { src: "/*", tok: "COMMENT", pos: 0, lit: "/*", err: "comment not terminated",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    // Numbers
    GoRow { src: "077", tok: "INT", pos: 0, lit: "077", err: "",
        expect: Expect::Kinds(&["IntLit"]), portable: true, note: "" },

    GoRow { src: "078.", tok: "FLOAT", pos: 0, lit: "078.", err: "",
        expect: Expect::Kinds(&["FloatLit"]), portable: true, note: "en tu DFA esto es float válido" },
    GoRow { src: "07801234567.", tok: "FLOAT", pos: 0, lit: "07801234567.", err: "",
        expect: Expect::Kinds(&["FloatLit"]), portable: true, note: "" },
    GoRow { src: "078e0", tok: "FLOAT", pos: 0, lit: "078e0", err: "",
        expect: Expect::Kinds(&["FloatLit"]), portable: true, note: "" },

    GoRow { src: "0E", tok: "FLOAT", pos: 2, lit: "0E", err: "exponent has no digits",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    GoRow { src: "078", tok: "INT", pos: 2, lit: "078", err: "invalid digit '8' in octal literal",
        expect: Expect::ErrorWithDiag, portable: true, note: "tu classify_number falla => Tok::Error" },
    GoRow { src: "07090000008", tok: "INT", pos: 3, lit: "07090000008", err: "invalid digit '9' in octal literal",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    GoRow { src: "0x", tok: "INT", pos: 2, lit: "0x", err: "hexadecimal literal has no digits",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    // NUL inside interpreted string: tu lexer HOY lo acepta (no chequea NUL en body)
    GoRow { src: "\"abc\u{0000}def\"", tok: "STRING", pos: 4, lit: "\"abc\\x00def\"", err: "illegal character NUL",
        expect: Expect::ErrorWithDiag, portable: false, note: "requiere rechazar NUL dentro de literales string" },

    // invalid UTF-8 byte 0x80 inside literal: NO portable con &str (Rust lo codifica como U+0080 válido)
    GoRow { src: "\"abc\u{0080}def\"", tok: "STRING", pos: 4, lit: "\"abc\\x80def\"", err: "illegal UTF-8 encoding",
        expect: Expect::ErrorWithDiag, portable: false, note: "para esto necesitas Lexer::new_bytes(&[u8])" },

    // BOM rules
    GoRow { src: "\u{FEFF}\u{FEFF}", tok: "ILLEGAL", pos: 3, lit: "\u{FEFF}\u{FEFF}", err: "illegal byte order mark",
        expect: Expect::ErrorWithDiag, portable: true, note: "primer BOM se ignora; segundo debe dar Error+diag" },

    // BOM inside line comment: NO portable si skippeas line comments con logos
    GoRow { src: "//\u{FEFF}", tok: "COMMENT", pos: 2, lit: "//\u{FEFF}", err: "illegal byte order mark",
        expect: Expect::ErrorWithDiag, portable: false, note: "requiere NO skippear line comments o validar source bytes" },

    // BOM in rune literal: tu rune validator NO lo ve como BOM token; lo ve como rune body.
    // Go lo prohíbe como BOM “solo al inicio”. Si quieres igualar a Go, hay que rechazar U+FEFF en literal bodies.
    GoRow { src: "'\u{FEFF}'", tok: "CHAR", pos: 1, lit: "'\u{FEFF}'", err: "illegal byte order mark",
        expect: Expect::ErrorWithDiag, portable: false, note: "requiere rechazar BOM dentro de literales (rune/string/raw)" },

    GoRow { src: "\"abc\u{FEFF}def\"", tok: "STRING", pos: 4, lit: "\"abc\\ufeffdef\"", err: "illegal byte order mark",
        expect: Expect::ErrorWithDiag, portable: false, note: "requiere rechazar BOM dentro de literales" },

    // NUL in identifiers (portable)
    GoRow { src: "abc\u{0000}def", tok: "IDENT", pos: 3, lit: "abc", err: "illegal character NUL",
        expect: Expect::ErrorWithDiag, portable: true, note: "tu lexer debería emitir Ident('abc') luego Error" },
    GoRow { src: "abc\u{0000}", tok: "IDENT", pos: 3, lit: "abc", err: "illegal character NUL",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    // Curly quotes
    GoRow { src: "“abc”", tok: "ILLEGAL", pos: 0, lit: "abc", err: "curly quotation mark ...",
        expect: Expect::ErrorWithDiag, portable: true, note: "" },

    // Extra: caso que estaba fallando: "\U0000FFFFF"
    // En Go NO es error: \U consume 8 hex; el 'F' extra queda como literal en el string.
    GoRow { src: "\"\\U0000FFFFF\"", tok: "STRING", pos: 0, lit: "\"\\U0000FFFFF\"", err: "",
        expect: Expect::Kinds(&["StringLit"]), portable: true,
        note: r#"Go consume 8 hex en \U; el 9no 'F' queda literal. No hay "invalid escape length"."# },
];

#[test]
fn go_scan_errors_ported_table() {
    let mut ran = 0usize;
    let mut skipped = 0usize;

    for row in GO_ERRORS_TABLE {
        if !row.portable {
            skipped += 1;
            continue;
        }
        ran += 1;

        let (toks, diags) = lex_all(row.src);
        let ks = kinds_no_injected_semis(&toks);

        match row.expect {
            Expect::Kinds(want) => {
                assert!(
                    diags.is_empty(),
                    "src={:?}\nexpected no diags (Go err={:?})\nGot diags: {:?}\nTokens={:?}\nKinds={:?}\nNote={}",
                    row.src, row.err, diags, toks, ks, row.note
                );
                assert_eq!(
                    ks, want,
                    "src={:?}\nGo tok={} pos={} lit={:?} err={:?}\nTokens={:?}\nKinds={:?}\nNote={}",
                    row.src, row.tok, row.pos, row.lit, row.err, toks, ks, row.note
                );
            }
            Expect::ErrorWithDiag => {
                let has_error_tok = toks.iter().any(|(_, t, _)| matches!(t, Tok::Error));
                assert!(
                    has_error_tok,
                    "src={:?}\nexpected Tok::Error\nGo tok={} pos={} lit={:?} err={:?}\nTokens={:?}\nKinds={:?}\nNote={}",
                    row.src, row.tok, row.pos, row.lit, row.err, toks, ks, row.note
                );
                assert!(
                    !diags.is_empty(),
                    "src={:?}\nexpected at least one diag\nGo tok={} pos={} lit={:?} err={:?}\nTokens={:?}\nKinds={:?}\nNote={}",
                    row.src, row.tok, row.pos, row.lit, row.err, toks, ks, row.note
                );
            }
        }
    }

    // Para que sea obvio en CI qué tanto corpus está activo:
    assert!(ran >= 40, "ran only {ran} cases; table/skips mismatch?");
    eprintln!("go_scan_errors_ported_table: ran={ran} skipped(non-portable)={skipped}");
}
