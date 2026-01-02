// crates/parser/tests/lexer_semis.rs
use go125_parser::lexer::{Lexer, Tok};

fn injected_semis(input: &str) -> Vec<usize> {
    let mut lx = Lexer::new(input);
    let toks: Vec<_> = lx.by_ref().collect();
    toks.into_iter()
        .filter_map(|(s, t, e)| {
            if matches!(t, Tok::Semi) && s == e {
                Some(s)
            } else {
                None
            }
        })
        .collect()
}

fn tok_name(t: &Tok<'_>) -> &'static str {
    match t {
        Tok::Ident(_) => "IDENT",
        Tok::IntLit(_) => "INT",
        Tok::FloatLit(_) => "FLOAT",
        Tok::ImagLit(_) => "IMAG",
        Tok::RuneLit(_) => "CHAR",
        Tok::StringLit(_) => "STRING",
        Tok::RawStringLit(_) => "STRING",

        Tok::Semi => ";",

        Tok::Plus => "+",
        Tok::Minus => "-",
        Tok::Star => "*",
        Tok::Slash => "/",
        Tok::Percent => "%",

        Tok::Amp => "&",
        Tok::Pipe => "|",
        Tok::Caret => "^",
        Tok::Shl => "<<",
        Tok::Shr => ">>",
        Tok::AndNot => "&^",

        Tok::AddAssign => "+=",
        Tok::SubAssign => "-=",
        Tok::MulAssign => "*=",
        Tok::DivAssign => "/=",
        Tok::ModAssign => "%=",

        Tok::AndAssign => "&=",
        Tok::OrAssign => "|=",
        Tok::XorAssign => "^=",
        Tok::ShlAssign => "<<=",
        Tok::ShrAssign => ">>=",
        Tok::AndNotAssign => "&^=",

        Tok::LAnd => "&&",
        Tok::LOr => "||",
        Tok::Arrow => "<-",
        Tok::Inc => "++",
        Tok::Dec => "--",

        Tok::EqEq => "==",
        Tok::NotEq => "!=",
        Tok::Lt => "<",
        Tok::Gt => ">",
        Tok::Le => "<=",
        Tok::Ge => ">=",
        Tok::Assign => "=",
        Tok::Bang => "!",
        Tok::Define => ":=",
        Tok::Ellipsis => "...",
        Tok::Tilde => "~",

        Tok::LParen => "(",
        Tok::RParen => ")",
        Tok::LBrack => "[",
        Tok::RBrack => "]",
        Tok::LBrace => "{",
        Tok::RBrace => "}",
        Tok::Comma => ",",
        Tok::Dot => ".",
        Tok::Colon => ":",

        Tok::KwBreak => "break",
        Tok::KwCase => "case",
        Tok::KwChan => "chan",
        Tok::KwConst => "const",
        Tok::KwContinue => "continue",
        Tok::KwDefault => "default",
        Tok::KwDefer => "defer",
        Tok::KwElse => "else",
        Tok::KwFallthrough => "fallthrough",
        Tok::KwFor => "for",
        Tok::KwFunc => "func",
        Tok::KwGo => "go",
        Tok::KwGoto => "goto",
        Tok::KwIf => "if",
        Tok::KwImport => "import",
        Tok::KwInterface => "interface",
        Tok::KwMap => "map",
        Tok::KwPackage => "package",
        Tok::KwRange => "range",
        Tok::KwReturn => "return",
        Tok::KwSelect => "select",
        Tok::KwStruct => "struct",
        Tok::KwSwitch => "switch",
        Tok::KwType => "type",
        Tok::KwVar => "var",

        Tok::Error => "ERROR",
    }
}

fn lex_names(input: &str) -> Vec<&'static str> {
    let lx = Lexer::new(input);
    let mut out = Vec::new();
    for (_s, t, _e) in lx {
        out.push(tok_name(&t));
    }
    out
}

fn normalize_want(want: &str) -> String {
    // Nuestro lexer no emite COMMENT tokens => los ignoramos para poder reutilizar la tabla de Go.
    want.split_whitespace()
        .filter(|w| *w != "COMMENT")
        .collect::<Vec<_>>()
        .join(" ")
}

fn check_semi_case(input: &str, want: &str) {
    let want = normalize_want(want);
    let got = lex_names(input).join(" ");
    assert_eq!(got, want, "input=<<{input}>> got=[{got}] want=[{want}]");
}

struct SemiCase {
    input: &'static str,
    want: &'static str,
}

// Subset “oro” de la tabla gigante de Go (puedes ampliarla 1:1 si quieres).
// Mantengo COMMENT en want para facilitar copy/paste desde Go; el runner lo strippea.
#[rustfmt::skip]
const SEMICOLON_TESTS: &[SemiCase] = &[
    SemiCase { input: "", want: "" },
    SemiCase { input: "\u{FEFF};", want: ";" }, // first BOM is ignored
    SemiCase { input: ";", want: ";" },

    SemiCase { input: "foo\n", want: "IDENT ;" },
    SemiCase { input: "123\n", want: "INT ;" },
    SemiCase { input: "1.2\n", want: "FLOAT ;" },
    SemiCase { input: "'x'\n", want: "CHAR ;" },
    SemiCase { input: "\"x\"\n", want: "STRING ;" },
    SemiCase { input: "`x`\n", want: "STRING ;" },

    SemiCase { input: "+\n", want: "+" },
    SemiCase { input: "-\n", want: "-" },
    SemiCase { input: "*\n", want: "*" },
    SemiCase { input: "/\n", want: "/" },
    SemiCase { input: "%\n", want: "%" },

    SemiCase { input: "&\n", want: "&" },
    SemiCase { input: "|\n", want: "|" },
    SemiCase { input: "^\n", want: "^" },
    SemiCase { input: "<<\n", want: "<<" },
    SemiCase { input: ">>\n", want: ">>" },
    SemiCase { input: "&^\n", want: "&^" },

    SemiCase { input: "+=\n", want: "+=" },
    SemiCase { input: "-=\n", want: "-=" },
    SemiCase { input: "*=\n", want: "*=" },
    SemiCase { input: "/=\n", want: "/=" },
    SemiCase { input: "%=\n", want: "%=" },

    SemiCase { input: "&=\n", want: "&=" },
    SemiCase { input: "|=\n", want: "|=" },
    SemiCase { input: "^=\n", want: "^=" },
    SemiCase { input: "<<=\n", want: "<<=" },
    SemiCase { input: ">>=\n", want: ">>=" },
    SemiCase { input: "&^=\n", want: "&^=" },

    SemiCase { input: "&&\n", want: "&&" },
    SemiCase { input: "||\n", want: "||" },
    SemiCase { input: "<-\n", want: "<-" },
    SemiCase { input: "++\n", want: "++ ;" },
    SemiCase { input: "--\n", want: "-- ;" },

    SemiCase { input: "==\n", want: "==" },
    SemiCase { input: "<\n", want: "<" },
    SemiCase { input: ">\n", want: ">" },
    SemiCase { input: "=\n", want: "=" },
    SemiCase { input: "!\n", want: "!" },

    SemiCase { input: "!=\n", want: "!=" },
    SemiCase { input: "<=\n", want: "<=" },
    SemiCase { input: ">=\n", want: ">=" },
    SemiCase { input: ":=\n", want: ":=" },
    SemiCase { input: "...\n", want: "..." },

    SemiCase { input: "(\n", want: "(" },
    SemiCase { input: "[\n", want: "[" },
    SemiCase { input: "{\n", want: "{" },
    SemiCase { input: ",\n", want: "," },
    SemiCase { input: ".\n", want: "." },

    SemiCase { input: ")\n", want: ") ;" },
    SemiCase { input: "]\n", want: "] ;" },
    SemiCase { input: "}\n", want: "} ;" },
    SemiCase { input: ";\n", want: ";" },
    SemiCase { input: ":\n", want: ":" },

    SemiCase { input: "break\n", want: "break ;" },
    SemiCase { input: "case\n", want: "case" },
    SemiCase { input: "chan\n", want: "chan" },
    SemiCase { input: "const\n", want: "const" },
    SemiCase { input: "continue\n", want: "continue ;" },

    SemiCase { input: "default\n", want: "default" },
    SemiCase { input: "defer\n", want: "defer" },
    SemiCase { input: "else\n", want: "else" },
    SemiCase { input: "fallthrough\n", want: "fallthrough ;" },
    SemiCase { input: "for\n", want: "for" },

    SemiCase { input: "func\n", want: "func" },
    SemiCase { input: "go\n", want: "go" },
    SemiCase { input: "goto\n", want: "goto" },
    SemiCase { input: "if\n", want: "if" },
    SemiCase { input: "import\n", want: "import" },

    SemiCase { input: "interface\n", want: "interface" },
    SemiCase { input: "map\n", want: "map" },
    SemiCase { input: "package\n", want: "package" },
    SemiCase { input: "range\n", want: "range" },
    SemiCase { input: "return\n", want: "return ;" },

    SemiCase { input: "select\n", want: "select" },
    SemiCase { input: "struct\n", want: "struct" },
    SemiCase { input: "switch\n", want: "switch" },
    SemiCase { input: "type\n", want: "type" },
    SemiCase { input: "var\n", want: "var" },

    SemiCase { input: "foo//comment\n", want: "IDENT COMMENT ;" },
    SemiCase { input: "foo//comment", want: "IDENT COMMENT ;" },
    SemiCase { input: "foo/*comment*/\n", want: "IDENT COMMENT ;" },
    SemiCase { input: "foo/*\n*/", want: "IDENT COMMENT ;" },
    SemiCase { input: "foo/*comment*/    \n", want: "IDENT COMMENT ;" },
    SemiCase { input: "foo/*\n*/    ", want: "IDENT COMMENT ;" },

    SemiCase { input: "foo    // comment\n", want: "IDENT COMMENT ;" },
    SemiCase { input: "foo    // comment", want: "IDENT COMMENT ;" },
    SemiCase { input: "foo    /*comment*/\n", want: "IDENT COMMENT ;" },
    SemiCase { input: "foo    /*\n*/", want: "IDENT COMMENT ;" },

    SemiCase {
        input: "package main\n\nfunc main() {\n\tif {\n\t\treturn /* */ }\n}\n",
        want: "package IDENT ; func IDENT ( ) { if { return COMMENT } ; } ;",
    },
    SemiCase { input: "package main", want: "package IDENT ;" },
];

#[test]
fn test_semicolons_table_like_go() {
    for t in SEMICOLON_TESTS {
        check_semi_case(t.input, t.want);

        // Igual que Go: si el input termina en newlines, debe tokenizar igual sin esos '\n'.
        let mut trimmed = t.input;
        while trimmed.ends_with('\n') {
            trimmed = &trimmed[..trimmed.len() - 1];
            check_semi_case(trimmed, t.want);
        }
    }
}

// -----------------------------------------------------------------------------
// Tus tests existentes (se quedan)
// -----------------------------------------------------------------------------

#[test]
fn many_semis_mixed() {
    let src = r#"
package p
func f() {
    x := 1
    x++
    if x > 0 {
        return
    } else {
        x--
    }
}
"#;
    let semis = injected_semis(src);
    assert!(!semis.is_empty());
}

#[test]
fn comment_newline_equivalence() {
    let a = injected_semis("x/*\n*/y");
    let b = injected_semis("x\ny");
    assert_eq!(a.len(), b.len());
}

#[test]
fn semicolon_insertion_windows_newline_crlf() {
    assert_eq!(injected_semis("x\r\ny"), vec![1, 4]);
}

#[test]
fn semicolon_insertion_block_comment_with_cr_acts_like_newline() {
    assert_eq!(injected_semis("x/*\r*/y"), vec![3, 7]);
}

#[test]
fn block_comment_newline_does_not_insert_after_if() {
    assert_eq!(injected_semis("if/*\n*/x"), vec![8]);
}

#[test]
fn line_comment_at_eof_ok() {
    let src = "x//c";
    assert_eq!(injected_semis(src), vec![src.len()]);
}

#[test]
fn line_comment_before_crlf_ok() {
    let src = "x//c\r\ny";
    let cr_pos = src.find('\r').unwrap();
    assert_eq!(injected_semis(src), vec![cr_pos, src.len()]);
}

#[test]
fn line_comment_before_cr_ok() {
    let src = "x//c\ry";
    let cr_pos = src.find('\r').unwrap();
    assert_eq!(injected_semis(src), vec![cr_pos, src.len()]);
}

#[test]
fn semicolon_insertion_after_break_continue_fallthrough() {
    assert_eq!(injected_semis("break\nx"), vec![5, 7]);
    assert_eq!(injected_semis("continue\nx"), vec![8, 10]);
    assert_eq!(injected_semis("fallthrough\nx"), vec![11, 13]);
}

#[test]
fn semicolon_insertion_after_inc_dec() {
    assert_eq!(injected_semis("x++\ny"), vec![3, 5]);
    assert_eq!(injected_semis("x--\ny"), vec![3, 5]);
}

#[test]
fn semicolon_insertion_after_imag_literal() {
    assert_eq!(injected_semis("1i\nx"), vec![2, 4]);
}
