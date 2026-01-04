// crates/parser/tests/lexer_scan_table.rs
use go125_parser::error::Diag;
use go125_parser::lexer::{Lexer, Tok};

#[derive(Clone, Copy, Debug)]
struct Expected {
    kind: &'static str,
    lit: &'static str,
}

fn kind_name(t: &Tok<'_>) -> &'static str {
    match t {
        Tok::Underscore => "Underscore",

        Tok::Ident(_) => "Ident",
        Tok::IntLit(_) => "IntLit",
        Tok::FloatLit(_) => "FloatLit",
        Tok::ImagLit(_) => "ImagLit",
        Tok::RuneLit(_) => "RuneLit",
        Tok::StringLit(_) => "StringLit",
        Tok::RawStringLit(_) => "RawStringLit",

        // Keywords
        Tok::KwBreak => "KwBreak",
        Tok::KwCase => "KwCase",
        Tok::KwChan => "KwChan",
        Tok::KwConst => "KwConst",
        Tok::KwContinue => "KwContinue",
        Tok::KwDefault => "KwDefault",
        Tok::KwDefer => "KwDefer",
        Tok::KwElse => "KwElse",
        Tok::KwFallthrough => "KwFallthrough",
        Tok::KwFor => "KwFor",
        Tok::KwFunc => "KwFunc",
        Tok::KwGo => "KwGo",
        Tok::KwGoto => "KwGoto",
        Tok::KwIf => "KwIf",
        Tok::KwImport => "KwImport",
        Tok::KwInterface => "KwInterface",
        Tok::KwMap => "KwMap",
        Tok::KwPackage => "KwPackage",
        Tok::KwRange => "KwRange",
        Tok::KwReturn => "KwReturn",
        Tok::KwSelect => "KwSelect",
        Tok::KwStruct => "KwStruct",
        Tok::KwSwitch => "KwSwitch",
        Tok::KwType => "KwType",
        Tok::KwVar => "KwVar",

        // Operators / Delimiters
        Tok::Ellipsis => "Ellipsis",
        Tok::ShlAssign => "ShlAssign",
        Tok::ShrAssign => "ShrAssign",
        Tok::AndNotAssign => "AndNotAssign",
        Tok::AddAssign => "AddAssign",
        Tok::SubAssign => "SubAssign",
        Tok::MulAssign => "MulAssign",
        Tok::DivAssign => "DivAssign",
        Tok::ModAssign => "ModAssign",
        Tok::AndAssign => "AndAssign",
        Tok::OrAssign => "OrAssign",
        Tok::XorAssign => "XorAssign",
        Tok::Shl => "Shl",
        Tok::Shr => "Shr",
        Tok::AndNot => "AndNot",
        Tok::LAnd => "LAnd",
        Tok::LOr => "LOr",
        Tok::EqEq => "EqEq",
        Tok::NotEq => "NotEq",
        Tok::Le => "Le",
        Tok::Ge => "Ge",
        Tok::Inc => "Inc",
        Tok::Dec => "Dec",
        Tok::Define => "Define",
        Tok::Arrow => "Arrow",
        Tok::Assign => "Assign",
        Tok::Plus => "Plus",
        Tok::Minus => "Minus",
        Tok::Star => "Star",
        Tok::Slash => "Slash",
        Tok::Percent => "Percent",
        Tok::Amp => "Amp",
        Tok::Pipe => "Pipe",
        Tok::Caret => "Caret",
        Tok::Tilde => "Tilde",
        Tok::Bang => "Bang",
        Tok::Lt => "Lt",
        Tok::Gt => "Gt",
        Tok::LParen => "LParen",
        Tok::RParen => "RParen",
        Tok::LBrack => "LBrack",
        Tok::RBrack => "RBrack",
        Tok::LBrace => "LBrace",
        Tok::RBrace => "RBrace",
        Tok::Comma => "Comma",
        Tok::Semi => "Semi",
        Tok::Colon => "Colon",
        Tok::Dot => "Dot",

        Tok::Error => "Error",
    }
}

fn lex_all(input: &str) -> (Vec<(usize, Tok<'_>, usize)>, Vec<Diag>) {
    let mut lx = Lexer::new(input);
    let toks: Vec<_> = lx.by_ref().collect();
    let diags = lx.take_diags();
    (toks, diags)
}

#[test]
fn test_scan_table_kinds_and_spans() {
    // NOTE:
    // - This is the Go scanner's "TestScan" concept adapted to our lexer API.
    // - We intentionally avoid '\n' separators to avoid semicolon insertion noise.
    const SEP: &str = "  \t  ";

    // Comments are omitted here because our lexer treats them as trivia (skipped).
    // (We test comment/newline/semicolon behavior in lexer_semis.rs and lexer_golden.rs.)
    const TOKENS: &[Expected] = &[
        // Identifiers (including Unicode letters + Nd digits)
        Expected {
            kind: "Ident",
            lit: "foobar",
        },
        Expected {
            kind: "Ident",
            lit: "a۰۱۸",
        },
        Expected {
            kind: "Ident",
            lit: "foo६४",
        },
        Expected {
            kind: "Ident",
            lit: "bar９８７６",
        },
        Expected {
            kind: "Ident",
            lit: "ŝ",
        },
        Expected {
            kind: "Ident",
            lit: "ŝfoo",
        },
        // Integers
        Expected {
            kind: "IntLit",
            lit: "0",
        },
        Expected {
            kind: "IntLit",
            lit: "1",
        },
        Expected {
            kind: "IntLit",
            lit: "123456789012345678890",
        },
        Expected {
            kind: "IntLit",
            lit: "01234567",
        },
        Expected {
            kind: "IntLit",
            lit: "0xcafebabe",
        },
        // Floats (decimal)
        Expected {
            kind: "FloatLit",
            lit: "0.",
        },
        Expected {
            kind: "FloatLit",
            lit: ".0",
        },
        Expected {
            kind: "FloatLit",
            lit: "3.14159265",
        },
        Expected {
            kind: "FloatLit",
            lit: "1e0",
        },
        Expected {
            kind: "FloatLit",
            lit: "1e+100",
        },
        Expected {
            kind: "FloatLit",
            lit: "1e-100",
        },
        Expected {
            kind: "FloatLit",
            lit: "2.71828e-1000",
        },
        // Imaginary
        Expected {
            kind: "ImagLit",
            lit: "0i",
        },
        Expected {
            kind: "ImagLit",
            lit: "1i",
        },
        Expected {
            kind: "ImagLit",
            lit: "012345678901234567889i",
        },
        Expected {
            kind: "ImagLit",
            lit: "123456789012345678890i",
        },
        Expected {
            kind: "ImagLit",
            lit: "0.i",
        },
        Expected {
            kind: "ImagLit",
            lit: ".0i",
        },
        Expected {
            kind: "ImagLit",
            lit: "3.14159265i",
        },
        Expected {
            kind: "ImagLit",
            lit: "1e0i",
        },
        Expected {
            kind: "ImagLit",
            lit: "1e+100i",
        },
        Expected {
            kind: "ImagLit",
            lit: "1e-100i",
        },
        Expected {
            kind: "ImagLit",
            lit: "2.71828e-1000i",
        },
        // Rune literals
        Expected {
            kind: "RuneLit",
            lit: "'a'",
        },
        Expected {
            kind: "RuneLit",
            lit: "'\\000'",
        },
        Expected {
            kind: "RuneLit",
            lit: "'\\xFF'",
        },
        Expected {
            kind: "RuneLit",
            lit: "'\\uff16'",
        },
        Expected {
            kind: "RuneLit",
            lit: "'\\U0000ff16'",
        },
        // Strings
        Expected {
            kind: "StringLit",
            lit: "\"foobar\"",
        },
        Expected {
            kind: "RawStringLit",
            lit: "`foobar`",
        },
        Expected {
            kind: "RawStringLit",
            lit: "`foo\nbar`",
        },
        Expected {
            kind: "RawStringLit",
            lit: "`\r`",
        },
        Expected {
            kind: "RawStringLit",
            lit: "`foo\r\nbar`",
        },
        // Operators and delimiters
        Expected {
            kind: "Plus",
            lit: "+",
        },
        Expected {
            kind: "Minus",
            lit: "-",
        },
        Expected {
            kind: "Star",
            lit: "*",
        },
        Expected {
            kind: "Slash",
            lit: "/",
        },
        Expected {
            kind: "Percent",
            lit: "%",
        },
        Expected {
            kind: "Amp",
            lit: "&",
        },
        Expected {
            kind: "Pipe",
            lit: "|",
        },
        Expected {
            kind: "Caret",
            lit: "^",
        },
        Expected {
            kind: "Shl",
            lit: "<<",
        },
        Expected {
            kind: "Shr",
            lit: ">>",
        },
        Expected {
            kind: "AndNot",
            lit: "&^",
        },
        Expected {
            kind: "AddAssign",
            lit: "+=",
        },
        Expected {
            kind: "SubAssign",
            lit: "-=",
        },
        Expected {
            kind: "MulAssign",
            lit: "*=",
        },
        Expected {
            kind: "DivAssign",
            lit: "/=",
        },
        Expected {
            kind: "ModAssign",
            lit: "%=",
        },
        Expected {
            kind: "AndAssign",
            lit: "&=",
        },
        Expected {
            kind: "OrAssign",
            lit: "|=",
        },
        Expected {
            kind: "XorAssign",
            lit: "^=",
        },
        Expected {
            kind: "ShlAssign",
            lit: "<<=",
        },
        Expected {
            kind: "ShrAssign",
            lit: ">>=",
        },
        Expected {
            kind: "AndNotAssign",
            lit: "&^=",
        },
        Expected {
            kind: "LAnd",
            lit: "&&",
        },
        Expected {
            kind: "LOr",
            lit: "||",
        },
        Expected {
            kind: "Arrow",
            lit: "<-",
        },
        Expected {
            kind: "Inc",
            lit: "++",
        },
        Expected {
            kind: "Dec",
            lit: "--",
        },
        Expected {
            kind: "EqEq",
            lit: "==",
        },
        Expected {
            kind: "Lt",
            lit: "<",
        },
        Expected {
            kind: "Gt",
            lit: ">",
        },
        Expected {
            kind: "Assign",
            lit: "=",
        },
        Expected {
            kind: "Bang",
            lit: "!",
        },
        Expected {
            kind: "NotEq",
            lit: "!=",
        },
        Expected {
            kind: "Le",
            lit: "<=",
        },
        Expected {
            kind: "Ge",
            lit: ">=",
        },
        Expected {
            kind: "Define",
            lit: ":=",
        },
        Expected {
            kind: "Ellipsis",
            lit: "...",
        },
        Expected {
            kind: "LParen",
            lit: "(",
        },
        Expected {
            kind: "LBrack",
            lit: "[",
        },
        Expected {
            kind: "LBrace",
            lit: "{",
        },
        Expected {
            kind: "Comma",
            lit: ",",
        },
        Expected {
            kind: "Dot",
            lit: ".",
        },
        Expected {
            kind: "RParen",
            lit: ")",
        },
        Expected {
            kind: "RBrack",
            lit: "]",
        },
        Expected {
            kind: "RBrace",
            lit: "}",
        },
        Expected {
            kind: "Semi",
            lit: ";",
        },
        Expected {
            kind: "Colon",
            lit: ":",
        },
        Expected {
            kind: "Tilde",
            lit: "~",
        },
        // Keywords
        Expected {
            kind: "KwBreak",
            lit: "break",
        },
        Expected {
            kind: "KwCase",
            lit: "case",
        },
        Expected {
            kind: "KwChan",
            lit: "chan",
        },
        Expected {
            kind: "KwConst",
            lit: "const",
        },
        Expected {
            kind: "KwContinue",
            lit: "continue",
        },
        Expected {
            kind: "KwDefault",
            lit: "default",
        },
        Expected {
            kind: "KwDefer",
            lit: "defer",
        },
        Expected {
            kind: "KwElse",
            lit: "else",
        },
        Expected {
            kind: "KwFallthrough",
            lit: "fallthrough",
        },
        Expected {
            kind: "KwFor",
            lit: "for",
        },
        Expected {
            kind: "KwFunc",
            lit: "func",
        },
        Expected {
            kind: "KwGo",
            lit: "go",
        },
        Expected {
            kind: "KwGoto",
            lit: "goto",
        },
        Expected {
            kind: "KwIf",
            lit: "if",
        },
        Expected {
            kind: "KwImport",
            lit: "import",
        },
        Expected {
            kind: "KwInterface",
            lit: "interface",
        },
        Expected {
            kind: "KwMap",
            lit: "map",
        },
        Expected {
            kind: "KwPackage",
            lit: "package",
        },
        Expected {
            kind: "KwRange",
            lit: "range",
        },
        Expected {
            kind: "KwReturn",
            lit: "return",
        },
        Expected {
            kind: "KwSelect",
            lit: "select",
        },
        Expected {
            kind: "KwStruct",
            lit: "struct",
        },
        Expected {
            kind: "KwSwitch",
            lit: "switch",
        },
        Expected {
            kind: "KwType",
            lit: "type",
        },
        Expected {
            kind: "KwVar",
            lit: "var",
        },
    ];

    // Build a single input buffer, recording expected spans.
    let mut src = String::new();
    let mut expected_spans: Vec<(usize, usize, Expected)> = Vec::with_capacity(TOKENS.len());

    for &e in TOKENS {
        let start = src.len();
        src.push_str(e.lit);
        let end = src.len();
        expected_spans.push((start, end, e));
        src.push_str(SEP);
    }

    let (toks, diags) = lex_all(&src);
    assert!(
        diags.is_empty(),
        "expected no diags for scan-table corpus, got: {diags:?}"
    );
    assert!(
        !toks.iter().any(|(_, t, _)| matches!(t, Tok::Error)),
        "unexpected Tok::Error in scan-table corpus: toks={toks:?}"
    );

    assert_eq!(
        toks.len(),
        expected_spans.len(),
        "token count mismatch\nexpected={} got={}\nfirst few toks={:?}",
        expected_spans.len(),
        toks.len(),
        toks.iter().take(12).collect::<Vec<_>>()
    );

    for (i, ((exp_s, exp_e, exp), (s, t, e))) in expected_spans.iter().zip(toks.iter()).enumerate()
    {
        assert_eq!(
            *s, *exp_s,
            "start offset mismatch at #{i}: expected {exp_s}, got {s}, tok={t:?}"
        );
        assert_eq!(
            *e, *exp_e,
            "end offset mismatch at #{i}: expected {exp_e}, got {e}, tok={t:?}"
        );

        let got_kind = kind_name(t);
        assert_eq!(
            got_kind,
            exp.kind,
            "kind mismatch at #{i}: expected {}, got {}, span=[{},{}), slice={:?}, tok={t:?}",
            exp.kind,
            got_kind,
            s,
            e,
            &src[*s..*e],
        );

        // For this scan-table, we also assert the span slice equals the literal we injected.
        assert_eq!(
            &src[*s..*e],
            exp.lit,
            "literal slice mismatch at #{i}: expected {:?}, got {:?}",
            exp.lit,
            &src[*s..*e],
        );
    }
}
