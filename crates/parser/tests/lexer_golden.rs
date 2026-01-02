// crates/parser/tests/lexer_golden.rs
use go125_parser::error::Diag;
use go125_parser::lexer::{Lexer, Tok};

fn lex_all(input: &str) -> (Vec<(usize, Tok<'_>, usize)>, Vec<Diag>) {
    let mut lx = Lexer::new(input);
    let toks: Vec<_> = lx.by_ref().collect();
    let diags = lx.take_diags();
    (toks, diags)
}

fn kinds(toks: &[(usize, Tok<'_>, usize)]) -> Vec<&'static str> {
    toks.iter().map(|(_, t, _)| kind_name(t)).collect()
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

/// Collect positions of *injected* semicolons (the ones your lexer emits with start==end).
fn injected_semis(input: &str) -> Vec<usize> {
    let (toks, _diags) = lex_all(input);
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

fn assert_injected_semis(input: &str, expected_positions: &[usize]) {
    let got = injected_semis(input);
    assert_eq!(got, expected_positions);
}

#[test]
fn bom_only_at_start_ok() {
    // NOTE: semicolon insertion at EOF is expected because the last token is an identifier.
    let (toks, diags) = lex_all("\u{FEFF}package p");
    assert!(diags.is_empty());
    assert_eq!(kinds(&toks), vec!["KwPackage", "Ident", "Semi"]);
}

#[test]
fn bom_not_at_start_is_error() {
    let (toks, diags) = lex_all("package\u{FEFF} p");
    assert!(!diags.is_empty());
    assert!(kinds(&toks).contains(&"Error"));
}

#[test]
fn semicolon_insertion_basic_newline() {
    // After Ident => insert ';' at newline, and also at EOF (because trailing Ident).
    assert_injected_semis("x\ny", &[1, 3]);
}

#[test]
fn semicolon_insertion_after_return() {
    // After 'return' => insert ';' at newline. Then trailing Ident => insert ';' at EOF.
    assert_injected_semis("return\nx", &[6, 8]);
}

#[test]
fn semicolon_insertion_not_after_if() {
    // No ';' at newline after 'if', but trailing Ident => ';' at EOF.
    assert_injected_semis("if\nx", &[4]);
}

#[test]
fn semicolon_insertion_block_comment_no_newline_is_space() {
    // Comment has no newline => acts like space; newline triggers insertion; trailing Ident triggers EOF insertion.
    assert_injected_semis("x/*c*/\ny", &[6, 8]);
}

#[test]
fn semicolon_insertion_block_comment_with_newline_acts_like_newline() {
    // Block comment contains newline: insert at first newline inside comment (index 3).
    // Trailing Ident => insert at EOF (index 7).
    assert_injected_semis("x/*\n*/y", &[3, 7]);
}

#[test]
fn semicolon_insertion_eof() {
    // If semicolon insertion is needed at EOF, enqueue it at src_len.
    assert_injected_semis("x", &[1]);
}

#[test]
fn semicolon_after_rparen_rbrack_rbrace() {
    assert_injected_semis("(x)\ny", &[3, 5]);
    assert_injected_semis("a[0]\ny", &[4, 6]);
    assert_injected_semis("{ }\ny", &[3, 5]); // "{", "}", newline => insert at '\n', trailing Ident => EOF
}

#[test]
fn unterminated_block_comment_emits_error_and_finishes() {
    let (toks, diags) = lex_all("x/*");
    // Expected: Ident('x') then Error for unterminated comment. No injected EOF semi
    // because the lexer resets last_can_insert_semi on lex error.
    assert!(toks.len() >= 2, "toks={toks:?}");
    assert!(matches!(toks[0].1, Tok::Ident("x")), "toks={toks:?}");
    assert!(
        toks.iter().any(|(_, t, _)| matches!(t, Tok::Error)),
        "toks={toks:?}"
    );
    assert!(!diags.is_empty(), "expected diag for UnterminatedComment");
}
