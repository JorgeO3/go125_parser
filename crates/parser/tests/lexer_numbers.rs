// crates/parser/tests/lexer_numbers.rs
use go125_parser::error::Diag;
use go125_parser::lexer::{Lexer, Tok};

fn lex1(input: &str) -> (Tok<'_>, Vec<Diag>) {
    let mut lx = Lexer::new(input);
    let t = lx.next().unwrap().1;
    let diags = lx.take_diags();
    (t, diags)
}

fn lex2(input: &str) -> (Tok<'_>, Tok<'_>, Vec<Diag>) {
    let mut lx = Lexer::new(input);
    let t1 = lx.next().unwrap().1;
    let t2 = lx.next().unwrap().1;
    let diags = lx.take_diags();
    (t1, t2, diags)
}

fn lex_all(input: &str) -> (Vec<(usize, Tok<'_>, usize)>, Vec<Diag>) {
    let mut lx = Lexer::new(input);
    let toks: Vec<_> = lx.by_ref().collect();
    let diags = lx.take_diags();
    (toks, diags)
}

fn kinds(toks: &[(usize, Tok<'_>, usize)]) -> Vec<&'static str> {
    toks.iter().map(|(_, t, _)| kind_name(t)).collect()
}

fn kind_short(t: &Tok<'_>) -> &'static str {
    match t {
        Tok::Ident(_) => "Ident",
        Tok::IntLit(_) => "IntLit",
        Tok::FloatLit(_) => "FloatLit",
        Tok::ImagLit(_) => "ImagLit",
        Tok::Dot => "Dot",
        Tok::Semi => "Semi",
        Tok::Error => "Error",
        _ => "Other",
    }
}

fn kinds_short(toks: &[(usize, Tok<'_>, usize)]) -> Vec<&'static str> {
    toks.iter().map(|(_, t, _)| kind_short(t)).collect()
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

#[test]
fn ints_valid_decimal() {
    for s in ["0", "1", "9", "10", "123_456", "1_2_3"] {
        let (t, diags) = lex1(s);
        assert!(diags.is_empty(), "{s} produced diags: {diags:?}");
        assert!(matches!(t, Tok::IntLit(_)), "{s} -> {t:?}");
    }
}

#[test]
fn ints_valid_prefixed() {
    for s in [
        "0x1",
        "0x_1",
        "0XDEAD_BEEF",
        "0o7",
        "0o_7",
        "0b1",
        "0b_1010",
    ] {
        let (t, diags) = lex1(s);
        assert!(diags.is_empty(), "{s} produced diags: {diags:?}");
        assert!(matches!(t, Tok::IntLit(_)), "{s} -> {t:?}");
    }
}

#[test]
fn ints_invalid_underscore_rules() {
    for s in ["1__2", "1_", "0x_", "0b1_", "0o_"] {
        let (t, diags) = lex1(s);
        assert!(matches!(t, Tok::Error), "{s} should Error, got {t:?}");
        assert!(!diags.is_empty(), "{s} should emit diag");
    }
}

#[test]
fn floats_valid_decimal() {
    for s in [
        "0.0",
        ".5",
        "1.",
        "1.0",
        "1e9",
        "1E+9",
        "1e-9",
        "1.2e3",
        "1_2.3_4e5_6",
    ] {
        let (t, diags) = lex1(s);
        assert!(diags.is_empty(), "{s} produced diags: {diags:?}");
        assert!(matches!(t, Tok::FloatLit(_)), "{s} -> {t:?}");
    }
}

#[test]
fn floats_valid_hex() {
    for s in ["0x1p0", "0x1.8p+1", "0x.8p0", "0X1.FP-2", "0x_1.2p3"] {
        let (t, diags) = lex1(s);
        assert!(diags.is_empty(), "{s} produced diags: {diags:?}");
        assert!(matches!(t, Tok::FloatLit(_)), "{s} -> {t:?}");
    }
}

#[test]
fn floats_valid_hex_trailing_dot_mantissa() {
    // Mantisa hex puede ser "hex_digits '.'" (sin frac digits) si hay p-exp.
    let (t, diags) = lex1("0x1.p2");
    assert!(diags.is_empty(), "produced diags: {diags:?}");
    assert!(matches!(t, Tok::FloatLit(_)), "0x1.p2 -> {t:?}");
}

#[test]
fn floats_invalid_hex_requires_p() {
    // hex float must have p/P exponent; "0x1.2" is not a float literal per spec.
    for s in ["0x1.2", "0x.1", "0x1."] {
        let (t, diags) = lex1(s);
        assert!(matches!(t, Tok::Error), "{s} should Error, got {t:?}");
        assert!(!diags.is_empty());
    }
}

#[test]
fn imag_literals() {
    for s in ["1i", "0i", "1.0i", ".5i", "0x1p0i"] {
        let (t, diags) = lex1(s);
        assert!(diags.is_empty(), "{s} produced diags: {diags:?}");
        assert!(matches!(t, Tok::ImagLit(_)), "{s} -> {t:?}");
    }

    // whitespace breaks imaginary
    let (t1, t2, _diags) = lex2("1.0 i");
    assert!(matches!(t1, Tok::FloatLit(_)), "t1={t1:?}");
    assert!(matches!(t2, Tok::Ident("i")), "t2={t2:?}");
}

#[test]
fn maximal_munch_invalid_stays_single_token() {
    // You explicitly want "0b2" not to split as "0b" + "2"
    let (t, diags) = lex1("0b2");
    assert!(matches!(t, Tok::Error));
    assert!(!diags.is_empty());
}

#[test]
fn imaginary_does_not_need_separator_from_following_ident() {
    // Longest valid token: "1i" is a valid Imag literal, then "f" is an ident.
    let mut lx = Lexer::new("1if");
    let t1 = lx.next().unwrap().1;
    let t2 = lx.next().unwrap().1;

    assert!(matches!(t1, Tok::ImagLit("1i")), "t1={t1:?}");
    assert!(matches!(t2, Tok::Ident("f")), "t2={t2:?}");
}

#[test]
fn imaginary_from_trailing_dot_float() {
    // "1." is a valid float literal; "1.i" should lex as a single imaginary literal "1.i".
    let mut lx = Lexer::new("1.i");
    let t1 = lx.next().unwrap().1;
    assert!(matches!(t1, Tok::ImagLit("1.i")), "t1={t1:?}");
}

#[test]
fn imaginary_lookahead_must_not_consume_i_if_separated() {
    // lookahead solo consume 'i' inmediato, sin trivia.
    let mut lx = Lexer::new("1i /*space*/ i");
    let t1 = lx.next().unwrap().1;
    let t2 = lx.next().unwrap().1;
    assert!(matches!(t1, Tok::ImagLit(_)), "t1={t1:?}");
    assert!(matches!(t2, Tok::Ident("i")), "t2={t2:?}");
}

// -----------------------------------------------------------------------------
// Nuevos tests (A/B/C) que agregamos
// -----------------------------------------------------------------------------

#[test]
fn floats_invalid_underscore_positions_decimal() {
    // underscores no pegados a '.', 'e/E', sign, fin, etc.
    for s in [
        "1_.0", "1._0", "1e_9", "1e+_9", "1e9_", "1._", "1_", "0_", ".5_", "1._e2",
    ] {
        let (t, diags) = lex1(s);
        assert!(matches!(t, Tok::Error), "{s} should Error, got {t:?}");
        assert!(!diags.is_empty(), "{s} should emit diag");
    }
}

#[test]
fn hex_float_edge_cases() {
    // inválidos
    for s in [
        "0x1p",    // falta exponent digits
        "0x1p+",   // falta digits tras signo
        "0x.p1",   // mantisa sin digits
        "0x1.2e3", // hex float no usa e/E
    ] {
        let (t, diags) = lex1(s);
        assert!(matches!(t, Tok::Error), "{s} should Error, got {t:?}");
        assert!(!diags.is_empty(), "{s} should emit diag");
    }

    // válido: int hex con 'e' como dígito hex
    let (t, diags) = lex1("0x1e3");
    assert!(diags.is_empty());
    assert!(
        matches!(t, Tok::IntLit(_)),
        "0x1e3 should be IntLit, got {t:?}"
    );
}

#[test]
fn prefixed_int_invalid_digits_stay_single_token() {
    for s in ["0o8", "0o9", "0b102"] {
        let (t, diags) = lex1(s);
        assert!(matches!(t, Tok::Error), "{s} should Error, got {t:?}");
        assert!(!diags.is_empty(), "{s} should emit diag");
    }
}

// -----------------------------------------------------------------------------
// “Punto 5” (faltantes): bordes de imag-lookahead y números pegados a 'i'
// -----------------------------------------------------------------------------

#[test]
fn imag_lookahead_must_not_consume_i_when_double_i() {
    let (t1, t2, _diags) = lex2("1ii");
    assert!(matches!(t1, Tok::ImagLit("1i")), "t1={t1:?}");
    assert!(matches!(t2, Tok::Ident("i")), "t2={t2:?}");
}

#[test]
fn imag_lookahead_must_not_consume_i_when_followed_by_digit() {
    let (t1, t2, _diags) = lex2("1i2");
    assert!(matches!(t1, Tok::ImagLit("1i")), "t1={t1:?}");
    assert!(matches!(t2, Tok::IntLit("2")), "t2={t2:?}");
}

#[test]
fn imag_lookahead_must_not_consume_i_when_followed_by_underscore() {
    let (t1, t2, _diags) = lex2("1i_");
    assert!(matches!(t1, Tok::ImagLit("1i")), "t1={t1:?}");
    assert!(matches!(t2, Tok::Ident("_")), "t2={t2:?}");
}

#[test]
fn imag_does_not_form_from_invalid_number_plus_i() {
    let (t, diags) = lex1("1__2i");
    assert!(matches!(t, Tok::Error), "t={t:?}");
    assert!(!diags.is_empty(), "expected diag");
}

#[test]
fn decimal_float_exponent_requires_digits_even_with_sign() {
    for s in ["1e", "1e+", "1e-", "1.0e", "1.0e+", "1.0e-"] {
        let (t, diags) = lex1(s);
        assert!(matches!(t, Tok::Error), "{s} should Error, got {t:?}");
        assert!(!diags.is_empty(), "{s} should emit diag");
    }
}

#[test]
fn hex_float_exponent_requires_digits_even_with_sign() {
    for s in ["0x1p", "0x1p+", "0x1p-", "0x1.0p", "0x1.0p+", "0x1.0p-"] {
        let (t, diags) = lex1(s);
        assert!(matches!(t, Tok::Error), "{s} should Error, got {t:?}");
        assert!(!diags.is_empty(), "{s} should emit diag");
    }
}

#[test]
fn max_munch_keeps_entire_exponent_with_underscores_then_rejects_if_invalid() {
    for s in ["1e_9", "1e+_9", "0x1p_2", "0x1p+_2"] {
        let (t, diags) = lex1(s);
        assert!(matches!(t, Tok::Error), "{s} should Error, got {t:?}");
        assert!(!diags.is_empty(), "{s} should emit diag");
    }
}

// -----------------------------------------------------------------------------
// Bordes: '.' '.' seguido de dígito => Dot + FloatLit(".<d>")
// -----------------------------------------------------------------------------

#[test]
fn number_dots_then_digit_forms_dot_float() {
    // En Go, ".2" es un float literal válido.
    // Por maximal munch: "1..2" => IntLit("1"), Dot("."), FloatLit(".2")
    let (toks, diags) = lex_all("1..2");
    assert!(diags.is_empty(), "{diags:?}");
    assert_eq!(
        kinds(&toks),
        vec!["IntLit", "Dot", "FloatLit", "Semi"],
        "toks={toks:?}"
    );
}

#[test]
fn number_does_not_steal_range_dots_before_ident() {
    // Si después del segundo '.' NO hay dígito, entonces no puede formar ".<digit>".
    // Así que debe quedar como Dot, Dot, Ident.
    let (toks, diags) = lex_all("1..x");
    assert!(diags.is_empty(), "{diags:?}");
    assert_eq!(
        kinds(&toks),
        vec!["IntLit", "Dot", "Dot", "Ident", "Semi"],
        "toks={toks:?}"
    );
}

#[test]
fn number_does_not_steal_ellipsis() {
    let (toks, diags) = lex_all("1...2");
    assert!(diags.is_empty(), "{diags:?}");
    assert_eq!(
        kinds(&toks),
        vec!["IntLit", "Ellipsis", "IntLit", "Semi"],
        "toks={toks:?}"
    );
}

#[test]
fn go_style_double_dot_then_digits_is_dot_plus_float() {
    // Go (scanner del compilador) incluye "..123" en su smoke test y lo tokeniza como:
    // Dot + Literal(".123"). :contentReference[oaicite:4]{index=4}
    let (toks, diags) = lex_all("..123");
    assert!(diags.is_empty(), "diags={diags:?} toks={toks:?}");
    assert_eq!(
        kinds_short(&toks),
        vec!["Dot", "FloatLit", "Semi"],
        "toks={toks:?}"
    );
    assert!(matches!(toks[1].1, Tok::FloatLit(".123")), "toks={toks:?}");
}

#[test]
fn go_style_1_dot_dot_2_is_dot_plus_float_not_dot_dot_int() {
    // "1..2" => IntLit("1"), Dot, FloatLit(".2"), Semi (EOF)
    let (toks, diags) = lex_all("1..2");
    assert!(diags.is_empty(), "diags={diags:?} toks={toks:?}");
    assert_eq!(
        kinds_short(&toks),
        vec!["IntLit", "Dot", "FloatLit", "Semi"],
        "toks={toks:?}"
    );
    assert!(matches!(toks[2].1, Tok::FloatLit(".2")), "toks={toks:?}");
}

#[test]
fn dot_dot_ident_is_two_dots_then_ident() {
    // "..f" => Dot, Dot, Ident("f"), Semi
    let (toks, diags) = lex_all("..f");
    assert!(diags.is_empty(), "diags={diags:?} toks={toks:?}");
    assert_eq!(
        kinds_short(&toks),
        vec!["Dot", "Dot", "Ident", "Semi"],
        "toks={toks:?}"
    );
}

#[test]
fn legacy_octal_invalid_09_and_08_are_errors() {
    for s in ["09", "08", "0128"] {
        let (t, diags) = lex1(s);
        assert!(matches!(t, Tok::Error), "{s} should Error, got {t:?}");
        assert!(!diags.is_empty(), "{s} should emit diag");
    }
}

#[test]
fn binary_and_octal_do_not_allow_fractional_part() {
    for s in ["0b1.0", "0o7.1", "0b_101.0", "0o_7.1"] {
        let (t, diags) = lex1(s);
        assert!(matches!(t, Tok::Error), "{s} should Error, got {t:?}");
        assert!(!diags.is_empty(), "{s} should emit diag");
    }
}

#[test]
fn hex_int_with_e_digit_can_form_imag_literal() {
    // "0x1e3" es int hex válido; con sufijo 'i' debe formar ImagLit.
    // (En hex-int, 'e' es dígito, no exponente.)
    let (t, diags) = lex1("0x1e3i");
    assert!(diags.is_empty(), "diags={diags:?}");
    assert!(matches!(t, Tok::ImagLit("0x1e3i")), "t={t:?}");
}
