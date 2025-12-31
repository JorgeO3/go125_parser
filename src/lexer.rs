use crate::error::{Diag, LexError, LexErrorKind};
use logos::{Lexer as LogosLexer, Logos};

// =============================================================================
// 1. Token Definition (RawTok - DFA optimizado para logos)
// =============================================================================

/// Token interno optimizado para el DFA de logos
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
#[logos(error = LexErrorKind)]
#[logos(skip r"[\t\x0C\f\v ]+")]
#[rustfmt::skip]
enum RawTok {
    // Trivia
    #[regex(r"\r\n|\n|\r")] Newline,
    #[regex(r"//[^\n\r]*", logos::skip, allow_greedy = true)] _LineComment,
    #[regex(r"/\*([^*]|\*[^/])*\*/")] BlockComment,

    // Keywords
    #[token("break")] KwBreak,
    #[token("case")] KwCase,
    #[token("chan")] KwChan,
    #[token("const")] KwConst,
    #[token("continue")] KwContinue,
    #[token("default")] KwDefault,
    #[token("defer")] KwDefer,
    #[token("else")] KwElse,
    #[token("fallthrough")] KwFallthrough,
    #[token("for")] KwFor,
    #[token("func")] KwFunc,
    #[token("go")] KwGo,
    #[token("goto")] KwGoto,
    #[token("if")] KwIf,
    #[token("import")] KwImport,
    #[token("interface")] KwInterface,
    #[token("map")] KwMap,
    #[token("package")] KwPackage,
    #[token("range")] KwRange,
    #[token("return")] KwReturn,
    #[token("select")] KwSelect,
    #[token("struct")] KwStruct,
    #[token("switch")] KwSwitch,
    #[token("type")] KwType,
    #[token("var")] KwVar,

    // Literals
    #[regex(r"[_\p{XID_Start}][_\p{XID_Continue}]*")] Ident,
    #[regex(r"`[^`]*`")] RawString,
    #[regex(r#""([^"\\\n\r]|\\.)*""#, validate_interpreted_string)] String,
    #[regex(r"'([^'\\\n\r]|\\.)+'", validate_rune_lit)] Rune,
    #[regex(r"0[xX][0-9a-fA-F_]+", validate_number)] IntHex,
    #[regex(r"0[oO][0-7_]+", validate_number)] IntOct,
    #[regex(r"0[bB][01_]+", validate_number)] IntBin,
    #[regex(r"[0-9_]*\.[0-9_]+([eE][+-]?[0-9_]+)?|[0-9_]+[eE][+-]?[0-9_]+", validate_number)] Float,
    #[regex(r"[0-9][0-9_]*", validate_number)] IntDec,
    #[regex(r"(0[xX][0-9a-fA-F_]+|0[bB][01_]+|0[oO][0-7_]+|[0-9_]+|[0-9_]*\.[0-9_]+([eE][+-]?[0-9_]+)?|[0-9_]+[eE][+-]?[0-9_]+)i", validate_imag)] Imag,

    // Operators (multi-char first)
    #[token("...")] Ellipsis,
    #[token("<<=")] ShlAssign,
    #[token(">>=")] ShrAssign,
    #[token("&^=")] AndNotAssign,
    #[token("+=")] AddAssign,
    #[token("-=")] SubAssign,
    #[token("*=")] MulAssign,
    #[token("/=")] DivAssign,
    #[token("%=")] ModAssign,
    #[token("&=")] AndAssign,
    #[token("|=")] OrAssign,
    #[token("^=")] XorAssign,
    #[token("<<")] Shl,
    #[token(">>")] Shr,
    #[token("&^")] AndNot,
    #[token("&&")] LAnd,
    #[token("||")] LOr,
    #[token("==")] EqEq,
    #[token("!=")] NotEq,
    #[token("<=")] Le,
    #[token(">=")] Ge,
    #[token("++")] Inc,
    #[token("--")] Dec,
    #[token(":=")] Define,
    #[token("<-")] Arrow,
    #[token("=")] Assign,
    #[token("+")] Plus,
    #[token("-")] Minus,
    #[token("*")] Star,
    #[token("/")] Slash,
    #[token("%")] Percent,
    #[token("&")] Amp,
    #[token("|")] Pipe,
    #[token("^")] Caret,
    #[token("~")] Tilde,
    #[token("!")] Bang,
    #[token("<")] Lt,
    #[token(">")] Gt,

    // Delimiters
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("[")] LBrack,
    #[token("]")] RBrack,
    #[token("{")] LBrace,
    #[token("}")] RBrace,
    #[token(",")] Comma,
    #[token(";")] Semi,
    #[token(":")] Colon,
    #[token(".")] Dot,

    #[regex(r".", priority = 0)] Error,
}

// =============================================================================
// 2. Lookup Tables (Compile-Time Generated con Macros)
// =============================================================================

/// Lookup table para clasificación rápida de tipos de token
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokKind {
    Literal, // Token con payload (requiere slice)
    Simple,  // Token sin payload (keyword/operador)
    Trivia,  // Whitespace, comentarios
}

/// Macro para generar lookup tables de forma declarativa
macro_rules! gen_lookup_table {
    (bool, $size:expr, $($variant:ident),* $(,)?) => {{
        let mut table = [false; $size];
        $(table[RawTok::$variant as usize] = true;)*
        table
    }};
    ($enum:ty, $size:expr, $default:expr, $($variant:ident => $value:expr),* $(,)?) => {{
        let mut table = [$default; $size];
        $(table[RawTok::$variant as usize] = $value;)*
        table
    }};
}

/// Tokens que permiten inserción automática de semicolon
const SEMI_INSERT_TABLE: [bool; 256] = gen_lookup_table!(
    bool,
    256,
    // Literales
    Ident,
    IntBin,
    IntOct,
    IntHex,
    IntDec,
    Float,
    Imag,
    Rune,
    String,
    RawString,
    // Keywords
    KwBreak,
    KwContinue,
    KwFallthrough,
    KwReturn,
    // Operadores
    Inc,
    Dec,
    // Delimitadores
    RParen,
    RBrack,
    RBrace,
);

/// Clasificación de tokens por tipo
const TOKEN_KIND_TABLE: [TokKind; 256] = gen_lookup_table!(
    TokKind, 256, TokKind::Simple,
    // Trivia
    Newline => TokKind::Trivia,
    _LineComment => TokKind::Trivia,
    BlockComment => TokKind::Trivia,
    // Literales
    Ident => TokKind::Literal,
    IntBin => TokKind::Literal,
    IntOct => TokKind::Literal,
    IntHex => TokKind::Literal,
    IntDec => TokKind::Literal,
    Float => TokKind::Literal,
    Imag => TokKind::Literal,
    Rune => TokKind::Literal,
    String => TokKind::Literal,
    RawString => TokKind::Literal,
);

impl RawTok {
    #[inline(always)]
    const fn can_insert_semicolon(self) -> bool {
        SEMI_INSERT_TABLE[self as usize]
    }

    #[inline(always)]
    const fn kind(self) -> TokKind {
        TOKEN_KIND_TABLE[self as usize]
    }

    /// Convierte RawTok a Tok usando dispatch especializado
    #[inline]
    const fn to_token<'src>(self, slice: &'src str) -> Tok<'src> {
        // Branch prediction favorecerá literales (más comunes)
        if matches!(self.kind(), TokKind::Literal) {
            return match self {
                Self::Ident => Tok::Ident(slice),
                Self::IntBin | Self::IntOct | Self::IntHex | Self::IntDec => Tok::IntLit(slice),
                Self::Float => Tok::FloatLit(slice),
                Self::Imag => Tok::ImagLit(slice),
                Self::Rune => Tok::RuneLit(slice),
                Self::String => Tok::StringLit(slice),
                Self::RawString => Tok::RawStringLit(slice),
                _ => unreachable!(),
            };
        }

        // Keywords y operadores (uso de macro para reducir código)
        macro_rules! simple_tok {
            ($($raw:ident => $tok:ident),* $(,)?) => {
                match self {
                    $(Self::$raw => Tok::$tok,)*
                    _ => unreachable!(),
                }
            };
        }

        simple_tok! {
            KwBreak => KwBreak, KwCase => KwCase, KwChan => KwChan, KwConst => KwConst,
            KwContinue => KwContinue, KwDefault => KwDefault, KwDefer => KwDefer, KwElse => KwElse,
            KwFallthrough => KwFallthrough, KwFor => KwFor, KwFunc => KwFunc, KwGo => KwGo,
            KwGoto => KwGoto, KwIf => KwIf, KwImport => KwImport, KwInterface => KwInterface,
            KwMap => KwMap, KwPackage => KwPackage, KwRange => KwRange, KwReturn => KwReturn,
            KwSelect => KwSelect, KwStruct => KwStruct, KwSwitch => KwSwitch, KwType => KwType, KwVar => KwVar,
            Ellipsis => Ellipsis, ShlAssign => ShlAssign, ShrAssign => ShrAssign, AndNotAssign => AndNotAssign,
            AddAssign => AddAssign, SubAssign => SubAssign, MulAssign => MulAssign, DivAssign => DivAssign,
            ModAssign => ModAssign, AndAssign => AndAssign, OrAssign => OrAssign, XorAssign => XorAssign,
            Shl => Shl, Shr => Shr, AndNot => AndNot, LAnd => LAnd, LOr => LOr, EqEq => EqEq, NotEq => NotEq,
            Le => Le, Ge => Ge, Inc => Inc, Dec => Dec, Define => Define, Arrow => Arrow, Assign => Assign,
            Plus => Plus, Minus => Minus, Star => Star, Slash => Slash, Percent => Percent, Amp => Amp,
            Pipe => Pipe, Caret => Caret, Tilde => Tilde, Bang => Bang, Lt => Lt, Gt => Gt,
            LParen => LParen, RParen => RParen, LBrack => LBrack, RBrack => RBrack, LBrace => LBrace,
            RBrace => RBrace, Comma => Comma, Semi => Semi, Colon => Colon, Dot => Dot, Error => Error,
        }
    }
}

// =============================================================================
// 3. Validation Functions (Optimized with Macros & Lookup Tables)
// =============================================================================

/// Tipo de escape sequence
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum EscapeKind {
    Invalid = 0,
    Simple = 1, // \n, \t, etc.
    Hex2 = 2,   // \xNN
    Hex4 = 4,   // \uNNNN
    Hex8 = 8,   // \UNNNNNNNN
    Octal = 16, // \NNN
}

/// Macro para generar digit lookup tables
macro_rules! gen_digit_table {
    (hex) => {{
        let mut table = [false; 256];
        let mut i = 0;
        while i < 10 {
            table[(b'0' as usize) + i] = true;
            i += 1;
        }
        i = 0;
        while i < 6 {
            table[(b'a' as usize) + i] = true;
            table[(b'A' as usize) + i] = true;
            i += 1;
        }
        table
    }};
    (octal) => {{
        let mut table = [false; 256];
        let mut i = 0;
        while i < 8 {
            table[(b'0' as usize) + i] = true;
            i += 1;
        }
        table
    }};
}

/// Lookup tables para validación de dígitos (const, branchless)
const HEX_DIGIT_TABLE: [bool; 256] = gen_digit_table!(hex);
const OCTAL_DIGIT_TABLE: [bool; 256] = gen_digit_table!(octal);

/// Lookup table para escape sequences
const ESCAPE_TABLE: [EscapeKind; 256] = {
    let mut table = [EscapeKind::Invalid; 256];
    // Simple escapes
    let simple = [b'a', b'b', b'f', b'n', b'r', b't', b'v', b'\\', b'"', b'\''];
    let mut i = 0;
    while i < simple.len() {
        table[simple[i] as usize] = EscapeKind::Simple;
        i += 1;
    }
    // Hex escapes
    table[b'x' as usize] = EscapeKind::Hex2;
    table[b'u' as usize] = EscapeKind::Hex4;
    table[b'U' as usize] = EscapeKind::Hex8;
    // Octal escapes (0-7)
    i = 0;
    while i < 8 {
        table[(b'0' as usize) + i] = EscapeKind::Octal;
        i += 1;
    }
    table
};

#[inline(always)]
const fn is_hex_digit(byte: u8) -> bool {
    HEX_DIGIT_TABLE[byte as usize]
}

#[inline(always)]
const fn is_octal_digit(byte: u8) -> bool {
    OCTAL_DIGIT_TABLE[byte as usize]
}

/// Valida N bytes hexadecimales consecutivos (optimizado)
#[inline]
fn validate_hex_bytes(bytes: &[u8], start: usize, count: usize) -> Result<(), LexErrorKind> {
    // Check bounds una sola vez
    let slice = bytes
        .get(start..start.wrapping_add(count))
        .ok_or(LexErrorKind::InvalidEscape)?;

    // Optimización: el compilador puede vectorizar este loop
    slice
        .iter()
        .all(|&b| is_hex_digit(b))
        .then_some(())
        .ok_or(LexErrorKind::InvalidEscape)
}

/// Valida escape sequences (single-pass, lookup-based)
#[inline]
fn validate_escapes(s: &str) -> Result<(), LexErrorKind> {
    let bytes = s.as_bytes();
    let len = bytes.len();

    if len < 2 {
        return Err(LexErrorKind::InvalidToken);
    }

    let mut i = 1; // Skip opening quote

    while i < len - 1 {
        if bytes[i] != b'\\' {
            i += 1;
            continue;
        }

        i += 1;
        if i >= len - 1 {
            return Err(LexErrorKind::InvalidEscape);
        }

        // Dispatch optimizado usando lookup table
        match ESCAPE_TABLE[bytes[i] as usize] {
            EscapeKind::Simple => i += 1,
            EscapeKind::Hex2 => {
                i += 1;
                validate_hex_bytes(bytes, i, 2)?;
                i += 2;
            }
            EscapeKind::Hex4 => {
                i += 1;
                validate_hex_bytes(bytes, i, 4)?;
                i += 4;
            }
            EscapeKind::Hex8 => {
                i += 1;
                validate_hex_bytes(bytes, i, 8)?;
                i += 8;
            }
            EscapeKind::Octal => {
                i += 1;
                // Consume hasta 2 octales adicionales
                let mut count = 0;
                while count < 2 && i < len - 1 && is_octal_digit(bytes[i]) {
                    i += 1;
                    count += 1;
                }
            }
            EscapeKind::Invalid => return Err(LexErrorKind::InvalidEscape),
        }
    }

    Ok(())
}

#[inline]
fn validate_interpreted_string(lex: &mut LogosLexer<'_, RawTok>) -> Result<(), LexErrorKind> {
    validate_escapes(lex.slice())
}

#[inline]
fn validate_rune_lit(lex: &mut LogosLexer<'_, RawTok>) -> Result<(), LexErrorKind> {
    let s = lex.slice();
    (s.len() >= 3)
        .then_some(())
        .ok_or(LexErrorKind::InvalidToken)?;
    validate_escapes(s)
}

/// Valida número (optimizado para hot path)
#[inline]
fn validate_number(lex: &mut LogosLexer<'_, RawTok>) -> Result<(), LexErrorKind> {
    let bytes = lex.slice().as_bytes();

    // Fast path: check common error conditions
    if bytes.is_empty()
        || bytes[0] == b'_'
        || bytes[bytes.len() - 1] == b'_'
        || bytes.windows(2).any(|w| w == b"__")
    {
        return Err(LexErrorKind::InvalidNumber);
    }

    Ok(())
}

#[inline]
fn validate_imag(lex: &mut LogosLexer<'_, RawTok>) -> Result<(), LexErrorKind> {
    lex.slice()
        .ends_with('i')
        .then_some(())
        .ok_or(LexErrorKind::InvalidNumber)?;
    validate_number(lex)
}

// =============================================================================
// 4. Public Token Definition
// =============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Tok<'input> {
    Ident(&'input str),
    IntLit(&'input str),
    FloatLit(&'input str),
    ImagLit(&'input str),
    RuneLit(&'input str),
    StringLit(&'input str),
    RawStringLit(&'input str),

    // Keywords
    KwBreak,
    KwCase,
    KwChan,
    KwConst,
    KwContinue,
    KwDefault,
    KwDefer,
    KwElse,
    KwFallthrough,
    KwFor,
    KwFunc,
    KwGo,
    KwGoto,
    KwIf,
    KwImport,
    KwInterface,
    KwMap,
    KwPackage,
    KwRange,
    KwReturn,
    KwSelect,
    KwStruct,
    KwSwitch,
    KwType,
    KwVar,

    // Operators
    Ellipsis,
    ShlAssign,
    ShrAssign,
    AndNotAssign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    Shl,
    Shr,
    AndNot,
    LAnd,
    LOr,
    EqEq,
    NotEq,
    Le,
    Ge,
    Inc,
    Dec,
    Define,
    Arrow,
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Amp,
    Pipe,
    Caret,
    Tilde,
    Bang,
    Lt,
    Gt,
    LParen,
    RParen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    Comma,
    Semi,
    Colon,
    Dot,

    Error,
}

impl<'input> std::fmt::Display for Tok<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

// =============================================================================
// 5. Lexer with Semicolon Insertion Logic (Simplified)
// =============================================================================

pub struct Lexer<'src> {
    logos: LogosLexer<'src, RawTok>,
    pending: Option<(usize, Tok<'src>, usize)>,
    diags: Vec<Diag>,
    last_can_insert_semi: bool,
    src_len: usize,
    eof_done: bool,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            logos: RawTok::lexer(input),
            pending: None,
            diags: Vec::with_capacity(16),
            last_can_insert_semi: false,
            src_len: input.len(),
            eof_done: false,
        }
    }

    pub fn take_diags(&mut self) -> Vec<Diag> {
        std::mem::take(&mut self.diags)
    }

    #[inline]
    fn push_lex_diag(&mut self, kind: LexErrorKind, span: std::ops::Range<usize>) {
        let sp = crate::error::Span::from_range(span);
        self.diags.push(LexError { kind, span: sp }.diag());
    }

    #[inline]
    fn emit_semi_at(&mut self, pos: usize) {
        self.pending = Some((pos, Tok::Semi, pos));
    }

    /// Maneja trivia y determina si debe insertarse semicolon
    #[inline]
    fn handle_trivia(&mut self, raw: RawTok, span: &std::ops::Range<usize>, slice: &str) -> bool {
        match raw {
            RawTok::Newline => {
                if self.last_can_insert_semi {
                    self.last_can_insert_semi = false;
                    self.emit_semi_at(span.start);
                }
                true
            }
            RawTok::BlockComment if slice.contains(['\n', '\r']) => {
                if self.last_can_insert_semi {
                    self.last_can_insert_semi = false;
                    self.emit_semi_at(span.end);
                }
                true
            }
            RawTok::BlockComment => true,
            _ => false,
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = (usize, Tok<'src>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // Return pending semicolon if any
            if let Some(tok) = self.pending.take() {
                return Some(tok);
            }

            if self.eof_done {
                return None;
            }

            match self.logos.next() {
                None => {
                    self.eof_done = true;
                    if self.last_can_insert_semi {
                        self.last_can_insert_semi = false;
                        self.emit_semi_at(self.src_len);
                        continue;
                    }
                    return None;
                }
                Some(Err(kind)) => {
                    let span = self.logos.span();
                    self.push_lex_diag(kind, span.clone());
                    self.last_can_insert_semi = false;
                    return Some((span.start, Tok::Error, span.end));
                }
                Some(Ok(raw)) => {
                    let span = self.logos.span();
                    let slice = self.logos.slice();

                    // Handle trivia (skip whitespace, comments)
                    if self.handle_trivia(raw, &span, slice) {
                        continue;
                    }

                    // Handle errors
                    if raw == RawTok::Error {
                        self.push_lex_diag(LexErrorKind::InvalidToken, span.clone());
                        self.last_can_insert_semi = false;
                        return Some((span.start, Tok::Error, span.end));
                    }

                    // Update semicolon insertion flag (branchless lookup)
                    self.last_can_insert_semi = raw.can_insert_semicolon();

                    // Convert and return token
                    let tok = raw.to_token(slice);
                    return Some((span.start, tok, span.end));
                }
            }
        }
    }
}
