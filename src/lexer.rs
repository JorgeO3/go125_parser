use std::collections::VecDeque;

use logos::{Lexer as LogosLexer, Logos};
use unicode_ident::{is_xid_continue, is_xid_start};

use crate::error::{Diag, DiagKind, LexError, LexErrorKind};

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(error = LexErrorKind)]
enum RawTok {
    // --- Trivia ---
    #[regex(r"[\t\x0C\f\v ]+", logos::skip)]
    _Ws,

    // Keep newlines as tokens to implement semicolon insertion.
    #[regex(r"\r\n|\n|\r")]
    Newline,

    // Line comment (newline excluded).
    #[regex(r"//[^\n\r]*", logos::skip)]
    _LineComment,

    // Block comment: we do NOT skip; we need to detect if it contains newlines.
    #[regex(r"(?s)/\*.*?\*/")]
    BlockComment,

    // --- Ident & keywords (keywords are mapped in adapter) ---
    #[regex(r"[_\p{XID_Start}][_\p{XID_Continue}]*")]
    Ident,

    // --- Literals ---
    // Raw string literal (backquoted). Can contain newlines.
    #[regex(r"`[^`]*`")]
    RawString,

    // Interpreted string literal.
    #[regex(r"\"([^\"\\\n\r]|\\.)*\"", validate_interpreted_string)]
    String,

    // Rune literal.
    #[regex(r"'([^'\\\n\r]|\\.)+'", validate_rune_lit)]
    Rune,

    // Numeric literals (approximate). We validate underscores lightly.
    #[regex(r"0[bB][01]([01_]*[01])?", validate_number)]
    Int,
    #[regex(r"0[oO][0-7]([0-7_]*[0-7])?", validate_number)]
    Int,
    #[regex(r"0[xX][0-9a-fA-F]([0-9a-fA-F_]*[0-9a-fA-F])?", validate_number)]
    Int,
    #[regex(r"0([0-7_]*[0-7])?", validate_number)]
    Int,
    #[regex(r"[1-9]([0-9_]*[0-9])?", validate_number)]
    Int,

    // Floats: decimal with dot and/or exponent.
    #[regex(r"([0-9]([0-9_]*[0-9])?\.[0-9]([0-9_]*[0-9])?([eE][+-]?[0-9]([0-9_]*[0-9])?)?)|([0-9]([0-9_]*[0-9])?[eE][+-]?[0-9]([0-9_]*[0-9])?)|(\.[0-9]([0-9_]*[0-9])?([eE][+-]?[0-9]([0-9_]*[0-9])?)?)", validate_number)]
    Float,

    // Imaginary: int or float followed by i.
    #[regex(r"((0[xX][0-9a-fA-F]([0-9a-fA-F_]*[0-9a-fA-F])?)|(0[bB][01]([01_]*[01])?)|(0[oO][0-7]([0-7_]*[0-7])?)|([0-9]([0-9_]*[0-9])?))(i)", validate_imag)]
    Imag,
    #[regex(r"(([0-9]([0-9_]*[0-9])?\.[0-9]([0-9_]*[0-9])?([eE][+-]?[0-9]([0-9_]*[0-9])?)?)|([0-9]([0-9_]*[0-9])?[eE][+-]?[0-9]([0-9_]*[0-9])?)|(\.[0-9]([0-9_]*[0-9])?([eE][+-]?[0-9]([0-9_]*[0-9])?)?))(i)", validate_imag)]
    Imag,

    // --- Operators and delimiters ---
    #[token("...")]
    Ellipsis,

    #[token("<<=")]
    ShlAssign,
    #[token(">>=")]
    ShrAssign,
    #[token("&^=")]
    AndNotAssign,

    #[token("+=")]
    AddAssign,
    #[token("-=")]
    SubAssign,
    #[token("*=")]
    MulAssign,
    #[token("/=")]
    DivAssign,
    #[token("%=")]
    ModAssign,
    #[token("&=")]
    AndAssign,
    #[token("|=")]
    OrAssign,
    #[token("^=")]
    XorAssign,

    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,
    #[token("&^")]
    AndNot,

    #[token("&&")]
    LAnd,
    #[token("||")]
    LOr,

    #[token("==")]
    EqEq,
    #[token("!=")]
    NotEq,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,

    #[token("++")]
    Inc,
    #[token("--")]
    Dec,

    #[token(":=")]
    Define,
    #[token("<-")]
    Arrow,

    #[token("=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("&")]
    Amp,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,
    #[token("!")]
    Bang,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,

    #[token(",")]
    Comma,
    #[token(";")]
    Semi,
    #[token(":")]
    Colon,
    #[token(".")]
    Dot,

    // Anything else.
    #[regex(r".")]
    Error,
}

fn validate_interpreted_string(lex: &mut LogosLexer<'_, RawTok>) -> Result<(), LexErrorKind> {
    // slice includes surrounding quotes
    let s = lex.slice();
    // quick checks: no raw newlines due to regex
    // validate escapes roughly
    let mut it = s[1..s.len() - 1].chars();
    while let Some(c) = it.next() {
        if c != '\\' {
            continue;
        }
        match it.next() {
            Some('a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '\\' | '"' | '\'' ) => {}
            Some('x') => {
                let h1 = it.next().ok_or(LexErrorKind::InvalidEscape)?;
                let h2 = it.next().ok_or(LexErrorKind::InvalidEscape)?;
                if !h1.is_ascii_hexdigit() || !h2.is_ascii_hexdigit() {
                    return Err(LexErrorKind::InvalidEscape);
                }
            }
            Some('u') => {
                for _ in 0..4 {
                    let h = it.next().ok_or(LexErrorKind::InvalidEscape)?;
                    if !h.is_ascii_hexdigit() {
                        return Err(LexErrorKind::InvalidEscape);
                    }
                }
            }
            Some('U') => {
                for _ in 0..8 {
                    let h = it.next().ok_or(LexErrorKind::InvalidEscape)?;
                    if !h.is_ascii_hexdigit() {
                        return Err(LexErrorKind::InvalidEscape);
                    }
                }
            }
            Some('0'..='7') => {
                // octal: up to 2 more digits
                for _ in 0..2 {
                    if let Some(peek) = it.clone().next() {
                        if ('0'..='7').contains(&peek) {
                            it.next();
                        } else {
                            break;
                        }
                    }
                }
            }
            _ => return Err(LexErrorKind::InvalidEscape),
        }
    }
    Ok(())
}

fn validate_rune_lit(lex: &mut LogosLexer<'_, RawTok>) -> Result<(), LexErrorKind> {
    // Very conservative: ensure it's at least one char between quotes; regex already guarantees.
    // Validate escapes using the same rules as strings.
    let s = lex.slice();
    if s.len() < 3 {
        return Err(LexErrorKind::InvalidToken);
    }
    // rune literal shares escape forms; allow 1+ content for now.
    // (A strict "single rune" check is done at semantic stage; for parsing we only need tokenization.)
    // Reuse the string escape validator by wrapping in quotes.
    let mut fake = String::with_capacity(s.len());
    fake.push('"');
    fake.push_str(&s[1..s.len() - 1]);
    fake.push('"');

    // local escape scan
    let mut it = fake[1..fake.len() - 1].chars();
    while let Some(c) = it.next() {
        if c != '\\' {
            continue;
        }
        match it.next() {
            Some('a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '\\' | '"' | '\'' ) => {}
            Some('x') => {
                let h1 = it.next().ok_or(LexErrorKind::InvalidEscape)?;
                let h2 = it.next().ok_or(LexErrorKind::InvalidEscape)?;
                if !h1.is_ascii_hexdigit() || !h2.is_ascii_hexdigit() {
                    return Err(LexErrorKind::InvalidEscape);
                }
            }
            Some('u') => {
                for _ in 0..4 {
                    let h = it.next().ok_or(LexErrorKind::InvalidEscape)?;
                    if !h.is_ascii_hexdigit() {
                        return Err(LexErrorKind::InvalidEscape);
                    }
                }
            }
            Some('U') => {
                for _ in 0..8 {
                    let h = it.next().ok_or(LexErrorKind::InvalidEscape)?;
                    if !h.is_ascii_hexdigit() {
                        return Err(LexErrorKind::InvalidEscape);
                    }
                }
            }
            Some('0'..='7') => {
                for _ in 0..2 {
                    if let Some(peek) = it.clone().next() {
                        if ('0'..='7').contains(&peek) {
                            it.next();
                        } else {
                            break;
                        }
                    }
                }
            }
            _ => return Err(LexErrorKind::InvalidEscape),
        }
    }

    Ok(())
}

fn validate_number(lex: &mut LogosLexer<'_, RawTok>) -> Result<(), LexErrorKind> {
    let s = lex.slice();
    if s.starts_with('_') || s.ends_with('_') || s.contains("__") {
        return Err(LexErrorKind::InvalidNumber);
    }
    Ok(())
}

fn validate_imag(lex: &mut LogosLexer<'_, RawTok>) -> Result<(), LexErrorKind> {
    let s = lex.slice();
    if !s.ends_with('i') {
        return Err(LexErrorKind::InvalidNumber);
    }
    Ok(())
}

/// Tokens exposed to LALRPOP.
#[derive(Debug, Clone, PartialEq)]
pub enum Tok {
    // Identifiers + literals
    Ident(String),
    IntLit(String),
    FloatLit(String),
    ImagLit(String),
    RuneLit(String),
    StringLit(String),
    RawStringLit(String),

    // Keywords
    KwBreak,
    KwDefault,
    KwFunc,
    KwInterface,
    KwSelect,
    KwCase,
    KwDefer,
    KwGo,
    KwMap,
    KwStruct,
    KwChan,
    KwElse,
    KwGoto,
    KwPackage,
    KwSwitch,
    KwConst,
    KwFallthrough,
    KwIf,
    KwRange,
    KwType,
    KwContinue,
    KwFor,
    KwImport,
    KwReturn,
    KwVar,

    // Operators and punctuation
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

    // Error token (to allow parser recovery).
    Error,
}

pub type SpannedTok = (usize, Tok, usize);

pub struct Lexer<'src> {
    logos: LogosLexer<'src, RawTok>,
    pending: VecDeque<SpannedTok>,
    diags: Vec<Diag>,
    last_can_insert_semi: bool,
    src_len: usize,
    eof_done: bool,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            logos: RawTok::lexer(input),
            pending: VecDeque::new(),
            diags: Vec::new(),
            last_can_insert_semi: false,
            src_len: input.len(),
            eof_done: false,
        }
    }

    pub fn take_diags(&mut self) -> Vec<Diag> {
        std::mem::take(&mut self.diags)
    }

    fn push_lex_diag(&mut self, kind: LexErrorKind, span: std::ops::Range<usize>) {
        let err = LexError { kind, span };
        self.diags.push(err.diag());
    }

    fn map_ident_or_keyword(&self, s: &str) -> Tok {
        match s {
            "break" => Tok::KwBreak,
            "default" => Tok::KwDefault,
            "func" => Tok::KwFunc,
            "interface" => Tok::KwInterface,
            "select" => Tok::KwSelect,
            "case" => Tok::KwCase,
            "defer" => Tok::KwDefer,
            "go" => Tok::KwGo,
            "map" => Tok::KwMap,
            "struct" => Tok::KwStruct,
            "chan" => Tok::KwChan,
            "else" => Tok::KwElse,
            "goto" => Tok::KwGoto,
            "package" => Tok::KwPackage,
            "switch" => Tok::KwSwitch,
            "const" => Tok::KwConst,
            "fallthrough" => Tok::KwFallthrough,
            "if" => Tok::KwIf,
            "range" => Tok::KwRange,
            "type" => Tok::KwType,
            "continue" => Tok::KwContinue,
            "for" => Tok::KwFor,
            "import" => Tok::KwImport,
            "return" => Tok::KwReturn,
            "var" => Tok::KwVar,
            _ => Tok::Ident(s.to_string()),
        }
    }

    fn can_insert_semi_after(tok: &Tok) -> bool {
        matches!(
            tok,
            Tok::Ident(_)
                | Tok::IntLit(_)
                | Tok::FloatLit(_)
                | Tok::ImagLit(_)
                | Tok::RuneLit(_)
                | Tok::StringLit(_)
                | Tok::RawStringLit(_)
                | Tok::KwBreak
                | Tok::KwContinue
                | Tok::KwFallthrough
                | Tok::KwReturn
                | Tok::Inc
                | Tok::Dec
                | Tok::RParen
                | Tok::RBrack
                | Tok::RBrace
        )
    }

    fn emit_semi_at(&mut self, pos: usize) {
        self.pending.push_back((pos, Tok::Semi, pos));
    }

    fn lex_next_raw(&mut self) -> Option<Result<RawTok, LexErrorKind>> {
        self.logos.next()
    }

    fn validate_identifier_unicode(s: &str) -> bool {
        let mut chars = s.chars();
        let Some(first) = chars.next() else { return false };
        if first != '_' && !is_xid_start(first) {
            return false;
        }
        for c in chars {
            if c != '_' && !is_xid_continue(c) {
                return false;
            }
        }
        true
    }
}

impl Iterator for Lexer<'_> {
    type Item = (usize, Tok, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(sp) = self.pending.pop_front() {
            return Some(sp);
        }

        if self.eof_done {
            return None;
        }

        loop {
            if let Some(sp) = self.pending.pop_front() {
                return Some(sp);
            }

            match self.lex_next_raw() {
                None => {
                    self.eof_done = true;
                    if self.last_can_insert_semi {
                        self.last_can_insert_semi = false;
                        self.emit_semi_at(self.src_len);
                        return self.pending.pop_front();
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

                    match raw {
                        RawTok::Newline => {
                            // Go semicolon insertion: when newline is seen, insert ';' if previous token qualifies.
                            if self.last_can_insert_semi {
                                self.last_can_insert_semi = false;
                                self.emit_semi_at(span.start);
                            }
                            continue;
                        }
                        RawTok::BlockComment => {
                            // If comment contains a newline, treat it like a newline boundary.
                            if slice.contains('\n') || slice.contains('\r') {
                                if self.last_can_insert_semi {
                                    self.last_can_insert_semi = false;
                                    self.emit_semi_at(span.end);
                                }
                            }
                            continue;
                        }
                        RawTok::Error => {
                            self.push_lex_diag(LexErrorKind::InvalidToken, span.clone());
                            self.last_can_insert_semi = false;
                            return Some((span.start, Tok::Error, span.end));
                        }
                        RawTok::Ident => {
                            if !Self::validate_identifier_unicode(slice) {
                                self.push_lex_diag(LexErrorKind::InvalidToken, span.clone());
                                self.last_can_insert_semi = false;
                                return Some((span.start, Tok::Error, span.end));
                            }
                            let tok = self.map_ident_or_keyword(slice);
                            self.last_can_insert_semi = Self::can_insert_semi_after(&tok);
                            return Some((span.start, tok, span.end));
                        }
                        RawTok::Int => {
                            let tok = Tok::IntLit(slice.to_string());
                            self.last_can_insert_semi = true;
                            return Some((span.start, tok, span.end));
                        }
                        RawTok::Float => {
                            let tok = Tok::FloatLit(slice.to_string());
                            self.last_can_insert_semi = true;
                            return Some((span.start, tok, span.end));
                        }
                        RawTok::Imag => {
                            let tok = Tok::ImagLit(slice.to_string());
                            self.last_can_insert_semi = true;
                            return Some((span.start, tok, span.end));
                        }
                        RawTok::Rune => {
                            let tok = Tok::RuneLit(slice.to_string());
                            self.last_can_insert_semi = true;
                            return Some((span.start, tok, span.end));
                        }
                        RawTok::String => {
                            let tok = Tok::StringLit(slice.to_string());
                            self.last_can_insert_semi = true;
                            return Some((span.start, tok, span.end));
                        }
                        RawTok::RawString => {
                            let tok = Tok::RawStringLit(slice.to_string());
                            self.last_can_insert_semi = true;
                            return Some((span.start, tok, span.end));
                        }

                        // Punct / ops: map 1:1
                        other => {
                            let tok = match other {
                                RawTok::Ellipsis => Tok::Ellipsis,
                                RawTok::ShlAssign => Tok::ShlAssign,
                                RawTok::ShrAssign => Tok::ShrAssign,
                                RawTok::AndNotAssign => Tok::AndNotAssign,
                                RawTok::AddAssign => Tok::AddAssign,
                                RawTok::SubAssign => Tok::SubAssign,
                                RawTok::MulAssign => Tok::MulAssign,
                                RawTok::DivAssign => Tok::DivAssign,
                                RawTok::ModAssign => Tok::ModAssign,
                                RawTok::AndAssign => Tok::AndAssign,
                                RawTok::OrAssign => Tok::OrAssign,
                                RawTok::XorAssign => Tok::XorAssign,
                                RawTok::Shl => Tok::Shl,
                                RawTok::Shr => Tok::Shr,
                                RawTok::AndNot => Tok::AndNot,
                                RawTok::LAnd => Tok::LAnd,
                                RawTok::LOr => Tok::LOr,
                                RawTok::EqEq => Tok::EqEq,
                                RawTok::NotEq => Tok::NotEq,
                                RawTok::Le => Tok::Le,
                                RawTok::Ge => Tok::Ge,
                                RawTok::Inc => Tok::Inc,
                                RawTok::Dec => Tok::Dec,
                                RawTok::Define => Tok::Define,
                                RawTok::Arrow => Tok::Arrow,
                                RawTok::Assign => Tok::Assign,
                                RawTok::Plus => Tok::Plus,
                                RawTok::Minus => Tok::Minus,
                                RawTok::Star => Tok::Star,
                                RawTok::Slash => Tok::Slash,
                                RawTok::Percent => Tok::Percent,
                                RawTok::Amp => Tok::Amp,
                                RawTok::Pipe => Tok::Pipe,
                                RawTok::Caret => Tok::Caret,
                                RawTok::Tilde => Tok::Tilde,
                                RawTok::Bang => Tok::Bang,
                                RawTok::Lt => Tok::Lt,
                                RawTok::Gt => Tok::Gt,
                                RawTok::LParen => Tok::LParen,
                                RawTok::RParen => Tok::RParen,
                                RawTok::LBrack => Tok::LBrack,
                                RawTok::RBrack => Tok::RBrack,
                                RawTok::LBrace => Tok::LBrace,
                                RawTok::RBrace => Tok::RBrace,
                                RawTok::Comma => Tok::Comma,
                                RawTok::Semi => Tok::Semi,
                                RawTok::Colon => Tok::Colon,
                                RawTok::Dot => Tok::Dot,
                                _ => {
                                    // should be impossible
                                    Tok::Error
                                }
                            };

                            self.last_can_insert_semi = Self::can_insert_semi_after(&tok);
                            return Some((span.start, tok, span.end));
                        }
                    }
                }
            }
        }
    }
}

impl Tok {
    /// Human-friendly classification for semicolon insertion decisions.
    pub fn is_basic_lit(&self) -> bool {
        matches!(
            self,
            Tok::IntLit(_)
                | Tok::FloatLit(_)
                | Tok::ImagLit(_)
                | Tok::RuneLit(_)
                | Tok::StringLit(_)
                | Tok::RawStringLit(_)
        )
    }
}

// Provide a minimal Display-ish for debugging.
impl std::fmt::Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

