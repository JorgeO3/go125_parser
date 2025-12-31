// src/bin/hot_lexer.rs
use go125_parser::lexer::Lexer;
use go125_parser::lexer::Tok;
use std::hint::black_box as bb;
use std::time::{Duration, Instant};

const SMALL: &str = r#"
package main
func main() { println("Hello, World!") }
"#;

const MEDIUM: &str = r#"
package geometry

type Point struct { X, Y float64 }

func (p Point) Abs() float64 {
    return sqrt(p.X*p.X + p.Y*p.Y)
}

func (p *Point) Scale(f float64) {
    p.X = p.X * f
    p.Y = p.Y * f
}
"#;

const LARGE: &str = r#"
package compiler

import ("fmt"; "strings")

type TokenKind int
const (
    TokEOF TokenKind = iota
    TokIdent
    TokNumber
    TokString
)

type Token struct {
    Kind TokenKind
    Text string
    Line int
}

type Lexer struct {
    input  []byte
    pos    int
    line   int
    tokens []Token
}

func NewLexer(source string) *Lexer {
    return &Lexer{ input: []byte(source), pos: 0, line: 1 }
}

func (l *Lexer) NextToken() Token {
    if l.pos >= len(l.input) { return Token{Kind: TokEOF, Line: l.line} }
    ch := l.input[l.pos]
    if isLetter(ch) { return l.readIdent() }
    if isDigit(ch) { return l.readNumber() }
    l.pos++
    return Token{Kind: TokIdent, Text: string(ch), Line: l.line}
}
"#;

// ----------------- arg parsing -----------------

fn arg_value<'a>(args: &'a [String], key: &str) -> Option<&'a str> {
    let mut it = args.iter();
    while let Some(a) = it.next() {
        if a == key {
            return it.next().map(|s| s.as_str());
        }
    }
    None
}

fn arg_usize(args: &[String], key: &str, default: usize) -> usize {
    arg_value(args, key)
        .and_then(|s| s.parse().ok())
        .unwrap_or(default)
}

fn arg_f64(args: &[String], key: &str, default: f64) -> f64 {
    arg_value(args, key)
        .and_then(|s| s.parse().ok())
        .unwrap_or(default)
}

fn arg_str<'a>(args: &'a [String], key: &str, default: &'a str) -> &'a str {
    arg_value(args, key).unwrap_or(default)
}

// ----------------- hot loop -----------------

#[inline(never)]
fn lex_once(input: &str) -> u64 {
    let mut checksum: u64 = 0;
    let mut n: u64 = 0;

    for (s, tok, e) in Lexer::new(bb(input)) {
        n = n.wrapping_add(1);
        checksum ^= (s as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15);
        checksum ^= (e as u64).wrapping_mul(0xC2B2_AE3D_27D4_EB4F);

        // Touch payloads so the DFA + slicing work can't be optimized away.
        match tok {
            Tok::Ident(x)
            | Tok::IntLit(x)
            | Tok::FloatLit(x)
            | Tok::ImagLit(x)
            | Tok::RuneLit(x)
            | Tok::StringLit(x)
            | Tok::RawStringLit(x) => {
                checksum ^= x.len() as u64;
                if let Some(b0) = x.as_bytes().first() {
                    checksum ^= (*b0 as u64) << 32;
                }
            }
            _ => {
                checksum ^= 1;
            }
        }
    }

    checksum ^ (n.wrapping_mul(0xD6E8_FEB8_6659_FD93))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let seconds = arg_f64(&args, "--seconds", 3.0);
    let repeat = arg_usize(&args, "--repeat", 64);
    let corpus = arg_str(&args, "--corpus", "large");

    let base = match corpus {
        "small" => SMALL,
        "medium" => MEDIUM,
        _ => LARGE,
    };

    // Build a bigger input *outside* the measured hot loop.
    let mut input = String::with_capacity(base.len() * repeat);
    for _ in 0..repeat {
        input.push_str(base);
        input.push('\n');
    }

    let dur = Duration::from_secs_f64(seconds);
    let start = Instant::now();

    let mut iters: u64 = 0;
    let mut acc: u64 = 0;

    while start.elapsed() < dur {
        acc ^= lex_once(&input);
        iters += 1;
    }

    eprintln!(
        "hot_lexer done: iters={iters}, checksum={acc}, bytes={}",
        input.len()
    );
    bb(acc);
}
