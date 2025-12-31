use criterion::{criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion, Throughput};
use go125_parser::lexer::Lexer;
use go125_parser::{ast, ast2};
use std::hint::black_box as bb;

// =============================================================================
// Test Corpus - Different sizes of Go code
// =============================================================================

const SMALL_HELLO_WORLD: &str = r#"
package main

func main() {
    println("Hello, World!")
}
"#;

const MEDIUM_STRUCT_METHODS: &str = r#"
package geometry

type Point struct {
    X, Y float64
}

func (p Point) Abs() float64 {
    return sqrt(p.X*p.X + p.Y*p.Y)
}

func (p *Point) Scale(f float64) {
    p.X = p.X * f
    p.Y = p.Y * f
}

type Rectangle struct {
    Width, Height float64
}

func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

func (r *Rectangle) Grow(delta float64) {
    r.Width += delta
    r.Height += delta
}
"#;

const LARGE_COMPLEX: &str = r#"
package compiler

import (
    "fmt"
    "strings"
)

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
    return &Lexer{
        input: []byte(source),
        pos:   0,
        line:  1,
    }
}

func (l *Lexer) NextToken() Token {
    if l.pos >= len(l.input) {
        return Token{Kind: TokEOF, Line: l.line}
    }

    ch := l.input[l.pos]
    if isLetter(ch) {
        return l.readIdent()
    }
    if isDigit(ch) {
        return l.readNumber()
    }

    l.pos++
    return Token{Kind: TokIdent, Text: string(ch), Line: l.line}
}

func (l *Lexer) readIdent() Token {
    start := l.pos
    for l.pos < len(l.input) && isLetter(l.input[l.pos]) {
        l.pos++
    }
    return Token{
        Kind: TokIdent,
        Text: string(l.input[start:l.pos]),
        Line: l.line,
    }
}

func (l *Lexer) readNumber() Token {
    start := l.pos
    for l.pos < len(l.input) && isDigit(l.input[l.pos]) {
        l.pos++
    }
    return Token{
        Kind: TokNumber,
        Text: string(l.input[start:l.pos]),
        Line: l.line,
    }
}

func isLetter(ch byte) bool {
    return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
}

func isDigit(ch byte) bool {
    return ch >= '0' && ch <= '9'
}
"#;

// =============================================================================
// Helpers: precompute token counts and string corpora outside measurement
// =============================================================================

fn token_count(input: &str) -> usize {
    Lexer::new(input).count()
}

fn make_unique_names(prefix: &str, n: usize) -> Vec<String> {
    // Precompute once: no allocs during the measured loop.
    let mut out = Vec::with_capacity(n);
    for i in 0..n {
        out.push(format!("{prefix}{i}"));
    }
    out
}

fn make_mixed_names(prefix: &str, n: usize, dup: &str) -> Vec<String> {
    // 50% unique, 50% duplicates, but precomputed.
    let mut out = Vec::with_capacity(n);
    for i in 0..n {
        if i % 2 == 0 {
            out.push(format!("{prefix}{i}"));
        } else {
            out.push(dup.to_string());
        }
    }
    out
}

// =============================================================================
// Benchmark 1: Lexer Performance
//  - iterate_only: measures DFA/tokenization + semicolon insertion without Vec allocation
//  - collect_with_capacity: measures realistic "parser feed" style (Vec push) with fixed capacity
// =============================================================================

fn bench_lexer(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer");

    let corpora = [
        ("small", SMALL_HELLO_WORLD),
        ("medium", MEDIUM_STRUCT_METHODS),
        ("large", LARGE_COMPLEX),
    ];

    // Precompute token counts once (outside timing).
    let counts: Vec<(&str, &str, usize)> = corpora
        .iter()
        .map(|(name, src)| (*name, *src, token_count(src)))
        .collect();

    for (name, input, tok_count) in counts {
        // ---- iterate-only ----
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("iterate_only_bytes", name),
            &input,
            |b, &input| {
                b.iter(|| {
                    let mut acc: u64 = 0;
                    for (l, t, r) in Lexer::new(bb(input)) {
                        // Consume values so the loop can't be optimized away.
                        acc = acc.wrapping_add(l as u64);
                        acc = acc.wrapping_add(r as u64);
                        acc = acc.wrapping_add((&raw const t as usize) as u64);
                    }
                    bb(acc);
                });
            },
        );

        // ---- collect with capacity ----
        group.throughput(Throughput::Elements(tok_count as u64));
        group.bench_with_input(
            BenchmarkId::new("collect_with_capacity_tokens", name),
            &(input, tok_count),
            |b, &(input, tok_count)| {
                b.iter(|| {
                    let mut v = Vec::with_capacity(tok_count);
                    v.extend(Lexer::new(bb(input)));
                    bb(v.len());
                    bb(v);
                });
            },
        );
    }

    group.finish();
}

// =============================================================================
// Benchmark 2: Arena Operations (AST1 vs AST2)
// - alloc_1000_literals: alloc-only
// - alloc_access_1000: alloc + random-ish access pattern (sequential here, stable)
// =============================================================================

fn bench_arena_ops(c: &mut Criterion) {
    let mut group = c.benchmark_group("arena_ops");
    group.throughput(Throughput::Elements(1000));

    group.bench_function("ast1_alloc_1000_literals", |b| {
        b.iter(|| {
            let mut arena = ast::AstArena::new();
            for _ in 0..1000 {
                let id = arena.alloc_expr(
                    ast::Expr::BasicLit(ast::BasicLit {
                        kind: ast::BasicLitKind::Int,
                        raw: ast::Span::new(0, 1),
                    }),
                    ast::Span::new(0, 1),
                );
                bb(id);
            }
            bb(arena);
        });
    });

    group.bench_function("ast2_alloc_1000_literals", |b| {
        b.iter(|| {
            let mut arena = ast2::AstArena::new();
            for _ in 0..1000 {
                let id = arena.alloc_expr(
                    ast2::Expr::BasicLit(ast2::BasicLit {
                        kind: ast2::BasicLitKind::Int,
                        raw: ast2::Span::new(0, 1),
                    }),
                    ast2::Span::new(0, 1),
                );
                bb(id);
            }
            bb(arena);
        });
    });

    group.bench_function("ast1_alloc_access_1000", |b| {
        b.iter(|| {
            let mut arena = ast::AstArena::new();
            let mut ids = Vec::with_capacity(1000);

            for i in 0..1000 {
                let id = arena.alloc_expr(
                    ast::Expr::BasicLit(ast::BasicLit {
                        kind: ast::BasicLitKind::Int,
                        raw: ast::Span::new(i, i + 1),
                    }),
                    ast::Span::new(i, i + 1),
                );
                ids.push(id);
            }

            // Access all (simulate traversal + deref)
            let mut acc: u64 = 0;
            for id in ids {
                let node = arena.get_expr(id);
                acc = acc.wrapping_add(node.span.start as u64);
            }
            bb(acc);
            bb(arena);
        });
    });

    group.bench_function("ast2_alloc_access_1000", |b| {
        b.iter(|| {
            let mut arena = ast2::AstArena::new();
            let mut ids = Vec::with_capacity(1000);

            for i in 0..1000 {
                let id = arena.alloc_expr(
                    ast2::Expr::BasicLit(ast2::BasicLit {
                        kind: ast2::BasicLitKind::Int,
                        raw: ast2::Span::new(i, i + 1),
                    }),
                    ast2::Span::new(i, i + 1),
                );
                ids.push(id);
            }

            let mut acc: u64 = 0;
            for id in ids {
                let sp = arena.exprs.span(id);
                acc = acc.wrapping_add(sp.start as u64);
            }
            bb(acc);
            bb(arena);
        });
    });

    group.finish();
}

// =============================================================================
// Benchmark 3: String Interning (AST1 vs AST2)
// Realistic: NO format!/alloc in hot loop.
// - unique_1000: inserts
// - duplicate_hit_1000: pre-intern once then only hits
// - mixed_1000: 50% inserts, 50% hits
// =============================================================================

fn bench_string_interning(c: &mut Criterion) {
    let mut group = c.benchmark_group("string_interning");
    group.throughput(Throughput::Elements(1000));

    let unique = make_unique_names("identifier_", 1000);
    let mixed = make_mixed_names("unique_", 1000, "common_identifier");
    let dup = "common_identifier";

    // ---- unique inserts ----
    group.bench_function("ast1_unique_1000", |b| {
        b.iter_batched(
            || &unique,
            |names| {
                let mut interner = ast::Interner::new();
                for s in names.iter() {
                    bb(interner.intern(s.as_str()));
                }
                bb(interner);
            },
            BatchSize::SmallInput,
        );
    });

    group.bench_function("ast2_unique_1000", |b| {
        b.iter_batched(
            || &unique,
            |names| {
                let mut interner = ast2::Interner::new();
                for s in names.iter() {
                    bb(interner.intern(s.as_str()));
                }
                bb(interner);
            },
            BatchSize::SmallInput,
        );
    });

    // ---- duplicate hit only ----
    group.bench_function("ast1_duplicate_hit_1000", |b| {
        b.iter(|| {
            let mut interner = ast::Interner::new();
            bb(interner.intern(dup)); // preload once
            for _ in 0..1000 {
                bb(interner.intern(dup)); // hits only
            }
            bb(interner);
        });
    });

    group.bench_function("ast2_duplicate_hit_1000", |b| {
        b.iter(|| {
            let mut interner = ast2::Interner::new();
            bb(interner.intern(dup)); // preload once
            for _ in 0..1000 {
                bb(interner.intern(dup)); // hits only
            }
            bb(interner);
        });
    });

    // ---- mixed workload ----
    group.bench_function("ast1_mixed_1000", |b| {
        b.iter_batched(
            || &mixed,
            |names| {
                let mut interner = ast::Interner::new();
                for s in names.iter() {
                    bb(interner.intern(s.as_str()));
                }
                bb(interner);
            },
            BatchSize::SmallInput,
        );
    });

    group.bench_function("ast2_mixed_1000", |b| {
        b.iter_batched(
            || &mixed,
            |names| {
                let mut interner = ast2::Interner::new();
                for s in names.iter() {
                    bb(interner.intern(s.as_str()));
                }
                bb(interner);
            },
            BatchSize::SmallInput,
        );
    });

    group.finish();
}

// =============================================================================
// Benchmark 4: Span Layout / Traversal
// Compare "node + span" and "node-only" traversals.
// =============================================================================

fn bench_span_layouts(c: &mut Criterion) {
    let mut group = c.benchmark_group("span_layouts");
    group.throughput(Throughput::Elements(1000));

    group.bench_function("ast1_node_plus_span_1000", |b| {
        b.iter(|| {
            let mut arena = ast::AstArena::new();
            let mut ids = Vec::with_capacity(1000);

            for i in 0..1000 {
                ids.push(arena.alloc_expr(
                    ast::Expr::BasicLit(ast::BasicLit {
                        kind: ast::BasicLitKind::Int,
                        raw: ast::Span::new(i, i + 1),
                    }),
                    ast::Span::new(i * 10, i * 10 + 5),
                ));
            }

            let mut acc = 0u64;
            for id in ids {
                let spanned = arena.get_expr(id);
                acc = acc.wrapping_add(spanned.span.start as u64);
                if let ast::Expr::BasicLit(lit) = &spanned.node {
                    acc = acc.wrapping_add(lit.raw.start as u64);
                }
            }

            bb(acc);
            bb(arena);
        });
    });

    group.bench_function("ast2_node_plus_span_1000", |b| {
        b.iter(|| {
            let mut arena = ast2::AstArena::new();
            let mut ids = Vec::with_capacity(1000);

            for i in 0..1000 {
                ids.push(arena.alloc_expr(
                    ast2::Expr::BasicLit(ast2::BasicLit {
                        kind: ast2::BasicLitKind::Int,
                        raw: ast2::Span::new(i, i + 1),
                    }),
                    ast2::Span::new(i * 10, i * 10 + 5),
                ));
            }

            let mut acc = 0u64;
            for id in ids {
                let sp = arena.exprs.span(id.clone());
                acc = acc.wrapping_add(sp.start as u64);
                if let ast2::Expr::BasicLit(lit) = arena.exprs.get(id) {
                    acc = acc.wrapping_add(lit.raw.start as u64);
                }
            }

            bb(acc);
            bb(arena);
        });
    });

    group.bench_function("ast1_node_only_1000", |b| {
        b.iter(|| {
            let mut arena = ast::AstArena::new();
            let mut ids = Vec::with_capacity(1000);

            for i in 0..1000 {
                ids.push(arena.alloc_expr(
                    ast::Expr::BasicLit(ast::BasicLit {
                        kind: ast::BasicLitKind::Int,
                        raw: ast::Span::new(i, i + 1),
                    }),
                    ast::Span::new(0, 1),
                ));
            }

            let mut acc = 0u64;
            for id in ids {
                if let ast::Expr::BasicLit(lit) = &arena.get_expr(id).node {
                    acc = acc.wrapping_add(lit.raw.start as u64);
                }
            }

            bb(acc);
        });
    });

    group.bench_function("ast2_node_only_1000", |b| {
        b.iter(|| {
            let mut arena = ast2::AstArena::new();
            let mut ids = Vec::with_capacity(1000);

            for i in 0..1000 {
                ids.push(arena.alloc_expr(
                    ast2::Expr::BasicLit(ast2::BasicLit {
                        kind: ast2::BasicLitKind::Int,
                        raw: ast2::Span::new(i, i + 1),
                    }),
                    ast2::Span::new(0, 1),
                ));
            }

            let mut acc = 0u64;
            for id in ids {
                if let ast2::Expr::BasicLit(lit) = arena.exprs.get(id) {
                    acc = acc.wrapping_add(lit.raw.start as u64);
                }
            }

            bb(acc);
        });
    });

    group.finish();
}

// =============================================================================
// Benchmark 5: Scalability
// Separate components so results are interpretable.
// - arena_only: alloc N nodes using pre-interned symbols (no interner work)
// - interner_only: intern N unique strings (no arena work)
// - combined: both, but NO format! in hot loop
// =============================================================================

fn bench_scalability(c: &mut Criterion) {
    let mut group = c.benchmark_group("scalability");

    for &n in &[100usize, 500, 1000, 5000, 10000] {
        let names = make_unique_names("var", n);

        // ----- interner only -----
        group.throughput(Throughput::Elements(n as u64));
        group.bench_with_input(
            BenchmarkId::new("ast1_interner_only", n),
            &names,
            |b, names| {
                b.iter(|| {
                    let mut interner = ast::Interner::new();
                    for s in names.iter() {
                        bb(interner.intern(s.as_str()));
                    }
                    bb(interner);
                });
            },
        );

        group.bench_with_input(
            BenchmarkId::new("ast2_interner_only", n),
            &names,
            |b, names| {
                b.iter(|| {
                    let mut interner = ast2::Interner::new();
                    for s in names.iter() {
                        bb(interner.intern(s.as_str()));
                    }
                    bb(interner);
                });
            },
        );

        // ----- arena only (pre-intern once) -----
        // Note: we pre-intern to avoid mixing interner cost into arena cost.
        group.bench_with_input(BenchmarkId::new("ast1_arena_only", n), &n, |b, &n| {
            b.iter(|| {
                let mut arena = ast::AstArena::new();
                // Use a tiny interner just to produce a stable symbol.
                // If you already have a Symbol type that can be constructed directly, do that instead.
                let mut interner = ast::Interner::new();
                let sym = interner.intern("x");

                for i in 0..n {
                    let id = arena.alloc_expr(ast::Expr::Ident(sym), ast::Span::new(i, i + 1));
                    bb(id);
                }
                bb(arena);
            });
        });

        group.bench_with_input(BenchmarkId::new("ast2_arena_only", n), &n, |b, &n| {
            b.iter(|| {
                let mut arena = ast2::AstArena::new();
                let mut interner = ast2::Interner::new();
                let sym = interner.intern("x");

                for i in 0..n {
                    let id = arena.alloc_expr(ast2::Expr::Ident(sym), ast2::Span::new(i, i + 1));
                    bb(id);
                }
                bb(arena);
            });
        });

        // ----- combined -----
        // Uses precomputed names (no format!) and does interner + arena.
        group.bench_with_input(BenchmarkId::new("ast1_combined", n), &names, |b, names| {
            b.iter(|| {
                let mut arena = ast::AstArena::new();
                let mut interner = ast::Interner::new();

                for (i, s) in names.iter().enumerate() {
                    let sym = interner.intern(s.as_str());
                    let id = arena.alloc_expr(ast::Expr::Ident(sym), ast::Span::new(i, i + 1));
                    bb(id);
                }

                bb((arena, interner));
            });
        });

        group.bench_with_input(BenchmarkId::new("ast2_combined", n), &names, |b, names| {
            b.iter(|| {
                let mut arena = ast2::AstArena::new();
                let mut interner = ast2::Interner::new();

                for (i, s) in names.iter().enumerate() {
                    let sym = interner.intern(s.as_str());
                    let id = arena.alloc_expr(ast2::Expr::Ident(sym), ast2::Span::new(i, i + 1));
                    bb(id);
                }

                bb((arena, interner));
            });
        });
    }

    group.finish();
}

// =============================================================================
// Criterion registration
// =============================================================================

criterion_group!(
    benches,
    bench_lexer,
    bench_arena_ops,
    bench_string_interning,
    bench_span_layouts,
    bench_scalability
);
criterion_main!(benches);
