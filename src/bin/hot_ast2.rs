// src/bin/hot_ast2.rs
use go125_parser::ast2;
use std::hint::black_box as bb;
use std::time::{Duration, Instant};

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

// ----------------- hot loop -----------------

#[inline(never)]
fn build_ast_once(items: usize, names: &[String]) -> u64 {
    let mut arena = ast2::AstArena::new();
    let mut interner = ast2::Interner::new();

    // Reserva para bajar reallocs (ajústalo según tu parser real)
    arena.exprs.reserve(items * 3);
    arena.stmts.reserve(items);
    arena.signatures.reserve(8);
    arena.funcs.reserve(2);

    arena.extras.exprs.reserve(items * 2);
    arena.extras.stmts.reserve(items);
    arena.extras.idents.reserve(items);

    let mut stmt_ids: Vec<ast2::StmtId> = Vec::with_capacity(items);

    for i in 0..items {
        let nm = &names[i % names.len()];
        let sym = interner.intern(nm);

        let s0 = i * 4;
        let ident = arena.alloc_expr(ast2::Expr::Ident(sym), ast2::Span::new(s0, s0 + 1));

        let lit = arena.alloc_expr(
            ast2::Expr::BasicLit(ast2::BasicLit {
                kind: ast2::BasicLitKind::Int,
                raw: ast2::Span::new(s0 + 1, s0 + 2),
            }),
            ast2::Span::new(s0 + 1, s0 + 2),
        );

        let bin = arena.alloc_expr(
            ast2::Expr::Binary {
                left: ident,
                op: ast2::BinaryOp::Add,
                right: lit,
            },
            ast2::Span::new(s0, s0 + 2),
        );

        // extras: ListRef (evita Vec por nodo)
        let lhs = arena.list_exprs([ident]);
        let rhs = arena.list_exprs([bin]);

        let st = arena.alloc_stmt(
            ast2::Stmt::Assign {
                lhs,
                op: ast2::AssignOp::Assign,
                rhs,
            },
            ast2::Span::new(s0, s0 + 2),
        );

        stmt_ids.push(st);
    }

    let stmts_list = arena.list_stmts(stmt_ids.iter().cloned());
    let block = ast2::Block { stmts: stmts_list };

    // Un func decl mínimo para tocar más arenas
    let empty_fields = ast2::FieldList {
        fields: ast2::ListRef::EMPTY,
    };
    let sig = arena.alloc_signature(
        ast2::Signature {
            params: empty_fields,
            results: None,
        },
        ast2::Span::new(0, 0),
    );
    let fname = interner.intern("f");
    let _fid = arena.alloc_func(
        ast2::FuncDecl {
            recv: None,
            name: fname,
            type_params: None,
            signature: sig,
            body: Some(block),
        },
        ast2::Span::new(0, 0),
    );

    // checksum: toca spans (side-table) y símbolos para que no se muera el trabajo
    let mut sum: u64 = interner.len() as u64;
    for id in stmt_ids.iter().take(64) {
        let sp = arena.stmt_span(id.clone());
        sum ^= (sp.start as u64).wrapping_mul(0xA24B_AED4_963E_E407);
    }

    bb(sum);
    sum
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let seconds = arg_f64(&args, "--seconds", 3.0);
    let items = arg_usize(&args, "--items", 50_000);
    let unique = arg_usize(&args, "--unique", 8_192);

    // Preconstruye nombres fuera del hot loop (evita format!/alloc en la medición)
    let mut names = Vec::with_capacity(unique);
    for i in 0..unique {
        names.push(format!("var{i}"));
    }

    let dur = Duration::from_secs_f64(seconds);
    let start = Instant::now();

    let mut iters: u64 = 0;
    let mut acc: u64 = 0;

    while start.elapsed() < dur {
        acc ^= build_ast_once(items, &names);
        iters += 1;
    }

    eprintln!("hot_ast2 done: iters={iters}, checksum={acc}, items={items}, unique={unique}");
    bb(acc);
}
