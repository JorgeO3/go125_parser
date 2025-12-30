use std::env;

fn main() {
    let path = env::args().nth(1).unwrap_or_else(|| {
        eprintln!("usage: cargo run --example parse -- <file.go>");
        std::process::exit(2);
    });

    let src = std::fs::read_to_string(&path).expect("read file");
    match go125_parser::parse_source(&src) {
        Ok(ast) => {
            println!("{ast:#?}");
        }
        Err(f) => {
            eprintln!("parse failed");
            for d in &f.diags {
                eprintln!("{:?} {:?}: {}", d.kind, d.span, d.message);
            }
            std::process::exit(1);
        }
    }
}
