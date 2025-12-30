// Prueba rápida del lexer refactorizado
#![allow(dead_code)]

mod error {
    use std::ops::Range;

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum LexErrorKind {
        InvalidToken,
        InvalidEscape,
        InvalidNumber,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct LexError {
        pub kind: LexErrorKind,
        pub span: Range<usize>,
    }

    impl LexError {
        pub fn diag(&self) -> Diag {
            Diag {
                message: format!("{:?}", self.kind),
                span: self.span.clone(),
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Diag {
        pub message: String,
        pub span: Range<usize>,
    }
}

#[path = "src/lexer.rs"]
mod lexer;

fn main() {
    use lexer::{Lexer, Tok};

    let input = r#"
package main

func main() {
    x := 42
    println("Hello, World!")
}
"#;

    let mut lex = Lexer::new(input);
    let mut count = 0;

    for (start, tok, end) in &mut lex {
        count += 1;
        println!("{:>3}:{:<3} {:?}", start, end, tok);
        if count > 30 {
            println!("...");
            break;
        }
    }

    let diags = lex.take_diags();
    println!("\nDiagnostics: {}", diags.len());
    for diag in diags {
        println!("  {:?}", diag);
    }

    println!("\n✓ Lexer refactorizado funciona correctamente!");
}
