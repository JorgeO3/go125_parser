# LALRPOP & Logos Integration Strategy: Definitive Guide

This guide condenses the architecture required to build a high-performance parser (e.g., for Go/Golang) using **LALRPOP** (Parser Generator) and **Logos** (Lexer).

## 1. Core Architecture & Build Pipeline

LALRPOP generates Rust code from a `.lalrpop` grammar file. Since we are overriding the default lexer with Logos, the pipeline is strict.

* **Cargo.toml**: Sync versions for `lalrpop` (build-dep) and `lalrpop-util`.
* **build.rs**: Must include `lalrpop::process_root().unwrap();`.
* **File Structure**:
* `lexer.rs`: Defines the Logos enum and the Iterator adapter.
* `grammar.lalrpop`: Defines the high-level grammar and the `extern` binding to Logos.



## 2. The Logos Bridge (Critical)

LALRPOP expects an `Iterator<Item = Result<(usize, Token, usize), Error>>`. Logos provides a `SpannedIter`. You must adapt one to the other.

### A. The Token Enum (Logos)

Use `&'input str` to avoid memory allocation (Zero-Copy). This is vital for performance when parsing large Go files.

```rust
// src/lexer.rs
use logos::Logos;

#[derive(Logos, Clone, Debug, PartialEq)]
// Skip whitespace immediately for speed
#[logos(skip r"[ \t\n\f]+")] 
pub enum Token<'input> {
    // Structural
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("{")] LBrace,
    #[token("}")] RBrace,
    #[token(";")] Semi,
    
    // Keywords
    #[token("func")] Func,
    #[token("package")] Package,

    // Zero-Copy Literals & Identifiers
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
    Identifier(&'input str),

    #[regex("[0-9]+", |lex| lex.slice())]
    IntLiteral(&'input str),

    // Error handling
    #[error] 
    Error,
}

```

### B. The Adapter (The Glue)

LALRPOP requires a specific wrapper to map Logos output to `(Start, Token, End)`.

```rust
// src/lexer.rs
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct Lexer<'input> {
    // Wrap Logos iterator
    token_stream: logos::SpannedIter<'input, Token<'input>>, 
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self { token_stream: Token::lexer(input).spanned() }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, usize, ()>; // Custom Error type if needed

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream.next().map(|(token, span)| {
            match token {
                // Map Logos Token to Ok tuple
                Ok(t) => Ok((span.start, t, span.end)),
                // Handle Lexer Errors
                Err(_) => Err(()), 
            }
        })
    }
}

```

## 3. Grammar Definition (`grammar.lalrpop`)

You must explicitly tell LALRPOP that you are bringing your own lexer using the `extern` block. This maps the "String Literals" inside the grammar to the `Token` enum variants in Rust.

```rust
use crate::lexer::Token; // Import your Logos token

grammar<'input>(input: &'input str); // Lifetime context is required for zero-copy

// 1. AST MAPPING
pub File: File = {
    <p:PackageDecl> <i:ImportDecl*> <t:TopLevelDecl*> => File { ... }
};

// 2. EXTERNAL LEXER BINDING
extern {
    type Location = usize;
    type Error = (); // Match your Iterator error type

    enum Token<'input> {
        "func"    => Token::Func,
        "package" => Token::Package,
        "("       => Token::LParen,
        ")"       => Token::RParen,
        ";"       => Token::Semi,
        
        // Data carrying tokens
        "id"      => Token::Identifier(<&'input str>),
        "int"     => Token::IntLiteral(<&'input str>),
    }
}

```

## 4. High-Value Patterns & Heuristics

### A. Precedence & Associativity (The Modern Way)

Do not write tiered grammars (Factor -> Term -> Expr). Use `#[precedence]` attributes for cleaner, flatter grammars (essential for complex languages like Go).

```rust
pub Expr: Box<Expr> = {
    #[precedence(level="0")] // Highest priority (atoms)
    "id" => Box::new(Expr::Var(<>)),
    "(" <Expr> ")",

    #[precedence(level="1")] #[assoc(side="left")]
    <l:Expr> "*" <r:Expr> => Box::new(Expr::Binary(l, Op::Mul, r)),
    <l:Expr> "/" <r:Expr> => Box::new(Expr::Binary(l, Op::Div, r)),

    #[precedence(level="2")] #[assoc(side="left")]
    <l:Expr> "+" <r:Expr> => Box::new(Expr::Binary(l, Op::Add, r)),
};

```

### B. Macros (DRY Principle)

Go has many comma-separated lists. Use macros to avoid repetition.

* **Tip:** `v:(<T> ",")*` creates a Vector of items followed by commas.
* **Tip:** `e:T?` handles the optional trailing element.

```rust
// Generic Comma-separated list with optional trailing comma
Comma<T>: Vec<T> = { 
    <mut v:(<T> ",")*> <e:T?> => match e { 
        None => v, 
        Some(e) => { v.push(e); v } 
    } 
};

// Usage
FuncParams = "(" <Comma<Param>> ")";

```

### C. Fallible Actions (`=>?`)

If you need to validate data *during* parsing (e.g., checking if an int fits in 64 bits), use the fallible arrow.

```rust
Num: i64 = {
    "int" =>? i64::from_str_radix(<>, 10).map_err(|_| ParseError::User { ... })
};

```

## 5. Error Recovery (`!`)

For a usable parser (like an IDE or Linter), you cannot stop at the first error. Use the `!` token to recover.

1. Inject an error storage vector into the grammar arguments.
2. Define a recovery rule.

```rust
grammar<'err>(errors: &'err mut Vec<ErrorRecovery<...>>);

Statement: Stmt = {
    LetStmt,
    ReturnStmt,
    // On error, consume tokens until a semicolon is found, log error, continue.
    ! => { errors.push(<>); Stmt::Error }, 
};

```

## 6. Location Tracking (`@L`, `@R`)

When building the AST, always capture locations for error reporting.

* `@L`: Byte offset of the start.
* `@R`: Byte offset of the end.

```rust
Identifier: Spanned<String> = {
    <l:@L> <s:"id"> <r:@R> => Spanned { 
        val: s.to_string(), 
        span: l..r 
    }
};

```

---