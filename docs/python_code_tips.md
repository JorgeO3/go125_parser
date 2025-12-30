# RustPython LALRPOP Architecture: Industrial-Grade Python Parsing

This guide dissects the architecture used by RustPython to parse the full Python 3 grammar using LALRPOP. It highlights advanced patterns for handling complexity, indentation, and performance optimization.

## 1. Single Entry Point Strategy (Table Size Optimization)

LALRPOP generates a parsing table for *each* public entry point. For a grammar as massive as Python's, this explodes compilation time and binary size.

* **The Hack:** Define **only one** public entry point (`Top`) that dispatches to internal sub-parsers based on the first token.
* **Implementation:**
```rust
// Only 'Top' is pub. It acts as a multiplexer.
pub Top: ast::Mod = {
    <start:@L> StartModule <body:Program> ... => ast::ModModule { ... }.into(),
    <start:@L> StartInteractive <body:Program> ... => ast::ModInteractive { ... }.into(),
    // ...
};

```


* **Result:** Drastic reduction in generated code size and faster build times.

## 2. Handling Python's "Significant Whitespace" (Indent/Dedent)

LALRPOP is whitespace-agnostic. Python is not. RustPython solves this entirely in the **Lexer**, keeping the grammar clean.

* **Lexer Responsibility:** The external lexer tracks indentation levels and emits virtual tokens: `Indent` and `Dedent`.
* **Grammar Usage:**
```rust
Suite: ast::Suite = {
    // ...
    "\n" Indent <s:Statements> Dedent => s, // Looks like block { ... } in C-like languages
};

```


* **Lesson:** Do not attempt to track indentation state inside the grammar logic. Use virtual tokens.

## 3. Advanced Context Handling (`set_context`)

Python reuses the same syntax for loading (reading) and storing (writing) variables.

* `x` in `y = x` is `ExprContext::Load`.
* `x` in `x = y` is `ExprContext::Store`.

The grammar parses everything as expressions first (defaulting to `Load`) and then "patches" the context when it realizes it's an assignment target.

* **Helper Function:** `set_context(expr, ctx)` recursively traverses the AST node and flips the context.
```rust
<location:@L> <target:TestOrStarExprList> <op:AugAssign> <rhs:TestListOrYieldExpr> ... => {
    ast::Stmt::AugAssign(
        ast::StmtAugAssign {
            // We parsed 'target' as an expression, now we force it to Store context
            target: Box::new(set_context(target, ast::ExprContext::Store)),
            // ...
        },
    )
},

```



## 4. Recursive Structuring for Recursive Structures

LALRPOP (like all LR parsers) prefers left-recursion for lists to keep the stack small, but Python lists are often cleaner with vectors.

### A. The `OneOrMore` & `TwoOrMore` Macros

Standardizes list parsing to avoid repetitive code.

```rust
OneOrMore<T>: Vec<T> = {
    <e:T> => vec![e],
    <mut v: OneOrMore<T>> "," <e:T> => { // Left-recursive for efficiency
        v.push(e);
        v
    }
};

```

### B. Statement Grouping

The grammar groups statements into "Small" (one-liners) and "Compound" (blocks like `if`, `def`), mirroring the official Python grammar structure. This structure is critical for correct precedence and association.

## 5. Inline Rules for Ambiguity Resolution

The `#[inline]` attribute is used strategically to inline rules into their parents. This helps resolve shift/reduce conflicts by exposing the inner tokens to the outer rule's lookahead.

```rust
#[inline]
TestList: ast::Expr = {
    GenericList<TestOrStarExpr>
};

```

## 6. Complex Error Validation inside Grammar

Instead of just accepting anything that parses, the grammar performs semantic validation immediately using Rust code (fallible actions `=>?`).

* **Example: Argument Validation**
The grammar parses a generic list of arguments, then calls `validate_arguments` to ensure rules like "positional args cannot follow keyword args".
```rust
Parameters: ast::Arguments = {
    "(" <a: (...)?> ")" =>? {
        // Logic moved to Rust helper function for cleanliness
        a.as_ref().map(validate_arguments).transpose()?; 
        // ...
        Ok(args)
    }
};

```



## 7. Mini-AI Cheatsheet: RustPython Best Practices

Instructions for the model based on this codebase:

1. **Single Entry Point:** If the grammar is large, create a single `pub Top` enum that wraps the different parsing modes to save compilation time.
2. **Context Patching:** For languages where L-values and R-values look the same (like Python variables), parse as R-value (Expression) and use a Rust helper to transform it to L-value (Target) when the assignment operator `=` is found.
3. **Virtual Block Tokens:** Assume `Indent`/`Dedent` tokens exist. Do not mix whitespace logic with grammar logic.
4. **Helper Validators:** Keep the grammar readable. If a rule needs complex validation (like argument ordering), call an external Rust function via `=>?` instead of writing complex grammar rules to enforce it.
5. **Flatten Recursion:** Use `mut v` and `.push()` in recursive list rules to build vectors efficiently without deep recursion overhead.
