# Solang Solidity Parser: Manual Precedence & Dual-Language Architecture

This guide analyzes the Solang parser, which handles both Solidity and Yul (inline assembly). It is characterized by its explicit, tiered precedence handling and robust error recovery strategy.

## 1. Explicit Precedence Tiering (The "Cascading" Pattern)

Unlike parsers that rely on `#[precedence]` annotations, Solang defines precedence **structurally** in the grammar. This is more verbose but offers absolute control over the operator hierarchy, matching the C/C++ style standard often found in blockchain languages.

### The Pattern:

* Define a rule for each precedence level (`Precedence0` to `Precedence14`).
* Each level can:
1. Parse an operator at its level and recurse.
2. Fall through to the next lower precedence level.



```rust
// Highest precedence (Assignment)
Precedence14: Expression = {
    <l:Precedence13> "=" <r:Precedence14> => Expression::Assign(...),
    Precedence13, // Fallthrough
}

// ... levels down ...

// Lowest precedence (Terminals)
Precedence0: Expression = {
    <FunctionCall>,
    <Variable>,
    LiteralExpression
}

```

* **Why:** This ensures `a = b + c * d` is parsed as `a = (b + (c * d))` without ambiguity, strictly enforcing the order of operations defined in the Solidity spec.

## 2. Dual-Language Parsing (Solidity + Yul)

Solidity allows embedding Yul (an intermediate assembly language) inside `assembly { ... }` blocks. The parser seamlessly switches modes.

* **Grammar Switch:**
```rust
NonIfStatement: Statement = {
    // ...
    "assembly" ... <block:YulBlock> => Statement::Assembly { block, ... },
}

```


* **Yul Sub-Grammar:** A completely separate set of rules (`YulStatement`, `YulExpression`, `YulBlock`) is defined within the same file to handle the distinct syntax of Yul (which is functional/Lisp-like) compared to Solidity (C-like).

## 3. Robust Error Recovery (`ErrorRecovery`)

Solang is designed to be a compiler that reports *all* errors, not just the first one.

### A. The Error Sink

The grammar accepts a mutable vector for errors:

```rust
grammar<'input, 'err>(..., parser_errors: &'err mut Vec<ErrorRecovery<...>>);

```

### B. The "Bad Token" Trap (`!`)

It aggressively traps unexpected tokens using the `!` macro to prevent the parser from crashing, allowing it to record the error and continue.

```rust
ImportDirective: SourceUnitPart = {
    // Valid import
    "import" "*" "as" <id> "from" ... => ...,

    // Invalid import (missing 'from')
    "import" "*" "as" <id> <bad:!> ... => {
        // 1. Construct ErrorRecovery struct
        // 2. Push to parser_errors
        // 3. Return a valid node anyway to keep parsing
    }
}

```

* **Lesson:** For production compilers, anticipate common mistakes (like missing keywords) and create specific rules to catch them.

## 4. Location Tracking (`Loc`)

Solang tracks file numbers along with byte offsets, essential for multi-file projects (imports).

```rust
<l:@L> "contract" <r:@R> => ContractTy::Contract(Loc::File(file_no, l, r)),

```

* **Context:** The `file_no` is passed as an argument to the grammar and embedded into every AST node's location data.

## 5. Handling Versioning (Pragmas)

The parser includes specialized rules for parsing `pragma solidity` version strings directly, rather than treating them as generic strings.

```rust
VersionComparator: VersionComparator = {
    <op:VersionOp> <version:Version> => ..., // e.g., >=0.8.0
    <from:Version> "-" <to:Version> => ...,  // e.g., 0.5.0 - 0.6.0
}

```

This allows the compiler to validate version constraints at the parsing stage.

## 6. Mini-AI Cheatsheet: Solang Best Practices

Instructions for the model based on this codebase:

1. **Structural Precedence:** If precise control is needed (or if porting a C++ grammar), manually tier your expression rules (`Precedence0`...`PrecedenceN`) instead of using auto-precedence.
2. **Dual Grammars:** If a language embeds another (like JS inside HTML, or Assembly inside C), define distinct sets of rules (`Yul*` vs `Solidity*`) and link them at the block level.
3. **Error Accumulation:** Pass a `&mut Vec<Error>` to the grammar. When a rule fails recoverably (e.g., via `!`), push the error and return a valid dummy node.
4. **File Context:** For multi-file compilers, pass the `file_id` into the grammar to tag every AST node with its source file, not just byte offsets.
5. **Strict Keywords:** If a keyword is used in the wrong place (e.g., `default` outside a switch), catch it with a specific rule and error out, rather than letting the generic identifier rule swallow it.
