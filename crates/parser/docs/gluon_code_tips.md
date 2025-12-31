# Gluon LALRPOP Architecture: Arena Allocation & Vector Recycling

This guide analyzes the architecture of the Gluon parser. Its defining characteristic is the **almost total absence of standard Heap allocations (malloc)** during parsing. Everything is managed through an `Arena` and a `TempVecs` system for temporary lists.

## 1. Memory Management: The "Arena + Reference" Pattern

The parser does not return proprietary structures (`Box<Expr>`, `Vec<Expr>`). It returns references with an `'ast` lifetime pointing to contiguous pre-allocated memory (Arena).

### A. Dependency Injection in Grammar

```rust
grammar<'input, 'env, 'ast, Id>(
    arena: ast::ArenaRef<'_, 'ast, Id>, // <--- The Allocator
    temp_vecs: &mut TempVecs<'ast, Id>, // <--- Buffer of reusable vectors
    // ...
)

```

* **Arena (`arena`):** Instead of `Box::new()`, `arena.alloc()` is used. This makes AST construction extremely fast and cache-friendly.
* **Lifetime `'ast`:** All AST nodes live as long as the arena.

### B. Temporary Vectors (`TempVecs`)

Parsing lists (e.g., function arguments `a, b, c`) usually implies creating many small `Vec`s that are discarded later. Gluon avoids this.

1. Uses a `TempVecs` (a stack of reusable vectors).
2. Parses elements into this temporary buffer.
3. Moves elements to the Arena (`Slice<T>`) and clears the buffer.

## 2. High-Performance Macros: "Parse & Drain"

Macros here are not just syntactic sugar; they are **memory managers**.

### A. The `SepSlice` Macro (TempVec to Arena)

This macro parses a list separated by a token and returns an `&'ast mut Slice<Rule>` (an array in the arena), not a `Vec`.

```rust
#[inline]
SepSlice<Rule, Separator>: &'ast mut Slice<Rule> =
    <start: Many1Vec<(<Rule> Separator)>?> <last: Rule?> => {
        match start {
            // 1. Take items from the temporary buffer (start)
            // 2. Add the last item (last)
            // 3. Move everything to the Arena (arena.alloc_extend)
            Some(start) => arena.alloc_extend(temp_vecs.drain(start).chain(last)),
            None => arena.alloc_extend(last),
        }
    };

```

* **Lesson for AI:** If you need lists, do not return `Vec<T>`. Parse into a temporary accumulator and transfer it to the final storage immediately.

## 3. Resilient Error Handling (`RecoverError`)

The parser does not panic or stop at the first error. It uses a recovery system to report multiple errors in a single pass.

### A. The `RecoverError` Token

```rust
RecoverError: () = {
    <!> => errors.push(<>.error), // Captures the LALRPOP error (!) and saves it
};

```

### B. "Fail-Safe" Productions

Observe how `ValueBinding` (a `let` declaration) handles broken syntax:

```rust
<metadata: Metadata> "let" <name: Sp<Ident>> => {
    // If the user writes "let x" and forgets the "=":
    // 1. Report custom error "Expected ="
    errors.push(...);
    // 2. Return a valid but "dummy" AST node (Expr::Error) to continue
    arena.alloc(ValueBinding { ..., expr: pos::spanned(span, Expr::Error(None)) })
}

```

* **Pattern:** If the rule fails, consume the error, log it in `errors`, and return an `Error` AST node so the rest of the file can still be parsed.

## 4. Indentation and Blocks (Layout Sensitivity)

Gluon is indentation-sensitive (like Python or Haskell), but the LALRPOP grammar **does not handle whitespace**.

* **The Trick:** The external Lexer processes indentation and inserts virtual tokens:
* `"block open"`
* `"block separator"`
* `"block close"`



```rust
BlockExpr: Expr<'ast, Id> = {
    "block open" <exprs: SepVec1<SpExpr, "block separator">> "block close" => {
        // Converts the list of expressions into a "Do" chain (monadic bind)
        // ... iter.fold(...)
    },
};

```

## 5. Spans and Location (`Sp<T>`)

Almost all important nodes are wrapped in `Sp<T>` (Spanned), which stores the start and end byte.

```rust
#[inline]
Sp<Rule>: Spanned<Rule, BytePos> =
    <l: @L> <rule: Rule> <r: @R> =>
        pos::spanned2(l, r, rule);

```

* **Optimization:** Note `shrink_hidden_spans`. Sometimes the span includes comments or spaces we want to ignore in the final AST.

## 6. Mini-AI Cheatsheet: Implementation Tips

Instructions for the model based on this code:

1. **Prioritize Arenas:** If the context has an `arena`, use it for everything (`arena.alloc`). Never use `Box::new` or `Vec::new` if you can avoid it.
2. **Use `TempVecs` for Lists:** To parse `(A, B, C)`, use the `VecStart` -> `Many1Rest` -> `Drain to Arena` pattern. Do not accumulate in a local `Vec` inside the parsing function.
3. **Return `&'ast T`:** Grammar rules should return references with a lifetime, not owned values.
4. **Virtual Tokens:** For indentation-sensitive languages, assume the Lexer emits block start/end tokens; do not try to count spaces in the grammar.
5. **Error != Panic:** Define `Error` variants in your AST (`Expr::Error`, `Pattern::Error`). If parsing fails in a rule, return that variant and continue.
6. **Inline is your friend:** Use `#[inline]` on small rules (wrappers, spans) to reduce the generated parser state table size and improve performance.