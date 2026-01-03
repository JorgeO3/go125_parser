# https://just.systems

default:
    echo 'Hello, world!'

fuzz-test:
    cargo +nightly fuzz run lexer --release

gen-grammar:
    @lalrpop crates/parser/src/parser.lalrpop
    # @cargo check --package parser

gen-grammar-error-clean:
    -lalrpop crates/parser/src/parser.lalrpop > log.txt 2>&1
    python crates/parser/compact_lalrpop_v4.py log.txt > crates/parser/cleaned_errores.txt