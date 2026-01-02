# https://just.systems

default:
    echo 'Hello, world!'

fuzz-test:
    cargo +nightly fuzz run lexer --release