use go125_parser::parse_source;

fn assert_parses(src: &str) {
    if let Err(f) = parse_source(src) {
        panic!("expected parse ok, got diagnostics: {:#?}", f.diags);
    }
}

#[test]
fn parses_imports_and_decls() {
    assert_parses(
        r#"
package main

import (
    "fmt"
    . "math"
    _ "net/http"
)

const (
    A = 1
    B int = 2
)

var (
    x = 1
    y, z int
)

type (
    T = int
    U[T any] struct { F T }
    V interface {
        M(x int) int
        ~int | ~string
    }
)

func main() {
    fmt.Println(Sqrt(4))
}
"#,
    );
}

#[test]
fn parses_statements() {
    assert_parses(
        r#"
package p

func f(x int) int {
    if x < 0 { return -x }
    for i := 0; i < 10; i++ {
        if i == 5 { break }
        continue
    }
    for range []int{1,2,3} {
    }
    switch x {
    case 0, 1:
        x++
    default:
        x = 3
    }
    select {
    case ch <- x:
        return x
    default:
        return 0
    }
}
"#,
    );
}

#[test]
fn parses_expressions() {
    assert_parses(
        r#"
package p

func f(a, b, c int, ch chan<- int) {
    _ = a + b*c - (a<<2)
    _ = a == b || a < c && b <= c
    _ = &a
    _ = <-ch
    _ = []int{1,2,3}[0]
    _ = []int{1,2,3}[1:]
    _ = []int{1,2,3}[:2]
    _ = []int{1,2,3}[0:2:3]
    _ = map[string]int{"a":1, "b":2}["a"]
    _ = f(a, b, c)
    _ = f(a, b, c...)
}
"#,
    );
}
