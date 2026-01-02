// crates/parser/tests/lexer_numbers_go_scanner.rs
use go125_parser::lexer::{Lexer, Tok};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Kind {
    Int,
    Float,
    Imag,
}

struct Case {
    kind: Kind,           // token kind esperado en Go (solo cuando err == "")
    src: &'static str,    // input
    tokens: &'static str, // literales esperados (separados por espacios como en Go)
    err: &'static str,    // "" => no error; != "" => debe producir diag (en tu lexer: Tok::Error)
}

fn scan_lits(input: &str) -> (Vec<String>, bool, Option<(Kind, bool /*is_error*/)>) {
    let mut lx = Lexer::new(input);

    // Guardamos spans para poder “pegar” una `i` adyacente al token previo
    // (caso: Tok::Error("0b10.0") + Ident("i") => "0b10.0i")
    let mut lits: Vec<(usize, usize, String, &'static str)> = Vec::new();
    let mut first: Option<(Kind, bool)> = None;

    for (s, t, e) in lx.by_ref() {
        // Ignorar semicolons inyectados (s == e)
        if matches!(t, Tok::Semi) && s == e {
            continue;
        }

        // Render token -> "literal" estilo Go.
        let (lit, tag) = match t {
            Tok::Ident(x) => (x.to_string(), "Ident"),
            Tok::IntLit(x) => (x.to_string(), "IntLit"),
            Tok::FloatLit(x) => (x.to_string(), "FloatLit"),
            Tok::ImagLit(x) => (x.to_string(), "ImagLit"),
            Tok::Dot => (".".to_string(), "Dot"),
            Tok::Plus => ("+".to_string(), "Plus"),
            Tok::Minus => ("-".to_string(), "Minus"),
            Tok::Error => (input[s..e].to_string(), "Error"),
            _ => (format!("{t}"), "Other"),
        };

        // Track del primer token “real”
        if first.is_none() {
            let k = match t {
                Tok::IntLit(_) => Some((Kind::Int, false)),
                Tok::FloatLit(_) => Some((Kind::Float, false)),
                Tok::ImagLit(_) => Some((Kind::Imag, false)),
                Tok::Error => Some((Kind::Int, true)), // Kind dummy; usamos is_error=true
                _ => None,
            };
            if let Some(v) = k {
                first = Some(v);
            }
        }

        // Adaptación para emular la impresión del scanner de Go en casos con error:
        // si aparece Ident("i") inmediatamente adyacente al token anterior, lo pegamos.
        if matches!(t, Tok::Ident("i")) {
            if let Some((_, prev_e, prev_lit, prev_tag)) = lits.last_mut() {
                if *prev_e == s {
                    let prev_numericish = *prev_tag == "IntLit"
                        || *prev_tag == "FloatLit"
                        || *prev_tag == "ImagLit"
                        || *prev_tag == "Error";
                    if prev_numericish {
                        prev_lit.push('i');
                        *prev_e = e;
                        continue;
                    }
                }
            }
        }

        lits.push((s, e, lit, tag));
    }

    let had_error = !lx.take_diags().is_empty();
    let out: Vec<String> = lits.into_iter().map(|(_, _, lit, _)| lit).collect();
    (out, had_error, first)
}

#[test]
fn go_scanner_testnumbers_ported() {
    // Tabla copiada de go/src/go/scanner/scanner_test.go:TestNumbers
    // Adaptación:
    // - Si err != "", en tu lexer esperamos Tok::Error (primer token) + al menos 1 diag.
    // - Si err == "", esperamos kind correcto y 0 diags.
    #[rustfmt::skip]
    let cases: &[Case] = &[
        // binaries
        Case { kind: Kind::Int,  src: "0b0",     tokens: "0b0",     err: "" },
        Case { kind: Kind::Int,  src: "0b1010",  tokens: "0b1010",  err: "" },
        Case { kind: Kind::Int,  src: "0B1110",  tokens: "0B1110",  err: "" },

        Case { kind: Kind::Int,  src: "0b",      tokens: "0b",      err: "binary literal has no digits" },
        Case { kind: Kind::Int,  src: "0b0190",  tokens: "0b0190",  err: "invalid digit '9' in binary literal" },
        Case { kind: Kind::Int,  src: "0b01a0",  tokens: "0b01 a0", err: "" }, // only accept 0-9

        Case { kind: Kind::Float, src: "0b.",    tokens: "0b.",     err: "invalid radix point in binary literal" },
        Case { kind: Kind::Float, src: "0b.1",   tokens: "0b.1",    err: "invalid radix point in binary literal" },
        Case { kind: Kind::Float, src: "0b1.0",  tokens: "0b1.0",   err: "invalid radix point in binary literal" },
        Case { kind: Kind::Float, src: "0b1e10", tokens: "0b1e10",  err: "'e' exponent requires decimal mantissa" },
        Case { kind: Kind::Float, src: "0b1P-1", tokens: "0b1P-1",  err: "'P' exponent requires hexadecimal mantissa" },

        Case { kind: Kind::Imag,  src: "0b10i",   tokens: "0b10i",   err: "" },
        Case { kind: Kind::Imag,  src: "0b10.0i", tokens: "0b10.0i", err: "invalid radix point in binary literal" },

        // octals
        Case { kind: Kind::Int,  src: "0o0",    tokens: "0o0",    err: "" },
        Case { kind: Kind::Int,  src: "0o1234", tokens: "0o1234", err: "" },
        Case { kind: Kind::Int,  src: "0O1234", tokens: "0O1234", err: "" },

        Case { kind: Kind::Int,  src: "0o",     tokens: "0o",     err: "octal literal has no digits" },
        Case { kind: Kind::Int,  src: "0o8123", tokens: "0o8123", err: "invalid digit '8' in octal literal" },
        Case { kind: Kind::Int,  src: "0o1293", tokens: "0o1293", err: "invalid digit '9' in octal literal" },
        Case { kind: Kind::Int,  src: "0o12a3", tokens: "0o12 a3", err: "" }, // only accept 0-9

        Case { kind: Kind::Float, src: "0o.",     tokens: "0o.",     err: "invalid radix point in octal literal" },
        Case { kind: Kind::Float, src: "0o.2",    tokens: "0o.2",    err: "invalid radix point in octal literal" },
        Case { kind: Kind::Float, src: "0o1.2",   tokens: "0o1.2",   err: "invalid radix point in octal literal" },
        Case { kind: Kind::Float, src: "0o1E+2",  tokens: "0o1E+2",  err: "'E' exponent requires decimal mantissa" },
        Case { kind: Kind::Float, src: "0o1p10",  tokens: "0o1p10",  err: "'p' exponent requires hexadecimal mantissa" },

        Case { kind: Kind::Imag,  src: "0o10i",   tokens: "0o10i",   err: "" },
        Case { kind: Kind::Imag,  src: "0o10e0i", tokens: "0o10e0i", err: "'e' exponent requires decimal mantissa" },

        // 0-octals
        Case { kind: Kind::Int, src: "0",    tokens: "0",    err: "" },
        Case { kind: Kind::Int, src: "0123", tokens: "0123", err: "" },

        Case { kind: Kind::Int, src: "08123",      tokens: "08123",      err: "invalid digit '8' in octal literal" },
        Case { kind: Kind::Int, src: "01293",      tokens: "01293",      err: "invalid digit '9' in octal literal" },
        Case { kind: Kind::Int, src: "0F.",        tokens: "0 F .",      err: "" }, // only accept 0-9
        Case { kind: Kind::Int, src: "0123F.",     tokens: "0123 F .",   err: "" },
        Case { kind: Kind::Int, src: "0123456x",   tokens: "0123456 x",  err: "" },

        // decimals
        Case { kind: Kind::Int, src: "1",    tokens: "1",    err: "" },
        Case { kind: Kind::Int, src: "1234", tokens: "1234", err: "" },

        Case { kind: Kind::Int, src: "1f",   tokens: "1 f",  err: "" }, // only accept 0-9

        Case { kind: Kind::Imag, src: "0i",    tokens: "0i",    err: "" },
        Case { kind: Kind::Imag, src: "0678i", tokens: "0678i", err: "" },

        // decimal floats
        Case { kind: Kind::Float, src: "0.",        tokens: "0.",        err: "" },
        Case { kind: Kind::Float, src: "123.",      tokens: "123.",      err: "" },
        Case { kind: Kind::Float, src: "0123.",     tokens: "0123.",     err: "" },

        Case { kind: Kind::Float, src: ".0",        tokens: ".0",        err: "" },
        Case { kind: Kind::Float, src: ".123",      tokens: ".123",      err: "" },
        Case { kind: Kind::Float, src: ".0123",     tokens: ".0123",     err: "" },

        Case { kind: Kind::Float, src: "0.0",       tokens: "0.0",       err: "" },
        Case { kind: Kind::Float, src: "123.123",   tokens: "123.123",   err: "" },
        Case { kind: Kind::Float, src: "0123.0123", tokens: "0123.0123", err: "" },

        Case { kind: Kind::Float, src: "0e0",       tokens: "0e0",       err: "" },
        Case { kind: Kind::Float, src: "123e+0",    tokens: "123e+0",    err: "" },
        Case { kind: Kind::Float, src: "0123E-1",   tokens: "0123E-1",   err: "" },

        Case { kind: Kind::Float, src: "0.e+1",     tokens: "0.e+1",     err: "" },
        Case { kind: Kind::Float, src: "123.E-10",  tokens: "123.E-10",  err: "" },
        Case { kind: Kind::Float, src: "0123.e123", tokens: "0123.e123", err: "" },

        Case { kind: Kind::Float, src: ".0e-1",     tokens: ".0e-1",     err: "" },
        Case { kind: Kind::Float, src: ".123E+10",  tokens: ".123E+10",  err: "" },
        Case { kind: Kind::Float, src: ".0123E123", tokens: ".0123E123", err: "" },

        Case { kind: Kind::Float, src: "0.0e1",           tokens: "0.0e1",           err: "" },
        Case { kind: Kind::Float, src: "123.123E-10",     tokens: "123.123E-10",     err: "" },
        Case { kind: Kind::Float, src: "0123.0123e+456",  tokens: "0123.0123e+456",  err: "" },

        Case { kind: Kind::Float, src: "0e",     tokens: "0e",     err: "exponent has no digits" },
        Case { kind: Kind::Float, src: "0E+",    tokens: "0E+",    err: "exponent has no digits" },
        Case { kind: Kind::Float, src: "1e+f",   tokens: "1e+ f",  err: "exponent has no digits" },
        Case { kind: Kind::Float, src: "0p0",    tokens: "0p0",    err: "'p' exponent requires hexadecimal mantissa" },
        Case { kind: Kind::Float, src: "1.0P-1", tokens: "1.0P-1", err: "'P' exponent requires hexadecimal mantissa" },

        Case { kind: Kind::Imag, src: "0.i",        tokens: "0.i",        err: "" },
        Case { kind: Kind::Imag, src: ".123i",      tokens: ".123i",      err: "" },
        Case { kind: Kind::Imag, src: "123.123i",   tokens: "123.123i",   err: "" },
        Case { kind: Kind::Imag, src: "123e+0i",    tokens: "123e+0i",    err: "" },
        Case { kind: Kind::Imag, src: "123.E-10i",  tokens: "123.E-10i",  err: "" },
        Case { kind: Kind::Imag, src: ".123E+10i",  tokens: ".123E+10i",  err: "" },

        // hexadecimals
        Case { kind: Kind::Int, src: "0x0",        tokens: "0x0",        err: "" },
        Case { kind: Kind::Int, src: "0x1234",     tokens: "0x1234",     err: "" },
        Case { kind: Kind::Int, src: "0xcafef00d", tokens: "0xcafef00d", err: "" },
        Case { kind: Kind::Int, src: "0XCAFEF00D", tokens: "0XCAFEF00D", err: "" },

        Case { kind: Kind::Int, src: "0x",   tokens: "0x",     err: "hexadecimal literal has no digits" },
        Case { kind: Kind::Int, src: "0x1g", tokens: "0x1 g",  err: "" },

        Case { kind: Kind::Imag, src: "0xf00i", tokens: "0xf00i", err: "" },

        // hexadecimal floats
        Case { kind: Kind::Float, src: "0x0p0",           tokens: "0x0p0",           err: "" },
        Case { kind: Kind::Float, src: "0x12efp-123",     tokens: "0x12efp-123",     err: "" },
        Case { kind: Kind::Float, src: "0xABCD.p+0",      tokens: "0xABCD.p+0",      err: "" },
        Case { kind: Kind::Float, src: "0x.0189P-0",      tokens: "0x.0189P-0",      err: "" },
        Case { kind: Kind::Float, src: "0x1.ffffp+1023",  tokens: "0x1.ffffp+1023",  err: "" },

        Case { kind: Kind::Float, src: "0x.",         tokens: "0x.",          err: "hexadecimal literal has no digits" },
        Case { kind: Kind::Float, src: "0x0.",        tokens: "0x0.",         err: "hexadecimal mantissa requires a 'p' exponent" },
        Case { kind: Kind::Float, src: "0x.0",        tokens: "0x.0",         err: "hexadecimal mantissa requires a 'p' exponent" },
        Case { kind: Kind::Float, src: "0x1.1",       tokens: "0x1.1",        err: "hexadecimal mantissa requires a 'p' exponent" },
        Case { kind: Kind::Float, src: "0x1.1e0",     tokens: "0x1.1e0",      err: "hexadecimal mantissa requires a 'p' exponent" },
        Case { kind: Kind::Float, src: "0x1.2gp1a",   tokens: "0x1.2 gp1a",   err: "hexadecimal mantissa requires a 'p' exponent" },
        Case { kind: Kind::Float, src: "0x0p",        tokens: "0x0p",         err: "exponent has no digits" },
        Case { kind: Kind::Float, src: "0xeP-",       tokens: "0xeP-",        err: "exponent has no digits" },
        Case { kind: Kind::Float, src: "0x1234PAB",   tokens: "0x1234P AB",   err: "exponent has no digits" },
        Case { kind: Kind::Float, src: "0x1.2p1a",    tokens: "0x1.2p1 a",    err: "" },

        Case { kind: Kind::Imag, src: "0xf00.bap+12i", tokens: "0xf00.bap+12i", err: "" },

        // separators
        Case { kind: Kind::Int,   src: "0b_1000_0001",  tokens: "0b_1000_0001",   err: "" },
        Case { kind: Kind::Int,   src: "0o_600",        tokens: "0o_600",         err: "" },
        Case { kind: Kind::Int,   src: "0_466",         tokens: "0_466",          err: "" },
        Case { kind: Kind::Int,   src: "1_000",         tokens: "1_000",          err: "" },
        Case { kind: Kind::Float, src: "1_000.000_1",   tokens: "1_000.000_1",    err: "" },
        Case { kind: Kind::Imag,  src: "10e+1_2_3i",    tokens: "10e+1_2_3i",     err: "" },
        Case { kind: Kind::Int,   src: "0x_f00d",       tokens: "0x_f00d",        err: "" },
        Case { kind: Kind::Float, src: "0x_f00d.0p1_2", tokens: "0x_f00d.0p1_2",  err: "" },

        Case { kind: Kind::Int,   src: "0b__1000",  tokens: "0b__1000",  err: "'_' must separate successive digits" },
        Case { kind: Kind::Int,   src: "0o60___0",  tokens: "0o60___0",  err: "'_' must separate successive digits" },
        Case { kind: Kind::Int,   src: "0466_",     tokens: "0466_",     err: "'_' must separate successive digits" },
        Case { kind: Kind::Float, src: "1_.",       tokens: "1_.",       err: "'_' must separate successive digits" },
        Case { kind: Kind::Float, src: "0._1",      tokens: "0._1",      err: "'_' must separate successive digits" },
        Case { kind: Kind::Float, src: "2.7_e0",    tokens: "2.7_e0",    err: "'_' must separate successive digits" },
        Case { kind: Kind::Imag,  src: "10e+12_i",  tokens: "10e+12_i",  err: "'_' must separate successive digits" },
        Case { kind: Kind::Int,   src: "0x___0",    tokens: "0x___0",    err: "'_' must separate successive digits" },
        Case { kind: Kind::Float, src: "0x1.0_p0",  tokens: "0x1.0_p0",  err: "'_' must separate successive digits" },
    ];

    for c in cases {
        let (lits, had_err, first) = scan_lits(c.src);
        let got = lits.join(" ");
        assert_eq!(got, c.tokens, "src={:?}", c.src);

        if c.err.is_empty() {
            assert!(
                !had_err,
                "src={:?}: expected no diag, but had diag(s)",
                c.src
            );
            let (k, is_error) = first.expect("expected at least one token");
            assert!(!is_error, "src={:?}: expected non-error first token", c.src);
            assert_eq!(k, c.kind, "src={:?}: wrong first token kind", c.src);
        } else {
            assert!(
                had_err,
                "src={:?}: expected diag (err={:?}) but got none",
                c.src, c.err
            );
            let (_k, is_error) = first.expect("expected at least one token");
            assert!(
                is_error,
                "src={:?}: expected Tok::Error as first token",
                c.src
            );
        }
    }
}
