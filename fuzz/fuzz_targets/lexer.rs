// fuzz_targets/lexer.rs
#![no_main]

use go125_parser::lexer::{Lexer, Tok};
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Usa lossless-ish para no reventar por UTF-8 inválido:
    let s = std::string::String::from_utf8_lossy(data);

    let mut lx = Lexer::new(&s);

    let mut max_end = 0usize;
    let mut last_real_end = 0usize;
    let mut steps = 0usize;
    let max_steps = s.len().saturating_mul(4) + 64;

    for (start, tok, end) in lx.by_ref() {
        assert!(start <= end);
        assert!(end <= s.len());

        let injected = matches!(tok, Tok::Semi) && start == end;

        if !injected {
            assert!(start >= last_real_end);
            last_real_end = end;
            assert!(end >= max_end);
        } else {
            assert!(start >= max_end);
        }

        max_end = max_end.max(end);

        steps += 1;
        assert!(steps <= max_steps);
    }
});
