use go125_parser::lexer::{Lexer, Tok};
use proptest::prelude::*;

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        .. ProptestConfig::default()
    })]
    #[test]
    fn never_panics_and_progresses(s in ".*") {
        let lx = Lexer::new(&s);

        // Max progress we have seen in the stream (end positions).
        let mut max_end = 0usize;

        // End position of the last *real* (non-injected) token.
        let mut last_real_end = 0usize;

        let max_steps = s.len().saturating_mul(4) + 64;

        for (steps, (start, tok, end)) in lx.enumerate() {
            // 1) spans must be in-bounds
            prop_assert!(start <= end, "start>end: ({start},{end}) tok={tok:?} input={s:?}");
            prop_assert!(end <= s.len(), "end out of bounds: ({start},{end}) len={} tok={tok:?} input={s:?}", s.len());

            let injected_semi = matches!(tok, Tok::Semi) && start == end;

            // 2) Real tokens must be monotonic (cannot overlap backwards)
            if !injected_semi {
                prop_assert!(
                    start >= last_real_end,
                    "real token moved backwards: start={start} < last_real_end={last_real_end} tok={tok:?} span=({start},{end}) input={s:?}"
                );
                last_real_end = end;
                // Real tokens shouldn't regress the global end.
                prop_assert!(
                    end >= max_end,
                    "real token end regressed: end={end} < max_end={max_end} tok={tok:?} input={s:?}"
                );
            } else {
                // 3) Injected semis must not appear before the already-consumed frontier.
                prop_assert!(
                    start >= max_end,
                    "injected semi before progress: pos={start} < max_end={max_end} input={s:?}"
                );
            }

            // 4) Update global progress
            max_end = max_end.max(end);

            // 5) Anti-hang guard
            prop_assert!(
                steps <= max_steps,
                "too many steps (possible hang): steps={steps} max_steps={max_steps} len={} input={s:?}",
                s.len()
            );
        }
    }
}
