use crate::error::{Diag, LexError, LexErrorKind};
use logos::{Lexer as LogosLexer, Logos};
use std::ops::Range;

// =============================================================================
// Character Classification & Validation Utilities
// =============================================================================

/// Check if value is in inclusive range [lo, hi]
#[inline(always)]
pub const fn in_u32_inclusive(x: u32, lo: u32, hi: u32) -> bool {
    x >= lo && x <= hi
}

#[inline(always)]
pub const fn in_u8_inclusive(x: u8, lo: u8, hi: u8) -> bool {
    x >= lo && x <= hi
}

/// Convert ASCII uppercase to lowercase
#[inline(always)]
pub const fn lower_ascii(b: u8) -> u8 {
    if in_u8_inclusive(b, b'A', b'Z') {
        b + 32
    } else {
        b
    }
}

#[inline(always)]
pub const fn is_dec_digit(b: u8) -> bool {
    in_u8_inclusive(b, b'0', b'9')
}

/// Canonical hexadecimal lookup table for the entire lexer
#[rustfmt::skip]
pub const HEX_LUT: [u8; 256] = {
    let mut t = [0xFFu8; 256];
    let mut i = b'0'; while i <= b'9' { t[i as usize] = i - b'0'; i += 1; }
    let mut i = b'a'; while i <= b'f' { t[i as usize] = (i - b'a') + 10; i += 1; }
    let mut i = b'A'; while i <= b'F' { t[i as usize] = (i - b'A') + 10; i += 1; }
    t
};

#[inline(always)]
pub const fn is_hex_digit(b: u8) -> bool {
    HEX_LUT[b as usize] != 0xFF
}

#[inline(always)]
pub const fn hex_value(b: u8) -> u32 {
    HEX_LUT[b as usize] as u32
}

/// Validate Unicode scalar value (excludes surrogates)
#[inline(always)]
pub const fn is_valid_unicode_scalar(x: u32) -> bool {
    x <= 0x10_FFFF && !in_u32_inclusive(x, 0xD800, 0xDFFF)
}

/// Check if byte sequence is valid decimal digits with underscores
#[inline(always)]
const fn is_decimal_digits_with_underscores(bytes: &[u8]) -> bool {
    if bytes.is_empty() {
        return false;
    }
    let mut prev_was_digit = is_dec_digit(bytes[0]);
    if !prev_was_digit {
        return false;
    }
    let mut i = 1;
    while i < bytes.len() {
        let b = bytes[i];
        let is_digit = is_dec_digit(b);
        if b == b'_' {
            if !prev_was_digit || i + 1 >= bytes.len() || !is_dec_digit(bytes[i + 1]) {
                return false;
            }
            prev_was_digit = false;
        } else if is_digit {
            prev_was_digit = true;
        } else {
            return false;
        }
        i += 1;
    }
    prev_was_digit
}

// =============================================================================
// Logos Extras (Lexer State)
// =============================================================================

/// Extra state for Logos lexer callbacks
#[derive(Clone, Copy, Debug)]
struct LexExtras {
    block_nl_off: u32, // Offset to newline in block comment (u32::MAX = none)
    num_info: u8,      // Number classification: 0=invalid, 1=int, 2=float
}

impl Default for LexExtras {
    fn default() -> Self {
        Self {
            block_nl_off: u32::MAX,
            num_info: 0,
        }
    }
}

// =============================================================================
// Block Comment Scanner
// =============================================================================

/// Scan block comment and cache newline position for semicolon insertion
#[inline]
fn lex_block_comment(lex: &mut LogosLexer<'_, RawTok>) -> Result<(), LexErrorKind> {
    use memchr::{memchr2, memmem};

    let rem = lex.remainder().as_bytes();

    if let Some(end_pos) = memmem::find(rem, b"*/") {
        lex.extras.block_nl_off = match memchr2(b'\n', b'\r', &rem[..end_pos]) {
            Some(nl) => (2 + nl) as u32, // +2 accounts for already-matched "/*"
            None => u32::MAX,
        };
        lex.bump(end_pos + 2);
        Ok(())
    } else {
        lex.extras.block_nl_off = u32::MAX;
        lex.bump(rem.len());
        Err(LexErrorKind::UnterminatedComment)
    }
}

// =============================================================================
// Raw String Scanner
// =============================================================================

/// Single-pass raw string scanner (backtick-delimited)
#[inline]
fn lex_raw_string(lex: &mut LogosLexer<'_, RawTok>) -> Result<(), LexErrorKind> {
    use memchr::memchr;

    let rem = lex.remainder().as_bytes();

    if let Some(pos) = memchr(b'`', rem) {
        lex.bump(pos + 1);
        Ok(())
    } else {
        lex.bump(rem.len());
        Err(LexErrorKind::InvalidToken)
    }
}

// =============================================================================
// Interpreted String Escape Validation (Go Rules)
// =============================================================================

mod esc {
    use super::*;

    #[repr(u8)]
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum EscState {
        Normal = 0,
        Escape = 1,
        Hex1 = 2,
        Hex2 = 3,
        Uni1 = 4,
        Uni2 = 5,
        Uni3 = 6,
        Uni4 = 7,
        Long1 = 8,
        Long2 = 9,
        Long3 = 10,
        Long4 = 11,
        Long5 = 12,
        Long6 = 13,
        Long7 = 14,
        Long8 = 15,
        Oct2 = 16,
        Oct3 = 17,
        Error = 255,
    }

    #[repr(u8)]
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum EscCharClass {
        Other = 0,
        Backslash = 1,
        Simple = 2,
        X = 3,
        U = 4,
        BigU = 5,
        Oct = 6,
        Hex = 7,
    }

    /// Character class lookup table for escape sequences
    #[rustfmt::skip]
    const ESC_CLASS: [u8; 256] = {
        use EscCharClass as ECC;
        let mut t = [ECC::Other as u8; 256];
        t[b'\\' as usize] = ECC::Backslash as u8;

        // Simple escapes
        t[b'a' as usize] = ECC::Simple as u8;
        t[b'b' as usize] = ECC::Simple as u8;
        t[b'f' as usize] = ECC::Simple as u8;
        t[b'n' as usize] = ECC::Simple as u8;
        t[b'r' as usize] = ECC::Simple as u8;
        t[b't' as usize] = ECC::Simple as u8;
        t[b'v' as usize] = ECC::Simple as u8;
        t[b'"' as usize] = ECC::Simple as u8;
        // t[b'\'' as usize] = ECC::Simple as u8;

        // Prefixes
        t[b'x' as usize] = ECC::X as u8;
        t[b'u' as usize] = ECC::U as u8;
        t[b'U' as usize] = ECC::BigU as u8;

        // 0-7
        let mut i = b'0';
        while i <= b'7' {
            t[i as usize] = ECC::Oct as u8;
            i += 1;
        }

        // 8-9
        t[b'8' as usize] = ECC::Hex as u8;
        t[b'9' as usize] = ECC::Hex as u8;

        // hex letters excluding a/b/f (they must remain Simple for \a \b \f)
        t[b'c' as usize] = ECC::Hex as u8;
        t[b'd' as usize] = ECC::Hex as u8;
        t[b'e' as usize] = ECC::Hex as u8;

        // uppercase hex letters
        t[b'A' as usize] = ECC::Hex as u8;
        t[b'B' as usize] = ECC::Hex as u8;
        t[b'C' as usize] = ECC::Hex as u8;
        t[b'D' as usize] = ECC::Hex as u8;
        t[b'E' as usize] = ECC::Hex as u8;
        t[b'F' as usize] = ECC::Hex as u8;
        t
    };

    /// DFA transition table for escape sequence validation
    #[rustfmt::skip]
    const ESC_TRANS: [[u8; 8]; 18] = {
        use EscCharClass as ECC;
        use EscState as ES;

        let mut t = [[ES::Error as u8; 8]; 18];
        const fn set(t: &mut [[u8; 8]; 18], st: ES, cc: ECC, nx: ES) {
            t[st as usize][cc as usize] = nx as u8;
        }

        // Normal
        set(&mut t, ES::Normal, ECC::Other, ES::Normal);
        set(&mut t, ES::Normal, ECC::Simple, ES::Normal);
        set(&mut t, ES::Normal, ECC::X, ES::Normal);
        set(&mut t, ES::Normal, ECC::U, ES::Normal);
        set(&mut t, ES::Normal, ECC::BigU, ES::Normal);
        set(&mut t, ES::Normal, ECC::Oct, ES::Normal);
        set(&mut t, ES::Normal, ECC::Hex, ES::Normal);
        set(&mut t, ES::Normal, ECC::Backslash, ES::Escape);

        // Escape
        set(&mut t, ES::Escape, ECC::Simple, ES::Normal);
        set(&mut t, ES::Escape, ECC::Backslash, ES::Normal);
        set(&mut t, ES::Escape, ECC::X, ES::Hex1);
        set(&mut t, ES::Escape, ECC::U, ES::Uni1);
        set(&mut t, ES::Escape, ECC::BigU, ES::Long1);
        set(&mut t, ES::Escape, ECC::Oct, ES::Oct2);

        // \xHH
        set(&mut t, ES::Hex1, ECC::Oct, ES::Hex2);
        set(&mut t, ES::Hex1, ECC::Hex, ES::Hex2);
        set(&mut t, ES::Hex2, ECC::Oct, ES::Normal);
        set(&mut t, ES::Hex2, ECC::Hex, ES::Normal);

        // \uHHHH
        set(&mut t, ES::Uni1, ECC::Oct, ES::Uni2);
        set(&mut t, ES::Uni1, ECC::Hex, ES::Uni2);
        set(&mut t, ES::Uni2, ECC::Oct, ES::Uni3);
        set(&mut t, ES::Uni2, ECC::Hex, ES::Uni3);
        set(&mut t, ES::Uni3, ECC::Oct, ES::Uni4);
        set(&mut t, ES::Uni3, ECC::Hex, ES::Uni4);
        set(&mut t, ES::Uni4, ECC::Oct, ES::Normal);
        set(&mut t, ES::Uni4, ECC::Hex, ES::Normal);

        // \UHHHHHHHH
        set(&mut t, ES::Long1, ECC::Oct, ES::Long2);
        set(&mut t, ES::Long1, ECC::Hex, ES::Long2);
        set(&mut t, ES::Long2, ECC::Oct, ES::Long3);
        set(&mut t, ES::Long2, ECC::Hex, ES::Long3);
        set(&mut t, ES::Long3, ECC::Oct, ES::Long4);
        set(&mut t, ES::Long3, ECC::Hex, ES::Long4);
        set(&mut t, ES::Long4, ECC::Oct, ES::Long5);
        set(&mut t, ES::Long4, ECC::Hex, ES::Long5);
        set(&mut t, ES::Long5, ECC::Oct, ES::Long6);
        set(&mut t, ES::Long5, ECC::Hex, ES::Long6);
        set(&mut t, ES::Long6, ECC::Oct, ES::Long7);
        set(&mut t, ES::Long6, ECC::Hex, ES::Long7);
        set(&mut t, ES::Long7, ECC::Oct, ES::Long8);
        set(&mut t, ES::Long7, ECC::Hex, ES::Long8);
        set(&mut t, ES::Long8, ECC::Oct, ES::Normal);
        set(&mut t, ES::Long8, ECC::Hex, ES::Normal);

        // \OOO
        set(&mut t, ES::Oct2, ECC::Oct, ES::Oct3);
        set(&mut t, ES::Oct3, ECC::Oct, ES::Normal);

        t
    };

    #[inline(always)]
    const fn state_expects_hex(st: u8) -> bool {
        (st >= EscState::Hex1 as u8 && st <= EscState::Hex2 as u8)
            | (st >= EscState::Uni1 as u8 && st <= EscState::Uni4 as u8)
            | (st >= EscState::Long1 as u8 && st <= EscState::Long8 as u8)
    }

    #[inline(always)]
    const fn utf8_width(b0: u8) -> usize {
        if b0 < 0x80 {
            1
        } else if b0 < 0xE0 {
            2
        } else if b0 < 0xF0 {
            3
        } else {
            4
        }
    }

    /// Single-pass scan and validation for interpreted strings
    #[inline]
    pub fn lex_interpreted_string(
        lex: &mut LogosLexer<'_, super::RawTok>,
    ) -> Result<(), LexErrorKind> {
        use EscCharClass as ECC;
        use EscState as ES;

        let rem = lex.remainder().as_bytes();
        let mut state = ES::Normal as u8;
        let mut acc_hex = 0u32;
        let mut acc_oct = 0u32;
        let mut saw_bad_escape = false;
        let mut i = 0usize;

        while i < rem.len() {
            let byte = rem[i];

            // Check for closing quote (only valid in Normal state)
            if byte == b'"' && state == ES::Normal as u8 {
                lex.bump(i + 1);
                return if saw_bad_escape {
                    Err(LexErrorKind::InvalidEscape)
                } else {
                    Ok(())
                };
            }

            // Newlines terminate the string (Go behavior)
            if byte == b'\n' || byte == b'\r' {
                lex.bump(i);
                return Err(LexErrorKind::InvalidToken);
            }

            // Handle UTF-8 multi-byte sequences
            if byte >= 0x80 {
                if state != ES::Normal as u8 {
                    saw_bad_escape = true;
                    state = ES::Normal as u8;
                }
                i += utf8_width(byte);
                continue;
            }

            // ASCII path: run DFA
            let mut cc = ESC_CLASS[byte as usize];

            // In hex states, treat any hex digit as Hex class
            if state_expects_hex(state) && is_hex_digit(byte) {
                cc = ECC::Hex as u8;
            }

            let next = ESC_TRANS[state as usize][cc as usize];

            if next == ES::Error as u8 {
                saw_bad_escape = true;
                state = ES::Normal as u8;
                acc_hex = 0;
                acc_oct = 0;
                i += 1;
                continue;
            }

            // Accumulate hex values
            let is_hex_class = (cc == ECC::Hex as u8) | (cc == ECC::Oct as u8);
            let in_hex = state_expects_hex(state);
            if in_hex & is_hex_class {
                let hv = hex_value(byte);
                let is_first = (state == ES::Hex1 as u8)
                    | (state == ES::Uni1 as u8)
                    | (state == ES::Long1 as u8);
                acc_hex = ((acc_hex * (!is_first as u32)) << 4) | hv;
            }

            // Accumulate octal values
            let is_oct_class = cc == ECC::Oct as u8;
            let in_oct =
                (state == ES::Escape as u8) | (state == ES::Oct2 as u8) | (state == ES::Oct3 as u8);
            if in_oct & is_oct_class {
                let ov = (byte - b'0') as u32;
                let is_first_oct = state == ES::Escape as u8;
                acc_oct = acc_oct * (((!is_first_oct) as u32) * 8) + ov;
            }

            // Validate completed escape sequences
            let completing_u4 = (state == ES::Uni4 as u8) & is_hex_class;
            let completing_u8 = (state == ES::Long8 as u8) & is_hex_class;
            let completing_o3 = (state == ES::Oct3 as u8) & is_oct_class;

            if completing_u4 | completing_u8 {
                if !is_valid_unicode_scalar(acc_hex) {
                    saw_bad_escape = true;
                }
                acc_hex = 0;
            }

            if completing_o3 {
                if acc_oct > 255 {
                    saw_bad_escape = true;
                }
                acc_oct = 0;
            }

            if (state == ES::Hex2 as u8) & is_hex_class {
                acc_hex = 0;
            }

            state = next;
            i += 1;
        }

        // EOF before closing quote
        lex.bump(rem.len());
        Err(LexErrorKind::InvalidToken)
    }
}

// =============================================================================
// Rune Literal Validation (UTF-8 + Escapes)
// =============================================================================

mod rune {
    use super::*;

    /// UTF-8 sequence length lookup table
    #[rustfmt::skip]
    const UTF8_LEN: [u8; 256] = {
        let mut t = [0u8; 256];
        let mut i = 0usize;
        while i < 256 {
            let b = i as u8;
            if b < 0x80 { t[i] = 1; }
            else if b >= 0xC2 && b <= 0xDF { t[i] = 2; }
            else if b >= 0xE0 && b <= 0xEF { t[i] = 3; }
            else if b >= 0xF0 && b <= 0xF4 { t[i] = 4; }
            i += 1;
        }
        t
    };

    const UTF8_FIRST_MASK: [u8; 5] = [0, 0x7F, 0x1F, 0x0F, 0x07];

    /// Minimum valid second byte for UTF-8 sequences
    #[rustfmt::skip]
    const UTF8_B1_MIN: [u8; 256] = {
        let mut t = [0u8; 256];
        let mut i = 0usize;
        while i < 256 {
            let b0 = i as u8;
            let len = UTF8_LEN[i];
            t[i] = if len < 2 { 0 }
                   else if b0 == 0xE0 { 0xA0 }
                   else if b0 == 0xF0 { 0x90 }
                   else { 0x80 };
            i += 1;
        }
        t
    };

    /// Maximum valid second byte for UTF-8 sequences
    #[rustfmt::skip]
    const UTF8_B1_MAX: [u8; 256] = {
        let mut t = [0u8; 256];
        let mut i = 0usize;
        while i < 256 {
            let b0 = i as u8;
            let len = UTF8_LEN[i];
            t[i] = if len < 2 { 0 }
                   else if b0 == 0xED { 0x9F }
                   else if b0 == 0xF4 { 0x8F }
                   else { 0xBF };
            i += 1;
        }
        t
    };

    #[inline(always)]
    const fn is_utf8_cont(b: u8) -> bool {
        (b & 0xC0) == 0x80
    }

    /// Decode one UTF-8 codepoint from byte slice
    #[inline]
    const fn decode_utf8_one(data: &[u8], start: usize) -> Result<(u32, usize), LexErrorKind> {
        if start >= data.len() {
            return Err(LexErrorKind::InvalidToken);
        }

        let first_byte = data[start];
        let len = UTF8_LEN[first_byte as usize] as usize;

        if len == 0 || start + len > data.len() {
            return Err(LexErrorKind::InvalidToken);
        }

        if len == 1 {
            return match first_byte {
                b'\n' | b'\r' => Err(LexErrorKind::InvalidToken),
                _ => Ok((first_byte as u32, 1)),
            };
        }

        let second_byte = data[start + 1];
        if !in_u8_inclusive(
            second_byte,
            UTF8_B1_MIN[first_byte as usize],
            UTF8_B1_MAX[first_byte as usize],
        ) || !is_utf8_cont(second_byte)
        {
            return Err(LexErrorKind::InvalidToken);
        }

        let mut i = 2;
        while i < len {
            if !is_utf8_cont(data[start + i]) {
                return Err(LexErrorKind::InvalidToken);
            }
            i += 1;
        }

        let mask = UTF8_FIRST_MASK[len];
        let mut codepoint = (first_byte & mask) as u32;
        i = 1;
        while i < len {
            codepoint = (codepoint << 6) | ((data[start + i] & 0x3F) as u32);
            i += 1;
        }

        match is_valid_unicode_scalar(codepoint) {
            true => Ok((codepoint, len)),
            false => Err(LexErrorKind::InvalidToken),
        }
    }

    #[repr(u8)]
    enum RuneEscType {
        Invalid = 0,
        Simple = 1,
        Hex = 2,
        Unicode4 = 3,
        Unicode8 = 4,
        Octal = 5,
    }

    /// Escape type classification table
    #[rustfmt::skip]
    const RUNE_ESC_TYPE: [u8; 256] = {
        let mut t = [RuneEscType::Invalid as u8; 256];

        // Simple escapes
        t[b'a' as usize] = RuneEscType::Simple as u8;
        t[b'b' as usize] = RuneEscType::Simple as u8;
        t[b'f' as usize] = RuneEscType::Simple as u8;
        t[b'n' as usize] = RuneEscType::Simple as u8;
        t[b'r' as usize] = RuneEscType::Simple as u8;
        t[b't' as usize] = RuneEscType::Simple as u8;
        t[b'v' as usize] = RuneEscType::Simple as u8;
        t[b'\\' as usize] = RuneEscType::Simple as u8;
        // t[b'"' as usize] = RuneEscType::Simple as u8;
        t[b'\'' as usize] = RuneEscType::Simple as u8;
        t[b'x' as usize] = RuneEscType::Hex as u8;
        t[b'u' as usize] = RuneEscType::Unicode4 as u8;
        t[b'U' as usize] = RuneEscType::Unicode8 as u8;

        // Octal starts
        let mut i = b'0';
        while i <= b'7' {
            t[i as usize] = RuneEscType::Octal as u8;
            i += 1;
        }

        t
    };

    const RUNE_ESC_CONSUME: [u8; 6] = [0, 1, 3, 5, 9, 3];

    #[inline(always)]
    const fn is_octal_digit(b: u8) -> bool {
        b >= b'0' && b <= b'7'
    }

    /// Validate escape sequence in rune literal
    #[inline]
    fn validate_escape(body: &[u8], start: usize) -> Result<usize, LexErrorKind> {
        let esc_ch = *body.get(start).ok_or(LexErrorKind::InvalidEscape)?;
        let ty = RUNE_ESC_TYPE[esc_ch as usize];

        if ty == RuneEscType::Invalid as u8 {
            return Err(LexErrorKind::InvalidEscape);
        }

        let consume = RUNE_ESC_CONSUME[ty as usize] as usize;
        if start + consume > body.len() {
            return Err(LexErrorKind::InvalidEscape);
        }

        let is_hex = ty == RuneEscType::Hex as u8;
        let is_u4 = ty == RuneEscType::Unicode4 as u8;
        let is_u8 = ty == RuneEscType::Unicode8 as u8;
        let is_oct = ty == RuneEscType::Octal as u8;

        if is_hex {
            let h1 = body[start + 1];
            let h2 = body[start + 2];
            if !is_hex_digit(h1) || !is_hex_digit(h2) {
                return Err(LexErrorKind::InvalidEscape);
            }
        }

        if is_u4 || is_u8 {
            let hex_count = (4 * (is_u4 as usize)) + (8 * (is_u8 as usize));
            let mut v = 0u32;
            let mut i = 0usize;
            while i < hex_count {
                let b = body[start + 1 + i];
                if !is_hex_digit(b) {
                    return Err(LexErrorKind::InvalidEscape);
                }
                v = (v << 4) | hex_value(b);
                i += 1;
            }
            if !is_valid_unicode_scalar(v) {
                return Err(LexErrorKind::InvalidEscape);
            }
        }

        if is_oct {
            let o1 = body[start];
            let o2 = body[start + 1];
            let o3 = body[start + 2];
            if !is_octal_digit(o1) || !is_octal_digit(o2) || !is_octal_digit(o3) {
                return Err(LexErrorKind::InvalidEscape);
            }
            let val = ((o1 - b'0') as u32) * 64 + ((o2 - b'0') as u32) * 8 + ((o3 - b'0') as u32);
            if val > 255 {
                return Err(LexErrorKind::InvalidEscape);
            }
        }

        Ok(consume)
    }

    /// Single-pass scan and validation for rune literals
    #[inline]
    pub fn lex_rune_lit(lex: &mut LogosLexer<'_, super::RawTok>) -> Result<(), LexErrorKind> {
        let rem = lex.remainder().as_bytes();

        if rem.is_empty() || rem[0] == b'\n' || rem[0] == b'\r' {
            return Err(LexErrorKind::InvalidToken);
        }

        if rem[0] == b'\\' {
            // Escape sequence
            if rem.len() < 2 {
                lex.bump(rem.len());
                return Err(LexErrorKind::InvalidEscape);
            }

            let consumed = 1 + validate_escape(rem, 1)?;

            if rem.get(consumed) == Some(&b'\'') {
                lex.bump(consumed + 1);
                Ok(())
            } else {
                lex.bump(rem.len());
                Err(LexErrorKind::InvalidToken)
            }
        } else {
            // Single UTF-8 codepoint
            let (_cp, n) = decode_utf8_one(rem, 0)?;

            if rem.get(n) == Some(&b'\'') {
                lex.bump(n + 1);
                Ok(())
            } else {
                lex.bump(rem.len());
                Err(LexErrorKind::InvalidToken)
            }
        }
    }
}

// =============================================================================
// Number Scanning & Validation (Go-style DFA)
// =============================================================================

mod num {
    use super::*;

    #[repr(u8)]
    #[derive(Clone, Copy)]
    enum NumCharClass {
        Other = 0,
        Zero = 1,
        One = 2,
        Oct = 3,
        Dec = 4,
        Dot = 5,
        Us = 6,
        E = 7,
        P = 8,
        X = 9,
        O = 10,
        B = 11,
        Hex = 12,
        Sign = 13,
    }

    const NCLASS: usize = 14;

    #[repr(u8)]
    #[derive(Clone, Copy, PartialEq)]
    enum NumState {
        Err = 0,
        Start = 1,

        Zero = 2,
        DecInt = 3,
        DecIntUs = 4,

        LegacyOct = 5,
        LegacyOctUs = 6,

        BadLead = 7,
        BadLeadUs = 8,

        PreHex = 9,
        HexInt = 10,
        HexIntUs = 11,
        HexDotNoDig = 12,
        HexDotHaveDig = 13,
        HexFrac = 14,
        HexFracUs = 15,
        HexExpStart = 16,
        HexExpSign = 17,
        HexExp = 18,
        HexExpUs = 19,

        PreOct = 20,
        OctInt = 21,
        OctIntUs = 22,

        PreBin = 23,
        BinInt = 24,
        BinIntUs = 25,

        DotStart = 26,
        DecDot = 27,
        DecFrac = 28,
        DecFracUs = 29,
        DecExpStart = 30,
        DecExpSign = 31,
        DecExp = 32,
        DecExpUs = 33,
    }

    const NSTATE: usize = 34;

    /// Character class lookup for number tokenization
    static NUM_CLASS: [u8; 256] = {
        use NumCharClass as NCC;

        let mut t = [NCC::Other as u8; 256];
        t[b'0' as usize] = NCC::Zero as u8; 
        t[b'1' as usize] = NCC::One as u8;

        let mut i = b'2';
        while i <= b'7' {
            t[i as usize] = NCC::Oct as u8;
            i += 1;
        }

        t[b'8' as usize] = NCC::Dec as u8;
        t[b'9' as usize] = NCC::Dec as u8;

        t[b'.' as usize] = NCC::Dot as u8;
        t[b'_' as usize] = NCC::Us as u8;

        t[b'a' as usize] = NCC::Hex as u8;
        t[b'A' as usize] = NCC::Hex as u8;
        t[b'c' as usize] = NCC::Hex as u8;
        t[b'C' as usize] = NCC::Hex as u8;
        t[b'd' as usize] = NCC::Hex as u8;
        t[b'D' as usize] = NCC::Hex as u8;
        t[b'f' as usize] = NCC::Hex as u8;
        t[b'F' as usize] = NCC::Hex as u8;

        t[b'e' as usize] = NCC::E as u8;
        t[b'E' as usize] = NCC::E as u8;

        t[b'p' as usize] = NCC::P as u8;
        t[b'P' as usize] = NCC::P as u8;

        t[b'x' as usize] = NCC::X as u8;
        t[b'X' as usize] = NCC::X as u8;

        t[b'o' as usize] = NCC::O as u8;
        t[b'O' as usize] = NCC::O as u8;

        t[b'b' as usize] = NCC::B as u8;
        t[b'B' as usize] = NCC::B as u8;

        t[b'+' as usize] = NCC::Sign as u8;
        t[b'-' as usize] = NCC::Sign as u8;

        t
    };

    /// DFA transition table for number validation
    static NUM_TRANS: [[u8; NCLASS]; NSTATE] = {
        let mut t = [[NumState::Err as u8; NCLASS]; NSTATE];
        macro_rules! tr {
            ($st:expr, [$($c:expr),+ $(,)?] => $to:expr) => {
                $( t[$st as u8 as usize][$c as u8 as usize] = $to as u8; )+
            };
        }
        use NumCharClass as NCC;
        use NumState as NS;
        // Start state
        tr!(NS::Start, [NCC::Zero] => NS::Zero); 
        tr!(NS::Start, [NCC::One, NCC::Oct, NCC::Dec] => NS::DecInt);
        tr!(NS::Start, [NCC::Dot] => NS::DotStart);
        
        // Zero state (prefix detection)
        tr!(NS::Zero, [NCC::X] => NS::PreHex); 
        tr!(NS::Zero, [NCC::O] => NS::PreOct); 
        tr!(NS::Zero, [NCC::B] => NS::PreBin);
        tr!(NS::Zero, [NCC::Dot] => NS::DecDot); 
        tr!(NS::Zero, [NCC::E] => NS::DecExpStart);
        tr!(NS::Zero, [NCC::Zero, NCC::One, NCC::Oct] => NS::LegacyOct); 
        tr!(NS::Zero, [NCC::Us] => NS::LegacyOctUs);
        tr!(NS::Zero, [NCC::Dec] => NS::BadLead);
        
        // Decimal integer
        tr!(NS::DecInt, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::DecInt); 
        tr!(NS::DecInt, [NCC::Us] => NS::DecIntUs);
        tr!(NS::DecInt, [NCC::Dot] => NS::DecDot); 
        tr!(NS::DecInt, [NCC::E] => NS::DecExpStart);
        tr!(NS::DecIntUs, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::DecInt);
        
        // Legacy octal
        tr!(NS::LegacyOct, [NCC::Zero, NCC::One, NCC::Oct] => NS::LegacyOct); 
        tr!(NS::LegacyOct, [NCC::Us] => NS::LegacyOctUs);
        tr!(NS::LegacyOct, [NCC::Dec] => NS::BadLead); 
        tr!(NS::LegacyOct, [NCC::Dot] => NS::DecDot);
        tr!(NS::LegacyOct, [NCC::E] => NS::DecExpStart); 
        tr!(NS::LegacyOctUs, [NCC::Zero, NCC::One, NCC::Oct] => NS::LegacyOct);
        tr!(NS::LegacyOctUs, [NCC::Dec] => NS::BadLead);

        // Bad leading digit
        tr!(NS::BadLead, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::BadLead); 
        tr!(NS::BadLead, [NCC::Us] => NS::BadLeadUs);
        tr!(NS::BadLead, [NCC::Dot] => NS::DecDot); 
        tr!(NS::BadLead, [NCC::E] => NS::DecExpStart);
        tr!(NS::BadLeadUs, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::BadLead);
        
        // Decimal float
        tr!(NS::DotStart, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::DecFrac);
        tr!(NS::DecDot, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::DecFrac); 
        tr!(NS::DecDot, [NCC::E] => NS::DecExpStart);
        tr!(NS::DecFrac, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::DecFrac); 
        tr!(NS::DecFrac, [NCC::Us] => NS::DecFracUs);
        tr!(NS::DecFrac, [NCC::E] => NS::DecExpStart); 
        tr!(NS::DecFracUs, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::DecFrac);
        
        // Decimal exponent
        tr!(NS::DecExpStart, [NCC::Sign] => NS::DecExpSign); 
        tr!(NS::DecExpStart, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::DecExp);
        tr!(NS::DecExpSign, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::DecExp);
        tr!(NS::DecExp, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::DecExp); 
        tr!(NS::DecExp, [NCC::Us] => NS::DecExpUs);
        tr!(NS::DecExpUs, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::DecExp);
        
        // Hexadecimal
        tr!(NS::PreHex, [NCC::Us] => NS::HexIntUs); 
        tr!(NS::PreHex, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec, NCC::Hex, NCC::B, NCC::E] => NS::HexInt);
        tr!(NS::PreHex, [NCC::Dot] => NS::HexDotNoDig);
        tr!(NS::HexInt, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec, NCC::Hex, NCC::B, NCC::E] => NS::HexInt); 
        tr!(NS::HexInt, [NCC::Us] => NS::HexIntUs);
        tr!(NS::HexInt, [NCC::Dot] => NS::HexDotHaveDig); 
        tr!(NS::HexInt, [NCC::P] => NS::HexExpStart);
        tr!(NS::HexIntUs, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec, NCC::Hex, NCC::B, NCC::E] => NS::HexInt);
        tr!(NS::HexDotNoDig, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec, NCC::Hex, NCC::B, NCC::E] => NS::HexFrac);
        tr!(NS::HexDotHaveDig, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec, NCC::Hex, NCC::B, NCC::E] => NS::HexFrac);
        tr!(NS::HexDotHaveDig, [NCC::P] => NS::HexExpStart);
        tr!(NS::HexFrac, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec, NCC::Hex, NCC::B, NCC::E] => NS::HexFrac); 
        tr!(NS::HexFrac, [NCC::Us] => NS::HexFracUs);
        tr!(NS::HexFrac, [NCC::P] => NS::HexExpStart); 
        tr!(NS::HexFracUs, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec, NCC::Hex, NCC::B, NCC::E] => NS::HexFrac);

        // Hex exponent
        tr!(NS::HexExpStart, [NCC::Sign] => NS::HexExpSign); 
        tr!(NS::HexExpStart, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::HexExp);
        tr!(NS::HexExpSign, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::HexExp);
        tr!(NS::HexExp, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::HexExp); 
        tr!(NS::HexExp, [NCC::Us] => NS::HexExpUs);
        tr!(NS::HexExpUs, [NCC::Zero, NCC::One, NCC::Oct, NCC::Dec] => NS::HexExp);

        // Octal (0o prefix)
        tr!(NS::PreOct, [NCC::Us] => NS::OctIntUs); 
        tr!(NS::PreOct, [NCC::Zero, NCC::One, NCC::Oct] => NS::OctInt);
        tr!(NS::OctInt, [NCC::Zero, NCC::One, NCC::Oct] => NS::OctInt); 
        tr!(NS::OctInt, [NCC::Us] => NS::OctIntUs);
        tr!(NS::OctIntUs, [NCC::Zero, NCC::One, NCC::Oct] => NS::OctInt);
        
        // Binary
        tr!(NS::PreBin, [NCC::Us] => NS::BinIntUs); tr!(NS::PreBin, [NCC::Zero, NCC::One] => NS::BinInt);
        tr!(NS::BinInt, [NCC::Zero, NCC::One] => NS::BinInt); tr!(NS::BinInt, [NCC::Us] => NS::BinIntUs);
        tr!(NS::BinIntUs, [NCC::Zero, NCC::One] => NS::BinInt);
        t
    };

    /// State properties: bit0=accept, bit1=is_float
    static STATE_PROPS: [u8; NSTATE] = {
        let mut t = [0u8; NSTATE];

        t[NumState::Zero as usize] = 1;
        t[NumState::DecInt as usize] = 1;
        t[NumState::LegacyOct as usize] = 1;
        t[NumState::HexInt as usize] = 1;
        t[NumState::OctInt as usize] = 1;
        t[NumState::BinInt as usize] = 1;

        t[NumState::DecDot as usize] = 3;
        t[NumState::DecFrac as usize] = 3;
        t[NumState::DecExp as usize] = 3;

        // hex float: accept only once exponent digits exist
        t[NumState::HexExp as usize] = 3;

        t
    };

    #[inline(always)]
    const fn lower_ascii_branchless(b: u8) -> u8 {
        b | 0x20
    }

    /// Consume consecutive digits (decimal or hex based on base parameter)
    #[inline]
    const fn consume_digits(src: &[u8], mut i: usize, n: usize, base: u8) -> usize {
        if base == 16 {
            while i < n {
                let b = src[i];
                let ok = is_hex_digit(b) | (b == b'_');
                if !ok {
                    break;
                }
                i += 1;
            }
        } else {
            while i < n {
                let b = src[i];
                let ok = is_dec_digit(b) | (b == b'_');
                if !ok {
                    break;
                }
                i += 1;
            }
        }
        i
    }

    /// Maximal munch number scanner with integrated DFA classification
    /// Stores classification result in `lex.extras.num_info`: 0=invalid, 1=int, 2=float
    #[inline]
    pub fn lex_number(lex: &mut LogosLexer<'_, super::RawTok>) -> Result<(), LexErrorKind> {
        lex.extras.num_info = 0;

        let src = lex.source().as_bytes();
        let start = lex.span().start;
        let n = src.len();

        if start >= n {
            return Ok(());
        }

        let mut i = start;
        let mut base: u8 = 10;
        let mut state = NumState::Start as u8;
        let mut invalid = false;

        #[inline(always)]
        const fn step(state: &mut u8, invalid: &mut bool, b: u8) {
            if *invalid {
                return;
            }
            let cc = NUM_CLASS[b as usize] as usize;
            let nx = NUM_TRANS[*state as usize][cc];
            *state = nx;
            if nx == NumState::Err as u8 {
                *invalid = true;
            }
        }

        // Handle integer part or leading dot
        if src[i] != b'.' {
            let first = src[i];
            i += 1;
            step(&mut state, &mut invalid, first);

            if first == b'0' {
                base = 8;
                if i < n {
                    let nl = lower_ascii_branchless(src[i]);
                    let is_x = (nl == b'x') as u8;
                    let is_o = (nl == b'o') as u8;
                    let is_b = (nl == b'b') as u8;
                    let has_prefix = is_x | is_o | is_b;
                    base = (8u8 + (8u8 * is_x)).wrapping_sub(6u8 * is_b);
                    if has_prefix != 0 {
                        let p = src[i];
                        i += 1;
                        step(&mut state, &mut invalid, p);
                    }
                }
            }

            let mut j = i;
            i = consume_digits(src, i, n, base);
            while j < i {
                step(&mut state, &mut invalid, src[j]);
                j += 1;
            }
        } else {
            let dot = src[i];
            i += 1;
            step(&mut state, &mut invalid, dot);
            base = 10;

            let mut j = i;
            i = consume_digits(src, i, n, base);
            while j < i {
                step(&mut state, &mut invalid, src[j]);
                j += 1;
            }
        }

        // Handle fractional part
        if i < n && src[i] == b'.' {
            let is_range = (i + 1 < n && src[i + 1] == b'.') as u8;
            if is_range == 0 {
                let dot = src[i];
                i += 1;
                step(&mut state, &mut invalid, dot);

                let mut j = i;
                i = consume_digits(src, i, n, base);
                while j < i {
                    step(&mut state, &mut invalid, src[j]);
                    j += 1;
                }
            }
        }

        // Handle exponent
        if i < n {
            let e = lower_ascii_branchless(src[i]);
            let is_exp = ((e == b'e') | (e == b'p')) as u8;
            if is_exp != 0 {
                let eb = src[i];
                i += 1;
                step(&mut state, &mut invalid, eb);

                if i < n {
                    let s = src[i];
                    let is_sign = ((s == b'+') | (s == b'-')) as u8;
                    if is_sign != 0 {
                        i += 1;
                        step(&mut state, &mut invalid, s);
                    }
                }

                let mut j = i;
                i = consume_digits(src, i, n, 10);
                while j < i {
                    step(&mut state, &mut invalid, src[j]);
                    j += 1;
                }
            }
        }

        // Extend token span to match consumed bytes
        let already = lex.span().end;
        if i > already {
            lex.bump(i - already);
        }

        // Classify based on final state
        if !invalid {
            let props = STATE_PROPS[state as usize];
            if (props & 1) != 0 {
                lex.extras.num_info = if (props & 2) != 0 { 2 } else { 1 };
            }
        }

        Ok(())
    }
}

// =============================================================================
// Raw Token Definition (Logos Integration)
// =============================================================================

#[repr(u8)]
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
#[logos(error = LexErrorKind)]
#[logos(extras = LexExtras)]
#[logos(skip r"[ \t]+")]
#[rustfmt::skip]
enum RawTok {
    #[token("\u{FEFF}")] Bom,
    // Trivia
    #[regex(r"\r\n|\n|\r")] Newline,
    #[regex(r"//[^\n\r]*", logos::skip, allow_greedy = true)] _LineComment,
    #[token("/*", lex_block_comment)] BlockComment,
    
    // Keywords
    #[token("break")] KwBreak, 
    #[token("case")] KwCase, 
    #[token("chan")] KwChan,
    #[token("const")] KwConst, 
    #[token("continue")] KwContinue, 
    #[token("default")] KwDefault,
    #[token("defer")] KwDefer, 
    #[token("else")] KwElse, 
    #[token("fallthrough")] KwFallthrough,
    #[token("for")] KwFor, 
    #[token("func")] KwFunc, 
    #[token("go")] KwGo,
    #[token("goto")] KwGoto, 
    #[token("if")] KwIf, 
    #[token("import")] KwImport,
    #[token("interface")] KwInterface, 
    #[token("map")] KwMap, 
    #[token("package")] KwPackage,
    #[token("range")] KwRange, 
    #[token("return")] KwReturn, 
    #[token("select")] KwSelect,
    #[token("struct")] KwStruct, 
    #[token("switch")] KwSwitch, 
    #[token("type")] KwType,
    #[token("var")] KwVar,
    
    // Identifiers & Literals
    #[regex(r"[_\p{L}][_\p{L}\p{Nd}]*")] Ident,
    #[regex(r"[0-9]|\.[0-9]", num::lex_number)] Number,
    #[token("`", lex_raw_string)] RawString,
    #[token("\"", esc::lex_interpreted_string)] String,
    #[token("'", rune::lex_rune_lit)] Rune,
    
    // Operators
    #[token("...")] Ellipsis,
    #[token("<<=")] ShlAssign, 
    #[token(">>=")] ShrAssign, 
    #[token("&^=")] AndNotAssign,
    #[token("+=")] AddAssign, 
    #[token("-=")] SubAssign, 
    #[token("*=")] MulAssign,
    #[token("/=")] DivAssign, 
    #[token("%=")] ModAssign, 
    #[token("&=")] AndAssign,
    #[token("|=")] OrAssign, 
    #[token("^=")] XorAssign,
    #[token("<<")] Shl, 
    #[token(">>")] Shr, 
    #[token("&^")] AndNot,
    #[token("&&")] LAnd, 
    #[token("||")] LOr,
    #[token("==")] EqEq, 
    #[token("!=")] NotEq, 
    #[token("<=")] Le, 
    #[token(">=")] Ge,
    #[token("++")] Inc, 
    #[token("--")] Dec, 
    #[token(":=")] Define, 
    #[token("<-")] Arrow,
    #[token("=")] Assign, 
    #[token("+")] Plus, 
    #[token("-")] Minus, 
    #[token("*")] Star,
    #[token("/")] Slash, 
    #[token("%")] Percent, 
    #[token("&")] Amp, 
    #[token("|")] Pipe,
    #[token("^")] Caret, 
    #[token("~")] Tilde, 
    #[token("!")] Bang,
    #[token("<")] Lt, 
    #[token(">")] Gt,
    
    // Delimiters
    #[token("(")] LParen, 
    #[token(")")] RParen, 
    #[token("[")] LBrack, 
    #[token("]")] RBrack,
    #[token("{")] LBrace, 
    #[token("}")] RBrace,
    #[token(",")] Comma, 
    #[token(";")] Semi, 
    #[token(":")] Colon, 
    #[token(".")] Dot,
    
    // Catch-all
    #[regex(r".", priority = 0)] Error,
}

// =============================================================================
// Token Classification Lookup Tables
// =============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokKind {
    Literal,
    Simple,
    Trivia,
}

macro_rules! gen_lookup_table {
    (bool, $size:expr, $($variant:ident),* $(,)?) => {{
        let mut table = [false; $size];
        $(table[RawTok::$variant as usize] = true;)*
        table
    }};
    ($enum:ty, $size:expr, $default:expr, $($variant:ident => $value:expr),* $(,)?) => {{
        let mut table = [$default; $size];
        $(table[RawTok::$variant as usize] = $value;)*
        table
    }};
}

const RAWTOK_COUNT: usize = RawTok::Error as usize + 1;

/// Tokens that trigger automatic semicolon insertion (Go spec rule)
#[rustfmt::skip]
const SEMI_INSERT_TABLE: [bool; RAWTOK_COUNT] = gen_lookup_table!(
    bool, 
    RAWTOK_COUNT,
    Ident, 
    Number, 
    Rune, 
    String, 
    RawString,
    KwBreak, 
    KwContinue, 
    KwFallthrough, 
    KwReturn,
    Inc, 
    Dec, 
    RParen, 
    RBrack, 
    RBrace,
);

const TOKEN_KIND_TABLE: [TokKind; RAWTOK_COUNT] = gen_lookup_table!(
    TokKind, RAWTOK_COUNT, 
    TokKind::Simple,
    Newline => TokKind::Trivia, 
    _LineComment => TokKind::Trivia, 
    BlockComment => TokKind::Trivia,
    Ident => TokKind::Literal, 
    Number => TokKind::Literal, 
    Rune => TokKind::Literal,
    String => TokKind::Literal, 
    RawString => TokKind::Literal,
);

impl RawTok {
    #[inline(always)]
    const fn can_insert_semicolon(self) -> bool {
        SEMI_INSERT_TABLE[self as usize]
    }

    #[inline(always)]
    const fn kind(self) -> TokKind {
        TOKEN_KIND_TABLE[self as usize]
    }

    /// Convert raw token to public token representation
    #[inline]
    #[rustfmt::skip]
    const fn to_token<'src>(self, slice: &'src str) -> Tok<'src> {
        if matches!(self.kind(), TokKind::Literal) {
            return match self {
                Self::Ident => Tok::Ident(slice),
                Self::Rune => Tok::RuneLit(slice),
                Self::String => Tok::StringLit(slice),
                Self::RawString => Tok::RawStringLit(slice),
                Self::Number => unreachable!(),
                _ => unreachable!(),
            };
        }

        macro_rules! simple_tok {
            ($($raw:ident => $tok:ident),* $(,)?) => {
                match self {
                    $(Self::$raw => Tok::$tok,)*
                    _ => unreachable!(),
                }
            };
        }
        
        simple_tok! {
            KwBreak => KwBreak, KwCase => KwCase, KwChan => KwChan, KwConst => KwConst,
            KwContinue => KwContinue, KwDefault => KwDefault, KwDefer => KwDefer, KwElse => KwElse,
            KwFallthrough => KwFallthrough, KwFor => KwFor, KwFunc => KwFunc, KwGo => KwGo,
            KwGoto => KwGoto, KwIf => KwIf, KwImport => KwImport, KwInterface => KwInterface,
            KwMap => KwMap, KwPackage => KwPackage, KwRange => KwRange, KwReturn => KwReturn,
            KwSelect => KwSelect, KwStruct => KwStruct, KwSwitch => KwSwitch, KwType => KwType,
            KwVar => KwVar, Ellipsis => Ellipsis, ShlAssign => ShlAssign, ShrAssign => ShrAssign,
            AndNotAssign => AndNotAssign, AddAssign => AddAssign, SubAssign => SubAssign,
            MulAssign => MulAssign, DivAssign => DivAssign, ModAssign => ModAssign,
            AndAssign => AndAssign, OrAssign => OrAssign, XorAssign => XorAssign,
            Shl => Shl, Shr => Shr, AndNot => AndNot, LAnd => LAnd, LOr => LOr,
            EqEq => EqEq, NotEq => NotEq, Le => Le, Ge => Ge, Inc => Inc, Dec => Dec,
            Define => Define, Arrow => Arrow, Assign => Assign, Plus => Plus, Minus => Minus,
            Star => Star, Slash => Slash, Percent => Percent, Amp => Amp, Pipe => Pipe,
            Caret => Caret, Tilde => Tilde, Bang => Bang, Lt => Lt, Gt => Gt,
            LParen => LParen, RParen => RParen, LBrack => LBrack, RBrack => RBrack,
            LBrace => LBrace, RBrace => RBrace, Comma => Comma, Semi => Semi,
            Colon => Colon, Dot => Dot, Error => Error, Bom => Error,
        }
    }
}

// =============================================================================
// Public Token Definition (Zero-Copy)
// =============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Tok<'input> {
    Ident(&'input str), IntLit(&'input str), FloatLit(&'input str), ImagLit(&'input str),
    RuneLit(&'input str), StringLit(&'input str), RawStringLit(&'input str),
    KwBreak, KwCase, KwChan, KwConst, KwContinue, KwDefault, KwDefer, KwElse,
    KwFallthrough, KwFor, KwFunc, KwGo, KwGoto, KwIf, KwImport, KwInterface,
    KwMap, KwPackage, KwRange, KwReturn, KwSelect, KwStruct, KwSwitch, KwType, KwVar,
    Ellipsis, ShlAssign, ShrAssign, AndNotAssign, AddAssign, SubAssign, MulAssign,
    DivAssign, ModAssign, AndAssign, OrAssign, XorAssign, Shl, Shr, AndNot,
    LAnd, LOr, EqEq, NotEq, Le, Ge, Inc, Dec, Define, Arrow, Assign, Plus, Minus,
    Star, Slash, Percent, Amp, Pipe, Caret, Tilde, Bang, Lt, Gt,
    LParen, RParen, LBrack, RBrack, LBrace, RBrace, Comma, Semi, Colon, Dot, Error,
}

impl<'input> std::fmt::Display for Tok<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

// =============================================================================
// Lexer Wrapper (Semicolon Insertion + Diagnostics)
// =============================================================================

/// High-level lexer with automatic semicolon insertion and diagnostic collection
pub struct Lexer<'src> {
    logos: LogosLexer<'src, RawTok>,
    pending_semi_at: usize, // Position of pending injected semicolon (usize::MAX = none)
    diags: Vec<Diag>,
    last_can_insert_semi: bool,
    src_len: usize,
    eof_done: bool,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            logos: RawTok::lexer(input),
            pending_semi_at: usize::MAX,
            diags: Vec::with_capacity(16),
            last_can_insert_semi: false,
            src_len: input.len(),
            eof_done: false,
        }
    }

    pub fn take_diags(&mut self) -> Vec<Diag> {
        std::mem::take(&mut self.diags)
    }

    #[inline]
    fn push_lex_diag(&mut self, kind: LexErrorKind, span: Range<usize>) {
        let sp = crate::error::Span::from_range(span);
        self.diags.push(LexError { kind, span: sp }.diag());
    }

    #[inline]
    fn emit_semi_at(&mut self, pos: usize) {
        self.pending_semi_at = pos;
    }

    /// Handle trivia tokens and trigger semicolon insertion if needed
    #[inline]
    fn handle_trivia(&mut self, raw: RawTok, start: usize) -> bool {
        match raw {
            RawTok::Newline => {
                if self.last_can_insert_semi {
                    self.last_can_insert_semi = false;
                    self.emit_semi_at(start);
                }
                true
            }
            RawTok::BlockComment => {
                if self.last_can_insert_semi {
                    let off = self.logos.extras.block_nl_off;
                    if off != u32::MAX {
                        self.last_can_insert_semi = false;
                        self.emit_semi_at(start + off as usize);
                    }
                }
                true
            }
            _ => false,
        }
    }

    /// Handle EOF and inject final semicolon if needed
    #[inline]
    fn handle_eof(&mut self) {
        self.eof_done = true;
        if self.last_can_insert_semi {
            self.last_can_insert_semi = false;
            self.emit_semi_at(self.src_len);
        }
    }

    /// Handle lexer errors and emit diagnostics
    #[inline]
    fn handle_lex_error(&mut self, kind: LexErrorKind) -> (usize, Tok<'src>, usize) {
        let span = self.logos.span();
        let start = span.start;
        let end = span.end;
        self.push_lex_diag(kind, start..end);
        self.last_can_insert_semi = false;
        (start, Tok::Error, end)
    }

    /// Process raw token and convert to public token
    #[inline]
    fn handle_raw_token(&mut self, raw: RawTok) -> Option<(usize, Tok<'src>, usize)> {
        let span = self.logos.span();
        let start = span.start;
        let end = span.end;
        let slice = self.logos.slice();

        // BOM is only valid at file start
        if raw == RawTok::Bom {
            if start == 0 {
                return None;
            }
            self.push_lex_diag(LexErrorKind::InvalidToken, start..end);
            self.last_can_insert_semi = false;
            return Some((start, Tok::Error, end));
        }

        // Trivia handling
        if self.handle_trivia(raw, start) {
            return None;
        }

        // Generic error token
        if raw == RawTok::Error {
            self.push_lex_diag(LexErrorKind::InvalidToken, start..end);
            self.last_can_insert_semi = false;
            return Some((start, Tok::Error, end));
        }

        // Number token (requires special handling for imaginary suffix)
        if raw == RawTok::Number {
            return Some(self.handle_number_token(start, end, slice));
        }

        // Regular token
        self.last_can_insert_semi = raw.can_insert_semicolon();
        let tok = raw.to_token(slice);
        Some((start, tok, end))
    }

    /// Handle number tokens with imaginary suffix lookahead
    #[inline]
    fn handle_number_token(
        &mut self,
        start: usize,
        end: usize,
        slice: &'src str,
    ) -> (usize, Tok<'src>, usize) {
        let src = self.logos.source();
        let bytes = slice.as_bytes();
        let has_i_suffix = end < self.src_len && src.as_bytes()[end] == b'i';
        let info = self.logos.extras.num_info; // 0=invalid, 1=int, 2=float

        if has_i_suffix {
            let end_with_i = end + 1;
            // Imaginary literal is valid if number part is valid OR is decimal_digits
            let is_valid = info != 0 || is_decimal_digits_with_underscores(bytes);
            self.logos.bump(1);
            self.last_can_insert_semi = is_valid;

            if !is_valid {
                self.push_lex_diag(LexErrorKind::InvalidNumber, start..end_with_i);
            }

            return (
                start,
                if is_valid {
                    Tok::ImagLit(&src[start..end_with_i])
                } else {
                    Tok::Error
                },
                end_with_i,
            );
        }

        match info {
            1 => {
                self.last_can_insert_semi = true;
                (start, Tok::IntLit(slice), end)
            }
            2 => {
                self.last_can_insert_semi = true;
                (start, Tok::FloatLit(slice), end)
            }
            _ => {
                self.push_lex_diag(LexErrorKind::InvalidNumber, start..end);
                self.last_can_insert_semi = false;
                (start, Tok::Error, end)
            }
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = (usize, Tok<'src>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // 1. Emit pending injected semicolon
            if self.pending_semi_at != usize::MAX {
                let pos = self.pending_semi_at;
                self.pending_semi_at = usize::MAX;
                return Some((pos, Tok::Semi, pos));
            }

            // 2. Check for hard EOF
            if self.eof_done {
                return None;
            }

            // 3. Get next raw token from Logos
            match self.logos.next() {
                None => {
                    self.handle_eof();
                    continue;
                }
                Some(Err(kind)) => return Some(self.handle_lex_error(kind)),
                Some(Ok(raw)) => {
                    if let Some(item) = self.handle_raw_token(raw) {
                        return Some(item);
                    }
                    continue;
                }
            }
        }
    }
}
