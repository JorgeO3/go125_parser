use crate::error::{Diag, LexError, LexErrorKind};
use logos::{Lexer as LogosLexer, Logos};
use std::ops::Range;

// =============================================================================
// 0. Helpers (hot path)
// =============================================================================

#[inline(always)]
const fn first_newline_offset(s: &str) -> Option<usize> {
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'\n' || bytes[i] == b'\r' {
            return Some(i);
        }
        i += 1;
    }
    None
}

#[inline(always)]
pub const fn in_u32_inclusive(x: u32, lo: u32, hi: u32) -> bool {
    x >= lo && x <= hi
}

#[inline(always)]
pub const fn in_u8_inclusive(x: u8, lo: u8, hi: u8) -> bool {
    x >= lo && x <= hi
}

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

#[inline(always)]
pub const fn is_hex_digit(b: u8) -> bool {
    let c = lower_ascii(b);
    is_dec_digit(c) || in_u8_inclusive(c, b'a', b'f')
}

#[inline(always)]
pub const fn hex_val(b: u8) -> Option<u32> {
    let c = lower_ascii(b);
    if in_u8_inclusive(c, b'0', b'9') {
        Some((c - b'0') as u32)
    } else if in_u8_inclusive(c, b'a', b'f') {
        Some((c - b'a' + 10) as u32)
    } else {
        None
    }
}

#[inline(always)]
pub const fn is_valid_unicode_scalar(x: u32) -> bool {
    // x <= 0x10FFFF && not surrogate range [0xD800, 0xDFFF]
    x <= 0x10FFFF && !in_u32_inclusive(x, 0xD800, 0xDFFF)
}

// =============================================================================
// 1. Block comment scanner (manual)
// =============================================================================

#[inline]
fn lex_block_comment(lex: &mut LogosLexer<'_, RawTok>) -> Result<(), LexErrorKind> {
    let rem = lex.remainder().as_bytes();

    match rem.windows(2).position(|w| w == b"*/") {
        Some(pos) => {
            lex.bump(pos + 2);
            Ok(())
        }
        None => {
            lex.bump(rem.len());
            Err(LexErrorKind::InvalidToken)
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EscCharClass {
    Other = 0,
    Backslash = 1,
    Simple = 2, // a b f n r t v " '
    X = 3,
    U = 4,
    BigU = 5,
    Oct = 6, // 0-7
    Hex = 7, // 8-9 c d e A-F (a/b/f remapped in hex-states)
}

static CHAR_CLASS_TABLE: [u8; 256] = {
    let mut t = [EscCharClass::Other as u8; 256];

    t[b'\\' as usize] = EscCharClass::Backslash as u8;

    // Simple escapes
    t[b'a' as usize] = EscCharClass::Simple as u8;
    t[b'b' as usize] = EscCharClass::Simple as u8;
    t[b'f' as usize] = EscCharClass::Simple as u8;
    t[b'n' as usize] = EscCharClass::Simple as u8;
    t[b'r' as usize] = EscCharClass::Simple as u8;
    t[b't' as usize] = EscCharClass::Simple as u8;
    t[b'v' as usize] = EscCharClass::Simple as u8;
    t[b'"' as usize] = EscCharClass::Simple as u8;
    t[b'\'' as usize] = EscCharClass::Simple as u8;

    // Escape prefixes
    t[b'x' as usize] = EscCharClass::X as u8;
    t[b'u' as usize] = EscCharClass::U as u8;
    t[b'U' as usize] = EscCharClass::BigU as u8;

    // 0-7: Octal (also valid as hex digits)
    let mut i = b'0';
    while i <= b'7' {
        t[i as usize] = EscCharClass::Oct as u8;
        i += 1;
    }

    // 8-9: Hex
    t[b'8' as usize] = EscCharClass::Hex as u8;
    t[b'9' as usize] = EscCharClass::Hex as u8;

    // Hex letters:
    // IMPORTANT: do NOT mark a/b/f as Hex (they must remain Simple for \a \b \f).
    // We'll remap to Hex only in hex-expecting states.
    t[b'c' as usize] = EscCharClass::Hex as u8;
    t[b'd' as usize] = EscCharClass::Hex as u8;
    t[b'e' as usize] = EscCharClass::Hex as u8;

    // Uppercase hex letters (safe to mark as Hex; \A etc are invalid anyway)
    t[b'A' as usize] = EscCharClass::Hex as u8;
    t[b'B' as usize] = EscCharClass::Hex as u8;
    t[b'C' as usize] = EscCharClass::Hex as u8;
    t[b'D' as usize] = EscCharClass::Hex as u8;
    t[b'E' as usize] = EscCharClass::Hex as u8;
    t[b'F' as usize] = EscCharClass::Hex as u8;

    t
};

const HEX_VAL: [u8; 256] = {
    let mut t = [0xFFu8; 256];

    let mut i = b'0';
    while i <= b'9' {
        t[i as usize] = i - b'0';
        i += 1;
    }

    let mut i = b'a';
    while i <= b'f' {
        t[i as usize] = (i - b'a') + 10;
        i += 1;
    }

    let mut i = b'A';
    while i <= b'F' {
        t[i as usize] = (i - b'A') + 10;
        i += 1;
    }

    t
};

#[rustfmt::skip]
const STATE_TRANSITIONS: [[u8; 8]; 18] = {
    let mut t = [[EscState::Error as u8; 8]; 18];

    const fn set(t: &mut [[u8; 8]; 18], state: EscState, class: EscCharClass, next: EscState) {
        t[state as usize][class as usize] = next as u8;
    }

    // State::Normal
    set(&mut t, EscState::Normal, EscCharClass::Other, EscState::Normal);
    set(&mut t, EscState::Normal, EscCharClass::Simple, EscState::Normal);
    set(&mut t, EscState::Normal, EscCharClass::X, EscState::Normal);
    set(&mut t, EscState::Normal, EscCharClass::U, EscState::Normal);
    set(&mut t, EscState::Normal, EscCharClass::BigU, EscState::Normal);
    set(&mut t, EscState::Normal, EscCharClass::Oct, EscState::Normal);
    set(&mut t, EscState::Normal, EscCharClass::Hex, EscState::Normal);
    set(&mut t, EscState::Normal, EscCharClass::Backslash, EscState::Escape);

    // EscState::Escape
    set(&mut t, EscState::Escape, EscCharClass::Simple, EscState::Normal);
    set(&mut t, EscState::Escape, EscCharClass::Backslash, EscState::Normal);
    set(&mut t, EscState::Escape, EscCharClass::X, EscState::Hex1);
    set(&mut t, EscState::Escape, EscCharClass::U, EscState::Uni1);
    set(&mut t, EscState::Escape, EscCharClass::BigU, EscState::Long1);
    set(&mut t, EscState::Escape, EscCharClass::Oct, EscState::Oct2);

    // \xHH
    set(&mut t, EscState::Hex1, EscCharClass::Oct, EscState::Hex2);
    set(&mut t, EscState::Hex1, EscCharClass::Hex, EscState::Hex2);
    set(&mut t, EscState::Hex2, EscCharClass::Oct, EscState::Normal);
    set(&mut t, EscState::Hex2, EscCharClass::Hex, EscState::Normal);

    // \uHHHH
    set(&mut t, EscState::Uni1, EscCharClass::Oct, EscState::Uni2);
    set(&mut t, EscState::Uni1, EscCharClass::Hex, EscState::Uni2);
    set(&mut t, EscState::Uni2, EscCharClass::Oct, EscState::Uni3);
    set(&mut t, EscState::Uni2, EscCharClass::Hex, EscState::Uni3);
    set(&mut t, EscState::Uni3, EscCharClass::Oct, EscState::Uni4);
    set(&mut t, EscState::Uni3, EscCharClass::Hex, EscState::Uni4);
    set(&mut t, EscState::Uni4, EscCharClass::Oct, EscState::Normal);
    set(&mut t, EscState::Uni4, EscCharClass::Hex, EscState::Normal);

    // \UHHHHHHHH
    set(&mut t, EscState::Long1, EscCharClass::Oct, EscState::Long2);
    set(&mut t, EscState::Long1, EscCharClass::Hex, EscState::Long2);
    set(&mut t, EscState::Long2, EscCharClass::Oct, EscState::Long3);
    set(&mut t, EscState::Long2, EscCharClass::Hex, EscState::Long3);
    set(&mut t, EscState::Long3, EscCharClass::Oct, EscState::Long4);
    set(&mut t, EscState::Long3, EscCharClass::Hex, EscState::Long4);
    set(&mut t, EscState::Long4, EscCharClass::Oct, EscState::Long5);
    set(&mut t, EscState::Long4, EscCharClass::Hex, EscState::Long5);
    set(&mut t, EscState::Long5, EscCharClass::Oct, EscState::Long6);
    set(&mut t, EscState::Long5, EscCharClass::Hex, EscState::Long6);
    set(&mut t, EscState::Long6, EscCharClass::Oct, EscState::Long7);
    set(&mut t, EscState::Long6, EscCharClass::Hex, EscState::Long7);
    set(&mut t, EscState::Long7, EscCharClass::Oct, EscState::Long8);
    set(&mut t, EscState::Long7, EscCharClass::Hex, EscState::Long8);
    set(&mut t, EscState::Long8, EscCharClass::Oct, EscState::Normal);
    set(&mut t, EscState::Long8, EscCharClass::Hex, EscState::Normal);

    // \OOO
    set(&mut t, EscState::Oct2, EscCharClass::Oct, EscState::Oct3);
    set(&mut t, EscState::Oct3, EscCharClass::Oct, EscState::Normal);

    t
};

#[inline(always)]
const fn hex_value(byte: u8) -> u32 {
    HEX_VAL[byte as usize] as u32
}

#[inline(always)]
const fn state_expects_hex(state: u8) -> bool {
    (state >= EscState::Hex1 as u8 && state <= EscState::Hex2 as u8)
        | (state >= EscState::Uni1 as u8 && state <= EscState::Uni4 as u8)
        | (state >= EscState::Long1 as u8 && state <= EscState::Long8 as u8)
}

// =============================================================================
// 2. Escape + rune validation (spec-correct structural rules)
// =============================================================================
#[inline]
pub fn validate_escapes_interpreted_body(body: &[u8]) -> Result<(), LexErrorKind> {
    let mut state = EscState::Normal as u8;
    let mut acc_hex = 0u32;
    let mut acc_oct = 0u32;

    for &byte in body {
        let mut char_class = CHAR_CLASS_TABLE[byte as usize];

        // In hex states, treat ANY hex digit as CharClass::Hex (including a/b/f which are Simple in Escape state).
        if state_expects_hex(state) && is_hex_digit(byte) {
            char_class = EscCharClass::Hex as u8;
        }

        let next_state = STATE_TRANSITIONS[state as usize][char_class as usize];
        if next_state == EscState::Error as u8 {
            return Err(LexErrorKind::InvalidEscape);
        }

        let is_hex_class =
            (char_class == EscCharClass::Hex as u8) | (char_class == EscCharClass::Oct as u8);
        let is_oct_class = char_class == EscCharClass::Oct as u8;

        // Hex accumulator for \x, \u, \U
        let in_hex_state = state_expects_hex(state);
        if in_hex_state & is_hex_class {
            let hv = hex_value(byte);
            let is_first = (state == EscState::Hex1 as u8)
                | (state == EscState::Uni1 as u8)
                | (state == EscState::Long1 as u8);
            acc_hex = (acc_hex * (!is_first as u32)) << 4 | hv;
        }

        // Oct accumulator for \OOO (exactly 3 digits)
        let in_oct_state = (state == EscState::Escape as u8)
            | (state == EscState::Oct2 as u8)
            | (state == EscState::Oct3 as u8);
        if in_oct_state & is_oct_class {
            let ov = (byte - b'0') as u32; // safe because Oct class only for '0'..'7'
            let is_first_oct = state == EscState::Escape as u8;
            acc_oct = acc_oct * ((!is_first_oct as u32) * 8) + ov;
        }

        // Validate when sequences complete (on the completing digit)
        let completing_uni4 = (state == EscState::Uni4 as u8) & is_hex_class;
        let completing_long8 = (state == EscState::Long8 as u8) & is_hex_class;
        let completing_oct3 = (state == EscState::Oct3 as u8) & is_oct_class;

        if completing_uni4 | completing_long8 {
            if !is_valid_unicode_scalar(acc_hex) {
                return Err(LexErrorKind::InvalidEscape);
            }
            acc_hex = 0;
        }

        if completing_oct3 {
            if acc_oct > 255 {
                return Err(LexErrorKind::InvalidEscape);
            }
            acc_oct = 0;
        }

        // Reset after \x completes
        let completing_hex2 = (state == EscState::Hex2 as u8) & is_hex_class;
        if completing_hex2 {
            acc_hex = 0;
        }

        state = next_state;
    }

    if state != EscState::Normal as u8 {
        return Err(LexErrorKind::InvalidEscape);
    }

    Ok(())
}

#[inline]
fn validate_interpreted_string(lex: &mut LogosLexer<'_, RawTok>) -> Result<(), LexErrorKind> {
    let s = lex.slice().as_bytes();
    if s.len() < 2 || s[0] != b'"' || *s.last().unwrap() != b'"' {
        return Err(LexErrorKind::InvalidToken);
    }
    validate_escapes_interpreted_body(&s[1..s.len() - 1])
}

// =============================================================================
// UTF-8 decoding lookup tables (spec-correct)
// =============================================================================

// 0 = invalid lead, 1..4 = length
const UTF8_LEN: [u8; 256] = {
    let mut t = [0u8; 256];
    let mut i = 0usize;
    while i < 256 {
        let b = i as u8;
        if b < 0x80 {
            t[i] = 1;
        } else if b >= 0xC2 && b <= 0xDF {
            t[i] = 2;
        } else if b >= 0xE0 && b <= 0xEF {
            t[i] = 3;
        } else if b >= 0xF0 && b <= 0xF4 {
            t[i] = 4;
        } else {
            t[i] = 0;
        }
        i += 1;
    }
    t
};

const UTF8_FIRST_MASK: [u8; 5] = [0, 0x7F, 0x1F, 0x0F, 0x07];

// Constrain b1 to prevent overlong / surrogates / >0x10FFFF.
// Values are meaningful only when UTF8_LEN[b0] >= 2.
const UTF8_B1_MIN: [u8; 256] = {
    let mut t = [0u8; 256];
    let mut i = 0usize;
    while i < 256 {
        let b0 = i as u8;
        let len = UTF8_LEN[i];

        let min = if len < 2 {
            0
        } else if b0 == 0xE0 {
            0xA0
        } else if b0 == 0xF0 {
            0x90
        } else {
            0x80
        };

        t[i] = min;
        i += 1;
    }
    t
};

const UTF8_B1_MAX: [u8; 256] = {
    let mut t = [0u8; 256];
    let mut i = 0usize;
    while i < 256 {
        let b0 = i as u8;
        let len = UTF8_LEN[i];

        let max = if len < 2 {
            0
        } else if b0 == 0xED {
            0x9F
        } else if b0 == 0xF4 {
            0x8F
        } else {
            0xBF
        };

        t[i] = max;
        i += 1;
    }
    t
};

#[inline(always)]
const fn is_utf8_cont(b: u8) -> bool {
    (b & 0xC0) == 0x80
}

#[inline(always)]
const fn in_u8(b: u8, lo: u8, hi: u8) -> bool {
    b >= lo && b <= hi
}

#[inline(always)]
const fn is_valid_unicode_scalar(x: u32) -> bool {
    x <= 0x10_FFFF && !(x >= 0xD800 && x <= 0xDFFF)
}

// Decode exactly ONE UTF-8 scalar starting at `start`.
// Safe indexing only.
#[inline]
fn decode_utf8_one(data: &[u8], start: usize) -> Result<(u32, usize), &'static str> {
    let b0 = *data.get(start).ok_or(ERR_INVALID_UTF8)?;
    let len = UTF8_LEN[b0 as usize] as usize;
    if len == 0 {
        return Err(ERR_INVALID_UTF8);
    }

    let end = start + len;
    if end > data.len() {
        return Err(ERR_INVALID_UTF8);
    }

    if len == 1 {
        if b0 == b'\n' || b0 == b'\r' {
            return Err(ERR_INVALID_TOKEN);
        }
        return Ok((b0 as u32, 1));
    }

    let b1 = *data.get(start + 1).ok_or(ERR_INVALID_UTF8)?;
    let b1_min = UTF8_B1_MIN[b0 as usize];
    let b1_max = UTF8_B1_MAX[b0 as usize];
    if !in_u8(b1, b1_min, b1_max) || !is_utf8_cont(b1) {
        return Err(ERR_INVALID_UTF8);
    }

    if len >= 3 {
        let b2 = *data.get(start + 2).ok_or(ERR_INVALID_UTF8)?;
        if !is_utf8_cont(b2) {
            return Err(ERR_INVALID_UTF8);
        }
        if len == 4 {
            let b3 = *data.get(start + 3).ok_or(ERR_INVALID_UTF8)?;
            if !is_utf8_cont(b3) {
                return Err(ERR_INVALID_UTF8);
            }
        }
    }

    let mask = UTF8_FIRST_MASK[len];
    let mut cp = (b0 & mask) as u32;

    let mut i = 1usize;
    while i < len {
        let bx = data[start + i];
        cp = (cp << 6) | ((bx & 0x3F) as u32);
        i += 1;
    }

    if !is_valid_unicode_scalar(cp) {
        return Err(ERR_INVALID_SCALAR);
    }

    Ok((cp, len))
}

// =============================================================================
// Escape lookup tables
// =============================================================================

#[repr(u8)]
enum EscapeType {
    Invalid = 0,
    Simple = 1,   // \n, \t, etc
    Hex = 2,      // \xHH
    Unicode4 = 3, // \uHHHH
    Unicode8 = 4, // \UHHHHHHHH
    Octal = 5,    // \OOO
}

const ESCAPE_TYPE: [u8; 256] = {
    let mut t = [EscapeType::Invalid as u8; 256];

    // Simple escapes
    t[b'a' as usize] = EscapeType::Simple as u8;
    t[b'b' as usize] = EscapeType::Simple as u8;
    t[b'f' as usize] = EscapeType::Simple as u8;
    t[b'n' as usize] = EscapeType::Simple as u8;
    t[b'r' as usize] = EscapeType::Simple as u8;
    t[b't' as usize] = EscapeType::Simple as u8;
    t[b'v' as usize] = EscapeType::Simple as u8;
    t[b'\\' as usize] = EscapeType::Simple as u8;
    t[b'"' as usize] = EscapeType::Simple as u8;
    t[b'\'' as usize] = EscapeType::Simple as u8;

    // Prefixes
    t[b'x' as usize] = EscapeType::Hex as u8;
    t[b'u' as usize] = EscapeType::Unicode4 as u8;
    t[b'U' as usize] = EscapeType::Unicode8 as u8;

    // Octal starts
    let mut i = b'0';
    while i <= b'7' {
        t[i as usize] = EscapeType::Octal as u8;
        i += 1;
    }

    t
};

// bytes to consume starting at the byte after '\' (including that byte)
const ESC_CONSUME: [u8; 6] = [0, 1, 3, 5, 9, 3];

// Small hex value table (0xFF = invalid)
const HEX_VAL: [u8; 256] = {
    let mut t = [0xFFu8; 256];
    let mut i = b'0';
    while i <= b'9' {
        t[i as usize] = i - b'0';
        i += 1;
    }
    let mut i = b'a';
    while i <= b'f' {
        t[i as usize] = (i - b'a') + 10;
        i += 1;
    }
    let mut i = b'A';
    while i <= b'F' {
        t[i as usize] = (i - b'A') + 10;
        i += 1;
    }
    t
};

#[inline(always)]
const fn is_hex_digit(b: u8) -> bool {
    HEX_VAL[b as usize] != 0xFF
}

#[inline(always)]
const fn hex_value(b: u8) -> u32 {
    HEX_VAL[b as usize] as u32
}

#[inline(always)]
const fn is_octal_digit(b: u8) -> bool {
    b >= b'0' && b <= b'7'
}

// start points to the byte AFTER '\'
#[inline]
fn validate_escape(body: &[u8], start: usize) -> Result<usize, &'static str> {
    let esc_ch = *body.get(start).ok_or(ERR_INVALID_ESCAPE)?;
    let esc_ty = ESCAPE_TYPE[esc_ch as usize];
    if esc_ty == EscapeType::Invalid as u8 {
        return Err(ERR_INVALID_ESCAPE);
    }

    let consume = ESC_CONSUME[esc_ty as usize] as usize;
    if start + consume > body.len() {
        return Err(ERR_INVALID_ESCAPE);
    }

    let is_hex = esc_ty == EscapeType::Hex as u8;
    let is_u4 = esc_ty == EscapeType::Unicode4 as u8;
    let is_u8 = esc_ty == EscapeType::Unicode8 as u8;
    let is_oct = esc_ty == EscapeType::Octal as u8;

    if is_hex {
        let h1 = body.get(start + 1).copied().ok_or(ERR_INVALID_ESCAPE)?;
        let h2 = body.get(start + 2).copied().ok_or(ERR_INVALID_ESCAPE)?;
        if !is_hex_digit(h1) || !is_hex_digit(h2) {
            return Err(ERR_INVALID_ESCAPE);
        }
    }

    if is_u4 || is_u8 {
        let hex_count = (4 * (is_u4 as usize)) + (8 * (is_u8 as usize));
        let mut v = 0u32;
        let mut i = 0usize;
        while i < hex_count {
            let b = body.get(start + 1 + i).copied().ok_or(ERR_INVALID_ESCAPE)?;
            if !is_hex_digit(b) {
                return Err(ERR_INVALID_ESCAPE);
            }
            v = (v << 4) | hex_value(b);
            i += 1;
        }
        if !is_valid_unicode_scalar(v) {
            return Err(ERR_INVALID_ESCAPE);
        }
    }

    if is_oct {
        let o1 = body.get(start).copied().ok_or(ERR_INVALID_ESCAPE)?;
        let o2 = body.get(start + 1).copied().ok_or(ERR_INVALID_ESCAPE)?;
        let o3 = body.get(start + 2).copied().ok_or(ERR_INVALID_ESCAPE)?;

        if !is_octal_digit(o1) || !is_octal_digit(o2) || !is_octal_digit(o3) {
            return Err(ERR_INVALID_ESCAPE);
        }

        let val = ((o1 - b'0') as u32) * 64 + ((o2 - b'0') as u32) * 8 + ((o3 - b'0') as u32);
        if val > 255 {
            return Err(ERR_INVALID_ESCAPE);
        }
    }

    Ok(consume)
}

// =============================================================================
// Main rune literal validation
// =============================================================================

#[inline]
pub fn validate_rune_lit(slice: &[u8]) -> Result<(), &'static str> {
    if slice.len() < 3 {
        return Err(ERR_INVALID_TOKEN);
    }
    if slice.first().copied() != Some(b'\'') || slice.last().copied() != Some(b'\'') {
        return Err(ERR_INVALID_TOKEN);
    }

    let body = &slice[1..slice.len() - 1];
    if body.is_empty() {
        return Err(ERR_INVALID_TOKEN);
    }

    let consumed = if body[0] == b'\\' {
        1 + validate_escape(body, 1)?
    } else {
        let (_cp, n) = decode_utf8_one(body, 0)?;
        n
    };

    if consumed != body.len() {
        return Err(ERR_INVALID_TOKEN);
    }

    Ok(())
}

// #[inline]
// fn validate_rune_lit(lex: &mut LogosLexer<'_, RawTok>) -> Result<(), LexErrorKind> {
//     let s = lex.slice().as_bytes();
//     if s.len() < 3 || s[0] != b'\'' || *s.last().unwrap() != b'\'' {
//         return Err(LexErrorKind::InvalidToken);
//     }
//     let body = &s[1..s.len() - 1];

//     // Parse exactly ONE rune (either one UTF-8 scalar, or one escape producing one scalar)
//     let mut i = 0usize;
//     let mut rune_count = 0usize;

//     while i < body.len() {
//         rune_count += 1;
//         if rune_count > 1 {
//             return Err(LexErrorKind::InvalidToken);
//         }

//         if body[i] == b'\\' {
//             // validate escape structurally and consume exactly one escape unit producing one rune/byte
//             // reuse string escape logic by validating only this escape; implement inline for speed
//             i += 1;
//             if i >= body.len() {
//                 return Err(LexErrorKind::InvalidEscape);
//             }
//             match body[i] {
//                 b'a' | b'b' | b'f' | b'n' | b'r' | b't' | b'v' | b'\\' | b'"' | b'\'' => i += 1,
//                 b'x' => {
//                     i += 1;
//                     if i + 1 >= body.len() {
//                         return Err(LexErrorKind::InvalidEscape);
//                     }
//                     if hex_val(body[i]).is_none() || hex_val(body[i + 1]).is_none() {
//                         return Err(LexErrorKind::InvalidEscape);
//                     }
//                     i += 2;
//                 }
//                 b'u' => {
//                     i += 1;
//                     if i + 3 >= body.len() {
//                         return Err(LexErrorKind::InvalidEscape);
//                     }
//                     let mut v: u32 = 0;
//                     for _ in 0..4 {
//                         let d = hex_val(body[i]).ok_or(LexErrorKind::InvalidEscape)?;
//                         v = (v << 4) | d;
//                         i += 1;
//                     }
//                     if !is_valid_unicode_scalar(v) {
//                         return Err(LexErrorKind::InvalidEscape);
//                     }
//                 }
//                 b'U' => {
//                     i += 1;
//                     if i + 7 >= body.len() {
//                         return Err(LexErrorKind::InvalidEscape);
//                     }
//                     let mut v: u32 = 0;
//                     for _ in 0..8 {
//                         let d = hex_val(body[i]).ok_or(LexErrorKind::InvalidEscape)?;
//                         v = (v << 4) | d;
//                         i += 1;
//                     }
//                     if !is_valid_unicode_scalar(v) {
//                         return Err(LexErrorKind::InvalidEscape);
//                     }
//                 }
//                 b'0'..=b'7' => {
//                     if i + 2 >= body.len() {
//                         return Err(LexErrorKind::InvalidEscape);
//                     }
//                     let d1 = body[i];
//                     let d2 = body[i + 1];
//                     let d3 = body[i + 2];
//                     if !(b'0'..=b'7').contains(&d2) || !(b'0'..=b'7').contains(&d3) {
//                         return Err(LexErrorKind::InvalidEscape);
//                     }
//                     let val =
//                         ((d1 - b'0') as u32) * 64 + ((d2 - b'0') as u32) * 8 + ((d3 - b'0') as u32);
//                     if val > 255 {
//                         return Err(LexErrorKind::InvalidEscape);
//                     }
//                     i += 3;
//                 }
//                 _ => return Err(LexErrorKind::InvalidEscape),
//             }
//         } else {
//             // decode one UTF-8 scalar from body[i..]
//             let b0 = body[i];
//             let (cp, w) = if b0 < 0x80 {
//                 (b0 as u32, 1usize)
//             } else if (b0 & 0xE0) == 0xC0 {
//                 if i + 1 >= body.len() {
//                     return Err(LexErrorKind::InvalidToken);
//                 }
//                 let b1 = body[i + 1];
//                 if (b1 & 0xC0) != 0x80 {
//                     return Err(LexErrorKind::InvalidToken);
//                 }
//                 let cp = ((b0 & 0x1F) as u32) << 6 | ((b1 & 0x3F) as u32);
//                 (cp, 2)
//             } else if (b0 & 0xF0) == 0xE0 {
//                 if i + 2 >= body.len() {
//                     return Err(LexErrorKind::InvalidToken);
//                 }
//                 let b1 = body[i + 1];
//                 let b2 = body[i + 2];
//                 if (b1 & 0xC0) != 0x80 || (b2 & 0xC0) != 0x80 {
//                     return Err(LexErrorKind::InvalidToken);
//                 }
//                 let cp =
//                     ((b0 & 0x0F) as u32) << 12 | ((b1 & 0x3F) as u32) << 6 | ((b2 & 0x3F) as u32);
//                 (cp, 3)
//             } else if (b0 & 0xF8) == 0xF0 {
//                 if i + 3 >= body.len() {
//                     return Err(LexErrorKind::InvalidToken);
//                 }
//                 let b1 = body[i + 1];
//                 let b2 = body[i + 2];
//                 let b3 = body[i + 3];
//                 if (b1 & 0xC0) != 0x80 || (b2 & 0xC0) != 0x80 || (b3 & 0xC0) != 0x80 {
//                     return Err(LexErrorKind::InvalidToken);
//                 }
//                 let cp = ((b0 & 0x07) as u32) << 18
//                     | ((b1 & 0x3F) as u32) << 12
//                     | ((b2 & 0x3F) as u32) << 6
//                     | ((b3 & 0x3F) as u32);
//                 (cp, 4)
//             } else {
//                 return Err(LexErrorKind::InvalidToken);
//             };

//             if !is_valid_unicode_scalar(cp) {
//                 return Err(LexErrorKind::InvalidToken);
//             }
//             i += w;
//         }
//     }

//     if rune_count != 1 {
//         return Err(LexErrorKind::InvalidToken);
//     }
//     Ok(())
// }

// =============================================================================
// 3. Number scanning/validation (Go-style)
// =============================================================================

// =====================
// Character Classes
// =====================
#[derive(Clone, Copy)]
#[repr(u8)]
enum NumCharClass {
    Other = 0,
    Zero = 1,  // '0'
    One = 2,   // '1'
    Oct = 3,   // '2'..'7'
    Dec = 4,   // '8'..'9'
    Dot = 5,   // '.'
    Us = 6,    // '_'
    E = 7,     // 'e'|'E'  (exp in decimal, digit in hex mantissa)
    P = 8,     // 'p'|'P'  (exp in hex float)
    X = 9,     // 'x'|'X'
    O = 10,    // 'o'|'O'
    B = 11,    // 'b'|'B'  (prefix for bin; ALSO hex digit in mantissa)
    Hex = 12,  // 'a','c','d','f' (and A,C,D,F)
    Sign = 13, // '+'|'-'
}

const NUM_NCLASS: usize = 14;

// =====================
// States
// =====================
#[derive(Clone, Copy, PartialEq)]
#[repr(u8)]
enum NumState {
    Err = 0,

    Start = 1,

    // decimal / legacy leading-0
    Zero = 2,     // saw leading '0' only
    DecInt = 3,   // decimal int (non-leading-zero) digits
    DecIntUs = 4, // decimal int underscore just seen

    LegacyOct = 5,   // leading '0' int candidate with 0..7
    LegacyOctUs = 6, // underscore in legacy oct

    BadLead = 7, // leading '0' then saw 8/9 (invalid int unless becomes float)
    BadLeadUs = 8,

    // hex (0x...)
    PreHex = 9,  // after 0x / 0X (no mantissa digit yet)
    HexInt = 10, // hex mantissa digits (no dot yet)
    HexIntUs = 11,
    HexDotNoDig = 12,   // 0x. (no digit seen yet)
    HexDotHaveDig = 13, // 0x<digits>. (no frac digit yet)
    HexFrac = 14,       // hex digits after dot (or after 0x. with first digit)
    HexFracUs = 15,
    HexExpStart = 16, // after p/P
    HexExpSign = 17,
    HexExp = 18, // exponent digits (DEC digits)  => accept float
    HexExpUs = 19,

    // oct (0o...)
    PreOct = 20,
    OctInt = 21,
    OctIntUs = 22,

    // bin (0b...)
    PreBin = 23,
    BinInt = 24,
    BinIntUs = 25,

    // decimal floats
    DotStart = 26, // leading '.' seen, must see digit next
    DecDot = 27,   // saw digits then '.' (accept float even without frac digits)
    DecFrac = 28,
    DecFracUs = 29,
    DecExpStart = 30, // after e/E
    DecExpSign = 31,
    DecExp = 32, // exponent digits => accept float
    DecExpUs = 33,
}

const NUM_STATE: usize = 34;

// =====================
// CHAR_CLASS table
// =====================
static CHAR_CLASS: [u8; 256] = {
    use NumCharClass as C;

    let mut t = [C::Other as u8; 256];

    t[b'0' as usize] = C::Zero as u8;
    t[b'1' as usize] = C::One as u8;

    let mut i = b'2';
    while i <= b'7' {
        t[i as usize] = C::Oct as u8;
        i += 1;
    }

    t[b'8' as usize] = C::Dec as u8;
    t[b'9' as usize] = C::Dec as u8;

    t[b'.' as usize] = C::Dot as u8;
    t[b'_' as usize] = C::Us as u8;

    // Hex letters excluding b/e so we can give them their own class
    t[b'a' as usize] = C::Hex as u8;
    t[b'A' as usize] = C::Hex as u8;
    t[b'c' as usize] = C::Hex as u8;
    t[b'C' as usize] = C::Hex as u8;
    t[b'd' as usize] = C::Hex as u8;
    t[b'D' as usize] = C::Hex as u8;
    t[b'f' as usize] = C::Hex as u8;
    t[b'F' as usize] = C::Hex as u8;

    t[b'e' as usize] = C::E as u8;
    t[b'E' as usize] = C::E as u8;

    t[b'p' as usize] = C::P as u8;
    t[b'P' as usize] = C::P as u8;

    t[b'x' as usize] = C::X as u8;
    t[b'X' as usize] = C::X as u8;

    t[b'o' as usize] = C::O as u8;
    t[b'O' as usize] = C::O as u8;

    t[b'b' as usize] = C::B as u8;
    t[b'B' as usize] = C::B as u8;

    t[b'+' as usize] = C::Sign as u8;
    t[b'-' as usize] = C::Sign as u8;

    t
};

// =====================
// Transition table
// =====================
static TRANS: [[u8; NUM_NCLASS]; NUM_STATE] = {
    use NumCharClass as C;
    use NumState as S;

    let mut t = [[S::Err as u8; NUM_NCLASS]; NUM_STATE];

    macro_rules! tr {
        ($st:expr, [$($c:expr),+ $(,)?] => $to:expr) => {{
            $( t[$st as u8 as usize][$c as u8 as usize] = $to as u8; )+
        }};
    }

    // ---- Start ----
    tr!(S::Start, [C::Zero] => S::Zero);
    tr!(S::Start, [C::One, C::Oct, C::Dec] => S::DecInt);
    tr!(S::Start, [C::Dot] => S::DotStart);

    // ---- Zero (leading 0) ----
    tr!(S::Zero, [C::X] => S::PreHex);
    tr!(S::Zero, [C::O] => S::PreOct);
    tr!(S::Zero, [C::B] => S::PreBin);

    tr!(S::Zero, [C::Dot] => S::DecDot);
    tr!(S::Zero, [C::E] => S::DecExpStart);

    // legacy leading-0 integer path
    tr!(S::Zero, [C::Zero, C::One, C::Oct] => S::LegacyOct);
    tr!(S::Zero, [C::Us] => S::LegacyOctUs);
    tr!(S::Zero, [C::Dec] => S::BadLead);

    // ---- Decimal int (non-leading-zero) ----
    tr!(S::DecInt, [C::Zero, C::One, C::Oct, C::Dec] => S::DecInt);
    tr!(S::DecInt, [C::Us] => S::DecIntUs);
    tr!(S::DecInt, [C::Dot] => S::DecDot);
    tr!(S::DecInt, [C::E] => S::DecExpStart);

    tr!(S::DecIntUs, [C::Zero, C::One, C::Oct, C::Dec] => S::DecInt);

    // ---- Legacy oct int (leading 0, digits 0..7) ----
    tr!(S::LegacyOct, [C::Zero, C::One, C::Oct] => S::LegacyOct);
    tr!(S::LegacyOct, [C::Us] => S::LegacyOctUs);
    tr!(S::LegacyOct, [C::Dec] => S::BadLead); // 8/9 makes it invalid-int unless becomes float
    tr!(S::LegacyOct, [C::Dot] => S::DecDot);
    tr!(S::LegacyOct, [C::E] => S::DecExpStart);

    tr!(S::LegacyOctUs, [C::Zero, C::One, C::Oct] => S::LegacyOct);
    tr!(S::LegacyOctUs, [C::Dec] => S::BadLead); // underscore then 8/9 still invalid-int unless float later

    // ---- Bad leading-0 int (saw 8/9) ----
    tr!(S::BadLead, [C::Zero, C::One, C::Oct, C::Dec] => S::BadLead);
    tr!(S::BadLead, [C::Us] => S::BadLeadUs);
    tr!(S::BadLead, [C::Dot] => S::DecDot);
    tr!(S::BadLead, [C::E] => S::DecExpStart);

    tr!(S::BadLeadUs, [C::Zero, C::One, C::Oct, C::Dec] => S::BadLead);

    // ---- Dot-start decimal float ----
    tr!(S::DotStart, [C::Zero, C::One, C::Oct, C::Dec] => S::DecFrac);

    // ---- Decimal frac / dot ----
    tr!(S::DecDot, [C::Zero, C::One, C::Oct, C::Dec] => S::DecFrac);
    tr!(S::DecDot, [C::E] => S::DecExpStart);

    tr!(S::DecFrac, [C::Zero, C::One, C::Oct, C::Dec] => S::DecFrac);
    tr!(S::DecFrac, [C::Us] => S::DecFracUs);
    tr!(S::DecFrac, [C::E] => S::DecExpStart);

    tr!(S::DecFracUs, [C::Zero, C::One, C::Oct, C::Dec] => S::DecFrac);

    // ---- Decimal exponent ----
    tr!(S::DecExpStart, [C::Sign] => S::DecExpSign);
    tr!(S::DecExpStart, [C::Zero, C::One, C::Oct, C::Dec] => S::DecExp);

    tr!(S::DecExpSign, [C::Zero, C::One, C::Oct, C::Dec] => S::DecExp);

    tr!(S::DecExp, [C::Zero, C::One, C::Oct, C::Dec] => S::DecExp);
    tr!(S::DecExp, [C::Us] => S::DecExpUs);

    tr!(S::DecExpUs, [C::Zero, C::One, C::Oct, C::Dec] => S::DecExp);

    // ---- Hex prefix ----
    // digits in hex mantissa: 0-9 + a-f (with b/e separate classes)
    tr!(S::PreHex, [C::Us] => S::HexIntUs); // 0x_...
    tr!(S::PreHex, [C::Zero, C::One, C::Oct, C::Dec, C::Hex, C::B, C::E] => S::HexInt);
    tr!(S::PreHex, [C::Dot] => S::HexDotNoDig); // 0x. ...

    tr!(S::HexInt, [C::Zero, C::One, C::Oct, C::Dec, C::Hex, C::B, C::E] => S::HexInt);
    tr!(S::HexInt, [C::Us] => S::HexIntUs);
    tr!(S::HexInt, [C::Dot] => S::HexDotHaveDig);
    tr!(S::HexInt, [C::P] => S::HexExpStart); // 0x1p...

    tr!(S::HexIntUs, [C::Zero, C::One, C::Oct, C::Dec, C::Hex, C::B, C::E] => S::HexInt);

    // ---- Hex dot / frac ----
    // 0x. (no digit yet) must see a hex digit next; cannot go straight to p
    tr!(S::HexDotNoDig, [C::Zero, C::One, C::Oct, C::Dec, C::Hex, C::B, C::E] => S::HexFrac);

    // 0x<digits>. may go to p directly (0x1.p2) or have frac digits
    tr!(S::HexDotHaveDig, [C::Zero, C::One, C::Oct, C::Dec, C::Hex, C::B, C::E] => S::HexFrac);
    tr!(S::HexDotHaveDig, [C::P] => S::HexExpStart);

    tr!(S::HexFrac, [C::Zero, C::One, C::Oct, C::Dec, C::Hex, C::B, C::E] => S::HexFrac);
    tr!(S::HexFrac, [C::Us] => S::HexFracUs);
    tr!(S::HexFrac, [C::P] => S::HexExpStart);

    tr!(S::HexFracUs, [C::Zero, C::One, C::Oct, C::Dec, C::Hex, C::B, C::E] => S::HexFrac);

    // ---- Hex exponent (p) ----
    tr!(S::HexExpStart, [C::Sign] => S::HexExpSign);
    tr!(S::HexExpStart, [C::Zero, C::One, C::Oct, C::Dec] => S::HexExp);

    tr!(S::HexExpSign, [C::Zero, C::One, C::Oct, C::Dec] => S::HexExp);

    tr!(S::HexExp, [C::Zero, C::One, C::Oct, C::Dec] => S::HexExp);
    tr!(S::HexExp, [C::Us] => S::HexExpUs);

    tr!(S::HexExpUs, [C::Zero, C::One, C::Oct, C::Dec] => S::HexExp);

    // ---- Oct prefix (0o) ----
    tr!(S::PreOct, [C::Us] => S::OctIntUs); // 0o_7
    tr!(S::PreOct, [C::Zero, C::One, C::Oct] => S::OctInt);

    tr!(S::OctInt, [C::Zero, C::One, C::Oct] => S::OctInt);
    tr!(S::OctInt, [C::Us] => S::OctIntUs);

    tr!(S::OctIntUs, [C::Zero, C::One, C::Oct] => S::OctInt);

    // ---- Bin prefix (0b) ----
    tr!(S::PreBin, [C::Us] => S::BinIntUs); // 0b_101
    tr!(S::PreBin, [C::Zero, C::One] => S::BinInt);

    tr!(S::BinInt, [C::Zero, C::One] => S::BinInt);
    tr!(S::BinInt, [C::Us] => S::BinIntUs);

    tr!(S::BinIntUs, [C::Zero, C::One] => S::BinInt);

    t
};

// =====================
// Accepting state props
// bit0: accept, bit1: is_float
// =====================
static STATE_PROPS: [u8; NUM_STATE] = {
    use NumState as S;
    let mut t = [0u8; NUM_STATE];

    // int-accepting
    t[S::Zero as usize] = 1;
    t[S::DecInt as usize] = 1;
    t[S::LegacyOct as usize] = 1;
    t[S::HexInt as usize] = 1;
    t[S::OctInt as usize] = 1;
    t[S::BinInt as usize] = 1;

    // float-accepting
    t[S::DecDot as usize] = 3;
    t[S::DecFrac as usize] = 3;
    t[S::DecExp as usize] = 3;

    // hex float accepts ONLY once exponent digits exist
    t[S::HexExp as usize] = 3;

    t
};

#[inline]
pub fn classify_number(lit: &[u8]) -> Result<bool, LexErrorKind> {
    if lit.is_empty() {
        return Err(LexErrorKind::InvalidNumber);
    }

    let mut state = NumState::Start as u8;

    for &b in lit {
        let cc = CHAR_CLASS[b as usize] as usize;
        state = TRANS[state as usize][cc];
        if state == NumState::Err as u8 {
            return Err(LexErrorKind::InvalidNumber);
        }
    }

    let props = STATE_PROPS[state as usize];
    if (props & 1) == 0 {
        return Err(LexErrorKind::InvalidNumber);
    }
    Ok((props & 2) != 0)
}

#[inline]
fn lex_number(lex: &mut LogosLexer<'_, RawTok>) -> Result<(), LexErrorKind> {
    // regex starts at one digit OR ".<digit>", then we grow to maximal number token
    let src = lex.source().as_bytes();
    let start = lex.span().start;
    let mut i = start;
    let n = src.len();

    let mut base: u8 = 10;

    // integer part
    if src[i] != b'.' {
        if src[i] == b'0' {
            i += 1;
            if i < n {
                match lower_ascii(src[i]) {
                    b'x' => {
                        base = 16;
                        i += 1;
                    }
                    b'o' => {
                        base = 8;
                        i += 1;
                    }
                    b'b' => {
                        base = 2;
                        i += 1;
                    }
                    _ => {
                        base = 8;
                    }
                }
            } else {
                base = 8;
            }
        } else {
            i += 1;
        }

        if base <= 10 {
            while i < n && (is_dec_digit(src[i]) || src[i] == b'_') {
                i += 1;
            }
        } else {
            while i < n && (is_hex_digit(src[i]) || src[i] == b'_') {
                i += 1;
            }
        }
    }

    // fraction
    if i < n && src[i] == b'.' {
        // do not steal '.' from ellipsis
        if i + 1 < n && src[i + 1] == b'.' {
            // stop
        } else {
            i += 1;
            if base <= 10 {
                while i < n && (is_dec_digit(src[i]) || src[i] == b'_') {
                    i += 1;
                }
            } else {
                while i < n && (is_hex_digit(src[i]) || src[i] == b'_') {
                    i += 1;
                }
            }
        }
    }

    // exponent
    if i < n {
        let e = lower_ascii(src[i]);
        if e == b'e' || e == b'p' {
            i += 1;
            if i < n && (src[i] == b'+' || src[i] == b'-') {
                i += 1;
            }
            while i < n && (is_dec_digit(src[i]) || src[i] == b'_') {
                i += 1;
            }
        }
    }

    let already = lex.span().end;
    if i > already {
        lex.bump(i - already);
    }
    Ok(())
}

// =============================================================================
// 4. Token Definition (RawTok - DFA optimized for logos)
// =============================================================================

#[repr(u8)]
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
#[logos(error = LexErrorKind)]
#[logos(skip r"[\t\x0C\f\v ]+")]
#[rustfmt::skip]
enum RawTok {
    // BOM (allowed only at file start; wrapper enforces)
    #[token("\u{FEFF}")] Bom,

    // Trivia
    #[regex(r"\r\n|\n|\r")] Newline,
    #[regex(r"//[^\n\r]*", logos::skip, allow_greedy = true)] _LineComment,
    #[token("/*", lex_block_comment)] BlockComment,

    // Keywords (MUST be before Ident)
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

    // Identifiers (Go spec: letter = '_' or Unicode letter; digit = Unicode Nd)
    #[regex(r"[_\p{L}][_\p{L}\p{Nd}]*")] Ident,

    // Numbers: start with digit OR ".<digit>", then grow to full token in callback
    #[regex(r"[0-9]|\.[0-9]", lex_number)] Number,

    // Strings / runes
    #[regex(r"`[^`]*`")] RawString,
    #[regex(r#""([^"\\\n\r]|\\.)*""#, validate_interpreted_string)] String,
    #[regex(r"'([^'\\\n\r]|\\.)+'", validate_rune_lit)] Rune,

    // Operators (multi-char first)
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

    // Catch-all (MUST be low priority to avoid logos ambiguity errors)
    #[regex(r".", priority = 0)] Error,
}

// =============================================================================
// 5. Lookup tables (fast classification)
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

const SEMI_INSERT_TABLE: [bool; 256] = gen_lookup_table!(
    bool,
    256,
    // Ident + basic literals
    Ident,
    Number,
    Rune,
    String,
    RawString,
    // Keywords
    KwBreak,
    KwContinue,
    KwFallthrough,
    KwReturn,
    // Operators
    Inc,
    Dec,
    // Delimiters
    RParen,
    RBrack,
    RBrace,
);

const TOKEN_KIND_TABLE: [TokKind; 256] = gen_lookup_table!(
    TokKind, 256, TokKind::Simple,
    // Trivia
    Newline => TokKind::Trivia,
    _LineComment => TokKind::Trivia,
    BlockComment => TokKind::Trivia,
    // Payload tokens
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

    #[inline]
    const fn to_token<'src>(self, slice: &'src str) -> Tok<'src> {
        // NOTE: Number is handled in wrapper (needs classify + imag lookahead)
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
            KwSelect => KwSelect, KwStruct => KwStruct, KwSwitch => KwSwitch, KwType => KwType, KwVar => KwVar,

            Ellipsis => Ellipsis, ShlAssign => ShlAssign, ShrAssign => ShrAssign, AndNotAssign => AndNotAssign,
            AddAssign => AddAssign, SubAssign => SubAssign, MulAssign => MulAssign, DivAssign => DivAssign,
            ModAssign => ModAssign, AndAssign => AndAssign, OrAssign => OrAssign, XorAssign => XorAssign,
            Shl => Shl, Shr => Shr, AndNot => AndNot, LAnd => LAnd, LOr => LOr, EqEq => EqEq, NotEq => NotEq,
            Le => Le, Ge => Ge, Inc => Inc, Dec => Dec, Define => Define, Arrow => Arrow, Assign => Assign,
            Plus => Plus, Minus => Minus, Star => Star, Slash => Slash, Percent => Percent, Amp => Amp,
            Pipe => Pipe, Caret => Caret, Tilde => Tilde, Bang => Bang, Lt => Lt, Gt => Gt,

            LParen => LParen, RParen => RParen, LBrack => LBrack, RBrack => RBrack, LBrace => LBrace,
            RBrace => RBrace, Comma => Comma, Semi => Semi, Colon => Colon, Dot => Dot, Error => Error,
            Bom => Error, // wrapper handles BOM explicitly
        }
    }
}

// =============================================================================
// 6. Public Token Definition (zero-copy)
// =============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Tok<'input> {
    Ident(&'input str),
    IntLit(&'input str),
    FloatLit(&'input str),
    ImagLit(&'input str),
    RuneLit(&'input str),
    StringLit(&'input str),
    RawStringLit(&'input str),

    // Keywords
    KwBreak,
    KwCase,
    KwChan,
    KwConst,
    KwContinue,
    KwDefault,
    KwDefer,
    KwElse,
    KwFallthrough,
    KwFor,
    KwFunc,
    KwGo,
    KwGoto,
    KwIf,
    KwImport,
    KwInterface,
    KwMap,
    KwPackage,
    KwRange,
    KwReturn,
    KwSelect,
    KwStruct,
    KwSwitch,
    KwType,
    KwVar,

    // Operators / Delimiters
    Ellipsis,
    ShlAssign,
    ShrAssign,
    AndNotAssign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    Shl,
    Shr,
    AndNot,
    LAnd,
    LOr,
    EqEq,
    NotEq,
    Le,
    Ge,
    Inc,
    Dec,
    Define,
    Arrow,
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Amp,
    Pipe,
    Caret,
    Tilde,
    Bang,
    Lt,
    Gt,
    LParen,
    RParen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    Comma,
    Semi,
    Colon,
    Dot,

    Error,
}

impl<'input> std::fmt::Display for Tok<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

// =============================================================================
// 7. Lexer with Semicolon Insertion + Imag Lookahead
// =============================================================================

pub struct Lexer<'src> {
    logos: LogosLexer<'src, RawTok>,
    pending: Option<(usize, Tok<'src>, usize)>,
    diags: Vec<Diag>,
    last_can_insert_semi: bool,
    src_len: usize,
    eof_done: bool,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            logos: RawTok::lexer(input),
            pending: None,
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
        self.pending = Some((pos, Tok::Semi, pos));
    }

    #[inline]
    fn handle_trivia(&mut self, raw: RawTok, span: &Range<usize>, slice: &str) -> bool {
        match raw {
            RawTok::Newline => {
                if self.last_can_insert_semi {
                    self.last_can_insert_semi = false;
                    self.emit_semi_at(span.start);
                }
                true
            }
            RawTok::BlockComment => {
                if self.last_can_insert_semi {
                    if let Some(off) = first_newline_offset(slice) {
                        self.last_can_insert_semi = false;
                        self.emit_semi_at(span.start + off);
                    }
                }
                true
            }
            _ => false,
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = (usize, Tok<'src>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(tok) = self.pending.take() {
                return Some(tok);
            }

            if self.eof_done {
                return None;
            }

            match self.logos.next() {
                None => {
                    self.eof_done = true;
                    if self.last_can_insert_semi {
                        self.last_can_insert_semi = false;
                        self.emit_semi_at(self.src_len);
                        continue;
                    }
                    return None;
                }

                Some(Err(kind)) => {
                    let span = self.logos.span();
                    self.push_lex_diag(kind, span.clone());
                    self.last_can_insert_semi = false;
                    return Some((span.start, Tok::Error, span.end));
                }

                Some(Ok(raw)) => {
                    let span = self.logos.span();
                    let slice = self.logos.slice();

                    // BOM: only valid at start
                    if raw == RawTok::Bom {
                        if span.start == 0 {
                            continue; // ignore
                        }
                        self.push_lex_diag(LexErrorKind::InvalidToken, span.clone());
                        self.last_can_insert_semi = false;
                        return Some((span.start, Tok::Error, span.end));
                    }

                    // Trivia
                    if self.handle_trivia(raw, &span, slice) {
                        continue;
                    }

                    // Raw error token (catch-all)
                    if raw == RawTok::Error {
                        self.push_lex_diag(LexErrorKind::InvalidToken, span.clone());
                        self.last_can_insert_semi = false;
                        return Some((span.start, Tok::Error, span.end));
                    }

                    // Number: validate + classify + imag lookahead
                    if raw == RawTok::Number {
                        let mut end = span.end;
                        let src = self.logos.source();

                        // validate/classify numeric part
                        let is_float = match classify_number(slice.as_bytes()) {
                            Ok(f) => f,
                            Err(kind) => {
                                self.push_lex_diag(kind, span.clone());
                                self.last_can_insert_semi = false;
                                return Some((span.start, Tok::Error, span.end));
                            }
                        };

                        // imag lookahead: consume trailing 'i' if immediately present
                        if end < self.src_len && src.as_bytes()[end] == b'i' {
                            self.logos.bump(1);
                            end += 1;
                            self.last_can_insert_semi = true;
                            return Some((span.start, Tok::ImagLit(&src[span.start..end]), end));
                        }

                        self.last_can_insert_semi = true;
                        let tok = if is_float {
                            Tok::FloatLit(slice)
                        } else {
                            Tok::IntLit(slice)
                        };
                        return Some((span.start, tok, end));
                    }

                    // Update semicolon insertion flag
                    self.last_can_insert_semi = raw.can_insert_semicolon();

                    // Convert to Tok
                    let tok = raw.to_token(slice);

                    return Some((span.start, tok, span.end));
                }
            }
        }
    }
}
