//! Tokenizer for chasm assembly source code.

use anyhow::Result;

use crate::assemble::{AssemblyError, AssemblySource, AssemblyTokens, Token, TokenKind};

const HEX_LITERAL_HALFWORD_SEP: char = '.';

pub(crate) fn tokenize(source: &AssemblySource) -> Result<AssemblyTokens<'_>> {
    let mut tokens = AssemblyTokens::default();
    let mut line = 0;
    let mut col = 0;
    let mut chars = source.chars().enumerate().peekable();

    while let Some((i, c)) = chars.next() {
        match c {
            '\n' => {
                line += 1;
                col = 0;
            }
            c if c.is_whitespace() => {
                col += 1;
            }
            'x' => {
                // hex literal
                let hex_token = parse_hex_literal(source, &mut chars, i, line, &mut col)?;
                tokens.push(hex_token);
            }
            other => {
                let err = AssemblyError::new(
                    format!("Unexpected character '{other}'"),
                    line,
                    col,
                    None,
                    source,
                );
                return Err(err.into());
            }
        }
    }

    Ok(tokens)
}

fn parse_hex_literal<'src>(
    source: &'src AssemblySource,
    chars: &mut std::iter::Peekable<impl Iterator<Item = (usize, char)>>,
    start_index: usize,
    line: usize,
    col: &mut usize,
) -> Result<Token<'src>> {
    let start_col = *col;
    *col += 1;
    let mut ndigits = 0;

    while matches!(chars.peek(), Some((_, c)) if (c.is_ascii_hexdigit() || *c == HEX_LITERAL_HALFWORD_SEP)) {
        let (_, c) = chars.next().unwrap();
        if c == HEX_LITERAL_HALFWORD_SEP {
            if ndigits == 0 {
                let err = AssemblyError::new(
                    "Hex literal cannot start with a separator".to_string(),
                    line,
                    start_col,
                    Some(*col),
                    source,
                );
                return Err(err.into());
            } else if ndigits % 4 != 0 {
                let err = AssemblyError::new(
                    "Hex literal separator must be after a full half-word (4 digits)".to_string(),
                    line,
                    start_col,
                    Some(*col),
                    source,
                );
                return Err(err.into());
            }

            if ! matches!(chars.peek(), Some((_, c)) if c.is_ascii_hexdigit()) {
                let err = AssemblyError::new(
                    "Hex literal cannot end with a separator".to_string(),
                    line,
                    start_col,
                    Some(*col),
                    source,
                );
                return Err(err.into());
            }
        } else {
            ndigits += 1;
        }
        *col += 1;
    }

    if ! (2..=8).contains(&ndigits) {
        let err = AssemblyError::new(
            "Hex literal must have 2-8 digits".to_string(),
            line,
            start_col,
            Some(*col),
            source,
        );
        return Err(err.into());
    }

    if ndigits % 2 != 0 {
        let err = AssemblyError::new(
            "Hex literal must have an even number of digits".to_string(),
            line,
            start_col,
            Some(*col),
            source,
        );
        return Err(err.into());
    }

    let lexeme = &source[start_index..start_index + (*col - start_col)];
    let token_kind = match ndigits {
        2 => TokenKind::HexLiteralU8,
        4 => TokenKind::HexLiteralU16,
        8 => TokenKind::HexLiteralU32,
        _ => unreachable!(),
    };

    let t = Token {
        kind: token_kind,
        lexeme,
        line,
        column: start_col,
    };

    Ok(t)
}