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
            ';' => {
                skip_comment(&mut chars, &mut col);
            }
            '(' | ')' => {
                let paren = tokenize_paren(source, i, line, &mut col);
                tokens.push(paren);
            }
            'x' => {
                // hex literal
                let hex_token = tokenize_hex_literal(source, &mut chars, i, line, &mut col)?;
                tokens.push(hex_token);
            }
            '@' => {
                // label
                let label_token = tokenize_label(source, &mut chars, i, line, &mut col)?;
                tokens.push(label_token);
            }
            '&' => {
                // reference
                let ref_token = tokenize_ref(source, &mut chars, i, line, &mut col)?;
                tokens.push(ref_token);
            }
            '[' | ']' => {
                let bracket = tokenize_bracket(source, i, line, &mut col);
                tokens.push(bracket);
            }
            c if c.is_ascii_alphabetic() => {
                let identifier = tokenize_identifier(source, &mut chars, i, line, &mut col)?;
                tokens.push(identifier);
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

fn skip_comment(
    chars: &mut std::iter::Peekable<impl Iterator<Item = (usize, char)>>,
    col: &mut usize,
) {
    *col += 1;

    while matches!(chars.peek(), Some((_, c)) if *c != '\n') {
        let (_, _) = chars.next().unwrap();
        *col += 1;
    }
}

fn tokenize_bracket<'src>(
    source: &'src AssemblySource,
    start_index: usize,
    line: usize,
    col: &mut usize,
) ->Token<'src> {
    let lexeme = &source[start_index..start_index + 1];
    let kind = match &lexeme[..] {
        "[" => TokenKind::LBracket,
        "]" => TokenKind::RBracket,
        _ => unreachable!(),
    }; 
    let t = Token {
        kind,
        lexeme,
        line,
        column: *col,
        source,
    };

    *col += 1;
    t
}

fn tokenize_paren<'src>(
    source: &'src AssemblySource,
    start_index: usize,
    line: usize,
    col: &mut usize,
) ->Token<'src> {
    let lexeme = &source[start_index..start_index + 1];
    let kind = match &lexeme[..] {
        "(" => TokenKind::LParen,
        ")" => TokenKind::RParen,
        _ => unreachable!(),
    }; 
    let t = Token {
        kind,
        lexeme,
        line,
        column: *col,
        source,
    };

    *col += 1;
    t
}

fn tokenize_identifier<'src>(
    source: &'src AssemblySource,
    chars: &mut std::iter::Peekable<impl Iterator<Item = (usize, char)>>,
    start_index: usize,
    line: usize,
    col: &mut usize,
) -> Result<Token<'src>> {
    let mut cur_index = start_index + 1;
    let start_col = *col;
    *col += 1;

    while matches!(chars.peek(), Some((_, c)) if is_valid_identifier_char(*c)) {
        let (_, _) = chars.next().unwrap();
        *col += 1;
        cur_index += 1;
    }

    let mut is_pseudo = false;
    if matches!(chars.peek(), Some((_, c)) if *c == '!') {
        let (_, _) = chars.next().unwrap();
        *col += 1;
        cur_index += 1;
        is_pseudo = true;
    }

    let lexeme = &source[start_index..cur_index];
    let kind = if is_pseudo { TokenKind::PseudoIdentifier } else { TokenKind::Identifier };
    let t = Token {
        kind,
        lexeme,
        line,
        column: start_col,
        source,
    };

    Ok(t)
}

fn tokenize_ref<'src>(
    source: &'src AssemblySource,
    chars: &mut std::iter::Peekable<impl Iterator<Item = (usize, char)>>,
    start_index: usize,
    line: usize,
    col: &mut usize,
) -> Result<Token<'src>> {
    let mut cur_index = start_index + 1;
    let start_col = *col;
    *col += 1;

    while matches!(chars.peek(), Some((_, c)) if is_valid_identifier_char(*c)) {
        let (_, _) = chars.next().unwrap();
        *col += 1;
        cur_index += 1;
    }

    let lexeme = &source[start_index..cur_index];
    if lexeme.len() < 2 {
        let err = AssemblyError::new(
            "Reference must have at least one character after '&'".to_string(),
            line,
            start_col,
            Some(*col),
            source,
        );
        return Err(err.into());
    }

    let t = Token {
        kind: TokenKind::Ref,
        lexeme,
        line,
        column: start_col,
        source,
    };

    Ok(t)
}

fn tokenize_label<'src>(
    source: &'src AssemblySource,
    chars: &mut std::iter::Peekable<impl Iterator<Item = (usize, char)>>,
    start_index: usize,
    line: usize,
    col: &mut usize,
) -> Result<Token<'src>> {
    let mut cur_index = start_index + 1;
    let start_col = *col;
    *col += 1;

    while matches!(chars.peek(), Some((_, c)) if is_valid_identifier_char(*c)) {
        let (_, _) = chars.next().unwrap();
        *col += 1;
        cur_index += 1;
    }

    let lexeme = &source[start_index..cur_index];
    if lexeme.len() < 2 {
        let err = AssemblyError::new(
            "Label must have at least one character after '@'".to_string(),
            line,
            start_col,
            Some(*col),
            source,
        );
        return Err(err.into());
    }

    let t = Token {
        kind: TokenKind::Label,
        lexeme,
        line,
        column: start_col,
        source,
    };

    Ok(t)
}

fn tokenize_hex_literal<'src>(
    source: &'src AssemblySource,
    chars: &mut std::iter::Peekable<impl Iterator<Item = (usize, char)>>,
    start_index: usize,
    line: usize,
    col: &mut usize,
) -> Result<Token<'src>> {
    let start_col = *col;
    let mut cur_index = start_index + 1;
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
        cur_index += 1;
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

    let lexeme = &source[start_index..cur_index];
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
        source,
    };

    Ok(t)
}

fn is_valid_identifier_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '-' || c == '_'
}