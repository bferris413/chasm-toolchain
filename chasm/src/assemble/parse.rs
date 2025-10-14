//! Parser for chasm assembly source code.

use anyhow::Result;

use crate::assemble::{helpers::normalize_to_ascii_lower, AssemblyAst, AssemblyError, AssemblyTokens, HexLiteral, Instruction, Node, NodeKind, Register, Token, TokenKind};

pub(crate) fn parse(tokens: AssemblyTokens<'_>) -> Result<AssemblyAst<'_>> {
    let tokens = tokens.tokens;

    let mut nodes = Vec::new();
    let mut tokens = tokens.into_iter();
    while let Some(token) = tokens.next() {
        match token.kind {
            TokenKind::HexLiteralU32 => {
                let node = parse_hex_literal_u32(token);
                nodes.push(node);
            },
            TokenKind::HexLiteralU16 => {
                let node = parse_hex_literal_u16(token)?;
                nodes.push(node);
            },
            TokenKind::HexLiteralU8 => {
                let node = parse_hex_literal_u8(token)?;
                nodes.push(node);
            },
            TokenKind::Identifier => {
                let node = parse_instruction(token, &mut tokens)?;
                nodes.push(node);
            }
            TokenKind::Label => {
                let node = parse_label(token)?;
                nodes.push(node);
            }
        }

    }

    Ok(AssemblyAst { nodes })
}

fn parse_hex_literal_u32(token: Token<'_>) -> Node<'_> {
    let mut value: u32 = 0;

    for c in token.lexeme[1..].chars() {
        if c == '.' {
            continue;
        }
        let digit = c.to_digit(16).expect("is hex digit");
        value = value
            .checked_mul(16) // base16 left shift
            .and_then(|v| v.checked_add(digit))
            .expect("overflow");
    }

    Node {
        kind: NodeKind::HexLiteral(HexLiteral::U32(value)),
        token,
    }
}

fn parse_hex_literal_u16(token: Token<'_>) -> Result<Node<'_>> {
    let value = u16::from_str_radix(&token.lexeme[1..], 16)?;
    let node = Node {
        kind: NodeKind::HexLiteral(HexLiteral::U16(value)),
        token,
    };

    Ok(node)
}

fn parse_hex_literal_u8(token: Token<'_>) -> Result<Node<'_>> {
    let value = u8::from_str_radix(&token.lexeme[1..], 16)?;
    let node = Node {
        kind: NodeKind::HexLiteral(HexLiteral::U8(value)),
        token,
    };

    Ok(node)
}

fn parse_label<'src>(label_token: Token<'src>) -> Result<Node<'src>> {
    assert!(label_token.lexeme.starts_with('@'));
    assert!(label_token.lexeme.len() > 1);

    let node = Node {
        kind: NodeKind::Label,
        token: label_token,
    };

    Ok(node)
}

fn parse_instruction<'src>(token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    assert!(matches!(token.kind, TokenKind::Identifier));

    let mut tmp_buf = [0u8; 8];
    let maybe_mnemonic = normalize_to_ascii_lower(token.lexeme, &mut tmp_buf);
    match maybe_mnemonic {
        "movs" => parse_movs(token, tokens),
        "adds" => parse_adds(token, tokens),
        other if Register::try_from(other).is_ok() => {
            let err = AssemblyError::new(
                format!("Expected instruction mnemonic, found register '{}'", token.lexeme),
                token.line,
                token.column,
                Some(token.column + token.lexeme.len()),
                token.source,
            );
            Err(err.into())

        }
        other => {
            let err = AssemblyError::new(
                format!("Expected instruction mnemonic, found unknown identifier '{}'", token.lexeme),
                token.line,
                token.column,
                Some(token.column + other.len()),
                token.source,
            );
            Err(err.into())
        }
    }
}

fn parse_movs<'src>(movs_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let maybe_register = tokens.next().ok_or_else(|| {
        AssemblyError::new(
            format!("Expected register after {} mnemonic, found EOF", movs_token.lexeme),
            movs_token.line,
            movs_token.column + movs_token.lexeme.len(),
            None,
            movs_token.source,
        )
    })?;

    let TokenKind::Identifier = maybe_register.kind else {
        let err = AssemblyError::new(
            format!("Expected register after {} mnemonic, found {} '{}'", movs_token.lexeme, maybe_register.kind, maybe_register.lexeme),
            maybe_register.line,
            maybe_register.column,
            Some(maybe_register.column + maybe_register.lexeme.len()),
            maybe_register.source,
        );
        return Err(err.into());
    };
    let dest_register = Register::try_from(maybe_register.lexeme).map_err(|e| {
        AssemblyError::new(
            format!("Invalid register '{}' after {} mnemonic: {e}", maybe_register.lexeme, movs_token.lexeme),
            maybe_register.line,
            maybe_register.column,
            Some(maybe_register.column + maybe_register.lexeme.len()),
            maybe_register.source,
        )
    })?;

    let dest_register = match dest_register {
        Register::General(reg) if (reg as u8) < 8 => reg,
        _ => {
            let err = AssemblyError::new(
                format!("Expected general-purpose register (r0-r7) after {} mnemonic, found '{:?}'", movs_token.lexeme, dest_register),
                maybe_register.line,
                maybe_register.column,
                Some(maybe_register.column + maybe_register.lexeme.len()),
                maybe_register.source,
            );
            return Err(err.into());
        }
    };

    let maybe_imm8 = tokens.next().ok_or_else(|| {
        AssemblyError::new(
            format!("Expected immediate value after register in {} instruction", movs_token.lexeme),
            maybe_register.line,
            maybe_register.column + maybe_register.lexeme.len(),
            None,
            maybe_register.source,
        )
    })?;

    let TokenKind::HexLiteralU8 = maybe_imm8.kind else {
        let err = AssemblyError::new(
            format!("Expected 8-bit immediate value after register in {} instruction, found '{}'", movs_token.lexeme, maybe_imm8.lexeme),
            maybe_imm8.line,
            maybe_imm8.column,
            Some(maybe_imm8.column + maybe_imm8.lexeme.len()),
            maybe_imm8.source,
        );
        return Err(err.into());
    };

    let NodeKind::HexLiteral(hl @ HexLiteral::U8(_imm8)) = parse_hex_literal_u8(maybe_imm8)?.kind else { unreachable!() };

    let node = Node {
        kind: NodeKind::Instruction(Instruction::Movs { dest: dest_register, value: hl }),
        token: movs_token,
    };

    Ok(node)
}

fn parse_adds<'src>(adds_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let maybe_register = tokens.next().ok_or_else(|| {
        AssemblyError::new(
            format!("Expected register after {} mnemonic, found EOF", adds_token.lexeme),
            adds_token.line,
            adds_token.column + adds_token.lexeme.len(),
            None,
            adds_token.source,
        )
    })?;

    let TokenKind::Identifier = maybe_register.kind else {
        let err = AssemblyError::new(
            format!("Expected register after {} mnemonic, found {} '{}'", adds_token.lexeme, maybe_register.kind, maybe_register.lexeme),
            maybe_register.line,
            maybe_register.column,
            Some(maybe_register.column + maybe_register.lexeme.len()),
            maybe_register.source,
        );
        return Err(err.into());
    };
    let dest_register = Register::try_from(maybe_register.lexeme).map_err(|e| {
        AssemblyError::new(
            format!("Invalid register '{}' after {} mnemonic: {e}", maybe_register.lexeme, adds_token.lexeme),
            maybe_register.line,
            maybe_register.column,
            Some(maybe_register.column + maybe_register.lexeme.len()),
            maybe_register.source,
        )
    })?;

    let dest_register = match dest_register {
        Register::General(reg) if (reg as u8) < 8 => reg,
        _ => {
            let err = AssemblyError::new(
                format!("Expected general-purpose register (r0-r7) after {} mnemonic, found '{:?}'", adds_token.lexeme, dest_register),
                maybe_register.line,
                maybe_register.column,
                Some(maybe_register.column + maybe_register.lexeme.len()),
                maybe_register.source,
            );
            return Err(err.into());
        }
    };

    let maybe_imm8 = tokens.next().ok_or_else(|| {
        AssemblyError::new(
            format!("Expected immediate value after register in {} instruction", adds_token.lexeme),
            maybe_register.line,
            maybe_register.column + maybe_register.lexeme.len(),
            None,
            maybe_register.source,
        )
    })?;

    let TokenKind::HexLiteralU8 = maybe_imm8.kind else {
        let err = AssemblyError::new(
            format!("Expected 8-bit immediate value after register in {} instruction, found '{}'", adds_token.lexeme, maybe_imm8.lexeme),
            maybe_imm8.line,
            maybe_imm8.column,
            Some(maybe_imm8.column + maybe_imm8.lexeme.len()),
            maybe_imm8.source,
        );
        return Err(err.into());
    };

    let NodeKind::HexLiteral(hl @ HexLiteral::U8(_imm8)) = parse_hex_literal_u8(maybe_imm8)?.kind else { unreachable!() };

    let node = Node {
        kind: NodeKind::Instruction(Instruction::Adds { dest: dest_register, value: hl }),
        token: adds_token,
    };

    Ok(node)
}