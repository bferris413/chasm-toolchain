//! Parser for chasm assembly source code.

use anyhow::Result;

use crate::assemble::helpers::normalize_to_ascii_lower;
use crate::assemble::{AssemblyAst, AssemblyError, AssemblyTokens, HexLiteral, Instruction, Node, NodeKind, Register, Token, TokenKind};

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
        "ldr" => parse_ldr(token, tokens),
        "movs" => parse_movs(token, tokens),
        "movw" => parse_movw(token, tokens),
        "movt" => parse_movt(token, tokens),
        "adds" => parse_adds(token, tokens),
        "orrs" => parse_orrs(token, tokens),
        "str" => parse_str(token, tokens),
        "b" => parse_branch(token, tokens),
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

fn parse_imm16<'src>(prev_token: &Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let maybe_imm16 = tokens.next().ok_or_else(|| {
        AssemblyError::new(
            format!("Expected 16-bit immediate value after {}", prev_token.lexeme),
            prev_token.line,
            prev_token.column,
            Some(prev_token.column + prev_token.lexeme.len()),
            prev_token.source,
        )
    })?;

    let TokenKind::HexLiteralU16 = maybe_imm16.kind else {
        let err = AssemblyError::new(
            format!("Expected 16-bit immediate value after {}, found '{}'", prev_token.lexeme, maybe_imm16.lexeme),
            maybe_imm16.line,
            maybe_imm16.column,
            Some(maybe_imm16.column + maybe_imm16.lexeme.len()),
            maybe_imm16.source,
        );
        return Err(err.into());
    };

    let imm16_node = parse_hex_literal_u16(maybe_imm16)?;

    Ok(imm16_node)
}

fn parse_register<'src>(prev_token: &Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<(Register, Token<'src>)> {
    let maybe_register = tokens.next().ok_or_else(|| {
        AssemblyError::new(
            format!("Expected register after '{}', found EOF", prev_token.lexeme),
            prev_token.line,
            prev_token.column,
            Some(prev_token.column + prev_token.lexeme.len()),
            prev_token.source,
        )
    })?;

    let TokenKind::Identifier = maybe_register.kind else {
        let err = AssemblyError::new(
            format!("Expected register after '{}', found {} '{}'", prev_token.lexeme, maybe_register.kind, maybe_register.lexeme),
            maybe_register.line,
            maybe_register.column,
            Some(maybe_register.column + maybe_register.lexeme.len()),
            maybe_register.source,
        );
        return Err(err.into());
    };
    let register = Register::try_from(maybe_register.lexeme).map_err(|e| {
        AssemblyError::new(
            format!("Invalid register '{}' after '{}': {e}", maybe_register.lexeme, prev_token.lexeme),
            maybe_register.line,
            maybe_register.column,
            Some(maybe_register.column + maybe_register.lexeme.len()),
            maybe_register.source,
        )
    })?;

    Ok((register, maybe_register))
}

fn parse_str<'src>(str_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let (src_register, st) = parse_register(&str_token, tokens)?;
    let src_reg = match src_register {
        Register::General(reg) if (reg as u8) < 8 => reg,
        _ => {
            let err = AssemblyError::new(
                format!("Expected general-purpose register (r0-r7), found '{:?}'", src_register),
                st.line,
                st.column,
                Some(st.column + st.lexeme.len()),
                st.source,
            );
            return Err(err.into());
        }
    };

    let (dest_addr_register, dt) = parse_register(&st, tokens)?;
    let dest_addr_reg = match dest_addr_register {
        Register::General(reg) if (reg as u8) < 8 => reg,
        _ => {
            let err = AssemblyError::new(
                format!("Expected general-purpose register (r0-r7), found '{:?}'", dest_addr_register),
                dt.line,
                dt.column,
                Some(dt.column + dt.lexeme.len()),
                dt.source,
            );
            return Err(err.into());
        }
    };

    let node = Node {
        kind: NodeKind::Instruction(Instruction::Str { dest_addr_reg, src: src_reg }),
        token: str_token,
    };

    Ok(node)
}

fn parse_orrs<'src>(orrs_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let (dest_register, dt) = parse_register(&orrs_token, tokens)?;

    let dest_register = match dest_register {
        Register::General(reg) if (reg as u8) < 8 => reg,
        _ => {
            let err = AssemblyError::new(
                format!("Expected general-purpose register (r0-r7), found '{:?}'", dest_register),
                dt.line,
                dt.column,
                Some(dt.column + dt.lexeme.len()),
                dt.source,
            );
            return Err(err.into());
        }
    };

    let (src_register, st) = parse_register(&dt, tokens)?;
    let src_register = match src_register {
        Register::General(reg) if (reg as u8) < 8 => reg,
        _ => {
            let err = AssemblyError::new(
                format!("Expected general-purpose register (r0-r7), found '{:?}'", src_register),
                st.line,
                st.column,
                Some(st.column + st.lexeme.len()),
                st.source,
            );
            return Err(err.into());
        }
    };

    let node = Node {
        kind: NodeKind::Instruction(Instruction::Orrs { dest: dest_register, src: src_register }),
        token: orrs_token,
    };

    Ok(node)
}

fn parse_ldr<'src>(ldr_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let (dest_register, dt) = parse_register(&ldr_token, tokens)?;

    let dest_register = match dest_register {
        Register::General(reg) => reg,
        _ => {
            let err = AssemblyError::new(
                format!("Expected general-purpose register (r0-r12), found '{:?}'", dest_register),
                dt.line,
                dt.column,
                Some(dt.column + dt.lexeme.len()),
                dt.source,
            );
            return Err(err.into());
        }
    };

    let (src_register, st) = parse_register(&dt, tokens)?;
    let src_register = match src_register {
        Register::General(reg) => reg,
        _ => {
            let err = AssemblyError::new(
                format!("Expected general-purpose register (r0-r12), found '{:?}'", src_register),
                st.line,
                st.column,
                Some(st.column + st.lexeme.len()),
                st.source,
            );
            return Err(err.into());
        }
    };

    let node = Node {
        kind: NodeKind::Instruction(Instruction::Ldr { dest: dest_register, src: src_register }),
        token: ldr_token,
    };

    Ok(node)
}

fn parse_movt<'src>(movt_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let (dest_register, dt) = parse_register(&movt_token, tokens)?;

    let reg = match dest_register {
        Register::General(reg) => reg,
        _ => {
            let err = AssemblyError::new(
                format!("Expected general-purpose register (r0-r15) after {} mnemonic, found '{:?}'", movt_token.lexeme, dest_register),
                dt.line,
                dt.column,
                Some(dt.column + dt.lexeme.len()),
                dt.source,
            );
            return Err(err.into());
        }
    };

    let NodeKind::HexLiteral(hl @ HexLiteral::U16(_imm16)) = parse_imm16(&dt, tokens)?.kind else { unreachable!() };

    let node = Node {
        kind: NodeKind::Instruction(Instruction::Movt { dest: reg, value: hl }),
        token: movt_token,
    };

    Ok(node)
}

fn parse_movw<'src>(movs_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let (dest_register, dt) = parse_register(&movs_token, tokens)?;

    let reg = match dest_register {
        Register::General(reg) => reg,
        _ => {
            let err = AssemblyError::new(
                format!("Expected general-purpose register (r0-r15) after {} mnemonic, found '{:?}'", movs_token.lexeme, dest_register),
                dt.line,
                dt.column,
                Some(dt.column + dt.lexeme.len()),
                dt.source,
            );
            return Err(err.into());
        }
    };

    let NodeKind::HexLiteral(hl @ HexLiteral::U16(_imm16)) = parse_imm16(&dt, tokens)?.kind else { unreachable!() };

    let node = Node {
        kind: NodeKind::Instruction(Instruction::Movw { dest: reg, value: hl }),
        token: movs_token,
    };

    Ok(node)
}

fn parse_movs<'src>(movs_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let (dest_register, dt) = parse_register(&movs_token, tokens)?;

    let dest_register = match dest_register {
        Register::General(reg) if (reg as u8) < 8 => reg,
        _ => {
            let err = AssemblyError::new(
                format!("Expected general-purpose register (r0-r7) after {} mnemonic, found '{:?}'", movs_token.lexeme, dest_register),
                dt.line,
                dt.column,
                Some(dt.column + dt.lexeme.len()),
                dt.source,
            );
            return Err(err.into());
        }
    };

    let maybe_imm8 = tokens.next().ok_or_else(|| {
        AssemblyError::new(
            format!("Expected immediate value after register in {} instruction", movs_token.lexeme),
            dt.line,
            dt.column,
            Some(dt.column + dt.lexeme.len()),
            dt.source,
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
    let (dest_register, dt) = parse_register(&adds_token, tokens)?;

    let dest_register = match dest_register {
        Register::General(reg) if (reg as u8) < 8 => reg,
        _ => {
            let err = AssemblyError::new(
                format!("Expected general-purpose register (r0-r7) after {} mnemonic, found '{:?}'", adds_token.lexeme, dest_register),
                dt.line,
                dt.column,
                Some(dt.column + dt.lexeme.len()),
                dt.source,
            );
            return Err(err.into());
        }
    };

    let maybe_imm8 = tokens.next().ok_or_else(|| {
        AssemblyError::new(
            format!("Expected immediate value after register in {} instruction", adds_token.lexeme),
            dt.line,
            dt.column,
            Some(dt.column + dt.lexeme.len()),
            dt.source,
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

fn parse_branch<'src>(branch_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let maybe_label = tokens.next().ok_or_else(|| {
        AssemblyError::new(
            format!("Expected label after {} mnemonic, found EOF", branch_token.lexeme),
            branch_token.line,
            branch_token.column,
            Some(branch_token.column + branch_token.lexeme.len()),
            branch_token.source,
        )
    })?;

    let TokenKind::Label = maybe_label.kind else {
        let err = AssemblyError::new(
            format!("Expected label after {} mnemonic, found {} '{}'", branch_token.lexeme, maybe_label.kind, maybe_label.lexeme),
            maybe_label.line,
            maybe_label.column,
            Some(maybe_label.column + maybe_label.lexeme.len()),
            maybe_label.source,
        );
        return Err(err.into());
    };

    let label = maybe_label;

    let node = Node {
        kind: NodeKind::Instruction(Instruction::Branch { label: label.lexeme.to_string() }),
        token: branch_token,
    };

    Ok(node)
}