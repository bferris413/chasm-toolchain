//! Parser for chasm assembly source code.

use std::collections::HashSet;

use anyhow::Result;

use crate::assemble::helpers::normalize_to_ascii_lower;
use crate::assemble::{AssemblyAst, AssemblyError, AssemblyTokens, BranchableRegister, Condition, HexLiteral, Instruction, Node, NodeKind, PoppableRegister, PseudoInstruction, PushableRegister, Register, Token, TokenKind, token};

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
                let node = parse_hex_literal_u16(token);
                nodes.push(node);
            },
            TokenKind::HexLiteralU8 => {
                let node = parse_hex_literal_u8(token);
                nodes.push(node);
            },
            TokenKind::Identifier => {
                let node = parse_instruction(token, &mut tokens)?;
                nodes.push(node);
            }
            TokenKind::PseudoIdentifier => {
                let node = parse_pseudo_instruction(token, &mut tokens)?;
                nodes.push(node);
            }
            TokenKind::Label => {
                let node = parse_label(token)?;
                nodes.push(node);
            }
            TokenKind::Ref => {
                let node = parse_ref(token)?;
                nodes.push(node);
            }
            other => {
                let err = AssemblyError::new(
                    format!("Unexpected token kind '{}'", other),
                    token.line,
                    token.column,
                    Some(token.column + token.lexeme.len()),
                    token.source,
                );
                return Err(err.into());
            }
        }
    }

    Ok(AssemblyAst { nodes })
}

fn parse_dereference<'src>(
    prev_token: Token<'src>,
    tokens: &mut dyn Iterator<Item = Token<'src>>
) -> Result<(Register, Token<'src>)> {
    let lbracket = next_symbol_token_as(&[TokenKind::LBracket], "[", &prev_token, tokens)?;
    let (reg, rt) = parse_register(&lbracket, tokens)?;
    let _rbracket = next_symbol_token_as(&[TokenKind::RBracket], "]", &rt, tokens)?;

    Ok((reg, rt))
}

fn parse_hex_literal<'src>(prev_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let hex_literal_token = next_symbol_token_as(&[TokenKind::HexLiteralU8, TokenKind::HexLiteralU16, TokenKind::HexLiteralU32], "hex literal", &prev_token, tokens)?;

    match hex_literal_token.kind {
        TokenKind::HexLiteralU8 => Ok(parse_hex_literal_u8(hex_literal_token)),
        TokenKind::HexLiteralU16 => Ok(parse_hex_literal_u16(hex_literal_token)),
        TokenKind::HexLiteralU32 => Ok(parse_hex_literal_u32(hex_literal_token)),
        _ => unreachable!(),
    }    
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

fn parse_hex_literal_u16(token: Token<'_>) -> Node<'_> {
    let value = u16::from_str_radix(&token.lexeme[1..], 16)
        .expect("parsing u16 hex literal");
    let node = Node {
        kind: NodeKind::HexLiteral(HexLiteral::U16(value)),
        token,
    };

    node
}

fn parse_hex_literal_u8(token: Token<'_>) -> Node<'_> {
    let value = u8::from_str_radix(&token.lexeme[1..], 16)
        .expect("parsing u8 hex literal");
    let node = Node {
        kind: NodeKind::HexLiteral(HexLiteral::U8(value)),
        token,
    };

    node
}

fn parse_ref<'src>(ref_token: Token<'src>) -> Result<Node<'src>> {
    assert!(ref_token.lexeme.starts_with('&'));
    assert!(ref_token.lexeme.len() > 1);

    let node = Node {
        kind: NodeKind::Ref,
        token: ref_token,
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
        "adds" => parse_adds(token, tokens),
        "ands" => parse_ands(token, tokens),
        "b" => parse_branch(token, tokens),
        "beq" => parse_branch_eq(token, tokens),
        "bl" => parse_branch_with_link(token, tokens),
        "bx" => parse_branch_exchange(token, tokens),
        "eors" => parse_eors(token, tokens),
        "ldr" => parse_ldr(token, tokens),
        "movs" => parse_movs(token, tokens),
        "movt" => parse_movt(token, tokens),
        "movw" => parse_movw(token, tokens),
        "orrs" => parse_orrs(token, tokens),
        "pop" => parse_pop(token, tokens),
        "push" => parse_push(token, tokens),
        "str" => parse_str(token, tokens),
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

fn parse_pseudo_instruction<'src>(token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    assert!(matches!(token.kind, TokenKind::PseudoIdentifier));

    match token.lexeme {
        "thumb-addr!" => parse_thumb_addr_pseudo(token, tokens),
        "pad-with-to!" => parse_pad_with_to_pseudo(token, tokens),
        "define!" => parse_define_pseudo(token, tokens),
        other => {
            let err = AssemblyError::new(
                format!("Found unknown pseudo-instruction '{}'", token.lexeme),
                token.line,
                token.column,
                Some(token.column + other.len()),
                token.source,
            );
            Err(err.into())
        }
    }
}

fn parse_pad_with_to_pseudo<'src>(pad_with_to_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let lparen = next_symbol_token_as(&[TokenKind::LParen], "(", &pad_with_to_token, tokens)?;

    let maybe_byte_literal = tokens.next().ok_or_else(|| {
        AssemblyError::new(
            format!("Expected 8-bit hex literal after '{}', found EOF", lparen.lexeme),
            lparen.line,
            lparen.column,
            Some(lparen.column + lparen.lexeme.len()),
            lparen.source,
        )
    })?;

    let pad_with_u8 = parse_hex_literal_u8(maybe_byte_literal);
    let pad_with = match pad_with_u8.kind {
        NodeKind::HexLiteral(HexLiteral::U8(b)) => b,
        _ => unreachable!(),
    };

    let comma = next_symbol_token_as(&[TokenKind::Comma], ",", &pad_with_u8.token, tokens)?;
    let maybe_4_byte_literal = tokens.next().ok_or_else(|| {
        AssemblyError::new(
            format!("Expected 8-bit hex literal after '{}', found EOF", comma.lexeme),
            comma.line,
            comma.column,
            Some(comma.column + comma.lexeme.len()),
            comma.source,
        )
    })?;

    let pad_to_u32 = parse_hex_literal_u32(maybe_4_byte_literal);
    let pad_to = match pad_to_u32.kind {
        NodeKind::HexLiteral(HexLiteral::U32(bytes)) => bytes,
        _ => unreachable!(),
    };

    let _rparen = next_symbol_token_as(&[TokenKind::RParen], ")", &pad_to_u32.token, tokens)?;

    let node = Node {
        kind: NodeKind::PseudoInstruction(PseudoInstruction::PadWithTo { pad_with, pad_to }),
        token: pad_with_to_token,
    };

    Ok(node)
}

fn parse_thumb_addr_pseudo<'src>(thumb_addr_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let lparen = next_symbol_token_as(&[TokenKind::LParen], "(", &thumb_addr_token, tokens)?;
    let reference = next_symbol_token_as(&[TokenKind::Ref], "&reference", &lparen, tokens)?;
    let _rparen = next_symbol_token_as(&[TokenKind::RParen], ")", &reference, tokens)?;

    let node = Node {
        kind: NodeKind::PseudoInstruction(PseudoInstruction::ThumbAddr { reference: reference.lexeme.to_string() }),
        token: thumb_addr_token,
    };

    Ok(node)
}

fn parse_define_pseudo<'src>(define_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let lparen = next_symbol_token_as(&[TokenKind::LParen], "(", &define_token, tokens)?;
    let identifier = next_symbol_token_as(&[TokenKind::Identifier], "identifier", &lparen, tokens)?;
    let comma = next_symbol_token_as(&[TokenKind::Comma], ",", &identifier, tokens)?;
    let hex_literal = parse_hex_literal(comma, tokens)?;
    let _rparen = next_symbol_token_as(&[TokenKind::RParen], ")", &hex_literal.token, tokens)?;

    let NodeKind::HexLiteral(hx) = hex_literal.kind else { unreachable!() };
    let node = Node {
        kind: NodeKind::PseudoInstruction(PseudoInstruction::Define { identifier: identifier.lexeme.to_string(), hex_literal: hx }),
        token: define_token,
    };

    Ok(node)
}

fn parse_imm16<'src>(prev_token: &Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let imm16 = next_symbol_token_as(&[TokenKind::HexLiteralU16], "u16 hex literal", prev_token, tokens)?;
    let imm16_node = parse_hex_literal_u16(imm16);

    Ok(imm16_node)
}

fn parse_register<'src>(prev_token: &Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<(Register, Token<'src>)> {
    let maybe_register = next_symbol_token_as(&[TokenKind::Identifier], "register", prev_token, tokens)?;
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

fn parse_branchable_register<'src>(prev_token: &Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<(BranchableRegister, Token<'src>)> {
    let (reg, rt) = parse_register(prev_token, tokens)?;
    let gen_reg = BranchableRegister::try_from(reg).map_err(|e| {
        AssemblyError::new(
            format!("Invalid branchable register '{}' after '{}': {e}", rt.lexeme, prev_token.lexeme),
            rt.line,
            rt.column,
            Some(rt.column + rt.lexeme.len()),
            rt.source,
        )

    })?;

    Ok((gen_reg, rt))
}

fn parse_poppable_register<'src>(prev_token: &Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<(PoppableRegister, Token<'src>)> {
    let (reg, rt) = parse_register(prev_token, tokens)?;
    let gen_reg = PoppableRegister::try_from(reg).map_err(|e| {
        AssemblyError::new(
            format!("Invalid poppable register '{}' after '{}': {e}", rt.lexeme, prev_token.lexeme),
            rt.line,
            rt.column,
            Some(rt.column + rt.lexeme.len()),
            rt.source,
        )

    })?;

    Ok((gen_reg, rt))
}

fn parse_pushable_register<'src>(prev_token: &Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<(PushableRegister, Token<'src>)> {
    let (reg, rt) = parse_register(prev_token, tokens)?;
    let gen_reg = PushableRegister::try_from(reg).map_err(|e| {
        AssemblyError::new(
            format!("Invalid pushable register '{}' after '{}': {e}", rt.lexeme, prev_token.lexeme),
            rt.line,
            rt.column,
            Some(rt.column + rt.lexeme.len()),
            rt.source,
        )

    })?;

    Ok((gen_reg, rt))
}

fn parse_eors<'src>(eors_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let (dest_register, dt) = parse_register(&eors_token, tokens)?;

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
        kind: NodeKind::Instruction(Instruction::Eors { dest: dest_register, src: src_register }),
        token: eors_token,
    };

    Ok(node)
}

fn parse_ands<'src>(ands_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let (dest_register, dt) = parse_register(&ands_token, tokens)?;

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
        kind: NodeKind::Instruction(Instruction::Ands { dest: dest_register, src: src_register }),
        token: ands_token,
    };

    Ok(node)
}

fn parse_pop<'src>(pop_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let _lcurly = next_symbol_token_as(&[TokenKind::LCurly], "{", &pop_token, tokens)?;
    let mut pop_regs = HashSet::with_capacity(13);

    loop {
        let (pop_reg, rt) = parse_poppable_register(&pop_token, tokens)?;
        if !pop_regs.insert(pop_reg) {
            let err = AssemblyError::new(
                format!("Register '{}' specified multiple times in pop instruction", rt.lexeme),
                rt.line,
                rt.column,
                Some(rt.column + rt.lexeme.len()),
                rt.source,
            );
            return Err(err.into());
        }

        let maybe_next = tokens.next();
        match maybe_next {
            Some(next_token) if next_token.kind == TokenKind::Comma => {
                // continue parsing registers
            }
            Some(next_token) if next_token.kind == TokenKind::RCurly => {
                // end of push register list
                break;
            }
            Some(next_token) => {
                let err = AssemblyError::new(
                    format!("Expected ',' or '}}' after register '{}', found '{}'", rt.lexeme, next_token.lexeme),
                    next_token.line,
                    next_token.column,
                    Some(next_token.column + next_token.lexeme.len()),
                    next_token.source,
                );
                return Err(err.into());
            }
            None => {
                let err = AssemblyError::new(
                    format!("Expected ',' or '}}' after register '{}', found EOF", rt.lexeme),
                    rt.line,
                    rt.column,
                    Some(rt.column + rt.lexeme.len()),
                    rt.source,
                );
                return Err(err.into());
            }
        }
    }

    if pop_regs.is_empty() {
        let err = AssemblyError::new(
            format!("Pop instruction requires at least one register to pop"),
            pop_token.line,
            pop_token.column,
            Some(pop_token.column + pop_token.lexeme.len()),
            pop_token.source,
        );
        return Err(err.into());
    }

    if pop_regs.len() > 14 { // r0..=r12 + lr/pc
        let err = AssemblyError::new(
            format!("Pop instruction cannot pop more than 14 registers, found {}", pop_regs.len()),
            pop_token.line,
            pop_token.column,
            Some(pop_token.column + pop_token.lexeme.len()),
            pop_token.source,
        );
        return Err(err.into());
    }

    if pop_regs.contains(&PoppableRegister::LR) && pop_regs.contains(&PoppableRegister::PC) {
        let err = AssemblyError::new(
            format!("Pop instruction cannot pop both LR and PC registers"),
            pop_token.line,
            pop_token.column,
            Some(pop_token.column + pop_token.lexeme.len()),
            pop_token.source,
        );
        return Err(err.into());
    }

    let pop_node = Node {
        kind: NodeKind::Instruction(Instruction::Pop { registers: pop_regs }),
        token: pop_token,
    };

    Ok(pop_node)
}

fn parse_push<'src>(push_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let _lcurly = next_symbol_token_as(&[TokenKind::LCurly], "{", &push_token, tokens)?;
    let mut push_regs = HashSet::with_capacity(13);

    loop {
        let (push_reg, rt) = parse_pushable_register(&push_token, tokens)?;
        if !push_regs.insert(push_reg) {
            let err = AssemblyError::new(
                format!("Register '{}' specified multiple times in push instruction", rt.lexeme),
                rt.line,
                rt.column,
                Some(rt.column + rt.lexeme.len()),
                rt.source,
            );
            return Err(err.into());
        }

        let maybe_next = tokens.next();
        match maybe_next {
            Some(next_token) if next_token.kind == TokenKind::Comma => {
                // continue parsing registers
            }
            Some(next_token) if next_token.kind == TokenKind::RCurly => {
                // end of push register list
                break;
            }
            Some(next_token) => {
                let err = AssemblyError::new(
                    format!("Expected ',' or '}}' after register '{}', found '{}'", rt.lexeme, next_token.lexeme),
                    next_token.line,
                    next_token.column,
                    Some(next_token.column + next_token.lexeme.len()),
                    next_token.source,
                );
                return Err(err.into());
            }
            None => {
                let err = AssemblyError::new(
                    format!("Expected ',' or '}}' after register '{}', found EOF", rt.lexeme),
                    rt.line,
                    rt.column,
                    Some(rt.column + rt.lexeme.len()),
                    rt.source,
                );
                return Err(err.into());
            }
        }
    }

    if push_regs.is_empty() {
        let err = AssemblyError::new(
            format!("Push instruction requires at least one register to push"),
            push_token.line,
            push_token.column,
            Some(push_token.column + push_token.lexeme.len()),
            push_token.source,
        );
        return Err(err.into());
    }

    if push_regs.len() > 14 { // r0..=r12 + lr
        let err = AssemblyError::new(
            format!("Push instruction cannot push more than 14 registers, found {}", push_regs.len()),
            push_token.line,
            push_token.column,
            Some(push_token.column + push_token.lexeme.len()),
            push_token.source,
        );
        return Err(err.into());
    }

    let push_node = Node {
        kind: NodeKind::Instruction(Instruction::Push { registers: push_regs }),
        token: push_token,
    };

    Ok(push_node)
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

    let (dest_addr_register, dt) = parse_dereference(st, tokens)?;
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

    let (src_register, st) = parse_dereference(dt, tokens)?;
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

    let hex_u8 = next_symbol_token_as(&[TokenKind::HexLiteralU8], "8-bit hex literal", &dt, tokens)?;
    let NodeKind::HexLiteral(hl @ HexLiteral::U8(_imm8)) = parse_hex_literal_u8(hex_u8).kind else { unreachable!() };

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

    let hex_u8 = next_symbol_token_as(&[TokenKind::HexLiteralU8], "8-bit hex literal", &dt, tokens)?;
    let NodeKind::HexLiteral(hl @ HexLiteral::U8(_imm8)) = parse_hex_literal_u8(hex_u8).kind else { unreachable!() };

    let node = Node {
        kind: NodeKind::Instruction(Instruction::Adds { dest: dest_register, value: hl }),
        token: adds_token,
    };

    Ok(node)
}

fn parse_branch<'src>(branch_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let reference = next_symbol_token_as(&[TokenKind::Ref], "&reference", &branch_token, tokens)?;
    let node = Node {
        kind: NodeKind::Instruction(Instruction::Branch { reference: reference.lexeme.to_string(), cond: None }),
        token: branch_token,
    };

    Ok(node)
}

fn parse_branch_with_link<'src>(branch_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let reference = next_symbol_token_as(&[TokenKind::Ref], "&reference", &branch_token, tokens)?;
    let node = Node {
        kind: NodeKind::Instruction(Instruction::BranchWithLink { reference: reference.lexeme.to_string(), cond: None }),
        token: branch_token,
    };

    Ok(node)
}

fn parse_branch_exchange<'src>(bx_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let (gen_reg, _rt) = parse_branchable_register(&bx_token, tokens)?;
    let node = Node {
        kind: NodeKind::Instruction(Instruction::BranchExchange { branch_reg: gen_reg }),
        token: bx_token,
    };

    Ok(node)
}

fn parse_branch_eq<'src>(branch_eq_token: Token<'src>, tokens: &mut dyn Iterator<Item = Token<'src>>) -> Result<Node<'src>> {
    let mut branch_node = parse_branch(branch_eq_token, tokens)?;
    let NodeKind::Instruction(Instruction::Branch { ref mut cond, .. }) = branch_node.kind else {
        unreachable!()
    };
    *cond = Some(Condition::Eq);

    Ok(branch_node)
}

fn next_symbol_token_as<'src>(
    kinds: &[TokenKind],
    sym: &str,
    prev_token: &Token<'src>,
    tokens: &mut dyn Iterator<Item = Token<'src>>
) -> Result<Token<'src>> {
    let Some(maybe_kind) = tokens.next() else {
        let err = AssemblyError::new(
            format!("Expected '{sym}' after '{}', found EOF", prev_token.lexeme),
            prev_token.line,
            prev_token.column,
            Some(prev_token.column + prev_token.lexeme.len()),
            prev_token.source,
        );
        return Err(err.into());
    };

    if !kinds.contains(&maybe_kind.kind) {
        let err = AssemblyError::new(
            format!("Expected '{sym}' after '{}', found '{}'", prev_token.lexeme, maybe_kind.lexeme),
            maybe_kind.line,
            maybe_kind.column,
            Some(maybe_kind.column + maybe_kind.lexeme.len()),
            maybe_kind.source,
        );
        return Err(err.into());
    };

    let token = maybe_kind;

    Ok(token)
}