//! Parser for chasm assembly source code.

use anyhow::Result;

use crate::assemble::{AssemblyAst, AssemblyTokens, Node, NodeKind, TokenKind};

pub(crate) fn parse(tokens: AssemblyTokens<'_>) -> Result<AssemblyAst<'_>> {
    let tokens = tokens.tokens;

    let mut nodes = Vec::new();
    for token in tokens.into_iter() {
        match token.kind {
            TokenKind::HexLiteralU32 => {
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

                let node = Node {
                    kind: NodeKind::HexLiteralU32(value),
                    token,
                };
                nodes.push(node);
            },
            TokenKind::HexLiteralU16 => {
                let value = u16::from_str_radix(&token.lexeme[1..], 16)?;
                let node = Node {
                    kind: NodeKind::HexLiteralU16(value),
                    token,
                };
                nodes.push(node);
            },
            TokenKind::HexLiteralU8 => {
                let value = u8::from_str_radix(&token.lexeme[1..], 16)?;
                let node = Node {
                    kind: NodeKind::HexLiteralU8(value),
                    token,
                };
                nodes.push(node);
            },
        }

    }

    Ok(AssemblyAst { nodes })
}