//! Machine code generation for chasm assembly source code.

use anyhow::Result;

use crate::assemble::{AssemblyAst, HexLiteral, Instruction, MachineCode, NodeKind};

pub(crate) fn codegen(ast: AssemblyAst<'_>) -> Result<MachineCode> {
    let mut code_bytes = Vec::new();
    for node in ast.nodes {
        match node.kind {
            NodeKind::HexLiteral(lit) => {
                generate_hex_literal(lit, &mut code_bytes);
            }
            NodeKind::Instruction(instr) => {
                generate_instruction(instr, &mut code_bytes);
            }
            _ => todo!(),
        }
    }
    
    Ok(MachineCode { bytes: code_bytes })
}

fn generate_hex_literal(lit: HexLiteral, output: &mut Vec<u8>) {
    match lit {
        HexLiteral::U32(value) => output.extend(&value.to_le_bytes()),
        HexLiteral::U16(value) => output.extend(&value.to_le_bytes()),
        HexLiteral::U8(value) => output.push(value),
    }
}

fn generate_instruction(instr: Instruction, output: &mut Vec<u8>) {
    match instr {
        Instruction::Movs { dest, value } => {
            match value {
                HexLiteral::U8(imm8) => {
                    let mut base_instr = 0b00100_000_00000000;
                    let dest_and_value = ((dest as u16) << 8) | (imm8 as u16);
                    base_instr |= dest_and_value;

                    output.extend(&base_instr.to_le_bytes());
                }
                _ => unimplemented!("Only immediate 8-bit values are supported for MOVS"),
            };
        }
        Instruction::Adds { dest, value } => {
            match value {
                HexLiteral::U8(imm8) => {
                    let mut base_instr = 0b00110_000_00000000;
                    let dest_and_value = ((dest as u16) << 8) | (imm8 as u16);
                    base_instr |= dest_and_value;

                    output.extend(&base_instr.to_le_bytes());
                }
                _ => unimplemented!("Only immediate 8-bit values are supported for ADDS"),
            };
        }
    }
}