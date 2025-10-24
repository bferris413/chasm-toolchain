//! Machine code generation for chasm assembly source code.

use std::collections::HashMap;

use anyhow::{bail, Result};

use crate::assemble::{AssemblyAst, HexLiteral, Instruction, MachineCode, NodeKind};

pub(crate) fn codegen(ast: AssemblyAst<'_>) -> Result<MachineCode> {
    let mut code_bytes = Vec::new();
    let mut labels = HashMap::new();

    for node in ast.nodes {
        match node.kind {
            NodeKind::HexLiteral(lit) => {
                generate_hex_literal(lit, &mut code_bytes);
            }
            NodeKind::Instruction(instr) => {
                generate_instruction(instr, &mut code_bytes, &labels)?;
            }
            NodeKind::Label => {
                let label = node.token.lexeme;

                if labels.contains_key(label) {
                    let old_addr = labels.get(label).unwrap();
                    bail!("Duplicate label '{label}', old address is {old_addr:02X}, new address is {:02X}", code_bytes.len());
                }

                labels.insert(label.to_string(), code_bytes.len());
            }
            other => todo!("{other:?}"),
        }
    }

    Ok(MachineCode { bytes: code_bytes, labels })
}

fn generate_hex_literal(lit: HexLiteral, output: &mut Vec<u8>) {
    match lit {
        HexLiteral::U32(value) => output.extend(&value.to_le_bytes()),
        HexLiteral::U16(value) => output.extend(&value.to_le_bytes()),
        HexLiteral::U8(value) => output.push(value),
    }
}

#[allow(clippy::unusual_byte_groupings)]
fn generate_instruction(
    instr: Instruction,
    output: &mut Vec<u8>,
    labels: &HashMap<String, usize>
) -> Result<()> {
    if output.len() % 2 != 0 {
        bail!("Attempted to generate unaligned {instr:?} at {:02X}", output.len());
    }
    match instr {
        Instruction::Ldr { dest, src } => {
            let mut base_instr = 0b01101000_00000000u16;
            let dest_bits = dest as u16 & 0x07;
            let src_bits = (src as u16 & 0x07) << 3;
            base_instr |= dest_bits | src_bits;

            output.extend(&base_instr.to_le_bytes());
        }
        Instruction::Movt { dest, value } => {
            match value {
                HexLiteral::U16(imm16) => {
                    let mut base_hw1 = 0b11110010_11000000u16;
                    let mut base_hw2 = 0b00000000_00000000u16;
                    let dest = dest as u16 & 0x0F;
                    base_hw2 |= dest << 8;
                    println!("dest: {dest:032b}");

                    let val = imm16.to_be_bytes();
                    let imm4 = (val[0] >> 4) as u16 & 0x0F;
                    let i = ((val[0] >> 3) as u16 & 0x01) << 10;
                    let imm3 = (val[0] as u16 & 0x07) << 12;
                    let imm8 = val[1] as u16 & 0xFF;
                    println!("imm4: {imm4:032b}");
                    println!("i___: {i:032b}");
                    println!("imm3: {imm3:032b}");
                    println!("imm8: {imm8:032b}");

                    base_hw1 |= imm4 | i;
                    base_hw2 |= imm3 | imm8;
                    println!("final: {base_hw1:016b}_{base_hw2:016b}");
                    output.extend(&base_hw1.to_le_bytes());
                    output.extend(&base_hw2.to_le_bytes());
                }
                _ => unimplemented!("Only immediate 16-bit values are supported for MOVT"),
            };
        }
        Instruction::Movw { dest, value } => {
            match value {
                HexLiteral::U16(imm16) => {
                    let mut base_hw1 = 0b11110010_01000000u16;
                    let mut base_hw2 = 0b00000000_00000000u16;
                    let dest = dest as u16 & 0x0F;
                    base_hw2 |= dest << 8;
                    println!("dest: {dest:032b}");

                    let val = imm16.to_be_bytes();
                    let imm4 = (val[0] >> 4) as u16 & 0x0F;
                    let i = ((val[0] >> 3) as u16 & 0x01) << 10;
                    let imm3 = (val[0] as u16 & 0x07) << 12;
                    let imm8 = val[1] as u16 & 0xFF;
                    println!("imm4: {imm4:032b}");
                    println!("i___: {i:032b}");
                    println!("imm3: {imm3:032b}");
                    println!("imm8: {imm8:032b}");

                    base_hw1 |= imm4 | i;
                    base_hw2 |= imm3 | imm8;
                    println!("final: {base_hw1:016b}_{base_hw2:016b}");
                    output.extend(&base_hw1.to_le_bytes());
                    output.extend(&base_hw2.to_le_bytes());
                }
                _ => unimplemented!("Only immediate 16-bit values are supported for MOVW"),
            };
        }
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
        Instruction::Branch { label } => {
            // Cortex-M4F PC is 4 bytes ahead of the current instruction
            let current_pc = output.len() + 4;
            let target_addr = match labels.get(&label) {
                Some(addr) => *addr,
                None => bail!("Undefined label '{label}' (forward references aren't supported yet)"),
            };

            let offset_bytes = target_addr as isize - current_pc as isize;
            if offset_bytes % 2 != 0 {
                bail!("Branch target address must be halfword-aligned, but label '{label}' is at byte address {target_addr:02X}");
            }

            let offset_halfwords = offset_bytes / 2;
            if !(-2048..=2046).contains(&offset_halfwords) {
                bail!("Branch target '{label}' is too far away (offset {offset_halfwords}), must be in range [-2048, +2046] halfwords");
            }

            let masked_offset = (offset_halfwords as u16) & 0x7FF; // first 11 bits

            let base_instr = 0b11100_00000000000;
            let encoded_branch = base_instr | masked_offset;
            output.extend(&encoded_branch.to_le_bytes());
        }
    }

    Ok(())
}