//! Machine code generation for chasm assembly source code.
//! 
//! ARMv7 generated using https://developer.arm.com/documentation/ddi0403/ee.

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
        // A7.7.9 AND(register)
        Instruction::Ands { dest, src } => {
            // Encoding T1
            if ((dest as u8) | (src as u8)) > 7 {
                bail!("Only registers R0-R7 are supported for ANDS instruction in T1 encoding");
            }

            let mut base_instr = 0b01000000_00000000u16;
            let dest_bits = dest as u16 & 0x07;
            let src_bits = (src as u16 & 0x07) << 3;
            base_instr |= dest_bits | src_bits;

            output.extend(&base_instr.to_le_bytes());
        }
        // A7.7.161 STR(immediate)
        Instruction::Str { dest_addr_reg, src } => {
            if (dest_addr_reg as u8) < 8 && (src as u8) < 8 {
                // Encoding T1
                let mut base_instr = 0b01100000_00000000u16;
                let src_bits = src as u16 & 0x07;
                let dest_bits = (dest_addr_reg as u16 & 0x07) << 3;
                base_instr |= dest_bits | src_bits;

                output.extend(&base_instr.to_le_bytes());
            } else {
                // Encoding T3
                todo!()
            }

        }
        // A7.7.92 ORRS(register)
        Instruction::Orrs { dest, src } => {
            if (dest as u8) < 8 && (src as u8) < 8 {
                // Encoding T1
                let mut base_instr = 0b01000011_00000000u16;
                let dest_bits = dest as u16 & 0x07;
                let src_bits = (src as u16 & 0x07) << 3;
                base_instr |= dest_bits | src_bits;

                output.extend(&base_instr.to_le_bytes());
            } else {
                // Encoding T2
                todo!()
            }

        }
        // A7.7.43 LDR(immediate)
        Instruction::Ldr { dest, src } => {
            if (dest as u8) < 8 && (src as u8) < 8 {
                // Encoding T1
                let mut base_instr = 0b01101000_00000000u16;
                let dest_bits = dest as u16 & 0x07;
                let src_bits = (src as u16 & 0x07) << 3;
                base_instr |= dest_bits | src_bits;

                output.extend(&base_instr.to_le_bytes());
            } else {
                // Encoding T3
                let mut base_hw1 = 0b11111000_11010000u16;
                let mut base_hw2 = 0b00000000_00000000u16;

                let src = src as u16 & 0x0F;
                let dest = dest as u16 & 0x0F;
                base_hw1 |= src;
                base_hw2 |= dest << 12;

                output.extend(&base_hw1.to_le_bytes());
                output.extend(&base_hw2.to_le_bytes());
            }
        }
        // A7.7.79 MOVT
        Instruction::Movt { dest, value } => {
            // Encoding T1
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
                _ => bail!("Only immediate 16-bit values are supported for MOVT"),
            };
        }
        // A7.7.76 MOV(immediate)
        Instruction::Movw { dest, value } => {
            match value {
                // Encoding T3
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
                _ => bail!("Only immediate 16-bit values are supported for MOVW"),
            };
        }
        // A7.7.76 MOV(immediate)
        Instruction::Movs { dest, value } => {
            match value {
                // Encoding T1
                HexLiteral::U8(imm8) => {
                    let mut base_instr = 0b00100_000_00000000;
                    let dest_and_value = ((dest as u16) << 8) | (imm8 as u16);
                    base_instr |= dest_and_value;

                    output.extend(&base_instr.to_le_bytes());
                }
                _ => bail!("Only immediate 8-bit values are supported for MOVS"),
            };
        }
        // A7.7.3 ADD(immediate)
        Instruction::Adds { dest, value } => {
            match value {
                // Encoding T2
                HexLiteral::U8(imm8) => {
                    let mut base_instr = 0b00110_000_00000000;
                    let dest_and_value = ((dest as u16) << 8) | (imm8 as u16);
                    base_instr |= dest_and_value;

                    output.extend(&base_instr.to_le_bytes());
                }
                _ => unimplemented!("Only immediate 8-bit values are supported for ADDS"),
            };
        }
        // A7.7.12 B
        Instruction::Branch { label, cond } => {
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
            match cond {
                Some(c) => {
                    // Encoding T1 only at the moment
                    if !(-256..=254).contains(&offset_halfwords) {
                        bail!("Branch target '{label}' is too far away (offset {offset_halfwords}), must be in range [-256, +254] halfwords");
                    }
                    let masked_offset = (offset_halfwords as u16) & 0xFF;
                    let cond = (c as u16) << 8;

                    let base_instr = 0b1101_0000_00000000;
                    let encoded_branch = base_instr | cond | masked_offset;
                    output.extend(&encoded_branch.to_le_bytes());
                }
                None => {
                    // Encoding T2
                    if !(-2048..=2046).contains(&offset_halfwords) {
                        bail!("Branch target '{label}' is too far away (offset {offset_halfwords}), must be in range [-2048, +2046] halfwords");
                    }
                    let masked_offset = (offset_halfwords as u16) & 0x7FF; // first 11 bits

                    let base_instr = 0b11100_00000000000;
                    let encoded_branch = base_instr | masked_offset;
                    output.extend(&encoded_branch.to_le_bytes());
                }
            }
        }
    }

    Ok(())
}