//! Machine code generation for chasm assembly source code.
//! 
//! ARMv7 generated using https://developer.arm.com/documentation/ddi0403/ee.

use std::collections::HashMap;

use anyhow::{bail, Result};

use crate::assemble::{AssemblyAst, Condition, HexLiteral, Instruction, MachineCode, NodeKind, PseudoInstruction};

const REF_PLACEHOLDER: u32 = 0x21436587;

pub(crate) fn codegen(ast: AssemblyAst<'_>) -> Result<MachineCode> {
    let mut code_bytes = Vec::new();
    let mut labels = HashMap::new();
    let mut unresolved_refs = HashMap::new();

    // generate valid code and collect forward refs
    for node in ast.nodes.iter() {
        match &node.kind {
            NodeKind::HexLiteral(lit) => {
                generate_hex_literal(lit, &mut code_bytes);
            }
            NodeKind::Instruction(instr) => {
                generate_instruction(&instr, &mut code_bytes, &labels, &mut unresolved_refs)?;
            }
            NodeKind::PseudoInstruction(instr) => {
                generate_pseudo_instruction(&instr, &mut code_bytes, &labels, &mut unresolved_refs)?;
            }
            NodeKind::Label => {
                let label = &node.token.lexeme[1..]; // strip '@'

                if let Some(old_addr) = labels.get(label) {
                    bail!("Duplicate label '{label}', old address is {old_addr:02X}, new address is {:02X}", code_bytes.len());
                }

                labels.insert(label.to_string(), code_bytes.len());
            }
            NodeKind::Ref => {
                let reference = &node.token.lexeme[1..]; // strip '&'
                match labels.get(reference) {
                    Some(&addr) => generate_ref(addr as u32, &mut code_bytes),
                    None => {
                        let patch = Patch::Raw(RawPatch { offset: code_bytes.len() });
                        match unresolved_refs.get_mut(reference) {
                            Some(patches) => patches.push(patch),
                            None => {
                                unresolved_refs.insert(reference.to_string(), vec![patch]);
                            }
                        }

                        generate_ref(REF_PLACEHOLDER, &mut code_bytes);
                    }
                }
            }
            other => todo!("{other:?}"),
        }
    }

    // resolve forward refs and patch binary
    for (r, patches) in unresolved_refs.iter_mut() {
        let target_addr = match labels.get(r) {
            Some(&addr) => addr as u32,
            None => bail!("Undefined reference '&{r}'"),
        };

        for patch in patches.iter() {
            match patch {
                Patch::Raw(raw_patch) => {
                    let place = raw_patch.offset;
                    let bytes = target_addr.to_le_bytes();
                    code_bytes[place..place + 4].copy_from_slice(&bytes);
                }
                Patch::Branch(branch_patch) => {
                    let place = branch_patch.offset;
                    let mut branch_bytes = Vec::new();

                    generate_branch(
                        &branch_patch.reference,
                        &branch_patch.cond,
                        &mut branch_bytes,
                        &labels,
                        &mut HashMap::new(),
                        Some(place),
                    )?;

                    code_bytes[place..place + branch_bytes.len()].copy_from_slice(&branch_bytes);
                }
                Patch::ThumbAddrPseudo(thumb_patch) => {
                    let place = thumb_patch.offset;
                    let mut thumb_bytes = Vec::new();

                    generate_thumb_addr_pseudo(
                        &thumb_patch.reference,
                        &mut thumb_bytes,
                        &labels,
                        &mut HashMap::new(),
                        Some(place),
                    )?;

                    code_bytes[place..place + thumb_bytes.len()].copy_from_slice(&thumb_bytes);
                }
            }
        }
    }

    Ok(MachineCode { bytes: code_bytes, labels })
}

fn generate_hex_literal(lit: &HexLiteral, output: &mut Vec<u8>) {
    match lit {
        HexLiteral::U32(value) => output.extend(&value.to_le_bytes()),
        HexLiteral::U16(value) => output.extend(&value.to_le_bytes()),
        HexLiteral::U8(value) => output.push(*value),
    }
}

fn generate_ref(addr: u32, output: &mut Vec<u8>) {
    output.extend(&addr.to_le_bytes());
}
#[allow(clippy::unusual_byte_groupings)]
fn generate_pseudo_instruction(
    pseudo: &PseudoInstruction,
    output: &mut Vec<u8>,
    labels: &HashMap<String, usize>,
    unresolved_refs: &mut HashMap<String, Vec<Patch>>,
) -> Result<()> {
    match pseudo {
        PseudoInstruction::ThumbAddr { reference } => {
            let not_as_patch = None;
            generate_thumb_addr_pseudo(reference, output, labels, unresolved_refs, not_as_patch)?;
        }
        PseudoInstruction::PadWithTo { pad_with, pad_to } => {
            let not_as_patch = None;
            generate_pad_with_to(*pad_with, *pad_to, output, labels, unresolved_refs, not_as_patch)?;
        }
    }

    Ok(())
}

#[allow(clippy::unusual_byte_groupings)]
fn generate_instruction(
    instr: &Instruction,
    output: &mut Vec<u8>,
    labels: &HashMap<String, usize>,
    unresolved_refs: &mut HashMap<String, Vec<Patch>>,
) -> Result<()> {
    if output.len() % 2 != 0 {
        bail!("Attempted to generate unaligned {instr:?} at {:02X}", output.len());
    }
    match instr {
        // A7.7.9 AND(register)
        Instruction::Ands { dest, src } => {
            // Encoding T1
            if ((*dest as u8) | (*src as u8)) > 7 {
                bail!("Only registers R0-R7 are supported for ANDS instruction in T1 encoding");
            }

            let mut base_instr = 0b01000000_00000000u16;
            let dest_bits = *dest as u16 & 0x07;
            let src_bits = (*src as u16 & 0x07) << 3;
            base_instr |= dest_bits | src_bits;

            output.extend(&base_instr.to_le_bytes());
        }
        // A7.7.161 STR(immediate)
        Instruction::Str { dest_addr_reg, src } => {
            if (*dest_addr_reg as u8) < 8 && (*src as u8) < 8 {
                // Encoding T1
                let mut base_instr = 0b01100000_00000000u16;
                let src_bits = *src as u16 & 0x07;
                let dest_bits = (*dest_addr_reg as u16 & 0x07) << 3;
                base_instr |= dest_bits | src_bits;

                output.extend(&base_instr.to_le_bytes());
            } else {
                // Encoding T3
                todo!()
            }

        }
        // A7.7.92 ORRS(register)
        Instruction::Orrs { dest, src } => {
            if (*dest as u8) < 8 && (*src as u8) < 8 {
                // Encoding T1
                let mut base_instr = 0b01000011_00000000u16;
                let dest_bits = *dest as u16 & 0x07;
                let src_bits = (*src as u16 & 0x07) << 3;
                base_instr |= dest_bits | src_bits;

                output.extend(&base_instr.to_le_bytes());
            } else {
                // Encoding T2
                todo!()
            }

        }
        // A7.7.43 LDR(immediate)
        Instruction::Ldr { dest, src } => {
            if (*dest as u8) < 8 && (*src as u8) < 8 {
                // Encoding T1
                let mut base_instr = 0b01101000_00000000u16;
                let dest_bits = *dest as u16 & 0x07;
                let src_bits = (*src as u16 & 0x07) << 3;
                base_instr |= dest_bits | src_bits;

                output.extend(&base_instr.to_le_bytes());
            } else {
                // Encoding T3
                let mut base_hw1 = 0b11111000_11010000u16;
                let mut base_hw2 = 0b00000000_00000000u16;

                let src = *src as u16 & 0x0F;
                let dest = *dest as u16 & 0x0F;
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
                    let dest = *dest as u16 & 0x0F;
                    base_hw2 |= dest << 8;

                    let val = imm16.to_be_bytes();
                    let imm4 = (val[0] >> 4) as u16 & 0x0F;
                    let i = ((val[0] >> 3) as u16 & 0x01) << 10;
                    let imm3 = (val[0] as u16 & 0x07) << 12;
                    let imm8 = val[1] as u16 & 0xFF;

                    base_hw1 |= imm4 | i;
                    base_hw2 |= imm3 | imm8;
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
                    let dest = *dest as u16 & 0x0F;
                    base_hw2 |= dest << 8;

                    let val = imm16.to_be_bytes();
                    let imm4 = (val[0] >> 4) as u16 & 0x0F;
                    let i = ((val[0] >> 3) as u16 & 0x01) << 10;
                    let imm3 = (val[0] as u16 & 0x07) << 12;
                    let imm8 = val[1] as u16 & 0xFF;

                    base_hw1 |= imm4 | i;
                    base_hw2 |= imm3 | imm8;
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
                    let dest_and_value = ((*dest as u16) << 8) | (*imm8 as u16);
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
                    let dest_and_value = ((*dest as u16) << 8) | (*imm8 as u16);
                    base_instr |= dest_and_value;

                    output.extend(&base_instr.to_le_bytes());
                }
                _ => unimplemented!("Only immediate 8-bit values are supported for ADDS"),
            };
        }
        // A7.7.12 B
        Instruction::Branch { reference, cond } => {
            let not_as_patch = None;
            generate_branch(reference, cond, output, labels, unresolved_refs, not_as_patch)?;
        }
    }

    Ok(())
}

fn generate_thumb_addr_pseudo(
    reference: &str,
    output: &mut Vec<u8>,
    labels: &HashMap<String, usize>,
    unresolved_refs: &mut HashMap<String, Vec<Patch>>,
    patch_offset: Option<usize>,
) -> Result<()> {
    let target_addr = match labels.get(&reference[1..]) {
        Some(addr) => *addr,
        None => {
            if patch_offset.is_some() {
                // we're expected to be patching - if the label isn't present now then it never will be
                bail!("Undefined reference '{reference}'")
            }

            // forward reference - store the info and use a dummy target to be patched later
            let patch = Patch::ThumbAddrPseudo(ThumbAddrPseudoPatch {
                offset: output.len(),
                reference: reference.to_string(),
            });

            let stripped_ref = reference[1..].to_string();
            match unresolved_refs.get_mut(&stripped_ref) {
                Some(patches) => patches.push(patch),
                None => {
                    unresolved_refs.insert(stripped_ref, vec![patch]);
                }
            }

            REF_PLACEHOLDER as usize
        }
    };

    let target_as_thumb_addr = target_addr | 0x01;
    output.extend(&(target_as_thumb_addr as u32).to_le_bytes());
    Ok(())
}

fn generate_pad_with_to(
    pad_with: u8,
    pad_to: u32,
    output: &mut Vec<u8>,

    // Unused now, but we'll likely allow padding to label addresses in the near future,
    // which implies reference lookups and patching.
    _labels: &HashMap<String, usize>,
    _unresolved_refs: &mut HashMap<String, Vec<Patch>>,
    _patch_offset: Option<usize>,
) -> Result<()> {
    if output.len() > pad_to as usize {
        bail!("Cannot pad to {pad_to:08X}, target address is behind current address {:016X}", output.len());
    }

    let bytes_needed = pad_to as usize - output.len();
    output.extend(vec![pad_with; bytes_needed]);

    Ok(())
}

fn generate_branch(
    reference: &str,
    cond: &Option<Condition>,
    output: &mut Vec<u8>,
    labels: &HashMap<String, usize>,
    unresolved_refs: &mut HashMap<String, Vec<Patch>>,
    patch_offset: Option<usize>,
) -> Result<()> {
    // Cortex-M4F PC is 4 bytes ahead of the branch instruction
    let instr_pc = patch_offset.as_ref()
        .map_or_else(|| output.len() + 4, |offset| offset + 4);

    let target_addr = match labels.get(&reference[1..]) {
        Some(addr) => *addr,
        None => {
            if patch_offset.is_some() {
                // we're expected to be patching - if the label isn't present now then it never will be
                bail!("Undefined reference '{reference}'")
            }

            // forward reference - store the info and use a dummy target to be patched later
            let patch = Patch::Branch(BranchPatch {
                offset: output.len(),
                reference: reference.to_string(),
                cond: *cond,
            });

            let stripped_ref = reference[1..].to_string();
            match unresolved_refs.get_mut(&stripped_ref) {
                Some(patches) => patches.push(patch),
                None => {
                    unresolved_refs.insert(stripped_ref, vec![patch]);
                }
            }

            output.len() + 4
        }
    };

    let offset_bytes = dbg!(dbg!(target_addr as isize) - dbg!(instr_pc as isize));
    if offset_bytes % 2 != 0 {
        bail!("Branch target address must be halfword-aligned, but label at '{reference}' is at byte address {target_addr:02X}");
    }

    let offset_halfwords = dbg!(offset_bytes / 2);
    match cond {
        Some(c) => {
            // Encoding T1 only at the moment
            if !(-256..=254).contains(&offset_halfwords) {
                bail!("Branch target '{reference}' is too far away (offset {offset_halfwords}), must be in range [-256, +254] halfwords");
            }
            let masked_offset = (offset_halfwords as u16) & 0xFF;
            let cond = (*c as u16) << 8;

            let base_instr = 0b1101_0000_00000000;
            let encoded_branch = base_instr | cond | masked_offset;
            output.extend(&encoded_branch.to_le_bytes());
        }
        None => {
            // Encoding T2
            if !(-2048..=2046).contains(&offset_halfwords) {
                bail!("Branch target '{reference}' is too far away (offset {offset_halfwords}), must be in range [-2048, +2046] halfwords");
            }
            let masked_offset = dbg!((offset_halfwords as u16) & 0x7FF); // first 11 bits

            let base_instr = 0b11100_00000000000;
            let encoded_branch = dbg!(base_instr | masked_offset);
            output.extend(dbg!(&encoded_branch.to_le_bytes()));
        }
    }

    Ok(())
}

#[derive(Debug)]
enum Patch {
    Raw(RawPatch),
    Branch(BranchPatch),
    ThumbAddrPseudo(ThumbAddrPseudoPatch),
}

#[derive(Debug)]
struct RawPatch {
    offset: usize,
}

#[derive(Debug)]
struct BranchPatch {
    offset: usize,
    reference: String,
    cond: Option<Condition>,
}

#[derive(Debug)]
struct ThumbAddrPseudoPatch {
    offset: usize,
    reference: String,
}