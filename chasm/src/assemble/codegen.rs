//! Machine code generation for chasm assembly source code.
//! 
//! ARMv7 generated using https://developer.arm.com/documentation/ddi0403/ee.

use std::{collections::{HashMap, HashSet}, ops::Deref};

use anyhow::{bail, Result};

use crate::assemble::{
    AssemblerPatch, AssemblyAst, AssemblyModule, BaseOffset, BranchPatch,
    BranchWithLinkPatch, Condition, GeneralRegister, HexLiteral, ImportPatch,
    Instruction, LinkerPatch, ModuleName, NodeKind, LabelNewOffsetPatch, PatchSize,
    PoppableRegister, PseudoInstruction, PushableRegister, RawPatch, RefKind,
    ThumbAddrPseudoPatch
};

const REF_PLACEHOLDER: u32 = 0x21436587;

pub(crate) fn codegen(modname: impl AsRef<str>, ast: AssemblyAst<'_>) -> Result<AssemblyModule> {
    let mut code = Vec::new();
    let mut labels = HashMap::new();
    let mut local_patches = HashMap::new();
    let mut definitions = HashMap::new();
    let mut pub_definitions = HashMap::new();
    let mut imports = HashSet::new();
    let mut linker_patches = Vec::new();

    // generate valid code
    for node in ast.nodes.into_iter() {
        match node.kind {
            NodeKind::HexLiteral(lit) => {
                generate_hex_literal(&lit, &mut code);
            }
            NodeKind::Instruction(instr) => {
                generate_instruction(&instr, &mut code, &labels, &mut local_patches, &mut linker_patches)?;
            }
            NodeKind::PseudoInstruction(instr) => {
                generate_pseudo_instruction(
                    instr,
                    &mut code,
                    &labels,
                    &mut local_patches,
                    &mut definitions,
                    &mut pub_definitions,
                    &mut imports
                )?;
            }
            NodeKind::Label => {
                let label = &node.token.lexeme[1..]; // strip '@'

                if let Some(old_addr) = labels.get(label) {
                    bail!("Duplicate label '{label}', old address is {:02X}, new address is {:02X}", old_addr.deref(), code.len());
                }

                labels.insert(label.to_string(), BaseOffset(code.len()));
            }
            NodeKind::LabelRef(ref_kind) => {
                match ref_kind {
                    RefKind::LocalRef => {
                        let reference = &node.token.lexeme[1..]; // strip '&'
                        match labels.get(reference) {
                            Some(&addr) => {
                                let patch = LinkerPatch::LabelNewOffset(LabelNewOffsetPatch {
                                    patch_at: code.len().into(),
                                    patch_size: PatchSize::U32,
                                    unpatched_value: *addr,
                                });
                                linker_patches.push(patch);
                                generate_ref(*addr as u32, &mut code);
                            }
                            None => {
                                // We won't emit a linker patch until we know if the label is present in the module or not
                                let patch = AssemblerPatch::Raw(RawPatch { patch_at: code.len().into() });
                                match local_patches.get_mut(reference) {
                                    Some(patches) => patches.push(patch),
                                    None => {
                                        local_patches.insert(reference.to_string(), vec![patch]);
                                    }
                                }

                                generate_ref(REF_PLACEHOLDER, &mut code);
                            }
                        }
                    }
                    RefKind::ModuleRef(module_ref) => {
                        if ! imports.contains(&module_ref.module) {
                            bail!("Module '{}' has not been imported", module_ref.module.0);
                        }

                        let patch = LinkerPatch::Import(ImportPatch {
                            patch_at: code.len().into(),
                            patch_size: PatchSize::U32,
                            import_module: module_ref,
                        });

                        linker_patches.push(patch);
                        generate_ref(REF_PLACEHOLDER, &mut code);
                    }
                }
            }
            NodeKind::DefinedRef => {
                let reference = &node.token.lexeme[1..]; // strip '$'
                match definitions.get(reference) {
                    Some(&hex_lit) => generate_hex_literal(&hex_lit, &mut code),
                    None => {
                        // No patch for these, we assume they're at the beginning of the module for now
                        bail!("Undefined definition '{reference}'");
                    }
                }
            }
            other => todo!("{other:?}"),
        }
    }

    // resolve forward references and do local patches
    for (r, patches) in local_patches.into_iter() {
        let target_addr = match labels.get(&r) {
            Some(&addr) => *addr as u32,
            None => bail!("Undefined reference '&{r}'"),
        };

        for patch in patches {
            match patch {
                AssemblerPatch::Raw(raw_patch) => {
                    let place = raw_patch.patch_at;
                    let bytes = target_addr.to_le_bytes();
                    code[*place..*place + 4].copy_from_slice(&bytes);

                    linker_patches.push(LinkerPatch::LabelNewOffset(LabelNewOffsetPatch {
                        patch_at: place,
                        patch_size: PatchSize::U32,
                        unpatched_value: target_addr as usize,
                    }));
                }
                AssemblerPatch::Branch(branch_patch) => {
                    let place = branch_patch.patch_at;
                    let mut branch_bytes = Vec::new();

                    generate_branch(
                        &branch_patch.reference,
                        &branch_patch.cond,
                        &mut branch_bytes,
                        &labels,
                        &mut HashMap::new(),
                        Some(place),
                    )?;

                    code[*place..*place + branch_bytes.len()].copy_from_slice(&branch_bytes);
                    linker_patches.push(LinkerPatch::BranchWithNewOffset(branch_patch));
                }
                AssemblerPatch::BranchWithLink(branch_patch) => {
                    let place = branch_patch.patch_at;
                    let mut branch_bytes = Vec::new();

                    generate_branch_with_link(
                        &branch_patch.reference,
                        &branch_patch.cond,
                        &mut branch_bytes,
                        &labels,
                        &mut HashMap::new(),
                        Some(place),
                    )?;

                    code[*place..*place + branch_bytes.len()].copy_from_slice(&branch_bytes);
                    linker_patches.push(LinkerPatch::BranchLinkWithNewOffset(branch_patch));
                }
                AssemblerPatch::ThumbAddrPseudo(thumb_patch) => {
                    let place = thumb_patch.patch_at;
                    let mut thumb_bytes = Vec::new();

                    generate_thumb_addr_pseudo(
                        &thumb_patch.reference,
                        &mut thumb_bytes,
                        &labels,
                        &mut HashMap::new(),
                        Some(place),
                    )?;

                    code[*place..*place + thumb_bytes.len()].copy_from_slice(&thumb_bytes);
                    linker_patches.push(LinkerPatch::ThumbAddrPseudoWithNewOffset(thumb_patch));
                }
            }
        }
    }

    let modname = modname.as_ref().to_string();
    Ok(AssemblyModule { modname, code, labels, definitions, imports, pub_definitions, linker_patches })
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
    pseudo: PseudoInstruction,
    output: &mut Vec<u8>,
    labels: &HashMap<String, BaseOffset>,
    unresolved_refs: &mut HashMap<String, Vec<AssemblerPatch>>,
    definitions: &mut HashMap<String, HexLiteral>,
    pub_definitions: &mut HashMap<String, HexLiteral>,
    imports: &mut HashSet<ModuleName>,
) -> Result<()> {
    match pseudo {
        PseudoInstruction::Define { identifier, hex_literal } => {
            generate_define(identifier, hex_literal, definitions, pub_definitions)?;
        }
        PseudoInstruction::DefinePub { identifier, hex_literal } => {
            generate_define_pub(identifier, hex_literal, definitions, pub_definitions)?;
        }
        PseudoInstruction::Import { module_name } => {
            generate_import(module_name, imports)?;
        }
        PseudoInstruction::Mov { reg, hex_literal } => {
            generate_mov(reg, hex_literal, output)?;
        }
        PseudoInstruction::PadWithTo { pad_with, pad_to } => {
            let not_as_patch = None;
            generate_pad_with_to(pad_with, pad_to, output, labels, unresolved_refs, not_as_patch)?;
        }
        PseudoInstruction::ThumbAddr { reference } => {
            let not_as_patch = None;
            generate_thumb_addr_pseudo(&reference, output, labels, unresolved_refs, not_as_patch)?;
        }
    }

    Ok(())
}

#[allow(clippy::unusual_byte_groupings)]
fn generate_instruction(
    instr: &Instruction,
    output: &mut Vec<u8>,
    labels: &HashMap<String, BaseOffset>,
    unresolved_refs: &mut HashMap<String, Vec<AssemblerPatch>>,
    linker_patches: &mut Vec<LinkerPatch>,
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
        // A7.7.36 EOR(register)
        Instruction::Eors { dest, src } => {
            // Encoding T1
            if ((*dest as u8) | (*src as u8)) > 7 {
                bail!("Only registers R0-R7 are supported for EORS instruction in Thumb-1 T1 encoding");
            }

            let mut base_instr = 0b01000000_01000000u16;
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
        // A7.7.101 PUSH
        Instruction::Push { registers } => {
            let is_t1_encoding = registers.iter().all(|reg| match reg {
                PushableRegister::General(r) => *r as u8 <= 7,
                PushableRegister::LR => true,
            });

            if is_t1_encoding {
                // Encoding T1
                let mut base_instr = 0b1011010_0_00000000u16;
                let mut reg_list_bits = 0u16;

                for reg in registers.iter() {
                    let bit_pos = match reg {
                        PushableRegister::General(r) => *r as u8,
                        PushableRegister::LR => 8,
                    };

                    assert!(bit_pos <= 8);
                    reg_list_bits |= 1 << bit_pos;
                }

                base_instr |= reg_list_bits;

                output.extend(&base_instr.to_le_bytes());
            } else {
                todo!()
            }
        }
        // A7.7.99 POP
        Instruction::Pop { registers } => {
            use PoppableRegister::*;
            let is_t1_encoding = registers.iter().all(|reg| match reg {
                General(r) => *r as u8 <= 7,
                PC  => true,
                LR => false,
            });

            if is_t1_encoding {
                // Encoding T1
                let mut base_instr = 0b1011110_0_00000000u16;
                let mut reg_list_bits = 0u16;

                for reg in registers.iter() {
                    let bit_pos = match reg {
                        General(r) => *r as u8,
                        PC => 8,
                        LR => unreachable!("LR cannot be popped in T1 encoding"),
                    };

                    assert!(bit_pos <= 8);
                    reg_list_bits |= 1 << bit_pos;
                }

                base_instr |= reg_list_bits;

                output.extend(&base_instr.to_le_bytes());
            } else {
                todo!()
            }
        }
        // A7.7.12 B
        Instruction::Branch { reference, cond } => {
            let not_as_patch = None;
            let branch_instr_addr = output.len();
            let should_linker_patch = generate_branch(reference, cond, output, labels, unresolved_refs, not_as_patch)?;
            if should_linker_patch {
                let patch = LinkerPatch::BranchWithNewOffset(BranchPatch {
                    patch_at: BaseOffset(branch_instr_addr),
                    reference: reference.to_string(),
                    cond: *cond,
                });
                linker_patches.push(patch);
            }
        }
        // A7.7.18 BL
        Instruction::BranchWithLink { reference, cond } => {
            let not_as_patch = None;
            let branch_instr_addr = output.len();
            let should_linker_patch = generate_branch_with_link(reference, cond, output, labels, unresolved_refs, not_as_patch)?;
            if should_linker_patch {
                let patch = LinkerPatch::BranchLinkWithNewOffset(BranchWithLinkPatch {
                    patch_at: BaseOffset(branch_instr_addr),
                    reference: reference.to_string(),
                    cond: *cond,
                });
                linker_patches.push(patch);
            }
        }
        // A7.7.20 BX
        Instruction::BranchExchange { branch_reg } => {
            let reg = branch_reg.to_u8() << 3;
            let mut base_instr = 0b01000111_00000000u16;
            base_instr |= reg as u16;

            output.extend(&base_instr.to_le_bytes());
        }
    }

    Ok(())
}

fn generate_import(module_name: ModuleName, imports: &mut HashSet<ModuleName>) -> Result<()> {
    if imports.contains(&module_name) {
        bail!("Duplicate import for module '{module_name}'");
    } else {
        imports.insert(module_name);
        Ok(())
    }
}

fn generate_mov(
    reg: GeneralRegister,
    hex_literal: HexLiteral,
    output: &mut Vec<u8>,
) -> Result<()> {
    match hex_literal {
        HexLiteral::U32(value) => {
            let imm16_low = (value & 0xFFFF) as u16;
            let imm16_high = ((value >> 16) & 0xFFFF) as u16;

            let movw_instr = Instruction::Movw {
                dest: reg,
                value: HexLiteral::U16(imm16_low),
            };
            let movt_instr = Instruction::Movt {
                dest: reg,
                value: HexLiteral::U16(imm16_high),
            };

            // None of the label/patch references are used
            generate_instruction(&movw_instr, output, &HashMap::new(), &mut HashMap::new(), &mut Vec::new())?;
            generate_instruction(&movt_instr, output, &HashMap::new(), &mut HashMap::new(), &mut Vec::new())
        }
        v @ HexLiteral::U16(..) => {
            let movw_instr = Instruction::Movw {
                dest: reg,
                value: v,
            };

            // None of the label/patch references are used
            generate_instruction(&movw_instr, output, &HashMap::new(), &mut HashMap::new(), &mut Vec::new())
        },
        v @ HexLiteral::U8(..) => {
            let movs_instr = Instruction::Movs {
                dest: reg,
                value: v,
            };

            // None of the label/patch references are used
            generate_instruction(&movs_instr, output, &HashMap::new(), &mut HashMap::new(), &mut Vec::new())
        },
    }

}

fn generate_define_pub(
    identifier: String,
    hex_literal: HexLiteral,
    definitions: &HashMap<String, HexLiteral>,
    pub_definitions: &mut HashMap<String, HexLiteral>,
) -> Result<()> {
    if definitions.contains_key(&identifier) {
        bail!("Duplicate definition of '{identifier}'");
    } else if pub_definitions.contains_key(&identifier) {
        bail!("Duplicate public definition of '{identifier}'");
    }

    pub_definitions.insert(identifier.to_string(), hex_literal);
    Ok(())
}

fn generate_define(
    identifier: String,
    hex_literal: HexLiteral,
    definitions: &mut HashMap<String, HexLiteral>,
    pub_definitions: &HashMap<String, HexLiteral>,
) -> Result<()> {
    if definitions.contains_key(&identifier) {
        bail!("Duplicate definition of '{identifier}'");
    } else if pub_definitions.contains_key(&identifier) {
        bail!("Duplicate public definition of '{identifier}'");
    }

    definitions.insert(identifier.to_string(), hex_literal);
    Ok(())
}

fn generate_thumb_addr_pseudo(
    reference: &str,
    output: &mut Vec<u8>,
    labels: &HashMap<String, BaseOffset>,
    unresolved_refs: &mut HashMap<String, Vec<AssemblerPatch>>,
    patch_offset: Option<BaseOffset>,
) -> Result<()> {
    let target_addr = match labels.get(&reference[1..]) {
        Some(addr) => *addr,
        None => {
            if patch_offset.is_some() {
                // we're expected to be patching - if the label isn't present now then it never will be
                bail!("Undefined reference '{reference}'")
            }

            // forward reference - store the info and use a dummy target to be patched later
            let patch = AssemblerPatch::ThumbAddrPseudo(ThumbAddrPseudoPatch {
                patch_at: output.len().into(),
                reference: reference.to_string(),
            });

            let stripped_ref = reference[1..].to_string();
            match unresolved_refs.get_mut(&stripped_ref) {
                Some(patches) => patches.push(patch),
                None => {
                    unresolved_refs.insert(stripped_ref, vec![patch]);
                }
            }

            BaseOffset(REF_PLACEHOLDER as usize)
        }
    };

    let target_as_thumb_addr = *target_addr | 0x01;
    output.extend(&(target_as_thumb_addr as u32).to_le_bytes());
    Ok(())
}

fn generate_pad_with_to(
    pad_with: u8,
    pad_to: BaseOffset,
    output: &mut Vec<u8>,

    // Unused now, but we'll likely allow padding to label addresses in the near future,
    // which implies reference lookups and patching.
    _labels: &HashMap<String, BaseOffset>,
    _unresolved_refs: &mut HashMap<String, Vec<AssemblerPatch>>,
    _patch_offset: Option<BaseOffset>,
) -> Result<()> {
    if output.len() > *pad_to as usize {
        bail!("Cannot pad to {:08X}, target address is behind current address {:016X}", *pad_to, output.len());
    }

    let bytes_needed = *pad_to as usize - output.len();
    output.extend(vec![pad_with; bytes_needed]);

    Ok(())
}
fn generate_branch_with_link(
    reference: &str,
    cond: &Option<Condition>,
    output: &mut Vec<u8>,
    labels: &HashMap<String, BaseOffset>,
    unresolved_refs: &mut HashMap<String, Vec<AssemblerPatch>>,
    patch_offset: Option<BaseOffset>,
) -> Result<bool> {
    // Cortex-M4F PC is 4 bytes ahead of the branch instruction
    let instr_pc = patch_offset.as_ref()
        .map_or_else(|| output.len() + 4, |offset| **offset + 4);

    let mut should_linker_patch = false;

    let target_addr = match labels.get(&reference[1..]) {
        Some(addr) => {
            should_linker_patch = true;
            *addr
        }
        None => {
            if patch_offset.is_some() {
                // we're expected to be patching - if the label isn't present now then it never will be
                bail!("Undefined reference '{reference}'")
            }

            // forward reference - store the info and use a dummy target to be patched later
            let patch = AssemblerPatch::BranchWithLink(BranchWithLinkPatch {
                patch_at: BaseOffset(output.len()),
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

            BaseOffset(output.len() + 4)
        }
    };

    let offset_bytes = *target_addr as isize - instr_pc as isize;
    if offset_bytes % 2 != 0 {
        bail!("Branch target address must be halfword-aligned, but label at '{reference}' is at byte address {:02X}", *target_addr);
    }

    match cond {
        Some(_c) => {
            todo!()
        }
        None => {
            // Encoding T1
            if !(-16777216..=16777214).contains(&offset_bytes) {
                bail!("Branch target '{reference}' is too far away (offset {offset_bytes}), must be in range [-16777216, +16777214] bytes");
            }
            let mut top_half = 0b11110_0_0000000000;
            let mut bot_half = 0b11_0_1_0_00000000000;
            
            let offset_halfwords = (offset_bytes >> 1) as usize;
            let imm11 = (offset_halfwords & 0x7FF) as u16;
            let imm10 = ((offset_halfwords >> 11) & 0x3FF) as u16;
            let s = if offset_bytes < 0 { 1u16 } else { 0u16 };
            let i2 = ((offset_halfwords >> 21) & 0x01) as u16;
            let j2 = ((!i2) ^ s) & 0x01;
            let i1 = ((offset_halfwords >> 22) & 0x01) as u16;
            let j1 = ((!i1) ^ s) & 0x01;

            top_half |= imm10;
            top_half |= s << 10;

            bot_half |= imm11;
            bot_half |= j2 << 11;
            bot_half |= j1 << 13;

            output.extend(&top_half.to_le_bytes());
            output.extend(&bot_half.to_le_bytes());
        }
    }

    Ok(should_linker_patch)
}

fn generate_branch(
    reference: &str,
    cond: &Option<Condition>,
    output: &mut Vec<u8>,
    labels: &HashMap<String, BaseOffset>,
    unresolved_refs: &mut HashMap<String, Vec<AssemblerPatch>>,
    patch_offset: Option<BaseOffset>,
) -> Result<bool> {
    // Cortex-M4F PC is 4 bytes ahead of the branch instruction
    let instr_pc = patch_offset.as_ref()
        .map_or_else(|| output.len() + 4, |offset| **offset + 4);

    let mut should_linker_patch = false;

    let target_addr = match labels.get(&reference[1..]) {
        Some(addr) => {
            should_linker_patch = true;
            *addr
        }
        None => {
            if patch_offset.is_some() {
                // we're expected to be patching - if the label isn't present now then it never will be
                bail!("Undefined reference '{reference}'")
            }

            // forward reference - store the info and use a dummy target to be patched later
            let patch = AssemblerPatch::Branch(BranchPatch {
                patch_at: BaseOffset(output.len()),
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

            BaseOffset(output.len() + 4)
        }
    };

    let offset_bytes = *target_addr as isize - instr_pc as isize;
    if offset_bytes % 2 != 0 {
        bail!("Branch target address must be halfword-aligned, but label at '{reference}' is at byte address {:02X}", *target_addr);
    }

    let offset_halfwords = offset_bytes / 2;
    match cond {
        Some(c) => {
            // Encoding T1
            if !(-256..=254).contains(&offset_bytes) {
                bail!("Branch target '{reference}' is too far away (offset {offset_bytes}), must be in range [-256, +254] bytes");
            }
            let masked_offset = (offset_halfwords as u16) & 0xFF;
            let cond = (*c as u16) << 8;

            let base_instr = 0b1101_0000_00000000;
            let encoded_branch = base_instr | cond | masked_offset;
            output.extend(&encoded_branch.to_le_bytes());
        }
        None => {
            // Encoding T2
            if !(-2048..=2046).contains(&offset_bytes) {
                bail!("Branch target '{reference}' is too far away (offset {offset_bytes}), must be in range [-2048, +2046] bytes");
            }
            let masked_offset = (offset_halfwords as u16) & 0x7FF; // first 11 bits

            let base_instr = 0b11100_00000000000;
            let encoded_branch = base_instr | masked_offset;
            output.extend(&encoded_branch.to_le_bytes());
        }
    }

    Ok(should_linker_patch)
}