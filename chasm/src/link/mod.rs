use std::collections::HashMap;

use anyhow::{Result, anyhow, bail};
use crate::assemble::AssemblyModule;
use crate::assemble::BaseOffset;
use crate::assemble::BranchPatch;
use crate::assemble::BranchWithLinkPatch;
use crate::assemble::HexLiteral;
use crate::assemble::ImportDefinitionRefPatch;
use crate::assemble::ImportLabelRefPatch;
use crate::assemble::LabelNewOffsetPatch;
use crate::assemble::LinkerPatch;
use crate::assemble::ModuleRef;
use crate::assemble::PatchSize;
use crate::assemble::RefKind;
use crate::assemble::ThumbAddrPseudoPatch;
use crate::assemble::codegen;

const LINK_ERR: &str = "Link error:";

/// Links multiple assembly modules into a single binary.
/// 
/// Current restrictions:
/// - There must be exactly one module named "main"
/// - The main module must be the first module in the list
/// - Remaining modules are linked in order
pub fn link(modules: Vec<AssemblyModule>) -> Result<Binary> {
    if modules.is_empty() {
        bail!("{LINK_ERR} no modules to link");
    }

    let _ = main_module_index(&modules).map_err(|e| anyhow!("{LINK_ERR} {e}"))?;

    let modules = modules.into_iter();
    let mut main_bin = Vec::new();
    let mut modules_to_offsets      = HashMap::new();
    let mut module_pub_definitions  = HashMap::new();
    let mut all_module_labels       = HashMap::new();
    let mut unresolved_defs         = HashMap::new();
    let mut unresolved_labels       = HashMap::new();
    let mut unresolved_branches     = HashMap::new();
    let mut unresolved_branch_with_links = HashMap::new();

    for mut module in modules {
        if modules_to_offsets.contains_key(&module.modname) {
            bail!("{LINK_ERR} duplicate module '{}'", module.modname);
        }

        // fix up label offsets within the module to point to new location in binary
        for (_, base_offset) in module.labels.iter_mut() {
            **base_offset += main_bin.len();
        }

        if ! module.pub_definitions.is_empty() {
            module_pub_definitions.insert(module.modname.clone(), module.pub_definitions);
        }

        for patch in module.linker_patches {
            match patch {
                LinkerPatch::LabelNewOffset(new_offset_patch) => {
                    patch_label_offset(&mut module.code, new_offset_patch, main_bin.len())?;
                }
                LinkerPatch::ImportLabelRef(import_label_patch) => {
                    if let Some(patch) = patch_label_import(&mut module.code, import_label_patch, &all_module_labels)? {
                        unresolved_labels.entry(module.modname.clone())
                            .or_insert_with(Vec::new)
                            .push(patch);
                    }
                },
                LinkerPatch::ImportDefinitionRef(import_def_patch) => {
                    if let Some(patch) = patch_def_import(&mut module.code, import_def_patch, &module_pub_definitions)? {
                        unresolved_defs.entry(module.modname.clone())
                            .or_insert_with(Vec::new)
                            .push(patch);
                    }
                },
                LinkerPatch::BranchWithImport(branch_patch) => {
                    if let Some(patch) = patch_branch(&mut module.code, &all_module_labels, branch_patch, main_bin.len())? {
                        unresolved_branches.entry(module.modname.clone())
                            .or_insert_with(Vec::new)
                            .push(patch);
                    }
                },
                LinkerPatch::BranchLinkWithImport(branch_with_link_patch) => {
                    if let Some(patch) = patch_branch_with_link(&mut module.code, &all_module_labels, branch_with_link_patch, main_bin.len())? {
                        unresolved_branch_with_links.entry(module.modname.clone())
                            .or_insert_with(Vec::new)
                            .push(patch);
                    }
                },
                LinkerPatch::ThumbAddrPseudoWithNewOffset(thumb_addr_pseudo_patch) => {
                    patch_thumb_addr_offset(&mut module.code, &module.labels, thumb_addr_pseudo_patch)?;
                },
            }
        }

        if ! module.labels.is_empty() {
            all_module_labels.insert(module.modname.clone(), module.labels);
        }

        modules_to_offsets.insert(module.modname, main_bin.len());
        main_bin.extend(module.code);
    }

    for (module, patches) in unresolved_defs {
        let module_code = &mut main_bin[modules_to_offsets[&module]..];
        for import_patch in patches {
            let maybe_unresolved_patch = patch_def_import(module_code, import_patch, &module_pub_definitions)
                .map_err(|e| anyhow!("{LINK_ERR} couldn't patch module '{}': {e}", module))?;
            
            if let Some(unresolved_patch) = maybe_unresolved_patch {
                bail!("{LINK_ERR} unresolved patch in module '{}': {:?}", module, unresolved_patch);
            }
        }
    }

    for (module, patches) in unresolved_labels {
        let module_code = &mut main_bin[modules_to_offsets[&module]..];
        for label_patch in patches {
            let maybe_unresolved_patch = patch_label_import(module_code, label_patch, &all_module_labels)
                .map_err(|e| anyhow!("{LINK_ERR} couldn't patch module '{}': {e}", module))?;
            
            if let Some(unresolved_patch) = maybe_unresolved_patch {
                bail!("{LINK_ERR} unresolved patch in module '{}': {:?}", module, unresolved_patch);
            }
        }
    }

    for (module, patches) in unresolved_branches {
        let module_code = &mut main_bin[modules_to_offsets[&module]..];
        let offset_by = modules_to_offsets[&module];
        for branch_patch in patches {
            let maybe_unresolved_patch = patch_branch(module_code, &all_module_labels, branch_patch, offset_by)
                .map_err(|e| anyhow!("{LINK_ERR} couldn't patch module '{}': {e}", module))?;

            if let Some(unresolved_patch) = maybe_unresolved_patch {
                bail!("{LINK_ERR} unresolved patch in module '{}': {:?}", module, unresolved_patch);
            }
        }
    }

    for (module, patches) in unresolved_branch_with_links {
        let module_code = &mut main_bin[modules_to_offsets[&module]..];
        let offset_by = modules_to_offsets[&module];
        for branch_patch in patches {
            let maybe_unresolved_patch = patch_branch_with_link(module_code, &all_module_labels, branch_patch, offset_by)
                .map_err(|e| anyhow!("{LINK_ERR} couldn't patch module '{}': {e}", module))?;

            if let Some(unresolved_patch) = maybe_unresolved_patch {
                bail!("{LINK_ERR} unresolved patch in module '{}': {:?}", module, unresolved_patch);
            }
        }
    }

    Ok(Binary(main_bin))
}

/// Applies a definition import patch to the module code.
/// 
/// If the import cannot be resolved, this function returns the patch, presumably to be
/// attempted after all modules are processed.
fn patch_def_import(
    module_code: &mut [u8],
    import_patch: ImportDefinitionRefPatch,
    module_pub_definitions: &HashMap<String, HashMap<String, HexLiteral>>,
) -> Result<Option<ImportDefinitionRefPatch>> {
    let ImportDefinitionRefPatch { patch_at, ref_size: patch_size, import_module: ModuleRef { module, member } } = &import_patch;

    let patch_at = patch_at.0;
    if patch_at > module_code.len() {
        bail!("patch offset {patch_at:?} is out of bounds (module code length {})", module_code.len());
    } 

    let patch_size = patch_size.as_n_bytes();
    if patch_at + patch_size > module_code.len() {
        bail!("patch size {} bytes at offset {patch_at:?} exceeds module code length {}", patch_size, module_code.len());
    }

    let maybe_val = module_pub_definitions
        .get(module.as_ref())
        .and_then(|defs| defs.get(member.as_ref()));

    let Some(patch_val) = maybe_val else {
        return Ok(Some(import_patch));
    };

    match patch_val {
        HexLiteral::U32(v) => {
            let bytes = v.to_le_bytes();
            module_code[patch_at..patch_at + 4].copy_from_slice(&bytes);
        },
        HexLiteral::U16(v) => {
            let bytes = v.to_le_bytes();
            module_code[patch_at..patch_at + 2].copy_from_slice(&bytes);
        },
        HexLiteral::U8(v) => {
            module_code[patch_at] = *v;
        },
    }
    
    Ok(None)
}

fn patch_label_import(
    module_code: &mut [u8],
    import_patch: ImportLabelRefPatch,
    all_module_labels: &HashMap<String, HashMap<String, BaseOffset>>,
) -> Result<Option<ImportLabelRefPatch>> {
    let ImportLabelRefPatch { patch_at, patch_size, import_module: ModuleRef { module, member } } = &import_patch;

    let patch_at = patch_at.0;
    if patch_at > module_code.len() {
        bail!("patch offset {patch_at:?} is out of bounds (module code length {})", module_code.len());
    } 

    let patch_size = *patch_size as usize;
    if patch_at + patch_size > module_code.len() {
        bail!("patch size {} bytes at offset {patch_at:?} exceeds module code length {}", patch_size, module_code.len());
    }

    let maybe_val = all_module_labels
        .get(module.as_ref())
        .and_then(|labels| labels.get(member.as_ref()));

    let Some(patch_val) = maybe_val else {
        return Ok(Some(import_patch));
    };

    assert!(patch_size == 4, "only 32-bit label references are currently supported");
    let patch_bytes = (**patch_val as u32).to_le_bytes();
    module_code[patch_at..patch_at + 4].copy_from_slice(&patch_bytes);
    
    Ok(None)
}

fn patch_thumb_addr_offset(
    module_code: &mut Vec<u8>,
    labels: &HashMap<String, BaseOffset>,
    thumb_addr_patch: ThumbAddrPseudoPatch,
) -> Result<()> {
    let ThumbAddrPseudoPatch { patch_at, reference } = thumb_addr_patch;
    let mut thumb_bytes = Vec::new();
    let is_patch = true;

    if patch_at.0 > module_code.len() {
        bail!("patch offset {patch_at:?} is out of bounds (module code length {})", module_code.len());
    }

    codegen::generate_thumb_addr_pseudo(
        &reference,
        &mut thumb_bytes,
        &labels,
        &mut HashMap::new(),
        is_patch,
    )?;

    module_code[*patch_at..*patch_at + thumb_bytes.len()].copy_from_slice(&thumb_bytes);

    Ok(())
}

fn patch_branch_with_link(
    module_code: &mut [u8],
    all_module_labels: &HashMap<String, HashMap<String, BaseOffset>>,
    branch_patch: BranchWithLinkPatch,
    offset_by: usize
) -> Result<Option<BranchWithLinkPatch>> {
    let BranchWithLinkPatch { patch_at, reference, cond } = &branch_patch;

    if ! matches!(reference.kind, RefKind::ModuleRef(_)) {
        // presently a no-op since `reference` is local and there is no "data" section (thereby no intra-module relocation)
        return Ok(None);
    }

    let patch_at = patch_at.0;
    if patch_at > module_code.len() {
        bail!("patch offset {patch_at:?} is out of bounds (module code length {})", module_code.len());
    } 

    let new_base_offset = BaseOffset(patch_at + offset_by);
    let mut branch_bytes = Vec::new();

    let still_needs_patch = codegen::generate_branch_with_link(
        &reference,
        &cond,
        &mut branch_bytes,
        &HashMap::new(), // no local labels
        all_module_labels,
        &mut HashMap::new(),
        Some(new_base_offset),
    )?;

    if still_needs_patch {
        Ok(Some(branch_patch))
    } else {
        module_code[patch_at..patch_at + branch_bytes.len()].copy_from_slice(&branch_bytes);
        Ok(None)
    }
}

fn patch_branch(
    module_code: &mut [u8],
    all_module_labels: &HashMap<String, HashMap<String, BaseOffset>>,
    branch_patch: BranchPatch,
    offset_by: usize
) -> Result<Option<BranchPatch>> {
    let BranchPatch { patch_at, reference, cond } = &branch_patch;

    if ! matches!(reference.kind, RefKind::ModuleRef(_)) {
        // presently a no-op since `reference` is local and there is no "data" section (thereby no intra-module relocation)
        return Ok(None);
    }

    let patch_at = patch_at.0;
    if patch_at > module_code.len() {
        bail!("patch offset {patch_at:?} is out of bounds (module code length {})", module_code.len());
    } 

    let new_base_offset = BaseOffset(patch_at + offset_by);
    let mut branch_bytes = Vec::new();

    let still_needs_patch = codegen::generate_branch(
        &reference,
        &cond,
        &mut branch_bytes,
        &HashMap::new(), // no local labels
        all_module_labels,
        &mut HashMap::new(),
        Some(new_base_offset),
    )?;

    if still_needs_patch {
        Ok(Some(branch_patch))
    } else {
        module_code[patch_at..patch_at + branch_bytes.len()].copy_from_slice(&branch_bytes);
        Ok(None)
    }
}

fn patch_label_offset(
    module_code: &mut [u8],
    new_offset_patch: LabelNewOffsetPatch,
    offset_by: usize
) -> Result<()> {
    let LabelNewOffsetPatch { patch_at, patch_size, unpatched_value, .. } = new_offset_patch;

    if patch_at.0 > module_code.len() {
        bail!("patch offset {patch_at:?} is out of bounds (module code length {})", module_code.len());
    }

    if module_code[patch_at.0..].len() < patch_size as usize {
        bail!("patch size {} bytes at offset {patch_at:?} exceeds module code length {}", patch_size as usize, module_code.len());
    }

    let new_value = unpatched_value + offset_by;

    if new_value > patch_size.max_value() {
        bail!("new offset value {new_value} exceeds maximum for patch size {patch_size:?} (max {})", patch_size.max_value());
    }

    match patch_size {
        PatchSize::U8 => {
            let byte = new_value as u8;
            module_code[patch_at.0] = byte;
        }
        PatchSize::U16 => {
            let bytes = (new_value as u16).to_le_bytes();
            module_code[patch_at.0..patch_at.0 + 2].copy_from_slice(&bytes);
        }
        PatchSize::U32 => {
            let bytes = (new_value as u32).to_le_bytes();
            module_code[patch_at.0..patch_at.0 + 4].copy_from_slice(&bytes);
        }
        PatchSize::U64 => {
            let bytes = (new_value as u64).to_le_bytes();
            module_code[patch_at.0..patch_at.0 + 8].copy_from_slice(&bytes);
        }
    };

    Ok(())
}

/// Returns the index of the main module.
/// 
/// If it doesn't exist, or there's more than one main module, returns error.
fn main_module_index(modules: &[AssemblyModule]) -> Result<usize> {
    let maybe_main_i = modules.iter().position(|m| m.modname == "main");

    let Some(main_i) = maybe_main_i else {
        bail!("no main module found");
    };

    let maybe_other_main_i = modules.iter().rposition(|m| m.modname == "main");
    if matches!(maybe_other_main_i, Some(i) if i != main_i) {
        bail!("more than one main module found");
    }

    Ok(main_i)
}

#[derive(Debug)]
pub struct Binary(Vec<u8>);

#[cfg(test)]
mod tests {
    use crate::assemble::{BaseOffset, LabelNewOffsetPatch, LabelRef, MemberName, PatchSize, RefKind, RefSize};

    use super::*;

    #[test]
    fn module_list_without_main_fails_to_link() {
        let modules = vec![
            AssemblyModule { modname: "mylib1".to_string(), ..Default::default() },
            AssemblyModule { modname: "mylib2".to_string(), ..Default::default() },
        ];

        let err = link(modules).unwrap_err().to_string();

        assert!(err.contains("no main module found"), "{}", err);
    } 

    #[test]
    fn empty_module_list_fails_to_link() {
        let modules = vec![];

        let err = link(modules).unwrap_err().to_string();

        assert!(err.contains("no modules to link"), "{}", err);
    }

    #[test]
    fn module_list_with_multiple_mains_fails_to_link() {
        let modules = vec![
            AssemblyModule { modname: "main".to_string(), ..Default::default() },
            AssemblyModule { modname: "mylib2".to_string(), ..Default::default() },
            AssemblyModule { modname: "main".to_string(), ..Default::default() },
        ];

        let err = link(modules).unwrap_err().to_string();

        assert!(err.contains("more than one main module found"), "{}", err);
    }
 
    #[test]
    fn module_list_with_duplicate_module_names_fails_to_link() {
        let modules = vec![
            AssemblyModule { modname: "main".to_string(), ..Default::default() },
            AssemblyModule { modname: "mylib2".to_string(), ..Default::default() },
            AssemblyModule { modname: "mylib2".to_string(), ..Default::default() },
        ];

        let err = link(modules).unwrap_err().to_string();

        assert!(err.contains("duplicate module 'mylib2'"), "{}", err);
    }    

    #[test]
    fn module_list_with_single_main_and_only_code_is_linked() {
        let modules = vec![
            AssemblyModule { modname: "main".to_string(), code: vec![0x00, 0x00], ..Default::default() },
            AssemblyModule { modname: "mylib2".to_string(), code: vec![0x11, 0x11], ..Default::default() },
            AssemblyModule { modname: "mylib3".to_string(), code: vec![0x22, 0x22], ..Default::default() },
        ];

        let linked_bin = link(modules).unwrap();

        assert_eq!(linked_bin.0.len(), 6);
        assert_eq!(&linked_bin.0[0..2], &[0x00, 0x00]);
        assert_eq!(&linked_bin.0[2..6], &[0x11, 0x11, 0x22, 0x22]);
    }    

    // NewOffset( /* … */ ),
    // Import( /* … */ ),
    // BranchWithNewOffset( /* … */ ),
    // BranchLinkWithNewOffset( /* … */ ),
    // ThumbAddrPseudoWithNewOffset( /* … */ ),

    #[test]
    fn label_refs_are_patched_with_new_offsets() {
        let main_bin_len = 1000;
        let main_bin = vec![0u8; main_bin_len];
        let modules = vec![
            AssemblyModule { modname: "main".to_string(), code: main_bin.clone(), ..Default::default() },
            AssemblyModule { 
                modname: "lib2".to_string(),
                code: vec![0x11, 0x11, 0x11, 0x12],
                linker_patches: vec![
                    LinkerPatch::LabelNewOffset(LabelNewOffsetPatch {
                        patch_at: BaseOffset(2),
                        patch_size: PatchSize::U16,
                        unpatched_value: 0x1211
                    })
                ],
                ..Default::default() },
        ];
        let mut exp_bin = main_bin;
        exp_bin.extend(vec![0x11, 0x11, 0xF9, 0x15]);

        let bin = link(modules).unwrap();

        assert_eq!(bin.0, exp_bin);
    }

    #[test]
    fn branches_with_module_local_references_are_not_patched() {
        // Code to support this test was written before realizing branches with local references
        // don't need to be patched.
        //
        // We'll assert that no changes are made and then update
        // when we supporting branching across module boundaries.
        let main_bin_len = 30;
        let main_bin = vec![0u8; main_bin_len];
        let lib2_bin = vec![0u8; 8];
        let modules = vec![
            AssemblyModule { modname: "main".to_string(), code: main_bin.clone(), ..Default::default() },
            AssemblyModule { 
                modname: "lib2".to_string(),
                code: lib2_bin.clone(),
                linker_patches: vec![
                    LinkerPatch::BranchWithImport(BranchPatch {
                        patch_at: BaseOffset(2),
                        reference: LabelRef { kind: RefKind::LocalRef(MemberName("label".to_string())), size: RefSize(32) },
                        cond: None
                    })
                ],
                labels: HashMap::from_iter([("label".to_string(), BaseOffset(200))]),
                ..Default::default() },
        ];

        let mut exp_bin = main_bin;
        exp_bin.extend(lib2_bin);

        let bin = link(modules).unwrap();

        assert_eq!(bin.0, exp_bin);
    }

    #[test]
    fn thumb_addr_pseudo_is_patched_with_new_offsets() {
        let main_bin_len = 200;
        let main_bin = vec![0u8; main_bin_len];
        let modules = vec![
            AssemblyModule { modname: "main".to_string(), code: main_bin.clone(), ..Default::default() },
            AssemblyModule { 
                modname: "lib2".to_string(),
                code: vec![0x11, 0x11, 0x11, 0x12, 0x12, 0x12],
                linker_patches: vec![
                    LinkerPatch::ThumbAddrPseudoWithNewOffset(ThumbAddrPseudoPatch {
                        patch_at: 2.into(),
                        reference: "label".to_string()
                    })
                ],
                labels: HashMap::from_iter([("label".to_string(), BaseOffset(30))]),
                ..Default::default()
            },
        ];
        let mut exp_bin = main_bin;
        // (label base offset + main_bin len) | 1 = 231 (0xE7) to 32-bit little endian
        exp_bin.extend(vec![0x11, 0x11, 0xE7, 0x00, 0x00, 0x00]);

        let bin = link(modules).unwrap();

        assert_eq!(bin.0, exp_bin);
    }

    #[test]
    fn definition_imports_are_patched_with_resolved_values() {
        let main_bin_len = 50;
        let main_bin = vec![0u8; main_bin_len];
        let modules = vec![
            AssemblyModule { 
                modname: "main".to_string(), 
                code: main_bin.clone(),
                linker_patches: vec![
                    // requires the second pass
                    LinkerPatch::ImportDefinitionRef(ImportDefinitionRefPatch {
                        patch_at: BaseOffset(4),
                        ref_size: RefSize(8),
                        import_module: ModuleRef { module: "lib2".into(), member: "OTHER".into() }
                    })
                ],
                ..Default::default() 
            },
            AssemblyModule { 
                modname: "lib2".to_string(),
                code: vec![0x11, 0x11, 0x11, 0x11, 0x22, 0x22, 0x22, 0x22],
                linker_patches: vec![
                    // requires the second pass
                    LinkerPatch::ImportDefinitionRef(ImportDefinitionRefPatch {
                        patch_at: BaseOffset(4),
                        ref_size: RefSize(16),
                        import_module: ModuleRef { module: "lib3".into(), member: "MYCONST".into() }
                    })
                ],
                pub_definitions: HashMap::from_iter([
                    ("MYCONST".to_string(), HexLiteral::U32(0x78563412)),
                    ("OTHER".to_string(), HexLiteral::U8(0x12))
                ]),
                ..Default::default() 
            },
            AssemblyModule { 
                modname: "lib3".to_string(), 
                code: vec![0x11, 0x11, 0x11, 0x11],
                pub_definitions: HashMap::from_iter([
                    ("MYCONST".to_string(), HexLiteral::U16(0x9988))
                ]),
                linker_patches: vec![
                    // requires the first pass
                    LinkerPatch::ImportDefinitionRef(ImportDefinitionRefPatch {
                        patch_at: BaseOffset(0),
                        ref_size: RefSize(32),
                        import_module: ModuleRef { module: "lib2".into(), member: "MYCONST".into() }
                    })
                ],
                ..Default::default() 
            },
        ];
        let mut exp_bin = main_bin;
        exp_bin[4] = 0x12;              // from the linker patch in main
        exp_bin.extend(vec![
            0x11, 0x11, 0x11, 0x11, 0x88, 0x99, 0x22, 0x22, // lib2 code
            0x12, 0x34, 0x56, 0x78,                         // lib3 code
        ]);

        let bin = link(modules).unwrap();

        assert_eq!(bin.0, exp_bin);
    }

    #[test]
    fn definition_imports_that_are_unresolved_return_err() {
        let main_bin_len = 8;
        let main_bin = vec![0u8; main_bin_len];
        let modules = vec![
            AssemblyModule { 
                modname: "main".to_string(), 
                code: main_bin.clone(),
                linker_patches: vec![
                    // requires the second pass
                    LinkerPatch::ImportDefinitionRef(ImportDefinitionRefPatch {
                        patch_at: BaseOffset(4),
                        ref_size: RefSize(8),
                        import_module: ModuleRef { module: "lib2".into(), member: "OTHER".into() }
                    })
                ],
                ..Default::default() 
            },
        ];

        let err = link(modules).unwrap_err();

        assert!(err.to_string().contains("unresolved patch in module 'main'"), "{}", err);
    }

    #[test]
    fn label_imports_are_patched_with_resolved_values() {
        let main_bin_len = 50;
        let main_bin = vec![0u8; main_bin_len];
        let lib2_bin = vec![0x11, 0x11, 0x11, 0x11, 0x22, 0x33, 0x33, 0x33];
        let mut lib3_bin = vec![0x11, 0x11, 0x11, 0x11, 0x00, 0x00, 0x00, 0x00];
        let modules = vec![
            AssemblyModule { 
                modname: "main".to_string(), 
                code: main_bin.clone(),
                linker_patches: vec![
                    // requires the second pass
                    LinkerPatch::ImportLabelRef(ImportLabelRefPatch {
                        patch_at: BaseOffset(4),
                        import_module: ModuleRef { module:"lib2".into(), member:"start".into() },
                        patch_size: PatchSize::U32,
                    })
                ],
                ..Default::default() 
            },
            AssemblyModule { 
                modname: "lib2".to_string(),
                code: lib2_bin.clone(),
                labels: HashMap::from_iter([
                    ("start".to_string(), BaseOffset(0)),
                    ("func".to_string(), BaseOffset(4)),
                ]),
                ..Default::default() 
            },
            AssemblyModule { 
                modname: "lib3".to_string(), 
                code: lib3_bin.clone(),
                linker_patches: vec![
                    // requires the first pass
                    LinkerPatch::ImportLabelRef(ImportLabelRefPatch {
                        patch_at: BaseOffset(2),
                        import_module: ModuleRef { module:"lib2".into(), member:"func".into() },
                        patch_size: PatchSize::U32,
                    })
                ],
                ..Default::default() 
            },
        ];

        // Manually construct the expected binary
        let mut exp_bin = main_bin;

        // main
        let exp_main_import_bytes = 50u32.to_le_bytes();
        exp_bin[4..8].copy_from_slice(&exp_main_import_bytes);
        // lib2
        exp_bin.extend(lib2_bin);
        // lib3
        let exp_lib3_import_bytes = 54u32.to_le_bytes();
        lib3_bin[2..6].copy_from_slice(&exp_lib3_import_bytes);
        exp_bin.extend(lib3_bin);

        let bin = link(modules).unwrap();

        assert_eq!(bin.0, exp_bin);
    }

    #[test]
    fn label_imports_that_are_unresolved_return_err() {
        let main_bin_len = 8;
        let main_bin = vec![0u8; main_bin_len];
        let modules = vec![
            AssemblyModule { 
                modname: "main".to_string(), 
                code: main_bin.clone(),
                linker_patches: vec![
                    LinkerPatch::ImportLabelRef(ImportLabelRefPatch {
                        patch_at: BaseOffset(4),
                        import_module: ModuleRef { module:"lib2".into(), member:"start".into() },
                        patch_size: PatchSize::U32,
                    })
                ],
                ..Default::default() 
            },
        ];

        let err = link(modules).unwrap_err();

        assert!(err.to_string().contains("unresolved patch in module 'main'"), "{}", err);
    }

    #[test]
    fn branches_to_another_module_are_patched() {
        let main_bin_len = 4;
        let main_bin = vec![0u8; main_bin_len];
        let lib2_bin = vec![0x11, 0x11, 0x11, 0x11];
        let mut lib3_bin = vec![0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00];
        let modules = vec![
            AssemblyModule { 
                modname: "main".to_string(), 
                code: main_bin.clone(),
                linker_patches: vec![
                    // requires the second pass
                    LinkerPatch::BranchWithImport(BranchPatch {
                        patch_at: BaseOffset(0),
                        reference: LabelRef { kind: RefKind::ModuleRef(ModuleRef { module: "lib2".into(), member: "start".into() }), size: RefSize(32) },
                        cond: None
                    })
                ],
                ..Default::default() 
            },
            AssemblyModule { 
                modname: "lib2".to_string(),
                code: lib2_bin.clone(),
                labels: HashMap::from_iter([
                    ("start".to_string(), BaseOffset(0)),
                    ("func".to_string(), BaseOffset(2)),
                ]),
                ..Default::default() 
            },
            AssemblyModule { 
                modname: "lib3".to_string(), 
                code: lib3_bin.clone(),
                linker_patches: vec![
                    // requires the first pass
                    LinkerPatch::BranchWithImport(BranchPatch {
                        patch_at: BaseOffset(4),
                        reference: LabelRef { kind: RefKind::ModuleRef(ModuleRef { module: "lib2".into(), member: "func".into() }), size: RefSize(32) },
                        cond: None
                    })
                ],
                ..Default::default() 
            },
        ];

        // Manually construct the expected binary
        let mut exp_bin = main_bin;

        exp_bin[0..2].copy_from_slice(&[0x00, 0xE0]);
        exp_bin.extend(lib2_bin);
        lib3_bin[4..6].copy_from_slice(&[0xFB, 0xE7]);
        exp_bin.extend(lib3_bin);

        let bin = link(modules).unwrap();

        assert_eq!(bin.0, exp_bin);
    }

    #[test]
    fn branch_with_links_to_another_module_are_patched() {
        let main_bin_len = 8;
        let main_bin = vec![0u8; main_bin_len];
        let lib2_bin = vec![0x11, 0x11, 0x11, 0x11, 0x22, 0x22, 0x22, 0x22];
        let mut lib3_bin = vec![0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00];
        let modules = vec![
            AssemblyModule { 
                modname: "main".to_string(), 
                code: main_bin.clone(),
                linker_patches: vec![
                    // requires the second pass
                    LinkerPatch::BranchLinkWithImport(BranchWithLinkPatch {
                        patch_at: BaseOffset(0),
                        reference: LabelRef { kind: RefKind::ModuleRef(ModuleRef { module: "lib2".into(), member: "start".into() }), size: RefSize(32) },
                        cond: None
                    })
                ],
                ..Default::default() 
            },
            AssemblyModule { 
                modname: "lib2".to_string(),
                code: lib2_bin.clone(),
                labels: HashMap::from_iter([
                    ("start".to_string(), BaseOffset(0)),
                    ("func".to_string(), BaseOffset(4)),
                ]),
                ..Default::default() 
            },
            AssemblyModule { 
                modname: "lib3".to_string(), 
                code: lib3_bin.clone(),
                linker_patches: vec![
                    // requires the first pass
                    LinkerPatch::BranchLinkWithImport(BranchWithLinkPatch {
                        patch_at: BaseOffset(4),
                        reference: LabelRef { kind: RefKind::ModuleRef(ModuleRef { module: "lib2".into(), member: "func".into() }), size: RefSize(32) },
                        cond: None
                    })
                ],
                ..Default::default() 
            },
        ];

        // Manually construct the expected binary
        let mut exp_bin = main_bin;
        exp_bin[0..4].copy_from_slice(&[0x00, 0xF0, 0x02, 0xF8]);
        exp_bin.extend(lib2_bin);
        lib3_bin[4..8].copy_from_slice(&[0xFF, 0xF7, 0xFA, 0xFF]);
        exp_bin.extend(lib3_bin);

        let bin = link(modules).unwrap();

        assert_eq!(bin.0, exp_bin);
    }

    #[test]
    fn branches_to_another_module_that_are_unresolved_return_err() {
        let main_bin_len = 8;
        let main_bin = vec![0u8; main_bin_len];
        let modules = vec![
            AssemblyModule { 
                modname: "main".to_string(), 
                code: main_bin.clone(),
                linker_patches: vec![
                    LinkerPatch::BranchWithImport(BranchPatch {
                        patch_at: BaseOffset(0),
                        reference: LabelRef { kind: RefKind::ModuleRef(ModuleRef { module: "lib2".into(), member: "start".into() }), size: RefSize(32) },
                        cond: None
                    })
                ],
                ..Default::default() 
            },
        ];

        let err = link(modules).unwrap_err();

        assert!(err.to_string().contains("unresolved patch in module 'main'"), "{}", err);
    }

    #[test]
    fn branche_with_links_to_another_module_that_are_unresolved_return_err() {
        let main_bin_len = 8;
        let main_bin = vec![0u8; main_bin_len];
        let modules = vec![
            AssemblyModule { 
                modname: "main".to_string(), 
                code: main_bin.clone(),
                linker_patches: vec![
                    LinkerPatch::BranchLinkWithImport(BranchWithLinkPatch {
                        patch_at: BaseOffset(0),
                        reference: LabelRef { kind: RefKind::ModuleRef(ModuleRef { module: "lib2".into(), member: "start".into() }), size: RefSize(32) },
                        cond: None
                    })
                ],
                ..Default::default() 
            },
        ];

        let err = link(modules).unwrap_err();

        assert!(err.to_string().contains("unresolved patch in module 'main'"), "{}", err);
    }
}
