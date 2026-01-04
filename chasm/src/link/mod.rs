use std::collections::HashMap;

use anyhow::{Result, anyhow, bail};
use crate::assemble::AssemblyModule;
use crate::assemble::BaseOffset;
use crate::assemble::BranchPatch;
use crate::assemble::LabelNewOffsetPatch;
use crate::assemble::LinkerPatch;
use crate::assemble::PatchSize;
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

    let mut modules = modules.into_iter();
    let main = modules.next().unwrap();
    let mut main_bin = main.code;
    let mut modules_to_offsets = HashMap::new();

    for mut module in modules {
        if modules_to_offsets.contains_key(&module.modname) {
            bail!("{LINK_ERR} duplicate module '{}'", module.modname);
        }

        // fix up label offsets within the module to point to new location in binary
        for (_, base_offset) in module.labels.iter_mut() {
            **base_offset += main_bin.len();
        }

        for patch in module.linker_patches {
            match patch {
                LinkerPatch::LabelNewOffset(new_offset_patch) => {
                    patch_label_offset(&mut module.code, new_offset_patch, main_bin.len())?;
                }
                LinkerPatch::Import(import_patch) => todo!(),
                LinkerPatch::BranchWithNewOffset(branch_patch) => {
                    patch_branch_offset(&mut module.code, &module.labels, branch_patch, main_bin.len())?;
                },
                LinkerPatch::BranchLinkWithNewOffset(branch_with_link_patch) => todo!(),
                LinkerPatch::ThumbAddrPseudoWithNewOffset(thumb_addr_pseudo_patch) => todo!(),
            }
        }

        modules_to_offsets.insert(module.modname, main_bin.len());
        main_bin.extend(module.code);
    }

    Ok(Binary(main_bin))
}

fn patch_branch_offset(
    _module_code: &mut Vec<u8>,
    _labels: &HashMap<String, BaseOffset>,
    _branch_patch: BranchPatch,
    _offset_by: usize
) -> Result<()> {
    // presently a no-op since `reference` is module-local 
    Ok(())

    // let BranchPatch { patch_at, reference, cond } = branch_patch;

    // if patch_at.0 > module_code.len() {
    //     bail!("patch offset {patch_at:?} is out of bounds (module code length {})", module_code.len());
    // }

    // let new_base_offset = BaseOffset(*patch_at + offset_by);
    // let mut branch_bytes = Vec::new();

    // codegen::generate_branch(
    //     &reference,
    //     &cond,
    //     &mut branch_bytes,
    //     &labels,
    //     &mut HashMap::new(),
    //     Some(new_base_offset),
    // )?;

    // module_code[*patch_at..*patch_at + branch_bytes.len()].copy_from_slice(&branch_bytes);

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
    use crate::assemble::{BaseOffset, LabelNewOffsetPatch, PatchSize};

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
                    LinkerPatch::BranchWithNewOffset(BranchPatch {
                        patch_at: BaseOffset(2),
                        reference: "&label".to_string(),
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
}