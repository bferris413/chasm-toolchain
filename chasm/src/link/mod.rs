use anyhow::{Result, anyhow, bail};
use crate::assemble::AssemblyModule;

const LINK_ERR: &str = "Link error:";

pub fn link(mut modules: Vec<AssemblyModule>) -> Result<Binary> {
    let main_i = main_module_index(&modules).map_err(|e| anyhow!("{LINK_ERR} {e}"))?;
    let bin = modules.swap_remove(main_i).code;
    Ok(Binary(bin))
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
    fn module_list_with_single_main_is_ok() {
        let modules = vec![
            AssemblyModule { modname: "main".to_string(), ..Default::default() },
            AssemblyModule { modname: "mylib2".to_string(), ..Default::default() },
            AssemblyModule { modname: "mylib3".to_string(), ..Default::default() },
        ];

        let link_res = link(modules);

        assert!(link_res.is_ok(), "{:?}", link_res);
    }    
}