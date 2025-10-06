//! Machine code generation for chasm assembly source code.

use anyhow::Result;

use crate::assemble::AssemblyAst;

pub(crate) fn codegen(ast: AssemblyAst) -> Result<()> {
    eprintln!("codegen");
    Ok(())
}