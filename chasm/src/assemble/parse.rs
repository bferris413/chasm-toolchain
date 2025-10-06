//! Parser for chasm assembly source code.

use anyhow::Result;

use crate::assemble::{AssemblyAst, AssemblyTokens};

pub(crate) fn parse(tokens: AssemblyTokens) -> Result<AssemblyAst> {
    eprintln!("parsing");
    Ok(AssemblyAst)
}