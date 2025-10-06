//! Tokenizer for chasm assembly source code.

use anyhow::Result;

use crate::assemble::{AssemblySource, AssemblyTokens};

pub(crate) fn tokenize(source: AssemblySource) -> Result<AssemblyTokens> {
    eprintln!("tokenizing");
    Ok(AssemblyTokens)
}