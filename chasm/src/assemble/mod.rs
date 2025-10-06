mod parse;
mod token;
mod codegen;

use std::fs;

use crate::AssembleArgs;
use token::tokenize;
use parse::parse;
use codegen::codegen;

use anyhow::{Context, Result};

pub fn assemble(args: &AssembleArgs) -> Result<()> {
    let source = fs::read_to_string(&args.file)
        .with_context(|| format!("Failed to read assembly file '{}'", &args.file))
        .map(|source| source.into())
        .and_then(tokenize)
        .and_then(parse)
        .and_then(codegen)?;

    Ok(())
}

struct AssemblySource {
    source: String,
}
impl From<String> for AssemblySource {
    fn from(source: String) -> Self {
        Self { source }
    }
}

struct AssemblyAst;
struct AssemblyTokens;