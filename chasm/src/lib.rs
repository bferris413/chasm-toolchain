use std::path::PathBuf;

use clap::Args;

pub mod assemble;
pub mod link;

#[derive(Args, Debug)]
pub struct AssembleArgs {
    /// The .cas files to assemble
    files: Vec<PathBuf>,

    /// Output the machine code as a hex text file instead of raw binary
    #[arg(short, long, default_value_t = false)]
    text: bool,

    /// If the assembler should skip linking (default behavior is to link)
    #[arg(long, default_value_t = false)]
    no_link: bool,
}