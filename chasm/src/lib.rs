use std::path::PathBuf;

use clap::Args;

pub mod assemble;
#[cfg(feature = "code")]
pub mod code;
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

#[derive(Args, Debug)]
pub struct LinkArgs {
    /// The chasm modules to link
    files: Vec<PathBuf>,
}

#[cfg(feature = "code")]
#[derive(Args, Debug)]
pub struct CodeArgs;