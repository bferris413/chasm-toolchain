use std::path::PathBuf;

use clap::Args;

pub mod assemble;
pub mod link;

#[derive(Args, Debug)]
pub struct AssembleArgs {
    /// The chasm assembly file (.cas)
    file: PathBuf,

    /// Output the machine code as a hex text file instead of raw binary
    #[arg(short, long, default_value_t = false)]
    text: bool,
}