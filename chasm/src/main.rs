use chasm::{AssembleArgs, CodeArgs, LinkArgs, assemble::assemble, link::link, code::code,};

use anyhow::Result;
use clap::{Parser, Subcommand};

fn main() -> Result<()> {
    use Command::*;
    let args = Cli::parse();

    match args.command {
        Assemble(assemble_args) => {
            assemble(assemble_args)
        }
        Link(link_args) => {
            link(link_args)
        }
        #[cfg(feature = "code")]
        Code(code_args) => {
            code(code_args)
        }
    }
}

#[derive(Parser)]
#[command(name = "chasm")]
#[command(about = "chasm toolchain driver", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Assemble a chasm assembly file (.cas) into machine code
    #[command(visible_alias = "as")]
    Assemble(AssembleArgs),

    /// Link a set of assembly modules together into a single binary
    Link(LinkArgs),

    /// Start the built-in Chasm assembly editor
    #[cfg(feature = "code")]
    Code(CodeArgs),
}

