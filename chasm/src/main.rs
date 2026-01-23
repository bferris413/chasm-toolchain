use chasm::{AssembleArgs, LinkArgs, assemble as asm, link};

use anyhow::Result;
use clap::{Parser, Subcommand};

fn main() -> Result<()> {
    use Command::*;
    let args = Cli::parse();

    match args.command {
        Assemble(assemble_args) => {
            asm::assemble(assemble_args)
        }
        Link(link_args) => {
            link::link(link_args)
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
}

