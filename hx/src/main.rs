use std::{fs, io::Write};

use anyhow::{bail, Context, Result};
use clap::Parser;

#[derive(Parser)]
struct Cli {
    /// Input file to process (default behavior is text -> bin)
    input: String,

    /// Write output to this file (default is stdout if not provided)
    output: Option<String>,

    // /// Read input as a binary file and output hex text
    // #[arg(short, long)]
    // reverse: bool,
}

fn main() -> Result<()> {
    let args = Cli::parse();

    let input_text = fs::read_to_string(&args.input).context("Couldn't read input file")?;
    let hex_digits = input_text.lines()
                                .map(str::trim)
                                .filter(|s| !s.is_empty())
                                .map(remove_all_whitespace)
                                .map(remove_comments)
                                .filter(|s| !s.is_empty())
                                .collect::<String>();
    
    dbg!(&hex_digits);

    if hex_digits.len() % 2 != 0 {
        bail!("Input hex string must have an even number of digits");
    }

    let hex_bytes = (0..hex_digits.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&hex_digits[i..i + 2], 16))
        .collect::<Result<Vec<u8>, _>>()
        .context("Failed to parse hex string")?;

    println!("{hex_bytes:?}");

    if let Some(outfile) = args.output {
        fs::write(&outfile, &hex_bytes).context("Failed to write output file")
    } else {
        std::io::stdout().write_all(&hex_bytes).context("Failed to write to stdout")
    }
}

fn remove_all_whitespace<T: AsRef<str>>(s: T) -> String {
    s.as_ref().chars().filter(|c| !c.is_whitespace()).collect()
}

fn remove_comments<T: AsRef<str>>(s: T) -> String {
    if let Some(index) = s.as_ref().find("#") {
        s.as_ref()[..index].to_string()
    } else {
        s.as_ref().to_string()
    }
}