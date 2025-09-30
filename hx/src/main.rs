use std::{fs::{self, File}, io::Write};

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
    let mut output: Box<dyn Write> = match args.output {
        Some(ref path) => Box::new(File::create(path).context("Couldn't create output file")?),
        None => Box::new(std::io::stdout()),
    };

    text_to_bin(&input_text, &mut output)
}

fn text_to_bin(input: &str, output: &mut impl Write) -> Result<()> {
    let hex_digits = input.lines()
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

    eprintln!("{hex_bytes:?}");

    output.write_all(&hex_bytes).context("Failed to write output")
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn given_plain_input_then_bytes_are_converted_and_written() {
        // no comments, whitespace, or special handling
        let input = "010203040506";
        let exp_output = vec![1, 2, 3, 4, 5, 6];
        let mut output = Vec::new();

        text_to_bin(input, &mut output).unwrap();

        assert_eq!(output, exp_output);
    }

    #[test]
    fn given_input_with_comments_then_bytes_are_converted_and_written() {
        let input = "
0102 # here is a comment
03040506
";
        let exp_output = vec![1, 2, 3, 4, 5, 6];
        let mut output = Vec::new();

        text_to_bin(input, &mut output).unwrap();

        assert_eq!(output, exp_output);
    }

    #[test]
    fn given_input_with_newlines_and_whitespace_then_bytes_are_converted_and_written() {
        let input = "
         01
02
03 04   05            


06
# just a line comment
";
        let exp_output = vec![1, 2, 3, 4, 5, 6];
        let mut output = Vec::new();

        text_to_bin(input, &mut output).unwrap();

        assert_eq!(output, exp_output);
    }

    #[test]
    fn given_input_with_non_hex_digits_then_error_is_returned() {
        let input = "010x02";
        let mut output = Vec::new();

        let result = text_to_bin(input, &mut output);

        assert!(result.is_err());
    }

    #[test]
    fn given_input_with_odd_number_of_hex_digits_then_error_is_returned() {
        let input = "10203";
        let mut output = Vec::new();

        let result = text_to_bin(input, &mut output);

        assert!(result.is_err());
    }
}