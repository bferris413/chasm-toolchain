use std::{fs::{self, File}, io::Write, num::NonZeroUsize};

use anyhow::{bail, Context, Result};
use clap::{Args, Parser};

/// An hex <-> binary converter tool.
#[derive(Debug, Parser)]
struct Cli {
    /// Input file to process (default behavior is text -> bin)
    input: String,

    /// Write output to this file (default is stdout if not provided)
    output: Option<String>,

    /// Read input as a binary file and output hex text
    #[arg(short, long, default_value_t = false)]
    reverse: bool,

    #[command(flatten)]
    format_args: FormatArgs,

}

#[derive(Args, Debug)]
struct FormatArgs {
    /// Number of hex byte pairs to write per line when converting binary to text (default is no line breaks, must be non-zero)
    /// 
    /// This has no effect unless --reverse is provided.
    #[arg(short='l', long)]
    row_len: Option<NonZeroUsize>,

    /// Include spaces between byte pairs when converting binary to text
    /// 
    /// This has no effect unless --reverse is provided.
    #[arg(short, long, default_value_t = false)]
    spaced: bool,

    /// Include offsets at the start of each line when converting binary to text
    /// 
    /// This has no effect unless --reverse is provided.
    #[arg(short, long, default_value_t = false)]
    offsets: bool,
}

fn main() -> Result<()> {
    let args = Cli::parse();

    if args.reverse {
        run_bin_to_text(&args)
    } else {
        run_text_to_bin(&args)
    }
}

fn run_text_to_bin(args: &Cli) -> Result<()> {
    let input_text = fs::read_to_string(&args.input).context("Couldn't read input file")?;
    let mut output = get_output_writer(&args.output)?;

    text_to_bin(&input_text, &mut output)
}

fn run_bin_to_text(args: &Cli) -> Result<()> {
    let input_bytes = fs::read(&args.input).context("Couldn't read input file")?;
    let mut output = get_output_writer(&args.output)?;

    bin_to_text(&input_bytes, &mut output, &args.format_args)
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

    output.write_all(&hex_bytes).context("Failed to write output")?;
    output.flush().context("Failed to flush output")
}

fn bin_to_text(bytes: &[u8], output: &mut impl Write, format_args: &FormatArgs) -> Result<()> {
    let mut buf_write = std::io::BufWriter::new(output);
    let chunk_size = format_args.row_len.map(|nz| nz.get()).unwrap_or(bytes.len());
    let maybe_space = if format_args.spaced { " " } else { "" };
    let mut offset = 0;

    for bytes_chunk in bytes.chunks(chunk_size) {
        if format_args.offsets {
            write!(buf_write, "0x{offset:08X}: ").context("Failed to write offset")?;
            offset += bytes_chunk.len();
        }
        for byte in bytes_chunk {
            write!(buf_write, "{byte:02X}{maybe_space}").context("Failed to write output")?;
        }
        writeln!(buf_write).context("Failed to write output")?;
    }

    buf_write.flush().context("Failed to flush output")
}

fn remove_all_whitespace<T: AsRef<str>>(s: T) -> String {
    s.as_ref().chars().filter(|c| !c.is_whitespace()).collect()
}

fn remove_comments<T: AsRef<str>>(s: T) -> String {
    if let Some(index) = s.as_ref().find(";") {
        s.as_ref()[..index].to_string()
    } else {
        s.as_ref().to_string()
    }
}

fn get_output_writer(path: &Option<String>) -> Result<Box<dyn Write>> {
    match path {
        Some(path) => Ok(Box::new(File::create(path).context("Couldn't create output file")?)),
        None => Ok(Box::new(std::io::stdout())),
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

    #[test]
    fn given_binary_input_then_hex_text_is_written() {
        let input = vec![1, 2, 3, 4, 5, 6];
        let exp_output = "010203040506";
        let mut output = Vec::new();
        let fmt_args = FormatArgs { row_len: None, spaced: false, offsets: false };

        bin_to_text(&input, &mut output, &fmt_args).unwrap();

        let output_str = String::from_utf8(output).unwrap();
        assert_eq!(output_str.trim(), exp_output);
    }

    #[test]
    fn given_binary_input_with_spaced_option_then_hex_text_is_written_with_spaces() {
        let input = vec![1, 2, 3, 4, 5, 6];
        let exp_output = "01 02 03 04 05 06 \n";
        let mut output = Vec::new();
        let fmt_args = FormatArgs { row_len: None, spaced: true, offsets: false };

        bin_to_text(&input, &mut output, &fmt_args).unwrap();

        let output_str = String::from_utf8(output).unwrap();
        assert_eq!(output_str, exp_output);
    }

    #[test]
    fn given_binary_input_with_row_len_option_then_hex_text_is_written_specified_number_of_bytes_per_line() {
        let input = vec![1, 2, 3, 4, 5, 6];
        let exp_output = "01 02 \n03 04 \n05 06 \n";
        let mut output = Vec::new();
        let fmt_args = FormatArgs {
            row_len: Some(2.try_into().unwrap()),
            spaced: true,
            offsets: false
        };

        bin_to_text(&input, &mut output, &fmt_args).unwrap();

        let output_str = String::from_utf8(output).unwrap();
        assert_eq!(output_str, exp_output);
    }

    #[test]
    fn given_binary_input_with_offset_option_then_hex_text_is_written_with_offsets() {
        let input = vec![1, 2, 3, 4, 5];
        let exp_output = "0x00000000: 01 02 \n0x00000002: 03 04 \n0x00000004: 05 \n";
        let mut output = Vec::new();
        let fmt_args = FormatArgs {
            row_len: Some(2.try_into().unwrap()),
            spaced: true,
            offsets: true
        };

        bin_to_text(&input, &mut output, &fmt_args).unwrap();

        let output_str = String::from_utf8(output).unwrap();
        assert_eq!(output_str, exp_output);
    }
}