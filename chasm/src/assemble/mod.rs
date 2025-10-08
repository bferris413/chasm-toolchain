mod parse;
mod token;
mod codegen;

use std::{fmt::{Debug, Display}, fs, ops::{Deref, DerefMut}};

use crate::AssembleArgs;
use token::tokenize;
use parse::parse;
use codegen::codegen;

use anyhow::{Context, Result};

pub fn assemble(args: &AssembleArgs) -> Result<()> {
    let _machine_code = fs::read_to_string(&args.file)
        .with_context(|| format!("Failed to read assembly file '{}'", &args.file))
        .map(|source| source.into())
        .and_then(|s| assemble_source(&s))?;

    Ok(())
}

fn assemble_source(source: &AssemblySource) -> Result<MachineCode> {
    dbg!(tokenize(source))
        .and_then(parse)
        .inspect(|ast| eprintln!("AST: {ast:?}"))
        .and_then(codegen)
}

struct AssemblySource {
    source: String,
}
impl From<String> for AssemblySource {
    fn from(source: String) -> Self {
        Self { source }
    }
}
impl Deref for AssemblySource {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.source
    }
}

#[derive(Debug)]
struct AssemblyAst<'src> {
    nodes: Vec<Node<'src>>,
}
#[derive(Debug)]
struct Node<'src> {
    kind: NodeKind,
    token: Token<'src>,
}
#[derive(Debug)]
enum NodeKind {
    HexLiteralU32(u32),
    HexLiteralU16(u16),
    HexLiteralU8(u8),
}

#[derive(Debug, Default)]
struct AssemblyTokens<'src> {
    tokens: Vec<Token<'src>>
}
impl<'src> Deref for AssemblyTokens<'src> {
    type Target = Vec<Token<'src>>;
    fn deref(&self) -> &Self::Target {
        &self.tokens
    }
}
impl<'src> DerefMut for AssemblyTokens<'src> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tokens
    }
}
#[derive(Debug, Default)]
struct MachineCode {
    bytes: Vec<u8>,
}

#[derive(Debug)]
struct Token<'src> {
    kind: TokenKind,
    lexeme: &'src str,
    line: usize,
    column: usize,
}

#[derive(Debug)]
enum TokenKind {
    HexLiteralU32,
    HexLiteralU16,
    HexLiteralU8,
}

struct AssemblyError {
    message: String,
    line: usize,
    column: usize,
    end_column: Option<usize>,
    source_pretty: String,
}
impl AssemblyError {
    fn new(message: String, line: usize, column: usize, end_column: Option<usize>, source: &AssemblySource) -> Self {
        let source_pretty = source.source.replace('\t', "    ");
        Self { message, line, column, end_column, source_pretty }
    }
}
impl Debug for AssemblyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error at {}:{}: {}", self.line + 1, self.column + 1, self.message)
    }
}
impl Display for AssemblyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{self:?}")
    }
}
impl std::error::Error for AssemblyError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn given_hex_word_then_it_is_generated_as_little_endian() {
        let source = AssemblySource::from("x1234.5678".to_string());

        let machine_code = assemble_source(&source).unwrap();

        assert_eq!(machine_code.bytes, vec![0x78, 0x56, 0x34, 0x12]);
    } 

    #[test]
    fn given_hex_half_word_then_it_is_generated_as_little_endian() {
        let source = AssemblySource::from("x1234".to_string());

        let machine_code = assemble_source(&source).unwrap();

        assert_eq!(machine_code.bytes, vec![0x34, 0x12]);
    } 

    #[test]
    fn given_hex_byte_then_it_is_generated() {
        let source = AssemblySource::from("x12".to_string());

        let machine_code = assemble_source(&source).unwrap();

        assert_eq!(machine_code.bytes, vec![0x12]);
    } 

    #[test]
    fn given_too_short_hex_string_then_error_is_returned() {
        let source = AssemblySource::from("x123".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Hex literal must have an even number of digits"));
    }

    #[test]
    fn given_hex_string_with_non_halfword_separator_then_error_is_returned() {
        let source = AssemblySource::from("x123.45678".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Hex literal separator must be after a full half-word (4 digits)"));
    }

    #[test]
    fn given_hex_string_starting_with_separator_then_error_is_returned() {
        let source = AssemblySource::from("x.12345678".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Hex literal cannot start with a separator"));
    }

    #[test]
    fn given_hex_string_ending_with_separator_then_error_is_returned() {
        let source = AssemblySource::from("x1234.".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Hex literal cannot end with a separator"));
    }

    #[test]
    fn given_hex_string_with_non_halfword_after_separator_then_error_is_returned() {
        let source = AssemblySource::from("x1234.567".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Hex literal must have an even number of digits"));
    }

    #[test]
    fn given_hex_string_with_good_and_bad_separatorthen_error_is_returned() {
        // first is fine, second is trailing
        let source = AssemblySource::from("x1234.5678.".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Hex literal cannot end with a separator"));
    }
}