mod parse;
mod token;
mod codegen;
mod helpers;

use std::{collections::HashMap, fmt::{Debug, Display}, fs, ops::{Deref, DerefMut}};

use crate::{assemble::helpers::normalize_to_ascii_lower, AssembleArgs};
use token::tokenize;
use parse::parse;
use codegen::codegen;

use anyhow::{bail, Context, Error, Result};

pub fn assemble(args: &AssembleArgs) -> Result<()> {
    let machine_code = fs::read_to_string(&args.file)
        .with_context(|| format!("Failed to read assembly file '{}'", &args.file.display()))
        .map(|source| source.into())
        .and_then(|s| assemble_source(&s))?;

    let outfile = args.file.with_extension("bin");
    fs::write(&outfile, &machine_code.bytes)
        .with_context(|| format!("Failed to write binary file '{}'", outfile.display()))?;

    Ok(())
}

fn assemble_source(source: &AssemblySource) -> Result<MachineCode> {
    tokenize(source)
        .and_then(parse)
        .and_then(codegen)
}

#[derive(Debug)]
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
    HexLiteral(HexLiteral),
    Instruction(Instruction),
    Register(Register),
    Label,
}

#[derive(Debug)]
enum Instruction {
    Movs { dest: GeneralRegister, value: HexLiteral },
    Adds { dest: GeneralRegister, value: HexLiteral },
}

#[derive(Debug)]
enum HexLiteral {
    U32(u32),
    U16(u16),
    U8(u8),
}
#[derive(Copy, Clone, Debug)]
enum Register {
    General(GeneralRegister),
    SP,
    LR,
    PC,
    Special(SpecialRegister)
}
impl TryFrom<&str> for Register {
    type Error = Error;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let mut tmp_buf = [0u8; 8];
        let s = normalize_to_ascii_lower(s, &mut tmp_buf);

        match s {
            "r0" => Ok(Register::General(GeneralRegister::R0)),
            "r1" => Ok(Register::General(GeneralRegister::R1)),
            "r2" => Ok(Register::General(GeneralRegister::R2)),
            "r3" => Ok(Register::General(GeneralRegister::R3)),
            "r4" => Ok(Register::General(GeneralRegister::R4)),
            "r5" => Ok(Register::General(GeneralRegister::R5)),
            "r6" => Ok(Register::General(GeneralRegister::R6)),
            "r7" => Ok(Register::General(GeneralRegister::R7)),
            "r8" => Ok(Register::General(GeneralRegister::R8)),
            "r9" => Ok(Register::General(GeneralRegister::R9)),
            "r10" => Ok(Register::General(GeneralRegister::R10)),
            "r11" => Ok(Register::General(GeneralRegister::R11)),
            "r12" => Ok(Register::General(GeneralRegister::R12)),
            "sp" => Ok(Register::SP),
            "lr" => Ok(Register::LR),
            "pc" => Ok(Register::PC),
            "psr" => Ok(Register::Special(SpecialRegister::Psr)),
            "primask" => Ok(Register::Special(SpecialRegister::Primask)),
            "faultmask" => Ok(Register::Special(SpecialRegister::Faultmask)),
            "basepri" => Ok(Register::Special(SpecialRegister::Basepri)),
            "control" => Ok(Register::Special(SpecialRegister::Control)),
            other => bail!("Unknown register '{other}'"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
enum GeneralRegister {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
}

#[derive(Copy, Clone, Debug)]
enum SpecialRegister {
    Psr,
    Primask,
    Faultmask,
    Basepri,
    Control,
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
    labels: HashMap<String, usize>,
}

#[derive(Debug)]
struct Token<'src> {
    kind: TokenKind,
    lexeme: &'src str,
    line: usize,
    column: usize,
    source: &'src AssemblySource,
}

#[derive(Debug)]
enum TokenKind {
    HexLiteralU32,
    HexLiteralU16,
    HexLiteralU8,
    Identifier,
    Label,
}
impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::HexLiteralU32 => write!(f, "hex literal (u32)"),
            TokenKind::HexLiteralU16 => write!(f, "hex literal (u16)"),
            TokenKind::HexLiteralU8 => write!(f, "hex literal (u8)"),
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::Label => write!(f, "label"),
        }
    }
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
    fn hex_word_gets_generated() {
        let source = AssemblySource::from("x1234.5678".to_string());

        let machine_code = assemble_source(&source).unwrap();

        assert_eq!(machine_code.bytes, vec![0x78, 0x56, 0x34, 0x12]);
    } 

    #[test]
    fn hex_half_word_gets_generated() {
        let source = AssemblySource::from("x1234".to_string());

        let machine_code = assemble_source(&source).unwrap();

        assert_eq!(machine_code.bytes, vec![0x34, 0x12]);
    } 

    #[test]
    fn hex_byte_gets_generated() {
        let source = AssemblySource::from("x12".to_string());

        let machine_code = assemble_source(&source).unwrap();

        assert_eq!(machine_code.bytes, vec![0x12]);
    } 

    #[test]
    fn too_short_hex_string_returns_error() {
        let source = AssemblySource::from("x123".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Hex literal must have an even number of digits"));
    }

    #[test]
    fn hex_string_with_non_halfword_separator_returns_error() {
        let source = AssemblySource::from("x123.45678".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Hex literal separator must be after a full half-word (4 digits)"));
    }

    #[test]
    fn hex_string_starting_with_separator_returns_error() {
        let source = AssemblySource::from("x.12345678".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Hex literal cannot start with a separator"));
    }

    #[test]
    fn hex_string_ending_with_separator_returns_error() {
        let source = AssemblySource::from("x1234.".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Hex literal cannot end with a separator"));
    }

    #[test]
    fn hex_string_with_non_halfword_after_separator_returns_error() {
        let source = AssemblySource::from("x1234.567".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Hex literal must have an even number of digits"));
    }

    #[test]
    fn hex_string_with_good_and_bad_separator_returns_error() {
        // first is fine, second is trailing
        let source = AssemblySource::from("x1234.5678.".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Hex literal cannot end with a separator"));
    }

    #[test]
    fn movs_with_well_formed_args_gets_generated() {
        let source = AssemblySource::from("MOVS R0 x01".to_string());

        let machine_code = assemble_source(&source).unwrap();

        assert_eq!(machine_code.bytes, vec![0x01, 0x20]);
    }

    #[test]
    fn movs_with_missing_value_returns_error() {
        let source = AssemblySource::from("MOVS R0".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Expected immediate value after register in MOVS instruction"));
    }

    #[test]
    fn movs_with_large_reg_returns_error() {
        let source = AssemblySource::from("MOVS R8 x01".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Expected general-purpose register (r0-r7)"));
    }

    #[test]
    fn movs_with_invalid_reg_returns_error() {
        let source = AssemblySource::from("MOVS xFF".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Expected register after MOVS mnemonic, found hex literal (u8) 'xFF'"));
    }

    #[test]
    fn movs_with_eof_returns_error() {
        let source = AssemblySource::from("MOVS    ".to_string());

        let err = assemble_source(&source).unwrap_err();

        dbg!(&err);
        assert!(err.to_string().contains("Expected register after MOVS mnemonic, found EOF"));
    }

    #[test]
    fn adds_gets_generated() {
        let source = AssemblySource::from("ADDS R0 x01".to_string());

        let machine_code = assemble_source(&source).unwrap();

        assert_eq!(machine_code.bytes, vec![0x01, 0x30]);
    }

    #[test]
    fn adds_with_r7_and_255_gets_generated() {
        let source = AssemblySource::from("ADDS R7 xFF".to_string());

        let machine_code = assemble_source(&source).unwrap();

        assert_eq!(machine_code.bytes, vec![0xFF, 0x37]);
    }

    #[test]
    fn adds_with_large_reg_returns_error() {
        let source = AssemblySource::from("ADDS R12 xA1".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Expected general-purpose register (r0-r7)"));
    }

    #[test]
    fn adds_with_missing_value_returns_error() {
        let source = AssemblySource::from("ADDS R7".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Expected immediate value after register in ADDS instruction"));
    }

    #[test]
    fn adds_with_invalid_reg_returns_error() {
        let source = AssemblySource::from("ADDS abc".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Invalid register 'abc' after ADDS mnemonic"));
    }

    #[test]
    fn adds_with_eof_returns_error() {
        let source = AssemblySource::from("ADDS\n\n".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Expected register after ADDS mnemonic, found EOF"));
    }

    #[test]
    fn label_stores_address_of_next_instruction() {
        let source = AssemblySource::from("MOVS R0 x01\n@label\nMOVS R1 x02".to_string());

        let machine_code = assemble_source(&source).unwrap();

        assert_eq!(machine_code.labels.get("@label"), Some(&2));
    }

    #[test]
    fn label_duplicate_returns_error() {
        let source = AssemblySource::from("MOVS R0 x01\n@label\nMOVS R1 x02\n@label".to_string());

        let err = assemble_source(&source).unwrap_err();

        assert!(err.to_string().contains("Duplicate label '@label'"));
    }

    #[test]
    fn label_stores_multiple_labels() {
        let source = AssemblySource::from("MOVS R0 x01\n@label\n\n\n  ADDS R1 x02\n@loop".to_string());

        let machine_code = assemble_source(&source).unwrap();

        assert_eq!(machine_code.labels.get("@label"), Some(&2));
        assert_eq!(machine_code.labels.get("@loop"), Some(&4));
    }

}