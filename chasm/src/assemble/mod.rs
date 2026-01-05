mod parse;
mod token;
pub (crate) mod codegen;
mod helpers;

use std::{borrow::Borrow, collections::{HashMap, HashSet}, fmt::{Debug, Display}, fs, ops::{Deref, DerefMut}};

use crate::{assemble::helpers::normalize_to_ascii_lower, AssembleArgs};
use token::tokenize;
use parse::parse;
use codegen::codegen;

use anyhow::{anyhow, bail, Context, Error, Result};

pub fn assemble(args: &AssembleArgs) -> Result<()> {
    let modname = args.file
        .file_stem()
        .ok_or_else(|| anyhow!("Failed to get filename from '{}'", args.file.display()))
        .map(|file_stem| file_stem.to_string_lossy())?;

    let assembly_module = fs::read_to_string(&args.file)
        .with_context(|| format!("Failed to read assembly file '{}'", &args.file.display()))
        .map(|source| source.into())
        .and_then(|s| assemble_source(modname, &s))?;

    let outfile = args.file.with_extension("bin");
    fs::write(&outfile, &assembly_module.code)
        .with_context(|| format!("Failed to write binary file '{}'", outfile.display()))?;

    Ok(())
}

fn assemble_source(modname: impl AsRef<str>, source: &AssemblySource) -> Result<AssemblyModule> {
    tokenize(source)
        .and_then(parse)
        .and_then(|ast| codegen(modname, ast))
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
    PseudoInstruction(PseudoInstruction),
    Register(Register),
    Label,
    LabelRef(RefKind),
    DefinedRef,
}

#[derive(Debug)]
enum RefKind {
    ModuleRef(ModuleRef),
    LocalRef
}

#[derive(Debug, Eq, PartialEq)]
pub struct ModuleRef {
    pub module: ModuleName,
    pub member: MemberName,
}

/// An offset from the start of an assembly module's code (i.e. from 0).
#[derive(Debug, Default, Clone, Copy, Eq, PartialEq)]
pub struct BaseOffset(pub usize);
impl Deref for BaseOffset {
    type Target = usize;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for BaseOffset {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl From<usize> for BaseOffset {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

#[derive(Debug)]
enum Instruction {
    Adds { dest: GeneralRegister, value: HexLiteral },
    Ands { dest: GeneralRegister, src: GeneralRegister },
    Branch { reference: String, cond: Option<Condition> },
    BranchExchange { branch_reg: BranchableRegister },
    BranchWithLink { reference: String, cond: Option<Condition> },
    Eors { dest: GeneralRegister, src: GeneralRegister },
    Ldr  { dest: GeneralRegister, src: GeneralRegister },
    Movs { dest: GeneralRegister, value: HexLiteral },
    Movt { dest: GeneralRegister, value: HexLiteral },
    Movw { dest: GeneralRegister, value: HexLiteral },
    Orrs { dest: GeneralRegister, src: GeneralRegister },
    Pop  { registers: HashSet<PoppableRegister> },
    Push { registers: HashSet<PushableRegister> },
    Str  { dest_addr_reg: GeneralRegister, src: GeneralRegister },
}

#[derive(Debug)]
enum PseudoInstruction {
    Define    { identifier: String, hex_literal: HexLiteral },
    DefinePub { identifier: String, hex_literal: HexLiteral },
    Import    { module_name: ModuleName },
    Mov       { reg: GeneralRegister, hex_literal: HexLiteral },
    PadWithTo { pad_with: u8, pad_to: BaseOffset },
    ThumbAddr { reference: String },
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum Condition {
    Eq = 0b0000,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum HexLiteral {
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
enum BranchableRegister {
    General(GeneralRegister),
    SP,
    LR,
    PC,
}
impl BranchableRegister {
    fn to_u8(&self) -> u8 {
        match self {
            BranchableRegister::General(gen_reg) => *gen_reg as u8,
            BranchableRegister::SP => 13,
            BranchableRegister::LR => 14,
            BranchableRegister::PC => 15,
        }
    }
}

impl TryFrom<Register> for BranchableRegister {
    type Error = Error;
    fn try_from(reg: Register) -> Result<Self, Self::Error> {
        match reg {
            Register::General(gen_reg) => Ok(BranchableRegister::General(gen_reg)),
            Register::SP => Ok(BranchableRegister::SP),
            Register::LR => Ok(BranchableRegister::LR),
            Register::PC => Ok(BranchableRegister::PC),
            Register::Special(r) => bail!("Special register '{r:?}' cannot be used in branch instructions"),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum PoppableRegister {
    General(GeneralRegister),
    LR,
    PC,
}

impl TryFrom<Register> for PoppableRegister {
    type Error = Error;
    fn try_from(reg: Register) -> Result<Self, Self::Error> {
        match reg {
            Register::General(gen_reg) => Ok(PoppableRegister::General(gen_reg)),
            Register::LR => Ok(PoppableRegister::LR),
            Register::PC => Ok(PoppableRegister::PC),
            Register::Special(r) => bail!("Special register '{r:?}' cannot be used in branch instructions"),
            Register::SP => {
                bail!("Register '{reg:?}' cannot be popped from the stack")
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum PushableRegister {
    General(GeneralRegister),
    LR,
}

impl TryFrom<Register> for PushableRegister {
    type Error = Error;
    fn try_from(reg: Register) -> Result<Self, Self::Error> {
        match reg {
            Register::General(gen_reg) => Ok(PushableRegister::General(gen_reg)),
            Register::LR => Ok(PushableRegister::LR),
            Register::Special(r) => bail!("Special register '{r:?}' cannot be used in branch instructions"),
            Register::SP | Register::PC => {
                bail!("Register '{reg:?}' cannot be pushed onto the stack")
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
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

#[derive(Debug, Default, Eq, PartialEq, Hash)]
pub struct ModuleName(String);
impl Borrow<str> for ModuleName {
    fn borrow(&self) -> &str {
        self.0.as_str()
    }
}
impl Display for ModuleName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Default, Eq, PartialEq, Hash)]
pub struct MemberName(String);
impl Borrow<str> for MemberName {
    fn borrow(&self) -> &str {
        self.0.as_str()
    }
}
impl Display for MemberName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Default)]
pub struct AssemblyModule {
    pub (crate) modname: String,
    pub (crate) code: Vec<u8>,
    pub (crate) labels: HashMap<String, BaseOffset>,
    pub (crate) definitions: HashMap<String, HexLiteral>,
    pub (crate) pub_definitions: HashMap<String, HexLiteral>,
    pub (crate) imports: HashSet<ModuleName>,
    pub (crate) linker_patches: Vec<LinkerPatch>,
}

/// A patch to be applied by the linker.
#[derive(Debug, Eq, PartialEq)]
pub enum LinkerPatch {
    LabelNewOffset(LabelNewOffsetPatch),
    Import(ImportPatch),
    BranchWithNewOffset(BranchPatch),
    BranchLinkWithNewOffset(BranchWithLinkPatch),
    ThumbAddrPseudoWithNewOffset(ThumbAddrPseudoPatch),
}

#[derive(Debug, Eq, PartialEq)]
pub struct LabelNewOffsetPatch {
    /// Where in the module's code the patch should be applied.
    pub patch_at: BaseOffset,
    /// The number of bytes to patch.
    pub patch_size: PatchSize,
    /// The unpatched value at the patch location.
    /// 
    /// This value will be added to the new starting offset in memory.
    pub unpatched_value: usize,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ImportPatch {
    /// Where in the module's code the patch should be applied.
    pub patch_at: BaseOffset,
    /// The number of bytes to patch.
    pub patch_size: PatchSize,
    /// Module to import the patch value from.
    pub import_module: ModuleRef,
}

/// Number of bytes to overwrite in a patch.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum PatchSize {
    U8 = 1,
    U16 = 2,
    U32 = 4,
    U64 = 8
}
impl PatchSize {
    pub(crate) fn max_value(&self) -> usize {
        match self {
            PatchSize::U8 => u8::MAX as usize,
            PatchSize::U16 => u16::MAX as usize,
            PatchSize::U32 => u32::MAX as usize,
            PatchSize::U64 => u64::MAX as usize,
        }
    }
}

/// A patch to be applied by the assembler.
//
// These are expected to be removed, eventually, since the linker must overwrite
// them anyways, but there's a bunch of tests that verify codegen using these
// patches, so they remain for now.
#[derive(Debug, Eq, PartialEq)]
pub enum AssemblerPatch {
    /// An in-place overwrite of raw bytes at a given offset.
    Raw(RawPatch),
    /// A patch that regenerates a branch instruction.
    Branch(BranchPatch),
    /// A patch that regenerates a branch with link instruction.
    BranchWithLink(BranchWithLinkPatch),
    /// A patch that recomputes a thumb address pseudo-instruction.
    ThumbAddrPseudo(ThumbAddrPseudoPatch),
}

#[derive(Debug, Eq, PartialEq)]
pub struct RawPatch {
    /// Where in the module's code the patch should be applied.
    patch_at: BaseOffset,
}

#[derive(Debug, Eq, PartialEq)]
pub struct BranchPatch {
    /// Where in the module's code the patch should be applied.
    pub patch_at: BaseOffset,
    pub reference: String,
    pub cond: Option<Condition>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct BranchWithLinkPatch {
    patch_at: BaseOffset,
    reference: String,
    cond: Option<Condition>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ThumbAddrPseudoPatch {
    pub patch_at: BaseOffset,
    pub reference: String,
}

#[derive(Clone, Debug)]
struct Token<'src> {
    kind: TokenKind,
    lexeme: &'src str,
    line: usize,
    column: usize,
    source: &'src AssemblySource,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum TokenKind {
    HexLiteralU32,
    HexLiteralU16,
    HexLiteralU8,
    Identifier,
    DefinedRef,
    Label,
    LabelRef,
    LBracket,
    RBracket,
    LCurly,
    RCurly,
    LParen,
    RParen,
    Comma,
    PseudoIdentifier,
    ModuleSep,
}
impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // The non-symbol representation of a token's kind
        match self {
            TokenKind::HexLiteralU32 => write!(f, "hex literal (u32)"),
            TokenKind::HexLiteralU16 => write!(f, "hex literal (u16)"),
            TokenKind::HexLiteralU8 => write!(f, "hex literal (u8)"),
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::Label => write!(f, "label"),
            TokenKind::LBracket => write!(f, "left bracket"),
            TokenKind::RBracket => write!(f, "right bracket"),
            TokenKind::LCurly => write!(f, "left curly"),
            TokenKind::RCurly => write!(f, "right curly"),
            TokenKind::LParen => write!(f, "left paren"),
            TokenKind::RParen => write!(f, "right paren"),
            TokenKind::LabelRef => write!(f, "& reference"),
            TokenKind::DefinedRef => write!(f, "$ reference"),
            TokenKind::PseudoIdentifier => write!(f, "pseudo identifier"),
            TokenKind::Comma => write!(f, "comma"),
            TokenKind::ModuleSep => write!(f, "module separator"),
        }
    }
}


#[derive(Debug)]
struct AssemblyError {
    message: String,
    line: usize,
    column: usize,
    end_column: Option<usize>,
    source: String,
}
impl AssemblyError {
    fn new(message: String, line: usize, column: usize, end_column: Option<usize>, source: &AssemblySource) -> Self {
        let source = source.source.clone(); 
        Self { message, line, column, end_column, source }
    }
}
impl Display for AssemblyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error at {}:{}: {}", self.line + 1, self.column + 1, self.message)
    }
}
impl std::error::Error for AssemblyError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hex_word_gets_generated() {
        let source = AssemblySource::from("x1234.5678".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![0x78, 0x56, 0x34, 0x12]);
    } 

    #[test]
    fn hex_half_word_gets_generated() {
        let source = AssemblySource::from("x1234".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![0x34, 0x12]);
    } 

    #[test]
    fn hex_byte_gets_generated() {
        let source = AssemblySource::from("x12".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![0x12]);
    } 

    #[test]
    fn too_short_hex_string_returns_error() {
        let source = AssemblySource::from("x123".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Hex literal must have an even number of digits"));
    }

    #[test]
    fn hex_string_with_non_halfword_separator_returns_error() {
        let source = AssemblySource::from("x123.45678".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Hex literal separator must be after a full half-word (4 digits)"));
    }

    #[test]
    fn hex_string_starting_with_separator_returns_error() {
        let source = AssemblySource::from("x.12345678".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Hex literal cannot start with a separator"));
    }

    #[test]
    fn hex_string_ending_with_separator_returns_error() {
        let source = AssemblySource::from("x1234.".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Hex literal cannot end with a separator"));
    }

    #[test]
    fn hex_string_with_non_halfword_after_separator_returns_error() {
        let source = AssemblySource::from("x1234.567".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Hex literal must have an even number of digits"));
    }

    #[test]
    fn hex_string_with_good_and_bad_separator_returns_error() {
        // first is fine, second is trailing
        let source = AssemblySource::from("x1234.5678.".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Hex literal cannot end with a separator"));
    }

    #[test]
    fn movs_with_well_formed_args_gets_generated() {
        let source = AssemblySource::from("MOVS R0 x01".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![0x01, 0x20]);
    }

    #[test]
    fn movs_with_missing_value_returns_error() {
        let source = AssemblySource::from("MOVS R0".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Expected '8-bit hex literal' after 'R0'"), "Err: {}", err);
    }

    #[test]
    fn movs_with_large_reg_returns_error() {
        let source = AssemblySource::from("MOVS R8 x01".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Expected general-purpose register (r0-r7)"));
    }

    #[test]
    fn movs_with_invalid_reg_returns_error() {
        let source = AssemblySource::from("MOVS xFF".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Expected 'register' after 'MOVS', found 'xFF'"), "Err: {}", err);
    }

    #[test]
    fn movs_with_eof_returns_error() {
        let source = AssemblySource::from("MOVS    ".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Expected 'register' after 'MOVS', found EOF"), "Err: {}", err);
    }

    #[test]
    fn adds_gets_generated() {
        let source = AssemblySource::from("ADDS R0 x01".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![0x01, 0x30]);
    }

    #[test]
    fn adds_with_r7_and_255_gets_generated() {
        let source = AssemblySource::from("ADDS R7 xFF".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![0xFF, 0x37]);
    }

    #[test]
    fn adds_with_large_reg_returns_error() {
        let source = AssemblySource::from("ADDS R12 xA1".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Expected general-purpose register (r0-r7)"));
    }

    #[test]
    fn adds_with_missing_value_returns_error() {
        let source = AssemblySource::from("ADDS R7".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Expected '8-bit hex literal' after 'R7'"), "Err: {}", err);
    }

    #[test]
    fn adds_with_invalid_reg_returns_error() {
        let source = AssemblySource::from("ADDS abc".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Invalid register 'abc' after 'ADDS'"), "Err: {}", err);
    }

    #[test]
    fn adds_with_eof_returns_error() {
        let source = AssemblySource::from("ADDS\n\n".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Expected 'register' after 'ADDS', found EOF"), "Err: {}", err);
    }

    #[test]
    fn label_stores_address_of_next_instruction() {
        let source = AssemblySource::from("
            MOVS R0 x01
            @label
            MOVS R1 x02
        ".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.labels.get("label"), Some(&BaseOffset(2)));
    }

    #[test]
    fn label_duplicate_returns_error() {
        let source = AssemblySource::from("
            MOVS R0 x01
            @label
            MOVS R1 x02
            @label
        ".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Duplicate label 'label'"));
    }

    #[test]
    fn label_stores_multiple_labels() {
        let source = AssemblySource::from("
            MOVS R0 x01
            @label
            
            
            ADDS R1 x02
            @loop
        ".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.labels.get("label"), Some(&BaseOffset(2)));
        assert_eq!(machine_code.labels.get("loop"), Some(&BaseOffset(4)));
    }

    #[test]
    fn branch_to_label_gets_generated() {
        let source = AssemblySource::from("
            MOVS R0 x01
            @loop
                ADDS R0 x01
                B &loop
        ".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![
            0x01, 0x20, // MOVS R0, #1
            0x01, 0x30, // ADDS R0, #1
            0xFD, 0xE7, // B to ADDS
        ]);
    }

    #[test]
    fn unaligned_instruction_returns_error() {
        let source = AssemblySource::from("
            x01
            MOVS R0 x01
        ".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Attempted to generate unaligned"));
    }

    #[test]
    fn branch_to_label_too_far_behind_generates_error() {
        let source = AssemblySource::from(format!("
            MOVS R0 x01
            @loop
                {}
                B &loop
        ", "x01\n".repeat(4096)));

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Branch target '&loop' is too far away (offset -4100)"), "Err: {}", err);
    }

    #[test]
    fn comments_get_skipped() {
        let source = AssemblySource::from("
            ; An arbitrary comment
            x01010101
            ADDS R0 x01 ; r0 += 1
            ; ...
        ".to_string());

        let machine_code = assemble_source("test", &source).unwrap(); 

        assert_eq!(machine_code.code, vec![
            0x01, 0x01, 0x01, 0x01,
            0x01, 0x30, // ADDS R0, #1
        ]);
    }

    #[test]
    fn minimal_program_assembles() {
        // This code is from metal-arm/0 and is known to run on ek-tm4c123gxl
        let source = AssemblySource::from("
            ; vector table
            x2000.8000         ; sp
            x0000.0009         ; reset handler address | 1 (thumb mode)

            ; Reset handler - Initializes and continuously increments R0
            MOVS R0 x01

            @loop
                ADDS R0 x01
                B &loop
        ".to_string());
        let exp_bytes = vec![
            0x00, 0x80, 0x00, 0x20, // sp addr
            0x09, 0x00, 0x00, 0x00, // reset handler addr
            0x01, 0x20, // MOVS
            0x01, 0x30, // ADD
            0xFD, 0xE7  // B
        ];

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, exp_bytes);
    }

    #[test]
    fn movw_gets_generated() {
        let source = AssemblySource::from("MOVW R5 xFFFF".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // b11110_1_100100_1111_0_111_0101_11111111
        // b11110110_01001111_01110101_11111111
        // xF64F.75FF
        // x4FF6.FF75
        assert_eq!(machine_code.code, vec![ 0x4F, 0xF6, 0xFF, 0x75 ]);
    }

    #[test]
    fn movw_with_high_nibble_register_gets_generated() {
        let source = AssemblySource::from("MOVW R9 xFFFF".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // b11110_1_100100_1111_0_111_1001_11111111
        // b11110110_01001111_01111001_11111111
        // xF64F.79FF
        // x4FF6.FF79
        assert_eq!(machine_code.code, vec![ 0x4F, 0xF6, 0xFF, 0x79 ]);
    }

    #[test]
    fn movt_gets_generated() {
        let source = AssemblySource::from("MOVT R2 x0001".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // b11110010_11000000_00000010_00000001
        // xF2C0.0201
        // xC0F2.0102
        assert_eq!(machine_code.code, vec![ 0xC0, 0xF2, 0x01, 0x02 ]);
    }

    #[test]
    fn movt_with_high_nibble_register_gets_generated() {
        let source = AssemblySource::from("MOVT R12 xFFFF".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // b11110_1_101100_1111_0_111_1100_11111111
        // b11110110_11001111_01111100_11111111
        // xF6CF.7CFF
        // xCFF6.FF7C
        assert_eq!(machine_code.code, vec![ 0xCF, 0xF6, 0xFF, 0x7C ]);
    }

    #[test]
    fn ldr_immediate_with_3_bit_src_dest_gets_generated_as_t1_encoding() {
        let source = AssemblySource::from("LDR R1 [R0]".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // b01101_00000_000_001
        // b01101000_00000001
        // x6801
        // x0168
        assert_eq!(machine_code.code, vec![ 0x01, 0x68 ]);
    }

    #[test]
    fn ldr_immediate_with_another_3_bit_src_dest_gets_generated_as_t1_encoding() {
        let source = AssemblySource::from("LDR R4 [R2]".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // b01101_00000_010_100
        // b01101000_00010100
        // x6814
        // x1468
        assert_eq!(machine_code.code, vec![ 0x14, 0x68 ]);
    }

    #[test]
    fn ldr_immediate_with_4_bit_src_gets_generated_as_t3_encoding() {
        let source = AssemblySource::from("LDR R2 [R9]".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // b111110001101_1001_0010_000000000000
        // b11111000_11011001_00100000_00000000
        // xD9F8.0020
        assert_eq!(machine_code.code, vec![ 0xD9, 0xF8, 0x00, 0x20 ]);
    }

    #[test]
    fn ldr_immediate_with_4_bit_dest_gets_generated_as_t3_encoding() {
        let source = AssemblySource::from("LDR R12 [R1]".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // b111110001101_0001_1100_000000000000
        // b11111000_11010001_11000000_00000000
        // xD1F8.00C0
        assert_eq!(machine_code.code, vec![ 0xD1, 0xF8, 0x00, 0xC0 ]);
    }

    #[test]
    fn orrs_register_gets_generated_as_t1_encoding() {
        let source = AssemblySource::from("ORRS R3 R5".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // b0100001100_101_011
        // b01000011_00101011
        // x2B43
        assert_eq!(machine_code.code, vec![ 0x2B, 0x43 ]);
    }

    #[test]
    fn str_immediate_gets_generated_as_t1_encoding() {
        let source = AssemblySource::from("STR R2 [R7]".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // b0110000000_111_010
        // b01100000_00111010
        // x3A60
        assert_eq!(machine_code.code, vec![ 0x3A, 0x60 ]);
    }

    #[test]
    fn ands_register_gets_generated_as_t1_encoding() {
        let source = AssemblySource::from("ANDS R6 R1".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // b0100000000_001_110
        // b01000000_00001110
        // x0E40
        assert_eq!(machine_code.code, vec![ 0x0E, 0x40 ]);
    }

    #[test]
    fn beq_gets_generated_as_t1_encoding() {
        let source = AssemblySource::from("@label BEQ &label".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // b11010000_11111110
        // xFED0
        assert_eq!(machine_code.code, vec![ 0xFE, 0xD0 ]);
    }

    #[test]
    fn standalone_ref_resolves_to_label_address() {
        let source = AssemblySource::from("
            x1111.1111
            @label
            x2222.2222
            &label 
        "
        .to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![
            0x11, 0x11, 0x11, 0x11,
            0x22, 0x22, 0x22, 0x22,
            0x04, 0x00, 0x00, 0x00, // address of label (8 bytes in)
        ]);
    }

    #[test]
    fn forward_ref_resolves_to_label_address() {
        let source = AssemblySource::from("
            &label
            x1111.1111
            x2222.2222
            x3333.3333
            @label
        ".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![
            0x10, 0x00, 0x00, 0x00, // address of label (16 bytes in)
            0x11, 0x11, 0x11, 0x11,
            0x22, 0x22, 0x22, 0x22,
            0x33, 0x33, 0x33, 0x33,
        ]);
    }

    #[test]
    fn forward_ref_and_back_ref_both_resolve_to_label_address() {
        let source = AssemblySource::from("
            &label
            x1111.1111
            x2222.2222
            @label
            x3333.3333
            x4444.4444
            &label
        ".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![
            0x0C, 0x00, 0x00, 0x00, // address of label (12 bytes in)
            0x11, 0x11, 0x11, 0x11,
            0x22, 0x22, 0x22, 0x22,
            0x33, 0x33, 0x33, 0x33,
            0x44, 0x44, 0x44, 0x44,
            0x0C, 0x00, 0x00, 0x00, // address of label (12 bytes in)
        ]);
    }

    #[test]
    fn branch_to_label_supports_forward_refs() {
        let source = AssemblySource::from("
            B &label
            MOVS R0 x01
            MOVS R0 x01
            MOVS R0 x01
            MOVS R0 x01
            MOVS R0 x01
            MOVS R0 x01
            MOVS R0 x01
            MOVS R0 x01
            @label
            MOVS R0 x02
            MOVS R0 x02
        ".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![
            // branch
            0x07, 0xE0,
            // multiple movs
            0x01, 0x20,
            0x01, 0x20,
            0x01, 0x20,
            0x01, 0x20,
            0x01, 0x20,
            0x01, 0x20,
            0x01, 0x20,
            0x01, 0x20,
            // the branched-to instruction
            0x02, 0x20,
            0x02, 0x20
        ]);
    }

    #[test]
    fn thumb_addr_pseudo_instr_evaluates_addr_then_ors_with_1_for_forward_refs() {
        let source = AssemblySource::from("
            thumb-addr!(&label)
            @label
        ".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![
            0x05, 0x00, 0x00, 0x00, // address of label (4 bytes in) | 1
        ]);
    }

    #[test]
    fn thumb_addr_pseudo_instr_evaluates_addr_then_ors_with_1_for_backward_refs() {
        let source = AssemblySource::from("
            x0000.0000
            x1111.1111
            @label
            x2222.2222
            x3333.3333
            x4444.4444
            thumb-addr!(&label)
        ".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![
            0x00, 0x00, 0x00, 0x00,
            0x11, 0x11, 0x11, 0x11,
            0x22, 0x22, 0x22, 0x22, // label here
            0x33, 0x33, 0x33, 0x33,
            0x44, 0x44, 0x44, 0x44,
            0x09, 0x00, 0x00, 0x00, // address of label (8 bytes in) | 1
        ]);
    }

    #[test]
    fn pad_with_to_pseudo_instr_fills_with_bytes_to_specified_addr() {
        let source = AssemblySource::from("
            x2222.2222
            pad-with-to!(x11, x0000.0100)
        ".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        let exp_bytes = {
            let mut bytes = vec![ 0x22, 0x22, 0x22, 0x22, ];
            bytes.extend(vec![0x11; 0x100 - bytes.len()]);
            bytes
        };
        assert_eq!(machine_code.code, exp_bytes);
    }

    #[test]
    fn pad_with_to_pseudo_instr_errors_when_target_addr_is_behind_current_addr() {
        let source = AssemblySource::from("
            x2222.2222
            x3333.3333
            pad-with-to!(x11, x0000.0000)
        ".to_string());

        let machine_code = assemble_source("test", &source);

        assert!(machine_code.is_err(), "{:?}", machine_code.unwrap());
        let err = machine_code.unwrap_err();
        assert!(err.to_string().contains("target address is behind current address"), "{:?}", err);
    }

    #[test]
    fn eors_with_two_register_ops_gets_generated_as_thumb1_t1() {
        let source = AssemblySource::from("EORS R1 R7".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // 01000000_01_111_001
        // 01000000_01111001
        assert_eq!(machine_code.code, vec![0x79, 0x40]);
    }

    #[test]
    fn eors_with_single_register_errors() {
        let source = AssemblySource::from("EORS R1".to_string());

        let machine_code = assemble_source("test", &source);

        assert!(machine_code.is_err(), "{:?}", machine_code.unwrap());
        let err = machine_code.unwrap_err().to_string();
        assert!(err.contains("Expected 'register' after 'R1'"), "{err}");
    }

    #[test]
    fn bx_with_lr_gets_generated_as_thumb1_t1() {
        let source = AssemblySource::from("BX LR".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // 01000111_0_1110_000
        // 01000111_01110000
        assert_eq!(machine_code.code, vec![0x70, 0x47]);
    }

    #[test]
    fn bl_with_forward_label_gets_generated_with_positive_addr() {
        let source = AssemblySource::from("
            BL &label
            x0000.0000
            x1111.1111
            @label
        ".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![
            0x00, 0xF0, 0x04, 0xF8,
            0x00, 0x00, 0x00, 0x00,
            0x11, 0x11, 0x11, 0x11
        ]);
    }

    #[test]
    fn bl_with_backward_label_gets_generated_with_negative_addr() {
        let source = AssemblySource::from("
            @label
            x0000.0000
            x1111.1111
            x2222.2222
            x3333.3333
            BL &label
        ".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![
            0x00, 0x00, 0x00, 0x00,
            0x11, 0x11, 0x11, 0x11,
            0x22, 0x22, 0x22, 0x22,
            0x33, 0x33, 0x33, 0x33,
            0xFF, 0xF7, 0xF6, 0xFF,
        ]);
    }

    #[test]
    fn push_with_r0_through_r7_gets_generated_as_t1_encoding() {
        let source = AssemblySource::from("PUSH {R0, R1, R2, R3, R4, R5, R6, R7, LR}".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // b1011010_1_11111111
        // b10110101_11111111
        assert_eq!(machine_code.code, vec![0xFF, 0xB5]);
    }

    #[test]
    fn push_with_duplicate_registers_errors() {
        let source = AssemblySource::from("PUSH {R0, R6, R7, R0}".to_string());

        let machine_code = assemble_source("test", &source);

        assert!(machine_code.is_err(), "{:?}", machine_code.unwrap());
        let err = machine_code.unwrap_err().to_string();
        assert!(err.contains("Register 'R0' specified multiple times"), "{err}");
    }

    #[test]
    fn push_with_no_registers_errors() {
        let source = AssemblySource::from("PUSH {}".to_string());

        let machine_code = assemble_source("test", &source);

        assert!(machine_code.is_err(), "{:?}", machine_code.unwrap());
        let err = machine_code.unwrap_err().to_string();
        assert!(err.contains("Expected 'register' after 'PUSH'"), "{err}");
    }

    #[test]
    fn push_with_unsupported_registers_in_list_errors() {
        let source = AssemblySource::from("PUSH {r1, r2, SP}".to_string());

        let machine_code = assemble_source("test", &source);

        assert!(machine_code.is_err(), "{:?}", machine_code.unwrap());
        let err = machine_code.unwrap_err().to_string();
        assert!(err.contains("Invalid pushable register 'SP'"), "{err}");
    }

    #[test]
    fn push_with_other_token_after_register_in_list_errors() {
        let source = AssemblySource::from("PUSH {r1 LR}".to_string());

        let machine_code = assemble_source("test", &source);

        assert!(machine_code.is_err(), "{:?}", machine_code.unwrap());
        let err = machine_code.unwrap_err().to_string();
        assert!(err.contains("Expected ',' or '}' after register"), "{err}");
    }

    #[test]
    fn push_with_nothing_after_register_in_list_errors() {
        let source = AssemblySource::from("PUSH { LR".to_string());

        let machine_code = assemble_source("test", &source);

        assert!(machine_code.is_err(), "{:?}", machine_code.unwrap());
        let err = machine_code.unwrap_err().to_string();
        assert!(err.contains("found EOF"), "{err}");
    }

    #[test]
    fn pop_with_registers_and_pc_gets_generated_as_t1_encoding() {
        let source = AssemblySource::from("POP {R1, R2, R4, R5, R6, R7, PC}".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        // b1011110_1_11110110
        // b10111101_11110110
        assert_eq!(machine_code.code, vec![0xF6, 0xBD]);
    }

    #[test]
    fn pop_with_missing_command_returns_err() {
        let source = AssemblySource::from("POP {R1 PC}".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Expected ',' or '}'"), "Err: {}", err);
    }

    #[test]
    fn pop_with_empty_register_set_returns_error() {
        let source = AssemblySource::from("POP {}".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Expected 'register' after 'POP'"), "Err: {}", err);
    }

    #[test]
    fn pop_with_lr_and_pc_returns_error() {
        let source = AssemblySource::from("POP {R1, LR, R0, PC}".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("cannot pop both LR and PC"), "Err: {}", err);
    }

    #[test]
    fn define_stores_hex_literal_with_identifier() {
        let source = AssemblySource::from("define!(GPIO-F, x1111.2222)".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        let hex_lit = machine_code.definitions.get("GPIO-F").unwrap();
        assert_eq!(*hex_lit, HexLiteral::U32(0x11112222));
    }

    #[test]
    fn define_without_identifier_fails() {
        let source = AssemblySource::from("define!(x1000, x1111.2222)".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Expected 'identifier' after '('"), "Err: {}", err);
    }

    #[test]
    fn define_without_comma_fails() {
        let source = AssemblySource::from("define!(BASE-LENGTH x1111.2222)".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Expected ',' after 'BASE-LENGTH'"), "Err: {}", err);
    }

    #[test]
    fn empty_define_fails() {
        let source = AssemblySource::from("define!()".to_string());

        let err = assemble_source("test", &source).unwrap_err();

        assert!(err.to_string().contains("Expected 'identifier' after '('"), "Err: {}", err);
    }

    #[test]
    fn defined_reference_standalone_to_identifier_gets_resolved() {
        let source = AssemblySource::from("
            define!(BASE-ADDR, x1111.2222)
            $BASE-ADDR
        ".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![
            0x22, 0x22, 0x11, 0x11
        ]);
    }

    #[test]
    fn mov_pseudo_with_u32_gets_generated_as_movw_movt() {
        let source = AssemblySource::from("mov!(r9, x0102.FFFF)".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![
            0x4F, 0xF6, 0xFF, 0x79, // MOVW R9 xFFFF
            0xC0, 0xF2, 0x02, 0x19, // MOVT R9 x0102
        ]);
    }

    #[test]
    fn mov_pseudo_with_u16_gets_generated_as_movw() {
        let source = AssemblySource::from("mov!(r0, x1234)".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![
            0x41, 0xF2, 0x34, 0x20, // MOVW R0 x1234
        ]);
    }

    #[test]
    fn mov_pseudo_with_u8_gets_generated_as_movs() {
        let source = AssemblySource::from("mov!(r4, x12)".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert_eq!(machine_code.code, vec![
            0x12, 0x24, // MOVS R4 x12
        ]);
    }

    #[test]
    fn import_pseudo_stores_imports() {
        let source = AssemblySource::from("import!(tm4c123gh6pm)".to_string());

        let machine_code = assemble_source("test", &source).unwrap();

        assert!(machine_code.imports.contains("tm4c123gh6pm"));
    }

    #[test]
    fn define_pub_pseudo_creates_a_publicly_visible_definition() {
        let source = AssemblySource::from("define-pub!(code-start, x2000.0000)".to_string());
        let exp_hex_lit = HexLiteral::U32(0x20000000);

        let machine_code = assemble_source("test", &source).unwrap();

        let res = machine_code.pub_definitions.get("code-start").unwrap();
        assert_eq!(res, &exp_hex_lit);
    }

    #[test]
    fn import_reference_creates_linker_patch_in_module() {
        let source = AssemblySource::from("
            import!(my-mod)
            &my-mod::SOME-CONST
        ".to_string());

        let module = assemble_source("test", &source).unwrap();

        let exp_patch = LinkerPatch::Import(ImportPatch {
            patch_at: BaseOffset(0),
            patch_size: PatchSize::U32,
            import_module: ModuleRef {
                module: ModuleName("my-mod".to_string()),
                member: MemberName("SOME-CONST".to_string())
            }
        });

        assert_eq!(module.linker_patches[0], exp_patch);
        assert_eq!(module.linker_patches.len(), 1);
    }

    #[test]
    fn import_reference_without_import_returns_err() {
        let source = AssemblySource::from("
            &my-mod::SOME-CONST
        ".to_string());

        let err = assemble_source("test", &source).unwrap_err().to_string();

        assert!(err.contains("Module 'my-mod' has not been imported"), "Err: {}", err);
    }

    #[test]
    fn label_ref_with_label_adds_linker_patch() {
        let source = AssemblySource::from("
            x1111.1111
            @start
            x2222.2222
            &start
        ".to_string());

        let module = assemble_source("test", &source).unwrap();

        let exp_patch = LinkerPatch::LabelNewOffset(LabelNewOffsetPatch {
            patch_at: BaseOffset(8),
            patch_size: PatchSize::U32,
            unpatched_value: 4,
        });

        assert_eq!(module.linker_patches[0], exp_patch);
        assert_eq!(module.linker_patches.len(), 1);
    }

    #[test]
    fn raw_forward_label_ref_adds_linker_patch() {
        let source = AssemblySource::from("
            &start
            x1111.1111
            x2222.2222
            @start
        ".to_string());

        let module = assemble_source("test", &source).unwrap();

        let exp_patch = LinkerPatch::LabelNewOffset(LabelNewOffsetPatch {
            patch_at: BaseOffset(0),
            patch_size: PatchSize::U32,
            unpatched_value: 12,
        });

        assert_eq!(module.linker_patches[0], exp_patch);
        assert_eq!(module.linker_patches.len(), 1);
    }

    #[test]
    fn branch_patch_with_forward_ref_adds_linker_patch() {
        let source = AssemblySource::from("
            B &start
            x1111.1111
            x2222.2222
            @start
        ".to_string());

        let module = assemble_source("test", &source).unwrap();

        let exp_patch = LinkerPatch::BranchWithNewOffset(BranchPatch {
            patch_at: BaseOffset(0),
            reference: "&start".to_string(),
            cond: None,
        });

        assert_eq!(module.linker_patches[0], exp_patch);
        assert_eq!(module.linker_patches.len(), 1);
    }

    #[test]
    fn branch_with_link_patch_with_forward_ref_adds_linker_patch() {
        let source = AssemblySource::from("
            BL &start
            x1111.1111
            x2222.2222
            @start
        ".to_string());

        let module = assemble_source("test", &source).unwrap();

        let exp_patch = LinkerPatch::BranchLinkWithNewOffset(BranchWithLinkPatch {
            patch_at: BaseOffset(0),
            reference: "&start".to_string(),
            cond: None
        });

        assert_eq!(module.linker_patches[0], exp_patch);
        assert_eq!(module.linker_patches.len(), 1);
    }

    #[test]
    fn thumb_addr_pseudo_patch_with_forward_ref_adds_linker_patch() {
        let source = AssemblySource::from("
            thumb-addr!(&start)
            x1111.1111
            @start
            x2222.2222
        ".to_string());

        let module = assemble_source("test", &source).unwrap();

        let exp_patch = LinkerPatch::ThumbAddrPseudoWithNewOffset(ThumbAddrPseudoPatch {
            patch_at: BaseOffset(0),
            reference: "&start".to_string(),
        });

        assert_eq!(module.linker_patches[0], exp_patch);
        assert_eq!(module.linker_patches.len(), 1);
    }

    #[test]
    fn branch_to_label_adds_linker_patch() {
        let source = AssemblySource::from("
            MOVS R0 x01
            @loop
                ADDS R0 x01
                B &loop
        ".to_string());

        let module = assemble_source("test", &source).unwrap();

        let exp_patch = LinkerPatch::BranchWithNewOffset(BranchPatch {
            patch_at: BaseOffset(4),
            reference: "&loop".to_string(),
            cond: None,
        });

        assert_eq!(module.linker_patches[0], exp_patch);
        assert_eq!(module.linker_patches.len(), 1);
    }

    #[test]
    fn branch_with_link_to_label_adds_linker_patch() {
        let source = AssemblySource::from("
            @myfunc
            BL &myfunc
        ".to_string());

        let module = assemble_source("test", &source).unwrap();

        let exp_patch = LinkerPatch::BranchLinkWithNewOffset(BranchWithLinkPatch {
            patch_at: BaseOffset(0),
            reference: "&myfunc".to_string(),
            cond: None,
        });

        assert_eq!(module.linker_patches[0], exp_patch);
        assert_eq!(module.linker_patches.len(), 1);
    }
}