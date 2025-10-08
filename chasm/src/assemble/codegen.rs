//! Machine code generation for chasm assembly source code.

use anyhow::Result;

use crate::assemble::{AssemblyAst, MachineCode, NodeKind};

pub(crate) fn codegen(ast: AssemblyAst<'_>) -> Result<MachineCode> {
    let mut code_bytes = Vec::new();
    for node in ast.nodes {
        match node.kind {
            NodeKind::HexLiteralU32(value) => {
                code_bytes.extend(&value.to_le_bytes());
            }
            NodeKind::HexLiteralU16(value) => {
                code_bytes.extend(&value.to_le_bytes());
            }
            NodeKind::HexLiteralU8(value) => {
                code_bytes.push(value);
            }
        }
    }
    
    Ok(MachineCode { bytes: code_bytes })
}