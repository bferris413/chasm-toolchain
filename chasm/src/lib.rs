use std::path::{Path, PathBuf};

use clap::Args;

pub mod assemble;
#[cfg(feature = "code")]
pub mod code;
pub mod link;
pub mod project;

#[derive(Args, Debug)]
pub struct AssembleArgs {
    /// The .cas files to assemble
    files: Vec<PathBuf>,

    /// Output the machine code as a hex text file instead of raw binary
    #[arg(short, long, default_value_t = false)]
    text: bool,

    /// If the assembler should skip linking (default behavior is to link)
    #[arg(long, default_value_t = false)]
    no_link: bool,
}

#[derive(Args, Debug)]
pub struct LinkArgs {
    /// The chasm modules to link
    files: Vec<PathBuf>,
}

#[cfg(feature = "code")]
#[derive(Args, Debug)]
pub struct CodeArgs {
    /// The directory of the Chasm project
    #[arg(default_value_t = ProjectDir(PathBuf::from(".")), value_parser = parse_project_dir)]
    project_dir: ProjectDir,
}

#[derive(Clone, Debug)]
struct ProjectDir(PathBuf);
impl std::fmt::Display for ProjectDir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.display().fmt(f)
    }
}

fn parse_project_dir(v: &str) -> Result<ProjectDir, String> {
    let path = Path::new(v);
    if !path.is_dir() {
        return Err(format!("{} is not a directory", v));
    }

    let fullpath = path.canonicalize().map_err(|e| format!("{e}"))?;
    Ok(ProjectDir(fullpath))
}