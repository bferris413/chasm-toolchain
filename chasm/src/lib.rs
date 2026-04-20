use std::path::{Path, PathBuf};

use clap::Args;

use crate::project::ChasmProject;

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
    #[arg(value_parser = parse_file_or_project)]
    file_or_project: FileOrProject,

    // #[arg(default_value_t)]
    // log_to: PathBuf,
}

#[derive(Clone, Debug)]
enum FileOrProject {
    // path to a plain file
    File(PathBuf),
    // path to a project directory (must contain chasm.toml and src/)
    Project(ChasmProject),
}
impl std::fmt::Display for FileOrProject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileOrProject::File(path) => path.display().fmt(f),
            FileOrProject::Project(project) => write!(f, "{project}"),
        }
    }
}

fn parse_file_or_project(v: &str) -> Result<FileOrProject, String> {
    let path = Path::new(v);
    let fullpath = path.canonicalize().map_err(|e| format!("{e}"))?;

    if fullpath.is_dir() {
        let project = ChasmProject::load(fullpath)
            .map_err(|e| format!("{e}"))?;

        Ok(FileOrProject::Project(project))
    } else if fullpath.is_file() {
        Ok(FileOrProject::File(fullpath))
    } else {
        Err(format!("{} is not a valid file or project path", v))
    }

}