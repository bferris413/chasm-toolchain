use std::path::PathBuf;

use anyhow::{Result, bail};

const LOAD_ERROR: &str = "Error loading project";
const CONFIG_FILE_NAME: &'static str = "chasm.toml";

/// The shape of a Chasm project on disk.
#[derive(Debug)]
pub struct ChasmProject {
    root: PathBuf,
    config: ProjectConfig,
}
impl ChasmProject {

    /// Load a Chasm project from the given root directory.
    /// 
    /// The root must contain a `chasm.toml` config file and a `src` directory.
    pub fn load(root: PathBuf) -> Result<Self> {
        if !root.is_dir() {
            bail!("{LOAD_ERROR}: {} is not a directory", root.display());
        }

        let config_path = root.join(CONFIG_FILE_NAME);
        if !config_path.is_file() {
            bail!("{LOAD_ERROR}: missing config file {}", config_path.display());
        }

        // TODO actually define what a config consits of and load it...
        let src_dir = root.join("src");
        if !src_dir.is_dir() {
            bail!("{LOAD_ERROR}: missing source directory {}", src_dir.display());
        }

        Ok(Self {
            root,
            config: ProjectConfig {},
        })
    }

    pub fn module_names(&self) -> Vec<ModulePath> {
        let src_dir = self.root.join("src");
        let mut modules = vec![];
        if let Ok(entries) = src_dir.read_dir() {
            for entry in entries.flatten() {
                if let Ok(file_type) = entry.file_type() {
                    if file_type.is_file() && is_chasm_assembly_file(&entry.path()) {
                        let path = entry.path();
                        let module_name = path.file_stem().unwrap();

                        modules.push(ModulePath {
                            path: entry.path(),
                            name: module_name.to_string_lossy().to_string(),
                        });
                    }
                }
            }
        }
        modules
    }
}    

fn is_chasm_assembly_file(path: &PathBuf) -> bool {
    path.extension().and_then(|s| s.to_str()) == Some("cas")
}

#[derive(Debug, Clone)]
pub struct ModulePath {
    pub path: PathBuf,
    pub name: String,
}

#[derive(Debug)]
struct ProjectConfig {
    // TODO
}