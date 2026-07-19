# chasm

An experimental assembler, linker, and terminal editor for Chasm assembly, currently targeting Cortex-M4F.

## Motivation

Chasm and the related [metal-arm](https://github.com/bferris413/metal-arm) project are vehicles for researching native toolchain and embedded development from scratch. The programs in `metal-arm` currently drive most of Chasm's features.

## Build

From this directory:

```sh
cargo build
cargo test
```

The default build includes the terminal editor. To build only the assembler and linker, disable default features:

```sh
cargo build --no-default-features
```

## Usage

```sh
# Assemble and link one or more .cas modules. One module must be named main.cas.
cargo run -- assemble main.cas other.cas

# Assemble modules without linking them.
cargo run -- assemble --no-link main.cas other.cas

# Link previously assembled modules.
cargo run -- link main.bin other.bin

# Open a .cas file or a project directory in the terminal editor.
cargo run -- code path/to/file.cas
```

A project directory must contain a `chasm.toml` file and a `src/` directory. Run `cargo run -- --help` or append `--help` to a subcommand for the complete CLI reference.

Chasm is under active development and its assembly language and file formats are not yet stable.
