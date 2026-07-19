# chasm-toolchain

An independent toolchain targeting embedded ARM.

This repository contains two standalone Rust crates:

- [`chasm`](chasm/) — an experimental assembler, linker, and terminal editor for Chasm assembly. It currently targets Cortex-M4F.
- [`hx`](hx/) — a small utility for converting plain hexadecimal dumps to binary files and back.

## Build

Each crate is built independently:

```sh
cargo build --manifest-path chasm/Cargo.toml
cargo build --manifest-path hx/Cargo.toml
```

See each crate's README for usage details.
