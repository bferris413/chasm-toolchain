# hx

Convert ASCII files of plain hex dumps to binary format and vice versa.

An opinionated alternative to `xxd`.

## Build

```sh
cargo build --release
```

## Usage

```sh
# Convert a hex dump to binary.
cargo run -- input.hex output.bin

# Convert binary to hexadecimal text on stdout.
cargo run -- --reverse input.bin

# Format the output with 16 bytes per row, spaces, and offsets.
cargo run -- --reverse --row-len 16 --spaced --offsets input.bin output.hex
```

Whitespace and `;` comments in hex input are ignored. Run `cargo run -- --help` for all options.
