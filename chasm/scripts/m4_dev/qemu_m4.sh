#!/bin/bash
#
# Starts QEMU emulating an M4 with a debug server listening at 1234.
#
# We're using Stellaris LM3S6965 for strictly toolchain-related development
# since QEMU supports the full board and it's very close to our real
# hardware (TI TM4C123GH6PM).

if [ -z "$1" ]; then
    printf "Usage: %s <path to firmware>\n" "$0"
    exit 1
fi

gdb_port=1234

printf "Starting QEMU with firmware: %s\n" "$1"
printf "GDB server listening at port: %s\n" "$gdb_port"
printf "\nCtrl-A → X to exit.\n\n"

qemu-system-arm \
    -M lm3s6965evb \
    -cpu cortex-m4 \
    -kernel "$1" \
    -nographic \
    -monitor tcp:127.0.0.1:4444,server,nowait \
    -S -gdb tcp::"$gdb_port"
