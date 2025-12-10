#!/bin/bash
#
# Starts QEMU emulating an M4 with a debug server listening at 1234.
#
# We're using Stellaris LM3S6965 for strictly toolchain-related development
# since QEMU supports the full board and it's very close to our real
# hardware TM4C123GH6PM.

if [ -z "$1" ]; then
    echo "Usage: $0 <path to firmware>"
    exit 1
fi

qemu-system-arm \
    -M lm3s6965evb \
    -cpu cortex-m4 \
    -kernel "$1" \
    -nographic \
    -S -gdb tcp::1234
