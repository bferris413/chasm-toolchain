#!/bin/bash
#
# Attach GDB to QEMU with a debug server running at 1234.

if [ -z "$1" ]; then
    echo "Usage: $0 <path to firmware>"
    exit 1
fi

arm-none-eabi-gdb \
    -ex "set architecture arm" \
    -ex "target remote :1234" \
    -ex "restore $1 binary 0x00000000"
