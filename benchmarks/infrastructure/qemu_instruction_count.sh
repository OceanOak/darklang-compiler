#!/usr/bin/env bash
# Count dynamically executed guest instructions with the pinned QEMU plugin.

set -euo pipefail

qemu_root="/opt/dcb/qemu"
if [[ "${1:-}" == --qemu-root=* ]]; then
    qemu_root="${1#--qemu-root=}"
    shift
fi

if [[ $# -lt 2 ]]; then
    echo "Usage: qemu_instruction_count.sh [--qemu-root=PATH] <arm64|x86_64> <binary> [arguments ...]" >&2
    exit 2
fi

architecture="$1"
shift
binary="$(realpath "$1")"
shift

case "$architecture" in
    arm64|aarch64)
        architecture="arm64"
        qemu_architecture="aarch64"
        sysroot="/usr/aarch64-linux-gnu"
        ;;
    x86_64|amd64)
        architecture="x86_64"
        qemu_architecture="x86_64"
        sysroot="/usr/x86_64-linux-gnu"
        ;;
    *)
        echo "Unsupported guest architecture: $architecture" >&2
        exit 2
        ;;
esac

qemu="$qemu_root/qemu-${qemu_architecture}"
plugin="$qemu_root/libinsn.so"

if [[ ! -x "$binary" ]]; then
    echo "Not an executable file: $binary" >&2
    exit 2
fi
if [[ ! -x "$qemu" || ! -r "$plugin" ]]; then
    echo "Pinned QEMU $architecture instruction counter is unavailable" >&2
    exit 1
fi
if [[ "$($qemu --version | head -n 1)" != "qemu-${qemu_architecture} version 11.1.1" ]]; then
    echo "Unexpected QEMU $architecture instruction counter version" >&2
    exit 1
fi

exec /usr/bin/timeout --signal=KILL 20s \
    "$qemu" \
    -L "$sysroot" \
    -d plugin \
    -D /dev/stderr \
    -plugin "$plugin,inline=on" \
    "$binary" "$@"
