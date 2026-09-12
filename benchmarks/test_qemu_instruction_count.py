#!/usr/bin/env python3
"""Tests for architecture selection in the QEMU instruction counter."""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path


REPOSITORY = Path(__file__).resolve().parent.parent
COUNTER = REPOSITORY / "benchmarks" / "infrastructure" / "qemu_instruction_count.sh"


class QemuInstructionCounterTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        (self.root / "libinsn.so").touch()
        self.binary = self.root / "guest"
        self.binary.write_text("#!/usr/bin/env bash\nexit 0\n")
        self.binary.chmod(0o755)

        emulator = """#!/usr/bin/env bash
set -euo pipefail
architecture="$(basename "$0" | sed 's/qemu-//')"
if [[ "${1:-}" == "--version" ]]; then
    echo "qemu-${architecture} version 11.1.1"
    exit 0
fi
printf '%s\n' "$@" > "$(dirname "$0")/${architecture}.args"
echo "guest output"
echo "total insns: 42" >&2
"""
        for architecture in ("aarch64", "x86_64"):
            path = self.root / f"qemu-{architecture}"
            path.write_text(emulator)
            path.chmod(0o755)

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def run_counter(self, architecture: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [
                str(COUNTER),
                f"--qemu-root={self.root}",
                architecture,
                str(self.binary),
                "argument",
            ],
            text=True,
            capture_output=True,
            check=False,
        )

    def test_arm64_selects_aarch64_qemu_and_sysroot(self) -> None:
        result = self.run_counter("arm64")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "guest output\n")
        arguments = (self.root / "aarch64.args").read_text().splitlines()
        self.assertIn("/usr/aarch64-linux-gnu", arguments)
        self.assertIn(str(self.binary), arguments)

    def test_x86_64_selects_x86_64_qemu_and_sysroot(self) -> None:
        result = self.run_counter("x86_64")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "guest output\n")
        arguments = (self.root / "x86_64.args").read_text().splitlines()
        self.assertIn("/usr/x86_64-linux-gnu", arguments)
        self.assertIn(str(self.binary), arguments)

    def test_unknown_architecture_is_rejected(self) -> None:
        result = self.run_counter("riscv64")
        self.assertEqual(result.returncode, 2)
        self.assertIn("Unsupported guest architecture: riscv64", result.stderr)


if __name__ == "__main__":
    unittest.main()
