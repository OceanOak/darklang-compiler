#!/usr/bin/env python3
"""
Diff local upstream execution tests against darklang/dark upstream.

This script treats `Builtin.testDerrorMessage` and `error="..."` as equivalent.
Any other content changes are reported as diffs.
"""

from __future__ import annotations

import argparse
import difflib
import re
import subprocess
import sys
import tempfile
from pathlib import Path


PAT_PAREN_INLINE = re.compile(
    r'\(\s*Builtin\.testDerrorMessage\s+"((?:[^"\\]|\\.)*)"\s*\)'
)
PAT_INLINE = re.compile(r'Builtin\.testDerrorMessage\s+"((?:[^"\\]|\\.)*)"')
PAT_NEXT_LINE_MESSAGE = re.compile(
    r'^\s*"(?P<message>(?:[^"\\]|\\.)*)"\s*(?P<close>\)?)\s*$'
)

ANSI_RESET = "\033[0m"
ANSI_BOLD = "\033[1m"
ANSI_DIM = "\033[2m"
ANSI_RED = "\033[31m"
ANSI_GREEN = "\033[32m"
ANSI_YELLOW = "\033[33m"
ANSI_CYAN = "\033[36m"


def run(cmd: list[str], cwd: Path | None = None) -> str:
    result = subprocess.run(
        cmd,
        cwd=str(cwd) if cwd else None,
        text=True,
        capture_output=True,
    )
    if result.returncode != 0:
        command = " ".join(cmd)
        raise RuntimeError(
            f"Command failed ({result.returncode}): {command}\n"
            f"stdout:\n{result.stdout}\n"
            f"stderr:\n{result.stderr}"
        )
    return result.stdout


def normalize_expected_derror_differences(text: str) -> str:
    lines = text.splitlines(keepends=False)
    output: list[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]

        # Do not rewrite comments.
        if line.lstrip().startswith("//"):
            output.append(line)
            i += 1
            continue

        rewritten = PAT_PAREN_INLINE.sub(lambda m: f'error="{m.group(1)}"', line)
        rewritten = PAT_INLINE.sub(lambda m: f'error="{m.group(1)}"', rewritten)

        # Handle multiline style:
        #   foo = Builtin.testDerrorMessage
        #     "msg"
        if "Builtin.testDerrorMessage" in rewritten and i + 1 < len(lines):
            next_line = lines[i + 1]
            if not next_line.lstrip().startswith("//"):
                match = PAT_NEXT_LINE_MESSAGE.match(next_line)
                if match:
                    rewritten = rewritten.replace(
                        "Builtin.testDerrorMessage",
                        f'error="{match.group("message")}"',
                        1,
                    )
                    if match.group("close"):
                        rewritten += match.group("close")
                    output.append(rewritten)
                    i += 2
                    continue

        output.append(rewritten)
        i += 1

    return "\n".join(output)


def collect_files(root: Path) -> dict[str, Path]:
    return {
        file.relative_to(root).as_posix(): file
        for file in sorted(root.rglob("*"))
        if file.is_file()
    }


def is_ignored(rel_path: str, ignore_prefixes: list[str]) -> bool:
    return any(rel_path.startswith(prefix) for prefix in ignore_prefixes)


def fetch_upstream_checkout(
    repo_url: str, ref: str, upstream_subdir: str
) -> tuple[tempfile.TemporaryDirectory[str], Path]:
    tempdir = tempfile.TemporaryDirectory()
    repo_dir = Path(tempdir.name) / "dark-upstream"

    run(
        [
            "git",
            "clone",
            "--depth",
            "1",
            "--filter=blob:none",
            "--sparse",
            repo_url,
            str(repo_dir),
        ]
    )

    run(["git", "sparse-checkout", "set", upstream_subdir], cwd=repo_dir)
    run(["git", "fetch", "--depth", "1", "origin", ref], cwd=repo_dir)
    run(["git", "checkout", ref], cwd=repo_dir)

    return tempdir, repo_dir / upstream_subdir


def should_use_color(mode: str, writing_to_file: bool) -> bool:
    if mode == "always":
        return True
    if mode == "never":
        return False
    # auto
    return (not writing_to_file) and sys.stdout.isatty()


def colorize_line(line: str) -> str:
    if line.startswith("upstream:") or line.startswith("local:"):
        return f"{ANSI_DIM}{line}{ANSI_RESET}"
    if line.startswith("note:"):
        return f"{ANSI_DIM}{line}{ANSI_RESET}"
    if line.startswith("summary:"):
        return f"{ANSI_BOLD}{line}{ANSI_RESET}"
    if line.startswith("files only in upstream:") or line.startswith("files only in local:"):
        return f"{ANSI_BOLD}{line}{ANSI_RESET}"
    if line.startswith("diff: "):
        return f"{ANSI_BOLD}{ANSI_YELLOW}{line}{ANSI_RESET}"
    if line.startswith("--- "):
        return f"{ANSI_RED}{line}{ANSI_RESET}"
    if line.startswith("+++ "):
        return f"{ANSI_GREEN}{line}{ANSI_RESET}"
    if line.startswith("@@ "):
        return f"{ANSI_CYAN}{line}{ANSI_RESET}"
    if line.startswith("+") and not line.startswith("+++ "):
        return f"{ANSI_GREEN}{line}{ANSI_RESET}"
    if line.startswith("-") and not line.startswith("--- "):
        return f"{ANSI_RED}{line}{ANSI_RESET}"
    return line


def main() -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Diff src/Tests/e2e/upstream against darklang/dark "
            "backend/testfiles/execution while ignoring expected "
            "Builtin.testDerrorMessage vs error= differences."
        )
    )
    parser.add_argument(
        "--local-dir",
        default="src/Tests/e2e/upstream",
        help="Local directory to compare",
    )
    parser.add_argument(
        "--upstream-dir",
        default=None,
        help=(
            "Existing upstream directory (backend/testfiles/execution). "
            "If omitted, this script fetches from GitHub."
        ),
    )
    parser.add_argument(
        "--repo-url",
        default="https://github.com/darklang/dark.git",
        help="Upstream git repo URL (used when --upstream-dir is omitted)",
    )
    parser.add_argument(
        "--ref",
        default="main",
        help="Upstream git ref to compare against (branch/tag/sha)",
    )
    parser.add_argument(
        "--upstream-subdir",
        default="backend/testfiles/execution",
        help="Subdirectory in upstream repo to compare",
    )
    parser.add_argument(
        "--output",
        default=None,
        help="Optional output file for the diff report",
    )
    parser.add_argument(
        "--ignore-prefix",
        action="append",
        default=[],
        help=(
            "Path prefix to ignore from comparison (relative to compared roots). "
            "May be provided multiple times."
        ),
    )
    parser.add_argument(
        "--color",
        choices=["auto", "always", "never"],
        default="auto",
        help="Colorize output (default: auto)",
    )
    args = parser.parse_args()

    local_dir = Path(args.local_dir).resolve()
    if not local_dir.exists():
        print(f"Local directory not found: {local_dir}", file=sys.stderr)
        return 2

    tempdir: tempfile.TemporaryDirectory[str] | None = None
    if args.upstream_dir:
        upstream_dir = Path(args.upstream_dir).resolve()
    else:
        tempdir, upstream_dir = fetch_upstream_checkout(
            args.repo_url, args.ref, args.upstream_subdir
        )

    if not upstream_dir.exists():
        print(f"Upstream directory not found: {upstream_dir}", file=sys.stderr)
        if tempdir is not None:
            tempdir.cleanup()
        return 2

    # By default ignore cloud tests and upstream-only metadata/CLI fixtures.
    ignore_prefixes = [
        "cloud/",
        "cli/",
        "README.md",
        "README.me",
    ] + list(args.ignore_prefix)

    upstream_files = {
        k: v
        for k, v in collect_files(upstream_dir).items()
        if not is_ignored(k, ignore_prefixes)
    }
    local_files = {
        k: v
        for k, v in collect_files(local_dir).items()
        if not is_ignored(k, ignore_prefixes)
    }

    local_only = sorted(set(local_files) - set(upstream_files))
    upstream_only = sorted(set(upstream_files) - set(local_files))
    shared = sorted(set(local_files) & set(upstream_files))

    meaningful_diffs: list[tuple[str, str, str]] = []
    for rel_path in shared:
        upstream_text = upstream_files[rel_path].read_text(
            encoding="utf-8", errors="replace"
        )
        local_text = local_files[rel_path].read_text(
            encoding="utf-8", errors="replace"
        )

        upstream_normalized = normalize_expected_derror_differences(upstream_text)
        local_normalized = normalize_expected_derror_differences(local_text)
        if upstream_normalized != local_normalized:
            meaningful_diffs.append((rel_path, upstream_normalized, local_normalized))

    report_lines: list[str] = []
    report_lines.append(f"upstream: {upstream_dir}")
    report_lines.append(f"local:    {local_dir}")
    report_lines.append(
        "note: ignoring expected Builtin.testDerrorMessage <-> error=\"...\" differences"
    )
    report_lines.append("")
    report_lines.append(
        "summary: "
        f"upstream_only={len(upstream_only)}, "
        f"local_only={len(local_only)}, "
        f"meaningful_diffs={len(meaningful_diffs)}"
    )
    report_lines.append("")

    if upstream_only:
        report_lines.append("files only in upstream:")
        report_lines.extend([f"  {f}" for f in upstream_only])
        report_lines.append("")

    if local_only:
        report_lines.append("files only in local:")
        report_lines.extend([f"  {f}" for f in local_only])
        report_lines.append("")

    for rel_path, upstream_text, local_text in meaningful_diffs:
        report_lines.append(f"diff: {rel_path}")
        diff_lines = difflib.unified_diff(
            upstream_text.splitlines(keepends=False),
            local_text.splitlines(keepends=False),
            fromfile=f"upstream/{rel_path}",
            tofile=f"local/{rel_path}",
            lineterm="",
        )
        report_lines.extend(list(diff_lines))
        report_lines.append("")

    use_color = should_use_color(args.color, writing_to_file=bool(args.output))
    if use_color:
        report_lines = [colorize_line(line) for line in report_lines]

    report = "\n".join(report_lines).rstrip() + "\n"

    if args.output:
        output_path = Path(args.output).resolve()
        output_path.write_text(report, encoding="utf-8")
        print(f"wrote report: {output_path}")
    else:
        print(report, end="")

    if tempdir is not None:
        tempdir.cleanup()

    if upstream_only or local_only or meaningful_diffs:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
