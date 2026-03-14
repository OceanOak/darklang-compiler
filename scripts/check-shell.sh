#!/bin/bash
# check-shell.sh
# Run shellcheck against each tracked bash script in the repository.

set -euo pipefail

if ! command -v shellcheck >/dev/null 2>&1; then
  echo "shellcheck is required but was not found on PATH."
  exit 1
fi

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

mapfile -d '' bash_files < <(rg -l -0 '^#!.*bash' run-tests scripts)

if [[ ${#bash_files[@]} -eq 0 ]]; then
  echo "No bash scripts found."
  exit 0
fi

shellcheck "${bash_files[@]}"
