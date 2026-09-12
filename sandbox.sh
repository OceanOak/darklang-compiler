#!/usr/bin/env bash
# sandbox.sh - Build a development template or run an agent using one.
# Usage: ./sandbox.sh build [codex|claude|shell] or ./sandbox.sh [codex|claude|shell]

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

configure_agent() {
  agent="${1:-codex}"
  case "$agent" in
    claude)
      template_variant="claude-code"
      smoke_command=(claude --version)
      ;;
    codex)
      template_variant="codex"
      smoke_command=(codex --version)
      ;;
    shell)
      template_variant="shell"
      smoke_command=(bash --version)
      ;;
    *)
      echo "Unsupported agent: $agent (expected claude, codex, or shell)" >&2
      return 1
      ;;
  esac

  template="dark-compiler:$agent"
  worktree="$(basename "$repo_root" | tr -c '[:alnum:]_.-' '-')"
  sandbox="dark-compiler-$worktree-$agent"
}

build_template() {
  command -v docker >/dev/null || { echo "docker is required to build the template" >&2; return 1; }

  archive="$(mktemp "${TMPDIR:-/tmp}/dark-compiler-template.XXXXXX")"
  trap 'rm -f "$archive"' EXIT

  build_args=(--build-arg "SANDBOX_TEMPLATE=$template_variant")
  for variable in HTTP_PROXY HTTPS_PROXY NO_PROXY http_proxy https_proxy no_proxy PROXY_CA_CERT_B64; do
    if [[ -n "${!variable:-}" ]]; then
      build_args+=(--build-arg "$variable=${!variable}")
    fi
  done

  docker build \
    "${build_args[@]}" \
    --tag "$template" \
    "$repo_root"
  docker run --rm "$template" "${smoke_command[@]}"
  docker image save "$template" --output "$archive"
  sbx template load "$archive"
}

command -v sbx >/dev/null || { echo "sbx is required" >&2; exit 1; }

if [[ "${1:-}" == "build" ]]; then
  configure_agent "${2:-codex}"
  build_template
else
  configure_agent "${1:-codex}"
  existing_sandboxes="$(sbx ls --quiet)"
  if grep --fixed-strings --line-regexp --quiet -- "$sandbox" <<< "$existing_sandboxes"; then
    exec sbx run --name "$sandbox"
  fi

  exec sbx run --name "$sandbox" --template "$template" "$agent" "$repo_root"
fi
