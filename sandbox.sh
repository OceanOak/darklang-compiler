#!/usr/bin/env bash
# sandbox.sh - Build or run the single-container development environment.
# Usage: ./sandbox.sh build [codex|claude|shell] or ./sandbox.sh [codex|claude|shell]

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

configure_agent() {
  agent="${1:-codex}"
  case "$agent" in
    claude) command=(claude) ;;
    codex) command=(codex) ;;
    shell) command=(bash) ;;
    *)
      echo "Unsupported agent: $agent (expected claude, codex, or shell)" >&2
      return 1
      ;;
  esac

  image="dark-compiler:dev"
  worktree="$(basename "$repo_root" | tr -c '[:alnum:]_.-' '-')"
  container="dark-compiler-$worktree-$agent"
}

build_image() {
  build_args=()
  for variable in HTTP_PROXY HTTPS_PROXY NO_PROXY http_proxy https_proxy no_proxy PROXY_CA_CERT_B64; do
    if [[ -n "${!variable:-}" ]]; then
      build_args+=(--build-arg "$variable=${!variable}")
    fi
  done

  docker build \
    "${build_args[@]}" \
    --tag "$image" \
    "$repo_root"
}

command -v docker >/dev/null || { echo "docker is required" >&2; exit 1; }

if [[ "${1:-}" == "build" ]]; then
  configure_agent "${2:-codex}"
  build_image
else
  configure_agent "${1:-codex}"
  docker image inspect "$image" >/dev/null 2>&1 || build_image
  exec docker run --rm --interactive --tty \
    --name "$container" \
    --volume "$repo_root:$repo_root" \
    --volume dark-compiler-codex-home:/home/agent/.codex \
    --volume dark-compiler-claude-home:/home/agent/.claude \
    --volume dark-compiler-nuget-packages:/home/agent/.nuget/packages \
    --workdir "$repo_root" \
    "$image" \
    "${command[@]}"
fi
