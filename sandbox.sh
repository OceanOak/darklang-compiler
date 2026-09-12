#!/usr/bin/env bash
# sandbox.sh - Build a development template or run an agent using one.
# Usage: ./sandbox.sh build [codex|claude|shell] or ./sandbox.sh [codex|claude|shell]

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
docker_disk_size="30g"

configure_agent() {
  agent="${1:-codex}"
  case "$agent" in
    claude | codex | shell) ;;
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

  build_args=()
  for variable in HTTP_PROXY HTTPS_PROXY NO_PROXY http_proxy https_proxy no_proxy PROXY_CA_CERT_B64; do
    if [[ -n "${!variable:-}" ]]; then
      build_args+=(--build-arg "$variable=${!variable}")
    fi
  done

  mapfile -t agent_versions < <(
    docker run --rm node:22-bookworm-slim sh -c \
      'npm view @openai/codex version && npm view @anthropic-ai/claude-code version'
  )
  if [[ "${#agent_versions[@]}" -ne 2 ]] || \
     [[ -z "${agent_versions[0]}" ]] || \
     [[ -z "${agent_versions[1]}" ]]; then
    echo "could not resolve current agent CLI versions from npm" >&2
    return 1
  fi
  build_args+=(
    --build-arg "CODEX_VERSION=${agent_versions[0]}"
    --build-arg "CLAUDE_CODE_VERSION=${agent_versions[1]}"
  )

  docker build \
    "${build_args[@]}" \
    --tag "$template" \
    "$repo_root"
  docker image save "$template" --output "$archive"
  sbx template load "$archive"
}

command -v sbx >/dev/null || { echo "sbx is required" >&2; exit 1; }

if [[ "${1:-}" == "build" ]]; then
  configure_agent "${2:-codex}"
  build_template
else
  configure_agent "${1:-codex}"
  DOCKER_SANDBOXES_DOCKER_SIZE="$docker_disk_size" \
    exec sbx run --name "$sandbox" "$agent" "$repo_root"
fi
