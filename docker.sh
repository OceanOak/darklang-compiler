#!/bin/bash
# Convenience script for Docker operations

set -e

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
host_workspace_root="$(cd "$script_dir/.." && pwd -P)"
workspace_root="/workspace"
default_worktree="main"

resolve_workspace_from_pwd() {
  local current_dir
  current_dir="$(pwd -P)"

  case "$current_dir" in
    "$host_workspace_root")
      printf "%s\n" "$workspace_root"
      ;;

    "$host_workspace_root"/*)
      local relative_path
      relative_path="${current_dir#"$host_workspace_root"/}"
      printf "%s/%s\n" "$workspace_root" "$relative_path"
      ;;

    *)
      printf "%s/%s\n" "$workspace_root" "$default_worktree"
      ;;
  esac
}

case "$1" in
  build)
    echo "Building Docker image..."
    docker compose build
    ;;

  up)
    echo "Starting container..."
    docker compose up -d
    echo "Container started. Use './docker.sh shell' from any c4d worktree directory to enter the matching path."
    ;;

  down)
    echo "Stopping container..."
    docker compose down
    ;;

  shell)
    workspace="$(resolve_workspace_from_pwd)"
    echo "Entering container shell at $workspace..."
    docker compose exec -w "$workspace" dev bash
    ;;

  restart)
    echo "Restarting container..."
    docker compose down
    docker compose up -d
    echo "Container restarted."
    ;;

  logs)
    docker compose logs -f dev
    ;;

  clean)
    workspace="$(resolve_workspace_from_pwd)"
    echo "Cleaning build artifacts in container..."
    docker compose exec -w "$workspace" dev dotnet clean
    ;;

  build-compiler)
    workspace="$(resolve_workspace_from_pwd)"
    echo "Building compiler in container..."
    docker compose exec -w "$workspace" dev dotnet build
    ;;

  status)
    echo "Container status:"
    docker compose ps
    ;;

  *)
    echo "Docker Development Environment"
    echo ""
    echo "Usage: ./docker.sh {command}"
    echo ""
    echo "Commands:"
    echo "  build           Build Docker image"
    echo "  up              Start container (detached)"
    echo "  down            Stop container"
    echo "  shell           Enter a container shell at the matching c4d path"
    echo "  restart         Restart container"
    echo "  logs            View container logs"
    echo "  clean           Clean build artifacts at the matching c4d path"
    echo "  build-compiler  Build compiler at the matching c4d path"
    echo "  status          Show container status"
    exit 1
    ;;
esac
