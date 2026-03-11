#!/bin/bash
# Convenience script for Docker operations

set -e

fix_nuget_permissions() {
  echo "Fixing NuGet cache permissions..."
  local target_home
  local target_uid
  local target_gid

  read -r target_home target_uid target_gid < <(
    docker compose exec --user dark -T dev sh -lc 'printf "%s %s %s\n" "$HOME" "$(id -u)" "$(id -g)"'
  )

  docker compose exec --user root -T \
    -e TARGET_HOME="$target_home" \
    -e TARGET_UID="$target_uid" \
    -e TARGET_GID="$target_gid" \
    dev bash -lc '
    set -e
    mkdir -p "$TARGET_HOME/.nuget/packages"
    chown -R "$TARGET_UID:$TARGET_GID" "$TARGET_HOME/.nuget"
    chmod -R u+rwX "$TARGET_HOME/.nuget"
  '
}

case "$1" in
  build)
    echo "Building Docker image..."
    docker compose build
    ;;

  up)
    echo "Starting container..."
    docker compose up -d
    fix_nuget_permissions
    echo "Container started. Use './docker.sh shell' to enter."
    ;;

  down)
    echo "Stopping container..."
    docker compose down
    ;;

  shell)
    echo "Entering container shell..."
    docker compose exec --user dark dev bash
    ;;

  restart)
    echo "Restarting container..."
    docker compose down
    docker compose up -d
    fix_nuget_permissions
    echo "Container restarted."
    ;;

  logs)
    docker compose logs -f dev
    ;;

  clean)
    echo "Cleaning build artifacts in container..."
    docker compose exec --user dark dev dotnet clean
    ;;

  build-compiler)
    echo "Building compiler in container..."
    docker compose exec --user dark dev dotnet build
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
    echo "  shell           Enter container shell"
    echo "  restart         Restart container"
    echo "  logs            View container logs"
    echo "  clean           Clean build artifacts"
    echo "  build-compiler  Build compiler in container"
    echo "  status          Show container status"
    exit 1
    ;;
esac
