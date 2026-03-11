FROM mcr.microsoft.com/dotnet/sdk:10.0-noble

# Bootstrap user/group. Entry point remaps this identity to the mounted workspace owner at runtime.

# Install development tools, benchmarking dependencies, and curl for Codex CLI installation
RUN apt-get update && apt-get install -y \
    git \
    vim \
    file \
    less \
    curl \
    sudo \
    nodejs \
    npm \
    htop \
    jq \
    bash-completion \
    # Benchmarking tools
    python3 \
    hyperfine \
    valgrind \
    gcc \
    # OCaml tools for benchmarking
    ocaml \
    opam \
    && rm -rf /var/lib/apt/lists/*

# Install Codex CLI + Claude Code
RUN npm install -g @openai/codex @anthropic-ai/claude-code

# Create bootstrap user (remapped at container start to match mounted workspace UID/GID)
RUN mkdir -p /Users/paulbiggar/projects && \
    groupadd -g 1000 dark && \
    useradd -m -u 1000 -g 1000 -s /bin/bash dark && \
    echo "dark ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers && \
    mkdir -p /home/dark/.nuget/packages /home/dark/.codex /home/dark/.claude && \
    chown -R 1000:1000 /home/dark

# Switch to dark user
USER dark

# Use the image-provided .NET installation as the system runtime/SDK
ENV HOME="/home/dark"
ENV DOTNET_ROOT="/usr/share/dotnet"
ENV DOTNET_CLI_HOME="/home/dark"
ENV DOTNET_MULTILEVEL_LOOKUP="0"

# Add .NET, Rust and local bin to PATH
ENV PATH="${DOTNET_ROOT}:/home/dark/.local/bin:/home/dark/.cargo/bin:${PATH}"

# Pre-download workload advertising manifests so first-run commands don't fail workload verification.
# Needs elevated privileges because the SDK is installed system-wide under /usr/share/dotnet.
RUN sudo dotnet workload update --advertising-manifests-only --ignore-failed-sources

# Install Rust for benchmarking
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

# Initialize opam and install ocamlfind for benchmarking
RUN opam init --disable-sandboxing --auto-setup --yes && \
    eval $(opam env) && \
    opam install ocamlfind --yes && \
    echo 'eval $(opam env)' >> ~/.bashrc

# Install darklang interpreter from latest GitHub release
COPY --chown=1000:1000 scripts/install-darklang-interpreter.sh /tmp/install-darklang-interpreter.sh
RUN bash /tmp/install-darklang-interpreter.sh && \
    rm /tmp/install-darklang-interpreter.sh

# Configure git aliases
RUN git config --global alias.ci commit && \
    git config --global alias.co checkout && \
    git config --global alias.st status

# Configure nice bash prompt with git branch and short path
RUN echo 'parse_git_branch() { git branch 2>/dev/null | grep "^*" | sed "s/* //"; }' >> ~/.bashrc && \
    echo 'short_path() { pwd | sed "s|$HOME|~|"; }' >> ~/.bashrc && \
    echo 'PS1="\[\033[1;32m\]\u@dark\[\033[0m\]:\[\033[1;34m\]\$(short_path)\[\033[0m\]\[\033[1;33m\]\$(parse_git_branch | sed \"s/.*/ (&)/\")\[\033[0m\]\$ "' >> ~/.bashrc

# Enable bash completion for git and other installed tools
RUN echo 'if [ -f /etc/bash_completion ]; then . /etc/bash_completion; fi' >> ~/.bashrc

# Remap dark user/group to match mounted workspace owner at runtime.
RUN sudo tee /usr/local/bin/docker-entrypoint.sh > /dev/null <<'EOF' && sudo chmod +x /usr/local/bin/docker-entrypoint.sh
#!/usr/bin/env bash
set -euo pipefail

workspace="/Users/paulbiggar/projects/c4d"
target_uid="$(stat -c '%u' "$workspace" 2>/dev/null || id -u dark)"
target_gid="$(stat -c '%g' "$workspace" 2>/dev/null || id -g dark)"

if ! getent group "$target_gid" > /dev/null; then
  if getent group dark > /dev/null; then
    groupmod -o -g "$target_gid" dark
  else
    groupadd -g "$target_gid" dark
  fi
fi

if [ "$(id -u dark)" != "$target_uid" ]; then
  usermod -o -u "$target_uid" dark
fi
if [ "$(id -g dark)" != "$target_gid" ]; then
  usermod -g "$target_gid" dark
fi

# Keep non-source writable state aligned with the remapped identity.
chown -R "$target_uid:$target_gid" /home/dark/.nuget || true

for dir in "$workspace/bin" "$workspace/obj"; do
  mkdir -p "$dir"
  if [ ! -w "$dir" ]; then
    chown -R "$target_uid:$target_gid" "$dir" || true
    chmod -R u+rwX "$dir" || true
  fi
done

exec runuser -u dark -- "$@"
EOF

# Set working directory to match host path
WORKDIR /Users/paulbiggar/projects/c4d

# Container will use volume mount for source code
# No COPY needed - source comes from host via volume

# Run entrypoint as root so user/group remap can happen before dropping privileges.
USER root

# Default command: bash shell
ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]
CMD ["bash"]
