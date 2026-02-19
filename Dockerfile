FROM mcr.microsoft.com/dotnet/sdk:10.0-noble

# Build args to align container UID/GID with the host user (important for Colima mounts)
ARG LOCAL_UID=501
ARG LOCAL_GID=20

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

# Create paulbiggar user with host-matching UID/GID to avoid volume permission issues
RUN mkdir -p /Users/paulbiggar/projects && \
    if getent group "${LOCAL_GID}" > /dev/null; then \
      useradd -m -u "${LOCAL_UID}" -g "${LOCAL_GID}" -s /bin/bash paulbiggar; \
    else \
      groupadd -g "${LOCAL_GID}" paulbiggar && \
      useradd -m -u "${LOCAL_UID}" -g "${LOCAL_GID}" -s /bin/bash paulbiggar; \
    fi && \
    echo "paulbiggar ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers && \
    mkdir -p /home/paulbiggar/.nuget/packages /home/paulbiggar/.codex /home/paulbiggar/.claude && \
    chown -R "${LOCAL_UID}:${LOCAL_GID}" /Users/paulbiggar /home/paulbiggar

# Switch to paulbiggar user
USER paulbiggar

# Use the image-provided .NET installation as the system runtime/SDK
ENV HOME="/home/paulbiggar"
ENV DOTNET_ROOT="/usr/share/dotnet"
ENV DOTNET_CLI_HOME="/home/paulbiggar"
ENV DOTNET_MULTILEVEL_LOOKUP="0"

# Add .NET, Rust and local bin to PATH
ENV PATH="${DOTNET_ROOT}:/home/paulbiggar/.local/bin:/home/paulbiggar/.cargo/bin:${PATH}"

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

# Install darklang interpreter
RUN mkdir -p ~/.local/bin && \
    curl -sSL https://github.com/darklang/dark/releases/download/v0.0.1/darklang-alpha-f0375cb7a3-linux-arm.gz | \
    gunzip > ~/.local/bin/darklang-interpreter && \
    chmod +x ~/.local/bin/darklang-interpreter

# Configure git aliases
RUN git config --global alias.ci commit && \
    git config --global alias.co checkout && \
    git config --global alias.st status

# Configure nice bash prompt with git branch and short path
RUN echo 'parse_git_branch() { git branch 2>/dev/null | grep "^*" | sed "s/* //"; }' >> ~/.bashrc && \
    echo 'short_path() { pwd | sed "s|/Users/paulbiggar/projects/compiler-for-dark|~/c4d|" | sed "s|$HOME|~|"; }' >> ~/.bashrc && \
    echo 'PS1="\[\033[1;32m\]\u@dark\[\033[0m\]:\[\033[1;34m\]\$(short_path)\[\033[0m\]\[\033[1;33m\]\$(parse_git_branch | sed \"s/.*/ (&)/\")\[\033[0m\]\$ "' >> ~/.bashrc

# Enable bash completion for git and other installed tools
RUN echo 'if [ -f /etc/bash_completion ]; then . /etc/bash_completion; fi' >> ~/.bashrc

# Ensure mounted workspace build directories are writable by the container user.
RUN sudo tee /usr/local/bin/docker-entrypoint.sh > /dev/null <<'EOF' && sudo chmod +x /usr/local/bin/docker-entrypoint.sh
#!/usr/bin/env bash
set -e

workspace="/Users/paulbiggar/projects/compiler-for-dark"

for dir in "$workspace/bin" "$workspace/obj"; do
  mkdir -p "$dir"
  if [ ! -w "$dir" ]; then
    sudo chown -R "$(id -u):$(id -g)" "$dir" || true
    chmod -R u+rwX "$dir" || true
  fi
done

exec "$@"
EOF

# Set working directory to match host path
WORKDIR /Users/paulbiggar/projects/compiler-for-dark

# Container will use volume mount for source code
# No COPY needed - source comes from host via volume

# Default command: bash shell
ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]
CMD ["bash"]
