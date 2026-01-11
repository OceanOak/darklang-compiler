FROM mcr.microsoft.com/dotnet/sdk:9.0-noble

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
    # Benchmarking tools
    python3 \
    hyperfine \
    valgrind \
    gcc \
    && rm -rf /var/lib/apt/lists/*

# Install Codex CLI + Claude Code
RUN npm install -g @openai/codex @anthropic-ai/claude-code

# Create project directory structure as root
RUN mkdir -p /Users/paulbiggar/projects

# Create paulbiggar user with same UID as host (501) for file permissions
RUN useradd -m -u 501 -s /bin/bash paulbiggar && \
    echo "paulbiggar ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers && \
    chown -R paulbiggar:paulbiggar /Users/paulbiggar

# Switch to paulbiggar user
USER paulbiggar

# Install Rust for benchmarking
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

# Install beads (bd) issue tracking
RUN curl -sSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash

# Configure git aliases
RUN git config --global alias.ci commit && \
    git config --global alias.co checkout && \
    git config --global alias.st status

# Add Rust and local bin to PATH
ENV PATH="/home/paulbiggar/.local/bin:/home/paulbiggar/.cargo/bin:${PATH}"

# Set working directory to match host path
WORKDIR /Users/paulbiggar/projects/compiler-for-dark

# Container will use volume mount for source code
# No COPY needed - source comes from host via volume

# Default command: bash shell
CMD ["bash"]
