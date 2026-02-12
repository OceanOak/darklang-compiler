FROM mcr.microsoft.com/dotnet/sdk:10.0-noble

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

# Create project directory structure as root
RUN mkdir -p /Users/paulbiggar/projects

# Create paulbiggar user with same UID as host (501) for file permissions
RUN useradd -m -u 501 -s /bin/bash paulbiggar && \
    echo "paulbiggar ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers && \
    mkdir -p /home/paulbiggar/.nuget/packages && \
    chown -R paulbiggar:paulbiggar /Users/paulbiggar /home/paulbiggar/.nuget

# Switch to paulbiggar user
USER paulbiggar

# Use the image-provided .NET installation as the system runtime/SDK
ENV DOTNET_ROOT="/usr/share/dotnet"
ENV DOTNET_MULTILEVEL_LOOKUP="0"

# Add .NET, Rust and local bin to PATH
ENV PATH="${DOTNET_ROOT}:/home/paulbiggar/.local/bin:/home/paulbiggar/.cargo/bin:${PATH}"

# Install Rust for benchmarking
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

# Initialize opam and install ocamlfind for benchmarking
RUN opam init --disable-sandboxing --auto-setup --yes && \
    eval $(opam env) && \
    opam install ocamlfind --yes && \
    echo 'eval $(opam env)' >> ~/.bashrc

# Install darklang interpreter
RUN mkdir -p ~/.local/bin && \
    curl -sSL  https://github.com/darklang/dark/releases/download/v0.0.2/darklang-alpha-6805337e8b-linux-arm.gz | \
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

# Set working directory to match host path
WORKDIR /Users/paulbiggar/projects/compiler-for-dark

# Container will use volume mount for source code
# No COPY needed - source comes from host via volume

# Default command: bash shell
CMD ["bash"]
