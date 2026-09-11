ARG SANDBOX_TEMPLATE=codex
FROM mcr.microsoft.com/dotnet/sdk:10.0-noble AS dotnet
FROM docker.io/docker/sandbox-templates:${SANDBOX_TEMPLATE}

# Dockerfile - Build the sbx development template for local compiler work.

USER root

# Reuse the official SDK installation while retaining the sbx template's agent runtime.
COPY --from=dotnet /usr/share/dotnet /usr/share/dotnet

# Trust an enclosing sandbox's proxy when this template is built from another sbx environment.
ARG PROXY_CA_CERT_B64
RUN if [ -n "$PROXY_CA_CERT_B64" ]; then \
      printf '%s' "$PROXY_CA_CERT_B64" | base64 --decode > /usr/local/share/ca-certificates/proxy-ca.crt && \
      update-ca-certificates; \
    fi

# Install development tools and benchmarking dependencies.
RUN apt-get update && apt-get install -y \
    git \
    vim \
    file \
    less \
    curl \
    htop \
    jq \
    sqlite3 \
    shellcheck \
    bash-completion \
    # Benchmarking tools
    python3 \
    hyperfine \
    valgrind \
    gcc \
    gcc-x86-64-linux-gnu \
    libc6-dev-amd64-cross \
    rustup \
    # OCaml tools for benchmarking
    ocaml \
    opam \
    # Emulation for running compiled binaries on non-native hosts
    qemu-user-binfmt \
    && rm -rf /var/lib/apt/lists/*

# The sbx base image provides the non-root agent user and passwordless sudo.
RUN mkdir -p /home/agent/.nuget/packages && chown -R agent:agent /home/agent/.nuget
USER agent

# Use the image-provided .NET installation as the system runtime/SDK
ENV HOME="/home/agent"
ENV DOTNET_ROOT="/usr/share/dotnet"
ENV DOTNET_CLI_HOME="/home/agent"
ENV DOTNET_MULTILEVEL_LOOKUP="0"

# Add .NET, dotnet tools and local bin to PATH
ENV PATH="${DOTNET_ROOT}:/home/agent/.dotnet/tools:/home/agent/.local/bin:/home/agent/.cargo/bin:${PATH}"

# Pin the native Rust toolchain and install the Linux x86_64 standard library
# used to build audited reference binaries for QEMU instruction measurement.
RUN rustup toolchain install 1.89.0 --profile minimal --target x86_64-unknown-linux-gnu && \
    rustup default 1.89.0

# Pre-download workload advertising manifests so first-run commands don't fail workload verification.
# Needs elevated privileges because the SDK is installed system-wide under /usr/share/dotnet.
RUN sudo /usr/share/dotnet/dotnet workload update --advertising-manifests-only --ignore-failed-sources

# Coverage tooling for ./run-coverage
RUN dotnet tool install -g coverlet.console && \
    dotnet tool install -g dotnet-reportgenerator-globaltool

# Initialize opam and install ocamlfind for benchmarking
RUN opam init --disable-sandboxing --auto-setup --yes && \
    eval $(opam env) && \
    opam install ocamlfind --yes && \
    echo 'eval $(opam env)' >> ~/.bashrc

# Install darklang interpreter from latest GitHub release
COPY --chown=agent:agent scripts/install-darklang-interpreter.sh /tmp/install-darklang-interpreter.sh
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

# sbx replaces this with the primary host workspace when the sandbox is created.
WORKDIR /home/agent/workspace

# Default command: bash shell
CMD ["bash"]
