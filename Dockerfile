ARG SANDBOX_TEMPLATE=codex
ARG QEMU_VERSION=11.1.1
ARG QEMU_COMMIT=c3d48b7d1e89604920e5b81b91140c2ad39a1943
FROM mcr.microsoft.com/dotnet/sdk:10.0-noble AS dotnet

FROM mcr.microsoft.com/dotnet/sdk:10.0-noble AS qemu-builder
ARG QEMU_VERSION
ARG QEMU_COMMIT
RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    libglib2.0-dev \
    ninja-build \
    pkg-config \
    python3 \
    python3-venv \
    && rm -rf /var/lib/apt/lists/*
RUN git clone --depth 1 --branch "v${QEMU_VERSION}" \
      https://gitlab.com/qemu-project/qemu.git /qemu-repository \
    && test "$(git -C /qemu-repository rev-parse HEAD)" = "$QEMU_COMMIT" \
    && mkdir /qemu \
    && git -C /qemu-repository archive HEAD | tar -x -C /qemu
WORKDIR /qemu/build
RUN ../configure \
    --target-list=aarch64-linux-user,x86_64-linux-user \
    --enable-plugins \
    --disable-system \
    --disable-docs \
    --disable-tools \
    && ninja qemu-aarch64 qemu-x86_64 tests/tcg/plugins/libinsn.so \
    && test "$(./qemu-aarch64 --version | head -n 1)" = "qemu-aarch64 version ${QEMU_VERSION}" \
    && test "$(./qemu-x86_64 --version | head -n 1)" = "qemu-x86_64 version ${QEMU_VERSION}" \
    && mkdir -p /opt/dcb/qemu \
    && cp qemu-aarch64 qemu-x86_64 tests/tcg/plugins/libinsn.so /opt/dcb/qemu/

FROM docker.io/docker/sandbox-templates:${SANDBOX_TEMPLATE}

# Dockerfile - Build the sbx development template for local compiler work.

USER root
ARG TARGETARCH

# Reuse the official SDK installation while retaining the sbx template's agent runtime.
COPY --from=dotnet /usr/share/dotnet /usr/share/dotnet
COPY --from=qemu-builder /opt/dcb/qemu /opt/dcb/qemu

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
    libatomic1 \
    libglib2.0-0t64 \
    valgrind \
    gcc \
    rustup \
    zlib1g \
    # OCaml tools for benchmarking
    ocaml \
    opam \
    && rm -rf /var/lib/apt/lists/*

# Install the linker and sysroot for the non-native Linux target. QEMU itself
# is pinned above so benchmark instruction counts do not depend on Ubuntu's
# host-architecture package version.
RUN case "$TARGETARCH" in \
      amd64) cross_packages="gcc-aarch64-linux-gnu libc6-dev-arm64-cross" ;; \
      arm64) cross_packages="gcc-x86-64-linux-gnu libc6-dev-amd64-cross" ;; \
      *) echo "Unsupported template architecture: $TARGETARCH" >&2; exit 1 ;; \
    esac \
    && apt-get update \
    && apt-get install -y --no-install-recommends $cross_packages \
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

# Pin Rust and install both Linux standard libraries so the same template can
# cross-check ARM64 from x86_64 and x86_64 from ARM64.
RUN rustup toolchain install 1.89.0 --profile minimal \
      --target aarch64-unknown-linux-gnu \
      --target x86_64-unknown-linux-gnu && \
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
