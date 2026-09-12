# syntax=docker/dockerfile:1
# Dockerfile - Build the single-container development environment for local compiler work.

ARG QEMU_VERSION=11.1.1
ARG QEMU_COMMIT=c3d48b7d1e89604920e5b81b91140c2ad39a1943
FROM mcr.microsoft.com/dotnet/sdk:10.0-noble AS qemu-builder
ARG QEMU_VERSION
ARG QEMU_COMMIT
ARG PROXY_CA_CERT_B64
# Keep the source, object files, and build dependencies in one layer so none of
# them survive in the builder image or BuildKit cache after the artifacts are copied.
RUN if [ -n "$PROXY_CA_CERT_B64" ]; then \
      printf '%s' "$PROXY_CA_CERT_B64" | base64 --decode > /usr/local/share/ca-certificates/proxy-ca.crt; \
      update-ca-certificates; \
    fi \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
      build-essential \
      git \
      libglib2.0-dev \
      ninja-build \
      pkg-config \
      python3 \
      python3-venv \
    && git clone --depth 1 --branch "v${QEMU_VERSION}" \
      https://gitlab.com/qemu-project/qemu.git /qemu-repository \
    && test "$(git -C /qemu-repository rev-parse HEAD)" = "$QEMU_COMMIT" \
    && mkdir /qemu \
    && git -C /qemu-repository archive HEAD | tar -x -C /qemu \
    && rm -rf /qemu-repository \
    && mkdir /qemu/build \
    && cd /qemu/build \
    && ../configure \
      --target-list=aarch64-linux-user,x86_64-linux-user \
      --enable-plugins \
      --disable-system \
      --disable-docs \
      --disable-tools \
    && ninja qemu-aarch64 qemu-x86_64 tests/tcg/plugins/libinsn.so \
    && test "$(./qemu-aarch64 --version | head -n 1)" = "qemu-aarch64 version ${QEMU_VERSION}" \
    && test "$(./qemu-x86_64 --version | head -n 1)" = "qemu-x86_64 version ${QEMU_VERSION}" \
    && strip --strip-unneeded qemu-aarch64 qemu-x86_64 tests/tcg/plugins/libinsn.so \
    && mkdir -p /opt/dcb/qemu \
    && cp qemu-aarch64 qemu-x86_64 tests/tcg/plugins/libinsn.so /opt/dcb/qemu/ \
    && cd / \
    && rm -rf /qemu \
    && apt-get purge -y --auto-remove \
      build-essential \
      git \
      libglib2.0-dev \
      ninja-build \
      pkg-config \
      python3-venv \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

FROM node:22-bookworm-slim AS node

FROM mcr.microsoft.com/dotnet/sdk:10.0-noble

USER root
ARG TARGETARCH

COPY --from=qemu-builder /opt/dcb/qemu /opt/dcb/qemu
COPY --from=node /usr/local /usr/local

# Trust a configured proxy when the image is built behind one.
ARG PROXY_CA_CERT_B64
RUN if [ -n "$PROXY_CA_CERT_B64" ]; then \
      printf '%s' "$PROXY_CA_CERT_B64" | base64 --decode > /usr/local/share/ca-certificates/proxy-ca.crt && \
      update-ca-certificates; \
    fi

# Install development tools, benchmarking dependencies, and the non-native
# linker/sysroot together so package indexes and archives occupy only this layer.
RUN case "$TARGETARCH" in \
      amd64) cross_packages="gcc-aarch64-linux-gnu libc6-dev-arm64-cross" ;; \
      arm64) cross_packages="gcc-x86-64-linux-gnu libc6-dev-amd64-cross" ;; \
      *) echo "Unsupported template architecture: $TARGETARCH" >&2; exit 1 ;; \
    esac \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
      git \
      vim \
      file \
      less \
      curl \
      sudo \
      htop \
      jq \
      sqlite3 \
      shellcheck \
      bash-completion \
      python3 \
      hyperfine \
      libatomic1 \
      libglib2.0-0t64 \
      valgrind \
      gcc \
      rustup \
      zlib1g \
      opam \
      $cross_packages \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/* \
    && usermod --login agent --home /home/agent --move-home ubuntu \
    && groupmod --new-name agent ubuntu \
    && echo "agent ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/agent \
    && chmod 0440 /etc/sudoers.d/agent \
    && mkdir -p /home/agent/.nuget/packages /workspace \
    && chown -R agent:agent /home/agent /workspace

# Install both supported agent CLIs in the image; neither needs a Docker daemon.
RUN npm install --global @openai/codex @anthropic-ai/claude-code \
    && npm cache clean --force

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
    rustup default 1.89.0 && \
    rm -rf /home/agent/.rustup/downloads /home/agent/.rustup/tmp

# Pre-download workload advertising manifests and install coverage tools, then
# discard the NuGet package and HTTP caches rather than retaining duplicate payloads.
RUN sudo /usr/share/dotnet/dotnet workload update --advertising-manifests-only --ignore-failed-sources && \
    dotnet tool install -g coverlet.console && \
    dotnet tool install -g dotnet-reportgenerator-globaltool && \
    dotnet nuget locals all --clear

# Install the latest OCaml compiler through opam, then retain only the switch
# and metadata needed to use it.
RUN opam init --disable-sandboxing --no-setup --bare --yes && \
    opam switch create default ocaml-base-compiler --yes && \
    eval $(opam env --switch=default) && \
    opam install ocamlfind --yes && \
    opam clean --all-switches --download-cache --logs --repo-cache --switch-cleanup --yes && \
    echo 'eval $(opam env)' >> ~/.bashrc

# Install the darklang interpreter without retaining the installer in a COPY layer.
RUN --mount=type=bind,source=scripts/install-darklang-interpreter.sh,target=/tmp/install-darklang-interpreter.sh \
    bash /tmp/install-darklang-interpreter.sh

# Configure the interactive shell in one metadata layer.
RUN git config --global alias.ci commit && \
    git config --global alias.co checkout && \
    git config --global alias.st status && \
    echo 'parse_git_branch() { git branch 2>/dev/null | grep "^*" | sed "s/* //"; }' >> ~/.bashrc && \
    echo 'short_path() { pwd | sed "s|$HOME|~|"; }' >> ~/.bashrc && \
    echo 'PS1="\[\033[1;32m\]\u@dark\[\033[0m\]:\[\033[1;34m\]\$(short_path)\[\033[0m\]\[\033[1;33m\]\$(parse_git_branch | sed \"s/.*/ (&)/\")\[\033[0m\]\$ "' >> ~/.bashrc && \
    echo 'if [ -f /etc/bash_completion ]; then . /etc/bash_completion; fi' >> ~/.bashrc

# sandbox.sh bind-mounts the checkout at the same absolute host path.
WORKDIR /workspace

# Default command: bash shell
CMD ["bash"]
