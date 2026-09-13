# syntax=docker/dockerfile:1
# Dockerfile - Build the single-container development environment for local compiler work.

ARG QEMU_VERSION=11.1.1
ARG QEMU_COMMIT=c3d48b7d1e89604920e5b81b91140c2ad39a1943
FROM mcr.microsoft.com/dotnet/sdk:10.0-noble AS dotnet
FROM node:26-bookworm-slim AS node
FROM docker.io/docker/sandbox-templates:claude-code AS claude
FROM rust:1.89.0-slim-bookworm AS rust
RUN --mount=type=cache,target=/usr/local/rustup/downloads \
    rustup target add aarch64-unknown-linux-gnu x86_64-unknown-linux-gnu && \
    rm -rf /usr/local/rustup/tmp/*

FROM ubuntu:noble AS qemu-builder
ARG QEMU_VERSION
ARG QEMU_COMMIT
ARG PROXY_CA_CERT_B64

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt/lists,sharing=locked \
    apt-get update && \
    apt-get install -y --no-install-recommends \
      build-essential \
      ca-certificates \
      git \
      libglib2.0-dev \
      ninja-build \
      pkg-config \
      python3 \
      python3-venv

RUN if [ -n "$PROXY_CA_CERT_B64" ]; then \
      printf '%s' "$PROXY_CA_CERT_B64" | base64 --decode > /usr/local/share/ca-certificates/proxy-ca.crt; \
      update-ca-certificates; \
    fi

RUN git clone --depth 1 --branch "v${QEMU_VERSION}" \
      https://gitlab.com/qemu-project/qemu.git /qemu
RUN test "$(git -C /qemu rev-parse HEAD)" = "$QEMU_COMMIT"

WORKDIR /qemu/build
RUN ../configure \
      --target-list=aarch64-linux-user,x86_64-linux-user \
      --enable-plugins \
      --disable-system \
      --disable-docs \
      --disable-tools
RUN ninja qemu-aarch64 qemu-x86_64 tests/tcg/plugins/libinsn.so
RUN ./qemu-aarch64 --version | grep -F "qemu-aarch64 version ${QEMU_VERSION}" && \
    ./qemu-x86_64 --version | grep -F "qemu-x86_64 version ${QEMU_VERSION}"
RUN strip --strip-unneeded qemu-aarch64 qemu-x86_64 tests/tcg/plugins/libinsn.so && \
    mkdir -p /opt/dcb/qemu && \
    cp qemu-aarch64 qemu-x86_64 tests/tcg/plugins/libinsn.so /opt/dcb/qemu/

FROM docker.io/docker/sandbox-templates:codex
ARG TARGETARCH
ARG PROXY_CA_CERT_B64

USER root

COPY --from=dotnet /usr/share/dotnet /usr/share/dotnet
COPY --from=node /usr/local /usr/local
COPY --from=claude --chown=agent:agent /home/agent/.local/bin/claude /home/agent/.local/bin/claude
COPY --from=claude --chown=agent:agent /home/agent/.local/share/claude /home/agent/.local/share/claude
COPY --from=rust /usr/local/cargo /usr/local/cargo
COPY --from=rust /usr/local/rustup /usr/local/rustup
COPY --from=qemu-builder /opt/dcb/qemu /opt/dcb/qemu

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt/lists,sharing=locked \
    case "$TARGETARCH" in \
      amd64) cross_packages="gcc-aarch64-linux-gnu libc6-dev-arm64-cross" ;; \
      arm64) cross_packages="gcc-x86-64-linux-gnu libc6-dev-amd64-cross" ;; \
      *) echo "Unsupported image architecture: $TARGETARCH" >&2; exit 1 ;; \
    esac && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
      bash-completion \
      build-essential \
      ca-certificates \
      curl \
      file \
      git \
      gzip \
      htop \
      hyperfine \
      jq \
      less \
      libatomic1 \
      libglib2.0-0t64 \
      libssl3t64 \
      libstdc++6 \
      ocaml \
      python3 \
      shellcheck \
      sqlite3 \
      sudo \
      tzdata \
      valgrind \
      vim \
      zlib1g \
      $cross_packages

RUN if [ -n "$PROXY_CA_CERT_B64" ]; then \
      printf '%s' "$PROXY_CA_CERT_B64" | base64 --decode > /usr/local/share/ca-certificates/proxy-ca.crt; \
      update-ca-certificates; \
    fi

RUN mkdir -p /home/agent/.nuget/packages /workspace && \
    chown -R agent:agent /home/agent /workspace

USER agent
ENV HOME=/home/agent
ENV DOTNET_ROOT=/usr/share/dotnet
ENV DOTNET_CLI_HOME=/home/agent
ENV DOTNET_MULTILEVEL_LOOKUP=0
ENV CARGO_HOME=/usr/local/cargo
ENV RUSTUP_HOME=/usr/local/rustup
ENV PATH=/usr/share/dotnet:/home/agent/.dotnet/tools:/home/agent/.local/bin:/usr/local/cargo/bin:$PATH

RUN --mount=type=bind,source=scripts/install-darklang-interpreter.sh,target=/tmp/install-darklang-interpreter.sh \
    bash /tmp/install-darklang-interpreter.sh

RUN git config --global alias.ci commit && \
    git config --global alias.co checkout && \
    git config --global alias.st status
RUN echo 'parse_git_branch() { git branch 2>/dev/null | grep "^*" | sed "s/* //"; }' >> ~/.bashrc && \
    echo 'short_path() { pwd | sed "s|$HOME|~|"; }' >> ~/.bashrc && \
    echo 'PS1="\[\033[1;32m\]\u@dark\[\033[0m\]:\[\033[1;34m\]\$(short_path)\[\033[0m\]\[\033[1;33m\]\$(parse_git_branch | sed \"s/.*/ (&)/\")\[\033[0m\]\$ "' >> ~/.bashrc && \
    echo 'if [ -f /etc/bash_completion ]; then . /etc/bash_completion; fi' >> ~/.bashrc

WORKDIR /workspace
