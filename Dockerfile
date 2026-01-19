# syntax=docker/dockerfile:1

FROM --platform=linux/amd64 debian:12-slim AS build-stage

ENV DEBIAN_FRONTEND=noninteractive
ENV SBCL_VERSION=2.3.9

# Dépendances système
RUN apt-get update && apt-get install -y \
    curl \
    ca-certificates \
    build-essential \
    libssl-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Installer SBCL
RUN curl -L https://downloads.sourceforge.net/project/sbcl/sbcl/${SBCL_VERSION}/sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2 \
    | tar -xj \
 && cd sbcl-${SBCL_VERSION}-x86-64-linux \
 && sh install.sh \
 && cd / \
 && rm -rf sbcl-${SBCL_VERSION}-x86-64-linux

# Installer Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp \
 && sbcl --non-interactive \
    --load quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
    --eval '(quit)' \
 && rm quicklisp.lisp

# Copier le projet
WORKDIR /app
COPY . /app

# Construire le binaire
RUN sbcl --script build.lisp

FROM --platform=linux/amd64 gcr.io/distroless/base-debian12

ENV PORT=4242

WORKDIR /app
COPY --from=build-stage /app/build/yano-bin /app/yano-bin

EXPOSE $PORT
ENTRYPOINT ["/app/yano-bin"]
