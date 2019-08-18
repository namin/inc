FROM ubuntu:latest

# Setup base image deps
RUN apt update && apt install -y \
    build-essential \
    curl \
    gdb \
    && rm -rf /var/lib/apt/lists/*

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh \
  && rustup default nightly \
  && rustup update nightly

# http://whitfin.io/speeding-up-rust-docker-builds
RUN USER=root cargo new --bin /dep
WORKDIR /dep

COPY ./Cargo.lock ./Cargo.lock
COPY ./Cargo.toml ./Cargo.toml

RUN cargo build --tests
RUN rm src/*.rs

RUN mkdir /inc
WORKDIR /inc
ADD . /inc
RUN cargo build
