FROM ubuntu:latest

# Setup base image deps
RUN apt update && apt install -y \
    build-essential \
    cargo \
    gdb \
    rustc \
    && rm -rf /var/lib/apt/lists/*

# http://whitfin.io/speeding-up-rust-docker-builds
RUN USER=root cargo new --bin /dep
WORKDIR /dep

COPY ./Cargo.lock ./Cargo.lock
COPY ./Cargo.toml ./Cargo.toml

RUN cargo build
RUN rm src/*.rs

RUN mkdir /inc
WORKDIR /inc
ADD . /inc
RUN cargo build
