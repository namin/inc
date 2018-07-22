FROM ubuntu:latest

# Setup base image deps
RUN apt-get update && apt-get install -y \
    build-essential \
    racket \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /inc
ADD . /inc
