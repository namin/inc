FROM debian:latest
WORKDIR /inc

# Setup base image deps
RUN apt-get update && apt-get install -y \
    build-essential \
    gcc-multilib \
    libncurses5-dev \
    libx11-dev \
    wget \
    && rm -rf /var/lib/apt/lists/*

# Install chez from source
RUN cd /tmp \
    && wget -q https://github.com/cisco/ChezScheme/releases/download/v9.5/csv9.5.tar.gz \
    && tar -xf csv9.5.tar.gz \
    && cd csv9.5 \
    && ./configure \
    && make install \
    && cd - \
    && rm -rf csv9.5.tar.gz csv9.5

ADD . /inc
