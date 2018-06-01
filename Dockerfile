FROM ubuntu:latest

# Setup base image deps
RUN apt-get update && apt-get install -y \
    build-essential \
    libncurses5-dev \
    libx11-dev \
    wget \
    && rm -rf /var/lib/apt/lists/*

# Install chez from source
RUN cd /tmp \
    && wget -q https://github.com/cisco/ChezScheme/releases/download/v9.5/csv9.5.tar.gz

RUN cd /tmp \
    && tar -xf csv9.5.tar.gz \
    && cd csv9.5 \
    && cp /usr/include/locale.h zlib/xlocale.h \
    && ./configure \
    && make install \
    && cd - \
    && rm -rf csv9.5.tar.gz csv9.5

WORKDIR /inc
ADD . /inc
