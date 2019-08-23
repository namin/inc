FROM buildpack-deps:stretch

ENV RUSTUP_HOME=/usr/local/rustup \
  CARGO_HOME=/usr/local/cargo \
  PATH=/usr/local/cargo/bin:$PATH \
  RUST_VERSION=1.37.0

RUN wget https://sh.rustup.rs -O /rustup.sh \
  && chmod +x /rustup.sh \
  && ./rustup.sh -y --no-modify-path --default-toolchain nightly \
  && rm rustup.sh \
  && chmod -R a+w $RUSTUP_HOME $CARGO_HOME \
  && rustup --version \
  && cargo --version \
  && rustc --version;

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
RUN cargo build --tests
