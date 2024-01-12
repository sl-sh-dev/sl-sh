FROM rust:1.52.1

WORKDIR /usr/src/sl-sh

RUN rustup target add x86_64-unknown-linux-musl

COPY . .
RUN cargo install --path .

CMD ["sl-sh"]
