FROM rust:latest AS rust-build

COPY . /loxvm

RUN cd /loxvm && \
    cargo build --release && \
    mv target /

FROM ubuntu:latest as runtime

COPY --from=rust-build /target/release/main /loxvm/

WORKDIR /work

ENTRYPOINT [ "/loxvm/main" ]
CMD []
