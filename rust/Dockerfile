FROM rust:latest

COPY . /loxvm

RUN cd /loxvm && \
    cargo build --release && \
    mv target /

CMD /target/release/main