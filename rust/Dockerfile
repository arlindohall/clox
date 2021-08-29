FROM rust:latest

COPY . /loxvm

RUN cd /loxvm && \
    cargo build --release && \
    mv target /

RUN rm -rf /loxvm && mkdir /loxvm

WORKDIR /loxvm

ENTRYPOINT [ "/target/release/main" ]
CMD []