#!/bin/bash
docker run \
    -it \
    --rm \
    -v $(pwd)/src:/loxvm/src \
    -v $(pwd)/Cargo.lock:/loxvm/Cargo.lock \
    -v $(pwd)/Cargo.toml:/loxvm/Cargo.toml \
    -w /loxvm \
    rust:latest \
    bash