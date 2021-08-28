#!/bin/bash
docker run -it --rm -v $(pwd):/loxvm -w /loxvm rust:latest bash