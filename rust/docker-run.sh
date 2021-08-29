#!/bin/bash
docker run --rm -i -v $(pwd):/loxvm arlindohall/loxvm:latest $@
