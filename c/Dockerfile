FROM ubuntu:latest

RUN apt update
RUN apt install -y cmake clang-9

COPY . /clox

RUN cd /clox && \
    mkdir build && \
    cd build && \
    export CC=/usr/bin/clang-9 && \
    export CXX=/usr/bin/clang++-9 && \
    cmake .. && \
    make

RUN mv /clox/build/clox /tmp/clox && \
    cd / && \
    rm -rf /clox && \
    mv /tmp/clox /

CMD /clox
