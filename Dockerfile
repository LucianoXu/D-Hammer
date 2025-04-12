FROM ubuntu:22.04

USER root

RUN apt-get update
RUN apt-get install -y clang-16
RUN apt-get install -y build-essential
RUN apt-get install -y cmake
RUN apt-get install -y git
RUN apt-get install -y ca-certificates
RUN apt-get install -y libboost-all-dev

COPY . /D-Hammer
WORKDIR /D-Hammer

RUN mkdir -p /build/Release
# RUN cmake -S . -B build/Release -DCMAKE_BUILD_TYPE=Release
# RUN cmake --build build/Release -j

CMD ["bash"]