FROM ubuntu:20.04

RUN apt-get update
RUN apt-get install -y cmake
COPY ci-ubuntu-install-compilers.sh /ci-ubuntu-install-compilers.sh
RUN chmod +x /ci-ubuntu-install-compilers.sh
RUN GCC_VERSIONS="7 8" LLVM_VERSIONS="7 8 9 10" ./ci-ubuntu-install-compilers.sh
RUN rm -rf /var/lib/apt/lists/*

ENTRYPOINT ["/bin/bash"]