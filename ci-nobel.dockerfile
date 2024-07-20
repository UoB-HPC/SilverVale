FROM ubuntu:24.04

RUN apt-get update
RUN apt-get install -y cmake
COPY ci-ubuntu-install-compilers.sh /ci-ubuntu-install-compilers.sh
RUN chmod +x /ci-ubuntu-install-compilers.sh
RUN GCC_VERSIONS="9 19 11 12 13 14" LLVM_VERSIONS="14 15 16 17 18" ./ci-ubuntu-install-compilers.sh
RUN rm -rf /var/lib/apt/lists/*

ENTRYPOINT ["/bin/bash"]