FROM ubuntu:22.04

RUN apt-get update
RUN apt-get install -y cmake
COPY ci-ubuntu-install-compilers.sh /ci-ubuntu-install-compilers.sh
RUN chmod +x /ci-ubuntu-install-compilers.sh
RUN LLVM_VERSIONS="11 12 13" ./ci-ubuntu-install-compilers.sh
RUN rm -rf /var/lib/apt/lists/*

ENTRYPOINT ["/bin/bash"]