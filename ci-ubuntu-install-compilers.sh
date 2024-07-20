#!/usr/bin/env bash

for version in $GCC_VERSIONS; do
  apt-get install -y "g++-$version" "gcc-$version-plugin-dev" "gfortran-$version"
done

for version in $LLVM_VERSIONS; do
  apt-get install -y "clang-$version"
done
