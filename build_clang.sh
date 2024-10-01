#!/usr/bin/env bash

set -eu

export DPCPP_HOME=~/llvm_workspace
mkdir -p $DPCPP_HOME
cd $DPCPP_HOME || exit

if [ -d "llvm-project" ]; then
  echo 'Repo llvm already exists, pulling changes...'
  cd "llvm-project"
#    git pull
else
  echo 'Repo llvm does not exist, cloning...'
  git clone https://github.com/llvm/llvm-project
  cd "llvm-project"
fi

cmake --fresh -S "llvm" -B "llvm/build" \
  -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang \
  -DCMAKE_BUILD_TYPE=Release \
  -DBUILD_SHARED_LIBS=OFF \
  -DLLVM_ENABLE_PROJECTS="clang;lld;openmp;compiler-rt" \
  -DLLVM_INCLUDE_BENCHMARKS=OFF \
  -DLLVM_INCLUDE_TESTS=OFF \
  -DLLVM_INCLUDE_DOCS=OFF \
  -DLLVM_INCLUDE_EXAMPLES=OFF \
  -DLLVM_BUILD_TESTS=OFF \
  -DLLVM_BUILD_DOCS=OFF \
  -DLLVM_BUILD_EXAMPLES=OFF \
  -DLLVM_TARGETS_TO_BUILD="host;NVPTX" \
  -DCMAKE_INSTALL_PREFIX="$PWD/llvm/build/install" \
  -GNinja

cmake --build llvm/build --target install
