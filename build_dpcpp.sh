#!/usr/bin/env bash

set -eu

export DPCPP_HOME=~/sycl_workspace
mkdir -p $DPCPP_HOME
cd $DPCPP_HOME || exit

if [ -d "llvm" ]; then
  echo 'Repo llvm already exists, pulling changes...'
  cd "llvm" && git pull origin sycl
else
  echo 'Repo llvm does not exist, cloning...'
  git clone https://github.com/intel/llvm -b sycl
fi

python $DPCPP_HOME/llvm/buildbot/configure.py --llvm-external-projects=compiler-rt

python $DPCPP_HOME/llvm/buildbot/compile.py
python $DPCPP_HOME/llvm/buildbot/compile.py --build-target install-llvm-cov
python $DPCPP_HOME/llvm/buildbot/compile.py --build-target install-llvm-profdata
python $DPCPP_HOME/llvm/buildbot/compile.py --build-target install-compiler-rt
