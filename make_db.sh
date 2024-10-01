#!/bin/bash

set -eu

BUILD_DIR=builds

function fetch_src() {
  local url=$1
  local branch=$2
  local check_file=$3
  local out_dir=$4
  mkdir -p ${BUILD_DIR:?}
  if [ ! -e "${BUILD_DIR:?}/$out_dir/$check_file" ]; then
    if ! git clone -b "$branch" "$url" "${BUILD_DIR:?}/$out_dir"; then
      echo "Failed to fetch source code." && exit 1
    fi
  else
    (cd "${BUILD_DIR:?}/$out_dir" && git fetch && git pull)
  fi
}

function prime_kokkos() {
  local kokkos_ver=$1
  local kokkos_dir="kokkos-$kokkos_ver"
  echo "Using Kokkos src $kokkos_dir"
  mkdir -p ${BUILD_DIR:?}
  if [ ! -e "${BUILD_DIR:?}/$kokkos_dir" ]; then
    (
      cd ${BUILD_DIR:?}
      wget "https://github.com/kokkos/kokkos/archive/$kokkos_ver.tar.gz"
      tar -xf "$kokkos_ver.tar.gz" -C .
      rm "$kokkos_ver.tar.gz"
    )
  fi
  export KOKKOS_SRC="$PWD/${BUILD_DIR:?}/$kokkos_dir"
}

function build() {
  local code=$1
  local model=$2
  local suffix=$3
  local dir="${code}_${model}${suffix:+_}${suffix}"
  mkdir -p ${BUILD_DIR:?}
  rm -rf "${BUILD_DIR:?}/$dir"
  set -x
  # we neutralise the link line to avoid annoying link errors
  #    -DCMAKE_CXX_LINK_EXECUTABLE="echo <CMAKE_CXX_COMPILER> <FLAGS> <CMAKE_CXX_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>" \
  local coverage="-fprofile-instr-generate -fcoverage-mapping -Wno-everything"
  CXXFLAGS="$coverage" LDFLAGS="$coverage" cmake "-B${BUILD_DIR:?}/${dir}" "-S${BUILD_DIR:?}/${code}-src" "-DMODEL=$model" -DCMAKE_CUDA_FLAGS="$coverage" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_VERBOSE_MAKEFILE=ON \
    "${@:4}"                                        #>/dev/null
  cmake --build "${BUILD_DIR:?}/$dir" -j "$(nproc)" #>/dev/null

  case "$code" in
  babelstream)

    (
      cd "${BUILD_DIR:?}/$dir"
      # shellcheck disable=SC2211
      ./*-stream --arraysize 16384 --numtimes 10
    )
    ;;
  minibude)

    (
      cd "${BUILD_DIR:?}/$dir"
      # shellcheck disable=SC2211
      ./*-bude --ppwi 2,4 --wgsize 1 --poses 512 --deck "../minibude-src/data/bm1"
    )
    ;;
  cloverleaf)
    (
      #    export HSA_OVERRIDE_GFX_VERSION=10.3.6
      cd "${BUILD_DIR:?}/$dir"
      cat <<EOF >clover_coverage.in
*clover

 state 1 density=0.2 energy=1.0
 state 2 density=1.0 energy=2.5 geometry=rectangle xmin=0.0 xmax=5.0 ymin=0.0 ymax=5.0

 x_cells=200
 y_cells=200

 xmin=0.0
 ymin=0.0
 xmax=10.0
 ymax=10.0

 initial_timestep=0.04
 timestep_rise=1.5
 end_step=4
 max_timestep=0.04
 end_time=30.0

*endclover
EOF

      set +e
      ./*-cloverleaf --file clover_coverage.in --profile --staging-buffer auto
      set -e
    )
    ;;
  tealeaf)
    (
      #    export HSA_OVERRIDE_GFX_VERSION=10.3.6
      cd "${BUILD_DIR:?}/$dir"
      cat <<EOF >tea_coverage.in
*tea
state 1 density=100.0 energy=0.0001
state 2 density=0.1 energy=25.0 geometry=rectangle xmin=0.0 xmax=1.0 ymin=1.0 ymax=2.0
state 3 density=0.1 energy=0.1 geometry=rectangle xmin=1.0 xmax=6.0 ymin=1.0 ymax=2.0
state 4 density=0.1 energy=0.1 geometry=rectangle xmin=5.0 xmax=6.0 ymin=1.0 ymax=8.0
state 5 density=0.1 energy=0.1 geometry=rectangle xmin=5.0 xmax=10.0 ymin=7.0 ymax=8.0
x_cells=128
y_cells=128
xmin=0.0
ymin=0.0
xmax=10.0
ymax=10.0
initial_timestep=0.004
end_step=5
max_iters=10000
use_cg
eps 1.0e-15
profiler_on
use_c_kernels
*endtea
EOF
      cat <<EOF >tea.problems
128 128 5 1.154362784372189e+02
EOF

      ./*-tealeaf --file tea_coverage.in --profile --staging-buffer auto
    )
    ;;

  esac

  set +x
}

prime_kokkos "4.1.00"
# oneapi_headers=/opt/intel/oneapi/compiler/2024.1/include

# (
#   rm -rf sycl-headers
#   git clone -n --depth=1 --filter=tree:0 https://github.com/intel/llvm -b sycl sycl-headers
#   cd sycl-headers
#   git sparse-checkout set --no-cone sycl/include
#   git checkout f22edfda59a43134fc0aee80521f403d4240e3fa
#   ls -lah
# )

# oneapi_headers=$PWD/sycl-headers/sycl/include

#oneapi_headers=/home/tom/Downloads/sycl_linux/include
## XXX objective is to get it to compile only, linking is not required; arch-specific options are there only to make the driver happy

export CCACHE_DISABLE=1

CLANG_CXX="/usr/bin/clang++"
CLANG_C="/usr/bin/clang"
#CLANG_CXX="/home/tom/software/llvm-ompt/7af27be6633a/bin/clang++"
${CLANG_CXX} -v

SYCL_WORKSPACE="/home/tom/sycl_workspace/llvm/build/install/"
LLVM_WORKSPACE="/home/tom/llvm_workspace/llvm-project/llvm/build/install/"

threads=32
#threads=1

sycl_flags="-Wno-unknown-attributes;-Wno-deprecated-declarations;-fsycl"

kinds=""
kinds+="srclen,srclen+cpp,srclen+cov"
kinds+=",sloc,sloc+cpp,sloc+cov"
kinds+=",lloc,lloc+cpp,lloc+cov"
kinds+=",src,src+cpp,src+cov"
kinds+=",tstree,tstree+cpp,tstree+cov"
kinds+=",stree,stree+cov"
kinds+=",streeinlined,streeinlined+cov"
kinds+=",irtree,irtree+cov"

#kinds="srclen,srclen+cpp,srclen+cov,sloc,sloc+cpp,sloc+cov,lloc,lloc+cpp,lloc+cov,src,src+cpp,src+cov,tstree,tstree+cpp,tstree+cov"
#kinds="tstree"
bin=/home/tom/SilverVale/cmake-build-release/sv
kokkos_glob="{*/kokkos-*,*Kokkos_*}"

function babelstream-fortran() {

  fetch_src "https://github.com/UoB-HPC/BabelStream" ijhpca-p3md CMakeLists.txt babelstream-fortran-src

  sed -i '/^\(\t\)bear --append -- /!s/^\(\t\)\($(FC) $(FCFLAGS)\)/\1bear --append -- \2/' $BUILD_DIR/babelstream-fortran-src/src/fortran/Makefile
  cat $BUILD_DIR/babelstream-fortran-src/src/fortran/Makefile | grep "FC"

  models=(Sequential Array OpenMP OpenMPTaskloop OpenACC OpenACCArray DoConcurrent)
  args=""

  for model in "${models[@]}"; do
    args+=" @db/builds.babelstream-fortran_$model:*babelstream-fortran-src/*"
    (
      mkdir -p "$BUILD_DIR/babelstream-fortran_$model"
      cd "$BUILD_DIR/babelstream-fortran_$model" || exit 1
      make -C ../babelstream-fortran-src/src/fortran/ -f Makefile COMPILER="gcc" IMPLEMENTATION="${model}"
      mv ../babelstream-fortran-src/src/fortran/compile_commands.json .
    )
    "$bin" index -j "$threads" -v --build "$PWD/$BUILD_DIR/babelstream-fortran_$model" --out "db/builds.babelstream-fortran_$model" --clear
    "$bin" dump --db "db/builds.babelstream-fortran_$model"
  done

  kinds=""
  kinds+="srclen,srclen+cpp"
  kinds+=",sloc,sloc+cpp"
  kinds+=",lloc,lloc+cpp"
  kinds+=",src,src+cpp"
  kinds+=",tstree,tstree+cpp"
  kinds+=",stree"
  kinds+=",irtree"

  for model in "serial"; do
    (
      set -x
      "$bin" delta -j "$threads" --kinds "$kinds" --prefix babelstream-fortran \
        --merges "*Stream.F90*:Stream.F90" \
        --maxGroups 20 --maxStreeThreads "$threads" \
        $args
    )
  done

  build babelstream serial "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}"
  build babelstream cuda "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCUDA_CLANG_DRIVER=ON -DCMAKE_CUDA_COMPILER="${CLANG_CXX}" -DCUDA_ARCH=sm_60
  build babelstream omp "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}"
  build babelstream omp target -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DOFFLOAD=ON -DOFFLOAD_FLAGS="-fopenmp;--offload-arch=sm_60"
  build babelstream hip "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DRELEASE_FLAGS="-xhip;--offload-arch=gfx1036" -DCXX_EXTRA_LIBRARIES="amdhip64"
  build babelstream kokkos "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DKokkos_ENABLE_OPENMP=ON -DKOKKOS_IN_TREE="$KOKKOS_SRC"
  build babelstream std-indices "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCXX_EXTRA_LIBRARIES="tbb"
  build babelstream std-data "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCXX_EXTRA_LIBRARIES="tbb"
  build babelstream std-ranges "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCXX_EXTRA_LIBRARIES="tbb"
  build babelstream tbb "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCXX_EXTRA_LIBRARIES="tbb"
  build babelstream ocl "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DOpenCL_LIBRARY=/usr/lib64/libOpenCL.so.1
  (
    export IGC_EnableDPEmulation=1
    export OverrideDefaultFP64Settings=1
    export LD_LIBRARY_PATH="$SYCL_WORKSPACE/lib:${LD_LIBRARY_PATH:-}"
    export PATH="$SYCL_WORKSPACE/bin:$PATH"
    build babelstream sycl "" -DCMAKE_CXX_COMPILER="clang" -DSYCL_COMPILER=ONEAPI-Clang -DCXX_EXTRA_FLAGS="$sycl_flags"
    build babelstream sycl2020-acc "" -DCMAKE_C_COMPILER="clang" -DCMAKE_CXX_COMPILER="clang++" -DSYCL_COMPILER=ONEAPI-Clang -DCXX_EXTRA_FLAGS="$sycl_flags"
    build babelstream sycl2020-usm "" -DCMAKE_C_COMPILER="clang" -DCMAKE_CXX_COMPILER="clang++" -DSYCL_COMPILER=ONEAPI-Clang -DCXX_EXTRA_FLAGS="$sycl_flags"
  )

  mkdir -p db

}

function babelstream() {

  fetch_src "https://github.com/UoB-HPC/BabelStream" ijhpca-p3md CMakeLists.txt babelstream-src

  build babelstream serial "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}"
  build babelstream cuda "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCUDA_CLANG_DRIVER=ON -DCMAKE_CUDA_COMPILER="${CLANG_CXX}" -DCUDA_ARCH=sm_60
  build babelstream omp "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}"
  build babelstream omp target -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DOFFLOAD=ON -DOFFLOAD_FLAGS="-fopenmp;--offload-arch=sm_60"
  build babelstream hip "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DRELEASE_FLAGS="-xhip;--offload-arch=gfx1036" -DCXX_EXTRA_LIBRARIES="amdhip64"
  build babelstream kokkos "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DKokkos_ENABLE_OPENMP=ON -DKOKKOS_IN_TREE="$KOKKOS_SRC"
  build babelstream std-indices "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCXX_EXTRA_LIBRARIES="tbb"
  build babelstream std-data "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCXX_EXTRA_LIBRARIES="tbb"
  build babelstream std-ranges "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCXX_EXTRA_LIBRARIES="tbb"
  build babelstream tbb "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCXX_EXTRA_LIBRARIES="tbb"
  build babelstream ocl "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DOpenCL_LIBRARY=/usr/lib64/libOpenCL.so.1
  (
    export IGC_EnableDPEmulation=1
    export OverrideDefaultFP64Settings=1
    export LD_LIBRARY_PATH="$SYCL_WORKSPACE/lib:${LD_LIBRARY_PATH:-}"
    export PATH="$SYCL_WORKSPACE/bin:$PATH"
    build babelstream sycl "" -DCMAKE_CXX_COMPILER="clang" -DSYCL_COMPILER=ONEAPI-Clang -DCXX_EXTRA_FLAGS="$sycl_flags"
    build babelstream sycl2020-acc "" -DCMAKE_C_COMPILER="clang" -DCMAKE_CXX_COMPILER="clang++" -DSYCL_COMPILER=ONEAPI-Clang -DCXX_EXTRA_FLAGS="$sycl_flags"
    build babelstream sycl2020-usm "" -DCMAKE_C_COMPILER="clang" -DCMAKE_CXX_COMPILER="clang++" -DSYCL_COMPILER=ONEAPI-Clang -DCXX_EXTRA_FLAGS="$sycl_flags"
  )

  mkdir -p db
  models=(serial cuda omp omp_target hip kokkos std-indices std-data std-ranges tbb ocl sycl sycl2020-usm sycl2020-acc)
  args=""
  for model in "${models[@]}"; do
    args+=" @db/builds.babelstream_$model:*babelstream-src/*"
    (
      case "$model" in
      sycl*)
        export LD_LIBRARY_PATH="$SYCL_WORKSPACE/lib:${LD_LIBRARY_PATH:-}"
        export PATH="$SYCL_WORKSPACE/bin:$PATH"
        export UPROOT_SO=libuproot-clang-dpcpp.so
        ;;
      esac

      #      "$bin" index -j "$threads" -v --build "${BUILD_DIR:?}/babelstream_$model" --excludes "$kokkos_glob" --out "db/builds.babelstream_$model" --cov-bin "${BUILD_DIR:?}/babelstream_$model/"*-stream --clear
    )
  done

  mkdir -p out_babelstream
  for model in "serial"; do
    (
      set -x
      "$bin" delta -j "$threads" --kinds "$kinds" --prefix babelstream \
        --merges "*Stream*.c*:Stream.cpp" \
        --maxGroups 20 --maxStreeThreads "$threads" \
        $args
    )
  done
  echo "Done"
}

function minibude() {

  fetch_src "https://github.com/UoB-HPC/miniBUDE" v2 CMakeLists.txt minibude-src

  build minibude serial "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DPPWI=2,4
  build minibude cuda "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DPPWI=2,4 -DCUDA_CLANG_DRIVER=ON -DCMAKE_CUDA_COMPILER="${CLANG_CXX}" -DCUDA_ARCH=sm_60
  build minibude omp "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DPPWI=2,4
  build minibude omp target -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DPPWI=2,4 -DOFFLOAD=ON -DOFFLOAD_FLAGS="-fopenmp;--offload-arch=sm_60"
  build minibude hip "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER=hipcc -DRELEASE_FLAGS="-xhip;--offload-arch=gfx1036" -DCXX_EXTRA_LIBRARIES="amdhip64"
  build minibude kokkos "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DPPWI=2,4 -DKokkos_ENABLE_OPENMP=ON -DKOKKOS_IN_TREE="$KOKKOS_SRC"
  build minibude std-indices "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DPPWI=2,4 -DCXX_EXTRA_LIBRARIES="tbb"
  build minibude std-ranges "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DPPWI=2,4 -DCXX_EXTRA_LIBRARIES="tbb"
  build minibude tbb "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DPPWI=2,4 -DCXX_EXTRA_LIBRARIES="tbb"
  build minibude ocl "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DPPWI=2,4 -DOpenCL_LIBRARY=/usr/lib64/libOpenCL.so.1
  (
    export IGC_EnableDPEmulation=1
    export OverrideDefaultFP64Settings=1
    export LD_LIBRARY_PATH="$SYCL_WORKSPACE/lib:${LD_LIBRARY_PATH:-}"
    export PATH="$SYCL_WORKSPACE/bin:$PATH"
    build minibude sycl "" -DCMAKE_CXX_COMPILER="clang" -DSYCL_COMPILER=ONEAPI-Clang -DPPWI=2,4 -DCXX_EXTRA_FLAGS="$sycl_flags"
  )

  mkdir -p db
  models=(serial cuda omp omp_target hip kokkos std-indices std-ranges tbb ocl sycl)
  args=""
  for model in "${models[@]}"; do

    if [ $model == "ocl" ] || [ $model == "sycl" ]; then
      args+=" @db/builds.minibude_$model:*minibude-src/*"
    else
      args+=" db/builds.minibude_$model:*minibude-src/*"
    fi

    #    args+=" @db/builds.minibude_$model:*minibude-src/*"
    (
      case "$model" in
      sycl*)
        export LD_LIBRARY_PATH="$SYCL_WORKSPACE/lib:${LD_LIBRARY_PATH:-}"
        export PATH="$SYCL_WORKSPACE/bin:$PATH"
        export UPROOT_SO=libuproot-clang-dpcpp.so
        ;;
      esac

      #            "$bin" index -j "$threads" -v --build "${BUILD_DIR:?}/minibude_$model" --excludes "$kokkos_glob" --out "db/builds.minibude_$model" --cov-bin "${BUILD_DIR:?}/minibude_$model/"*-bude --clear
    )
  done

  mkdir -p out_minibude
  for model in "serial"; do
    (
      set -x
      "$bin" delta -j "$threads" --kinds "$kinds" --prefix minibude \
        --maxGroups 3 --maxStreeThreads "10" \
        $args

      #      "$bin" dump --db "db/${BUILD_DIR:?}.minibude_$model"    --rootGlobs "*minibude-src/*"

    )
  done
  echo "Done"
}

function cloverleaf() {

  fetch_src "https://github.com/UoB-HPC/CloverLeaf" main CMakeLists.txt cloverleaf-src

  build cloverleaf serial "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}"
  build cloverleaf cuda "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCUDA_CLANG_DRIVER=ON -DCMAKE_CUDA_COMPILER="${CLANG_CXX}" -DCUDA_ARCH=sm_60
  build cloverleaf omp "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}"
  build cloverleaf omp "target" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DOFFLOAD=ON -DOFFLOAD_FLAGS="-fopenmp;--offload-arch=gfx1036"
  build cloverleaf hip "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER=hipcc -DRELEASE_FLAGS="--offload-arch=gfx1036" # -DCXX_EXTRA_LIBRARIES="amdhip64"
  build cloverleaf kokkos "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DKokkos_ENABLE_OPENMP=ON -DKOKKOS_IN_TREE="$KOKKOS_SRC"
  build cloverleaf std-indices "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCXX_EXTRA_LIBRARIES="tbb"
  build cloverleaf tbb "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCXX_EXTRA_LIBRARIES="tbb"
  (
    export IGC_EnableDPEmulation=1
    export OverrideDefaultFP64Settings=1
    export LD_LIBRARY_PATH="$SYCL_WORKSPACE/lib:${LD_LIBRARY_PATH:-}"
    export PATH="$SYCL_WORKSPACE/bin:$PATH"
    build cloverleaf sycl-usm "" -DCMAKE_C_COMPILER="clang" -DCMAKE_CXX_COMPILER="clang++" -DSYCL_COMPILER=ONEAPI-Clang -DCXX_EXTRA_FLAGS="$sycl_flags"
    build cloverleaf sycl-acc "" -DCMAKE_C_COMPILER="clang" -DCMAKE_CXX_COMPILER="clang++" -DSYCL_COMPILER=ONEAPI-Clang -DCXX_EXTRA_FLAGS="$sycl_flags"
  )

  mkdir -p db
  models=(serial cuda omp omp_target hip kokkos std-indices tbb sycl-usm sycl-acc)
  #  models=(serial  sycl-usm)
  args=""
  for model in "${models[@]}"; do

    if [ $model == "cuda" ]; then
      args+=" @db/builds.cloverleaf_$model:*cloverleaf-src/*"
    else
      args+=" db/builds.cloverleaf_$model:*cloverleaf-src/*"
    fi

    #    args+=" db/builds.cloverleaf_$model:*cloverleaf-src/*"
    (
      case "$model" in
      sycl*)
        export LD_LIBRARY_PATH="$SYCL_WORKSPACE/lib:${LD_LIBRARY_PATH:-}"
        export PATH="$SYCL_WORKSPACE/bin:$PATH"
        export UPROOT_SO=libuproot-clang-dpcpp.so
        ;;
      esac

      #      "$bin" index -j "$threads" -v --build "${BUILD_DIR:?}/cloverleaf_$model" --excludes "$kokkos_glob" --out "db/builds.cloverleaf_$model" --cov-bin "${BUILD_DIR:?}/cloverleaf_$model/"*-cloverleaf --clear
    )
  done

  mkdir -p out_cloverleaf
  for model in "serial"; do
    (
      set -x
      "$bin" delta -j "$threads" --kinds "$kinds" --prefix cloverleaf \
        --merges "*update_halo*.cpp:update_halo.cpp" \
        --merges "*update_tile_halo_kernel*.cpp:update_tile_halo_kernel.cpp" \
        --maxGroups 1 --maxStreeThreads "$threads" \
        $args
    )
  done

  echo "Done"
}

function tealeaf() {

  fetch_src "https://github.com/UoB-HPC/TeaLeaf" main CMakeLists.txt tealeaf-src

  build tealeaf serial "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}"
  build tealeaf cuda "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCUDA_CLANG_DRIVER=ON -DCMAKE_CUDA_COMPILER="${CLANG_CXX}" -DCUDA_ARCH=sm_60
  build tealeaf omp "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}"
  build tealeaf omp target -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DOFFLOAD=ON -DOFFLOAD_FLAGS="-fopenmp;--offload-arch=gfx1036"
  build tealeaf hip "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER=hipcc -DRELEASE_FLAGS="--offload-arch=gfx1036" # -DCXX_EXTRA_LIBRARIES="amdhip64"
  build tealeaf kokkos "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DKokkos_ENABLE_OPENMP=ON -DKOKKOS_IN_TREE="$KOKKOS_SRC"
  build tealeaf std-indices "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCXX_EXTRA_LIBRARIES="tbb"
  build tealeaf tbb "" -DCMAKE_C_COMPILER="${CLANG_C}" -DCMAKE_CXX_COMPILER="${CLANG_CXX}" -DCXX_EXTRA_LIBRARIES="tbb"
  (
    export IGC_EnableDPEmulation=1
    export OverrideDefaultFP64Settings=1
    export LD_LIBRARY_PATH="$SYCL_WORKSPACE/lib:${LD_LIBRARY_PATH:-}"
    export PATH="$SYCL_WORKSPACE/bin:$PATH"
    build tealeaf sycl-usm "" -DCMAKE_C_COMPILER="clang" -DCMAKE_CXX_COMPILER="clang++" -DSYCL_COMPILER=ONEAPI-Clang -DCXX_EXTRA_FLAGS="$sycl_flags"
    build tealeaf sycl-acc "" -DCMAKE_C_COMPILER="clang" -DCMAKE_CXX_COMPILER="clang++" -DSYCL_COMPILER=ONEAPI-Clang -DCXX_EXTRA_FLAGS="$sycl_flags"
  )

  mkdir -p db
  models=(serial cuda omp omp_target hip kokkos std-indices tbb sycl-usm sycl-acc)
  args=""
  for model in "${models[@]}"; do
    args+=" @db/builds.tealeaf_$model:*tealeaf-src/*"
    (
      case "$model" in
      sycl*)
        export LD_LIBRARY_PATH="$SYCL_WORKSPACE/lib:${LD_LIBRARY_PATH:-}"
        export PATH="$SYCL_WORKSPACE/bin:$PATH"
        export UPROOT_SO=libuproot-clang-dpcpp.so
        ;;
      esac
      "$bin" index -j "$threads" -v --build "${BUILD_DIR:?}/tealeaf_$model" --excludes "$kokkos_glob" --out "db/builds.tealeaf_$model" --cov-bin "${BUILD_DIR:?}/tealeaf_$model/"*-tealeaf --clear
    )
  done

  mkdir -p out_tealeaf
  for model in "serial"; do
    (
      set -x
      "$bin" delta -j "$threads" --kinds "$kinds" --prefix tealeaf \
        --merges "{*diffuse_overload.cpp,*solver_methods.cpp}:solver_methods.cpp" \
        --maxGroups 3 --maxStreeThreads "$threads" \
        $args
      # "$bin" dump --db "db/${BUILD_DIR:?}.tealeaf_$model"
    )
  done

  echo "Done"
}

function gromacs() {

  ver=2024.3
  mkdir -p ${BUILD_DIR:?}
  cd ${BUILD_DIR:?} || exit 1

  if [ ! -d "gromacs-$ver" ]; then
    wget "https://ftp.gromacs.org/gromacs/gromacs-$ver.tar.gz"
    tar xf "gromacs-$ver.tar.gz" && rm -rf "gromacs-$ver.tar.gz"
  fi

  (

    export LD_LIBRARY_PATH="$LLVM_WORKSPACE/lib:${LD_LIBRARY_PATH:-}"
    export PATH="$LLVM_WORKSPACE/bin:$PATH"

    rm -rf "gromacs_omp/CMakeCache.txt"
    cmake -Bgromacs_omp -S"gromacs-$ver" \
      -DCMAKE_VERBOSE_MAKEFILE=ON -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
      -DGMX_BUILD_OWN_FFTW=ON \
      -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++

    export UPROOT_SO=libuproot-clang-llvm.so
    "$bin" index -j "$threads" -v --build "gromacs_omp" \
      --includes "*/gromacs/nbnxm/*" \
      --out "db/builds.gromacs_omp"

    cmake --build build-cuda -j $(nproc)

  )
  return

  (

    return
    export IGC_EnableDPEmulation=1
    export OverrideDefaultFP64Settings=1
    export LD_LIBRARY_PATH="$SYCL_WORKSPACE/lib:${LD_LIBRARY_PATH:-}"
    export PATH="$SYCL_WORKSPACE/bin:$PATH"

    rm -rf "gromacs_sycl/CMakeCache.txt"
    cmake -Bgromacs_sycl -S"gromacs-$ver" \
      -DCMAKE_VERBOSE_MAKEFILE=ON -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
      -DGMX_BUILD_OWN_FFTW=ON \
      -DCMAKE_INCLUDE_PATH="$SYCL_WORKSPACE/include" \
      -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ \
      -DGMX_GPU_FFT_LIBRARY=none \
      -DGMX_GPU=SYCL -DGMX_SYCL=DPCPP

    export UPROOT_SO=libuproot-clang-dpcpp.so
    "$bin" index -j "$threads" -v --build "gromacs_sycl" \
      --includes "*/gromacs/nbnxm/sycl/*" \
      --out "db/builds.gromacs_sycl"

  )
  (

    return

    export LD_LIBRARY_PATH="$LLVM_WORKSPACE/lib:${LD_LIBRARY_PATH:-}"
    export PATH="$LLVM_WORKSPACE/bin:$PATH"

    rm -rf "gromacs_cuda/CMakeCache.txt"
    cmake -Bgromacs_cuda -S"gromacs-$ver" \
      -DCMAKE_VERBOSE_MAKEFILE=ON -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
      -DGMX_BUILD_OWN_FFTW=ON -DGMX_GPU=CUDA -DGMX_CLANG_CUDA=ON -DGMX_CUDA_TARGET_SM=60 \
      -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_CUDA_ARCHITECTURES=60 -DCMAKE_CUDA_COMPILER=clang++ # -DCMAKE_CUDA_FLAGS="-stdlib=libc++ -D_ALLOW_UNSUPPORTED_LIBCPP" # --debug-try    compile

    export UPROOT_SO=libuproot-clang-llvm.so
    "$bin" index -j "$threads" -v --build "gromacs_cuda" \
      --includes "*/gromacs/nbnxm/cuda/*" \
      --out "db/builds.gromacs_cuda"

    cmake --build build-cuda -j $(nproc)

  )

  kinds=""
  kinds+="srclen,srclen+cpp"
  kinds+=",sloc,sloc+cpp"
  kinds+=",lloc,lloc+cpp"
  kinds+=",src,src+cpp"
  kinds+=",tstree"
  kinds+=",stree"

  "$bin" delta -j "$threads" --kinds "$kinds" --prefix gromacs_nbnxm \
    --includes "*nbnxm_*_kernel*" \
    --excludes "*nbnxm_sycl_kernel.cpp" \
    --merges "*/gromacs/nbnxm/*/nbnxm_cuda.*:setup.cpp" \
    --merges "*/gromacs/nbnxm/*/nbnxm_sycl.*:setup.cpp" \
    --merges "*/gromacs/nbnxm/*/nbnxm_*_data_mgmt.*:data_mgmt.cpp" \
    --merges "*/gromacs/nbnxm/*/nbnxm_*_kernel_pruneonly.*:kernel_pruneonly.cpp" \
    --merges "*/gromacs/nbnxm/*/nbnxm_*_kernel_{F,body_f}_prune.*:kernel_f_prune.cpp" \
    --merges "*/gromacs/nbnxm/*/nbnxm_*_kernel_{VF,body_fv}_prune.*:kernel_vf_prune.cpp" \
    --merges "*/gromacs/nbnxm/*/nbnxm_*_kernel_{F,body_f}_noprune.*:kernel_f_noprune.cpp" \
    --merges "*/gromacs/nbnxm/*/nbnxm_*_kernel_{VF,body_fv}_noprune.*:kernel_vf_noprune.cpp" \
    --merges "*/gromacs/nbnxm/*/nbnxm_gpu_buffer_ops_internal*:gpu_buffer_ops_internal.cpp" \
    --maxGroups 3 --maxStreeThreads 16 \
    "db/builds.gromacs_cuda:*nbnxm/cuda/*" "db/builds.gromacs_sycl:*nbnxm/sycl/*"

  export LD_LIBRARY_PATH="$SYCL_WORKSPACE/lib:${LD_LIBRARY_PATH:-}"
  export PATH="$SYCL_WORKSPACE/bin:$PATH"
  export UPROOT_SO=libuproot-clang-dpcpp.so

  (
    return

    cd gromacs-$ver || exit 1
    rm -rf build-omp/CMakeCache.txt

    local coverage="-fprofile-instr-generate -fcoverage-mapping -Wno-everything"
    CXXFLAGS="$coverage" LDFLAGS="$coverage" cmake -Bbuild-omp -S. \
      -DCMAKE_VERBOSE_MAKEFILE=ON -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
      -DGMX_BUILD_OWN_FFTW=ON \
      -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++

    cmake --build build-omp -j $(nproc)

  )
  (

    return
    cd gromacs-$ver || exit 1
    rm -rf build-serial/CMakeCache.txt

    local coverage="-fprofile-instr-generate -fcoverage-mapping -Wno-everything"
    CXXFLAGS="$coverage" LDFLAGS="$coverage" cmake -Bbuild-serial -S. \
      -DCMAKE_VERBOSE_MAKEFILE=ON -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
      -DGMX_BUILD_OWN_FFTW=ON -DGMX_OPENMP=OFF -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++

    cmake --build build-serial -j $(nproc)

  )

}

function blender() {
  :

}

babelstream-fortran
babelstream
minibude
cloverleaf
tealeaf

gromacs
