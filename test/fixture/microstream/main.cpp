#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <vector>

#include "consume.h"

#ifdef USE_HIP
  #include <hip/hip_runtime.h>
#endif

#define startA (0.1)
#define startB (0.2)
#define startC (0.0)
#define startScalar (0.4)
#define ALIGNMENT (2 * 1024 * 1024) // 2MB

template <typename I, typename R, typename C, typename M, typename A, typename T, typename D>
auto execute(size_t times, I init, R read, C copy, M mul, A add, T triad, D dot) {
  std::array<std::vector<double>, 5> timings{};
  auto time = [&](auto idx, auto f) {
    using namespace std::chrono;
    auto t1 = high_resolution_clock::now();
    f();
    auto t2 = high_resolution_clock::now();
    timings[idx].emplace_back(duration_cast<duration<double>>(t2 - t1).count());
  };

  init();
  for (size_t k = 0; k < times; k++) {
    time(0, copy);
    time(1, mul);
    time(2, add);
    time(3, triad);
    time(4, dot);
  }
  read();
  return timings;
}

#if defined(USE_CUDA) || defined(USE_HIP)

  #if defined(USE_CUDA)
    #define deviceMalloc cudaMalloc
    #define deviceMemcpy cudaMemcpy
    #define deviceSynchronize cudaDeviceSynchronize
    #define getDeviceProperties cudaGetDeviceProperties
    #define getErrorString cudaGetErrorString
    #define errorType cudaError_t
    #define deviceProps cudaDeviceProp
    #define memcpyD2H cudaMemcpyDeviceToHost
    #define successValue cudaSuccess
  #elif defined(USE_HIP)
    #define deviceMalloc hipMalloc
    #define deviceMemcpy hipMemcpy
    #define deviceSynchronize hipDeviceSynchronize
    #define getDeviceProperties hipGetDeviceProperties
    #define getErrorString hipGetErrorString
    #define errorType hipError_t
    #define deviceProps hipDeviceProp_t
    #define memcpyD2H hipMemcpyDeviceToHost
    #define successValue hipSuccess
  #endif

  #define CHECK(EXPR)                                                                              \
    do {                                                                                           \
      if (auto e = (EXPR); e != successValue) {                                                    \
        std::fprintf(stderr, "%s:%d: %s (%d)\n  %s\n", __FILE__, __LINE__, getErrorString(e), e,   \
                     #EXPR);                                                                       \
        std::exit(e);                                                                              \
      }                                                                                            \
    } while (false)

template <typename F> __global__ void for_each(size_t size, F f) {
  for (int i = blockDim.x * blockIdx.x + threadIdx.x; i < size; i += gridDim.x * blockDim.x) {
    f(i);
  }
}

template <size_t TBSIZE, class T, typename F, typename G>
__global__ void map_reduce(int size, const T *a, const T *b, T *out, F f, G g) {
  __shared__ T partial[TBSIZE];
  int gid = blockDim.x * blockIdx.x + threadIdx.x;
  const auto tid = threadIdx.x;
  partial[tid] = {};
  for (; gid < size; gid += blockDim.x * gridDim.x)
    partial[tid] += f(a[gid], b[gid]);

  for (int offset = blockDim.x / 2; offset > 0; offset /= 2) {
    __syncthreads();
    if (tid < offset) partial[tid] = g(partial[tid], partial[tid + offset]);
  }
  if (tid == 0) out[blockIdx.x] = partial[tid];
}

template <typename T>
auto runAll(                                           //
    T *__restrict__, T *__restrict__, T *__restrict__, //
    T *h_a, T *h_b, T *h_c,                            //
    T initA, T initB, T initC,                         //
    T scalar, T &sum,                                  //
    int size, int times) {

  constexpr int TBSIZE = 1024;
  size_t blocks = (size + TBSIZE - 1) / TBSIZE;

  deviceProps props;
  CHECK(getDeviceProperties(&props, 0));
  int dotNumBlocks = props.multiProcessorCount * 4;

  T *d_a{}, *d_b{}, *d_c{}, *d_sum{};
  CHECK(deviceMalloc(&d_a, size * sizeof(T)));
  CHECK(deviceMalloc(&d_b, size * sizeof(T)));
  CHECK(deviceMalloc(&d_c, size * sizeof(T)));
  CHECK(deviceMalloc(&d_sum, dotNumBlocks * sizeof(T)));
  auto h_sums = (T *)std::aligned_alloc(ALIGNMENT, dotNumBlocks * sizeof(T));

  auto init = [&]() {
    for_each<<<blocks, TBSIZE, 0>>>(size, [=](auto i) {
      d_a[i] = initA;
      d_b[i] = initB;
      d_c[i] = initC;
    });
    CHECK(deviceSynchronize());
  };

  auto read = [&]() {
    CHECK(deviceMemcpy(h_a, d_a, size * sizeof(T), memcpyD2H));
    CHECK(deviceMemcpy(h_b, d_b, size * sizeof(T), memcpyD2H));
    CHECK(deviceMemcpy(h_c, d_c, size * sizeof(T), memcpyD2H));
  };

  auto copy = [&]() {
    for_each<<<blocks, TBSIZE, 0>>>(size, [=](auto i) { d_c[i] = d_a[i]; });
    CHECK(deviceSynchronize());
  };

  auto mul = [&]() {
    for_each<<<blocks, TBSIZE, 0>>>(size, [=](auto i) { d_b[i] = scalar * d_c[i]; });
    CHECK(deviceSynchronize());
  };

  auto add = [&]() {
    for_each<<<blocks, TBSIZE, 0>>>(size, [=](auto i) { d_c[i] = d_a[i] + d_b[i]; });
    CHECK(deviceSynchronize());
  };

  auto triad = [&]() {
    for_each<<<blocks, TBSIZE, 0>>>(size, [=](auto i) { d_a[i] = d_b[i] + scalar * d_c[i]; });
    CHECK(deviceSynchronize());
  };

  auto dot = [&]() {
    map_reduce<TBSIZE>
        <<<dotNumBlocks, TBSIZE, 0>>>(size, d_a, d_b, d_sum, std::multiplies<>(), std::plus<>());
    CHECK(deviceMemcpy(h_sums, d_sum, dotNumBlocks * sizeof(T), memcpyD2H));
    sum = {};
    for (int i = 0; i < dotNumBlocks; i++)
      sum += h_sums[i];
    CHECK(deviceSynchronize());
  };

  return execute(times, init, read, copy, mul, add, triad, dot);
}

#endif

#if defined(USE_SERIAL) || defined(USE_OMP) || defined(USE_OMP_TARGET)
template <typename T>
auto runAll(                                                 //
    T *__restrict__ a, T *__restrict__ b, T *__restrict__ c, //
    T *h_a, T *h_b, T *h_c,                                  //
    T initA, T initB, T initC,                               //
    T scalar, T &sum,                                        //
    int size, int times) {

  #ifdef USE_OMP_TARGET

    #pragma omp target enter data map(alloc : a[0 : size], b[0 : size], c[0 : size])
  {}
  #endif

  auto init = [&]() {
  #if defined(USE_OMP_TARGET)
    #pragma omp target teams distribute parallel for simd
  #elif defined(USE_OMP)
    #pragma omp parallel for
  #endif
    for (int i = 0; i < size; i++) {
      a[i] = initA;
      b[i] = initB;
      c[i] = initC;
    }
  };

  auto read = [&]() {
  #if defined(USE_OMP_TARGET)
    #pragma omp target update from(a[0 : size], b[0 : size], c[0 : size])
    {}
  #elif defined(USE_OMP)
    #pragma omp parallel for
  #endif
    for (int i = 0; i < size; i++) {
      h_a[i] = a[i];
      h_b[i] = b[i];
      h_c[i] = c[i];
    }
  };

  auto copy = [&]() {
  #if defined(USE_OMP_TARGET)
    #pragma omp target teams distribute parallel for simd
  #elif defined(USE_OMP)
    #pragma omp parallel for
  #endif
    for (int i = 0; i < size; i++)
      c[i] = a[i];
  };

  auto mul = [&]() {
  #if defined(USE_OMP_TARGET)
    #pragma omp target teams distribute parallel for simd
  #elif defined(USE_OMP)
    #pragma omp parallel for
  #endif
    for (int i = 0; i < size; i++)
      b[i] = scalar * c[i];
  };

  auto add = [&]() {
  #if defined(USE_OMP_TARGET)
    #pragma omp target teams distribute parallel for simd
  #elif defined(USE_OMP)
    #pragma omp parallel for
  #endif
    for (int i = 0; i < size; i++)
      c[i] = a[i] + b[i];
  };

  auto triad = [&]() {
  #if defined(USE_OMP_TARGET)
    #pragma omp target teams distribute parallel for simd
  #elif defined(USE_OMP)
    #pragma omp parallel for
  #endif
    for (int i = 0; i < size; i++)
      a[i] = b[i] + scalar * c[i];
  };

  auto dot = [&]() {
    sum = 0.0;
  #if defined(USE_OMP_TARGET)
    #pragma omp target teams distribute parallel for simd map(tofrom : sum) reduction(+ : sum)
  #elif defined(USE_OMP)
    #pragma omp parallel for reduction(+ : sum)
  #endif
    for (int i = 0; i < size; i++)
      sum += a[i] * b[i];
  };

  return execute(times, init, read, copy, mul, add, triad, dot);
}
#endif

template <typename T> auto run(int size, int times) {

  auto bytes = sizeof(T) * size;

  std::cout << "Running kernels " << times << " times" << std::endl;
  std::cout << "Number of elements: " << size << std::endl;
  std::cout << "Precision: " << typeid(T).name() << std::endl;
  std::cout << std::setprecision(1) << std::fixed //
            << "Array size: " << bytes * 1.0E-6 << " MB" << " (=" << bytes * 1.0E-9 << " GB)\n"
            << "Total size: " << 3.0 * bytes * 1.0E-6 << " MB" //
            << " (=" << 3.0 * bytes * 1.0E-9 << " GB)" << std::endl;

  std::vector<T> h_a(size), h_b(size), h_c(size);
  auto a = (T *)std::aligned_alloc(ALIGNMENT, bytes), //
      b = (T *)std::aligned_alloc(ALIGNMENT, bytes),  //
      c = (T *)std::aligned_alloc(ALIGNMENT, bytes);
  T sum = {};

  auto timings = runAll<T>(a, b, c, h_a.data(), h_b.data(), h_c.data(), //
                           startA, startB, startC, startScalar, sum, size, times);

  consume(a);
  consume(b);
  consume(c);

  std::free(a);
  std::free(b);
  std::free(c);

  std::cout << std::left << std::setw(12) << "Function"   //
            << std::left << std::setw(12) << "MBytes/sec" //
            << std::left << std::setw(12) << "Min (sec)"  //
            << std::left << std::setw(12) << "Max"        //
            << std::left << std::setw(12) << "Average" << std::fixed << std::endl;

  std::vector<std::pair<std::string, size_t>> kernels = //
      {{"Copy", 2 * bytes},
       {"Mul", 2 * bytes},
       {"Add", 3 * bytes},
       {"Triad", 3 * bytes},
       {"Dot", 2 * bytes}};
  for (size_t i = 0; i < timings.size(); ++i) {
    auto [min, max] = std::minmax_element(timings[i].begin() + 1, timings[i].end());
    auto avg = std::accumulate(timings[i].begin() + 1, timings[i].end(), 0.0) / (double)(times - 1);
    auto [name, total] = kernels[i];
    std::cout                                                                              //
        << std::left << std::setw(12) << name                                              //
        << std::left << std::setw(12) << std::setprecision(3) << (1.0E-6) * total / (*min) //
        << std::left << std::setw(12) << std::setprecision(5) << *min                      //
        << std::left << std::setw(12) << std::setprecision(5) << *max                      //
        << std::left << std::setw(12) << std::setprecision(5) << avg << std::endl;
  }

  T goldA = startA, goldB = startB, goldC = startC;
  const T scalar = startScalar;
  for (int i = 0; i < times; i++) {
    goldC = goldA;
    goldB = scalar * goldC;
    goldC = goldA + goldB;
    goldA = goldB + scalar * goldC;
  }
  T goldSum = goldA * goldB * size;

  double epsi = std::numeric_limits<T>::epsilon() * 100.0;
  auto err = [epsi](auto &xs, auto gold, auto name) {
    double err = std::accumulate(xs.begin(), xs.end(), 0.0,
                                 [&](double acc, auto x) { return acc + std::fabs(x - gold); });
    if ((err / xs.size()) > epsi) {
      std::cerr << "Validation failed on " << name << ". Average error " << err << std::endl;
      return true;
    }
    return false;
  };

  double errSum = std::fabs((sum - goldSum) / goldSum);
  auto sumErr = errSum > 1.0E-8;
  if (sumErr) {
    std::cerr << "Validation failed on sum. Error " << errSum << std::endl
              << std::setprecision(15) << "Sum was " << sum << " but should be " << goldSum
              << std::endl;
  }

  return !err(h_a, goldA, "a[]") && !err(h_b, goldB, "b[]") && !err(h_c, goldC, "b[]") && !sumErr;
}

int main(int argc, const char *argv[]) {
  int size = 33554432;
  int times = 100;
  if (argc - 1 >= 1) size = std::stoi(argv[1]);
  if (argc - 1 >= 2) times = std::stoi(argv[2]);
  return run<double>(size, times) ? EXIT_SUCCESS : EXIT_FAILURE;
}
