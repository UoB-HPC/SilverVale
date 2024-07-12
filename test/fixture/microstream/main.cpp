#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <vector>

#define startA (0.1)
#define startB (0.2)
#define startC (0.0)
#define startScalar (0.4)
#define ALIGNMENT (2 * 1024 * 1024) // 2MB

template <typename T>
auto runAll(                                                 //
    T *__restrict__ a, T *__restrict__ b, T *__restrict__ c, //
    T *h_a, T *h_b, T *h_c,                                  //
    T initA, T initB, T initC,                               //
    T scalar, T &sum,                                        //
    int size, int times) {

  auto init = [&]() {
#ifdef USE_OMP
  #pragma omp parallel for
#endif
    for (int i = 0; i < size; i++) {
      a[i] = initA;
      b[i] = initB;
      c[i] = initC;
    }
  };

  auto read = [&]() {
#ifdef USE_OMP
  #pragma omp parallel for
#endif
    for (int i = 0; i < size; i++) {
      h_a[i] = a[i];
      h_b[i] = b[i];
      h_c[i] = c[i];
    }
  };

  auto copy = [&]() {
#ifdef USE_OMP
  #pragma omp parallel for
#endif
    for (int i = 0; i < size; i++)
      c[i] = a[i];
  };

  auto mul = [&]() {
#ifdef USE_OMP
  #pragma omp parallel for
#endif
    for (int i = 0; i < size; i++)
      b[i] = scalar * c[i];
  };

  auto add = [&]() {
#ifdef USE_OMP
  #pragma omp parallel for
#endif
    for (int i = 0; i < size; i++)
      c[i] = a[i] + b[i];
  };

  auto triad = [&]() {
#ifdef USE_OMP
  #pragma omp parallel for
#endif
    for (int i = 0; i < size; i++)
      a[i] = b[i] + scalar * c[i];
  };

  auto dot = [&]() {
    sum = 0.0;
#ifdef USE_OMP
  #pragma omp parallel for reduction(+ : sum)
#endif
    for (int i = 0; i < size; i++)
      sum += a[i] * b[i];
  };

  std::array<std::vector<double>, 5> timings{};
  auto time = [&](auto idx, auto f) {
    using namespace std::chrono;
    auto t1 = high_resolution_clock::now();
    f();
    auto t2 = high_resolution_clock::now();
    timings[idx].emplace_back(duration_cast<duration<double>>(t2 - t1).count());
  };

  init();
  for (int k = 0; k < times; k++) {
    time(0, copy);
    time(1, mul);
    time(2, add);
    time(3, triad);
    time(4, dot);
  }
  read();
  return timings;
}

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

int main() { return run<double>(33554432, 100) ? EXIT_SUCCESS : EXIT_FAILURE; }
