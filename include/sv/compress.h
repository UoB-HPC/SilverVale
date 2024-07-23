#pragma once

#include <string>

#include "zstd.h"
#include "llvm/Support/raw_ostream.h"

namespace sv::utils {

class zstd_ostream : public llvm::raw_ostream {
  llvm::raw_fd_ostream out;
  ZSTD_CCtx *cctx;
  size_t pos{};
  std::vector<char> buffOut;

public:
  zstd_ostream(const std::string &name, std::error_code &code, int cLevel = ZSTD_CLEVEL_DEFAULT);
  ~zstd_ostream() override;
  void write_impl(const char *buffIn, size_t size) override;
  [[nodiscard]] uint64_t current_pos() const override;
};

std::optional<std::vector<char>> zStdDecompress(const std::string &filename);

} // namespace sv::utils