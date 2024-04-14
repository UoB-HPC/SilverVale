#include "p3md/compress.h"

#include <iostream>
#include <string>

#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

#include "zstd.h"

static bool zStdIsError(size_t err) {
  if (ZSTD_isError(err)) {
    std::cerr << ZSTD_getErrorName(err) << std::endl;
    return true;
  }
  return false;
}

p3md::utils::zstd_ostream::zstd_ostream(const std::string &name, std::error_code &code, int cLevel)
    : out(name, code), cctx(ZSTD_createCCtx()), buffOut(ZSTD_CStreamOutSize(), 0) {
  if (!cctx) {
    std::cerr << "Cannot create Zstd context" << std::endl;
    code = std::make_error_code(std::errc::io_error);
  }
  if (zStdIsError(ZSTD_CCtx_setParameter(cctx, ZSTD_c_compressionLevel, cLevel)))
    code = std::make_error_code(std::errc::io_error);

  if (zStdIsError(ZSTD_CCtx_setParameter(cctx, ZSTD_c_checksumFlag, 1)))
    code = std::make_error_code(std::errc::io_error);
}

p3md::utils::zstd_ostream::~zstd_ostream() {
  std::cout << "End" << std::endl;
  //    if (zStdIsError(ZSTD_flushStream(cctx, &output))) std::abort();
  ZSTD_outBuffer output = {buffOut.data(), buffOut.size(), 0};

  if (zStdIsError(ZSTD_endStream(cctx, &output))) std::abort();
  if (zStdIsError(ZSTD_freeCCtx(cctx))) { std::abort(); }
  out.close();
}

void p3md::utils::zstd_ostream::write_impl(const char *buffIn, size_t size) {
  ZSTD_inBuffer input = {buffIn, size, 0};
  do {
    ZSTD_outBuffer output = {buffOut.data(), buffOut.size(), 0};
    const size_t remaining = ZSTD_compressStream2(cctx, &output, &input, ZSTD_e_continue);
    if (zStdIsError(remaining)) std::abort();
    out.write(buffOut.data(), output.pos);
  } while (input.pos != input.size);
  pos += size;
}

[[nodiscard]] uint64_t p3md::utils::zstd_ostream::current_pos() const { return pos; }

std::optional<std::vector<char>> p3md::utils::zStdDecompress(const std::string &filename) {
  auto buffer = llvm::MemoryBuffer::getFile(filename);
  if (auto error = buffer.getError(); error) {
    std::cerr << error.message() << std::endl;
    return std::nullopt;
  }
  auto rSize = ZSTD_getFrameContentSize((*buffer)->getBufferStart(), (*buffer)->getBufferSize());
  if (rSize == ZSTD_CONTENTSIZE_ERROR) {
    // XXX just return it as there's an option to no comperss in build
    return std::vector<char>{(*buffer)->getBufferStart(), (*buffer)->getBufferEnd()};
  }
  if (rSize == ZSTD_CONTENTSIZE_UNKNOWN) {
    std::cerr << "File original size unknown: " << filename << std::endl;
    return std::nullopt;
  }

  std::vector<char> rBuff(rSize, 0);
  size_t dSize =
      ZSTD_decompress(rBuff.data(), rSize, (*buffer)->getBufferStart(), (*buffer)->getBufferSize());
  if (ZSTD_isError(dSize)) {
    std::cerr << ZSTD_getErrorName(dSize) << std::endl;
    return std::nullopt;
  }

  if (dSize != rSize) {
    std::cerr << "Impossible because zstd will check this condition!" << std::endl;
    return std::nullopt;
  }
  return rBuff;

} // namespace p3md::utils