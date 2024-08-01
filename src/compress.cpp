#include "sv/compress.h"

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <string>

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorOr.h"
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

sv::utils::zstd_ostream::zstd_ostream(const std::string &name, std::error_code &code, int cLevel)
    : raw_ostream(true), out(name, code), cctx(ZSTD_createCCtx()),
      buffOut(ZSTD_CStreamOutSize(), '\0') {
  if (!cctx) {
    std::cerr << "Cannot create Zstd context" << std::endl;
    code = std::make_error_code(std::errc::io_error);
  }
  if (zStdIsError(ZSTD_CCtx_setParameter(cctx, ZSTD_c_compressionLevel, cLevel)))
    code = std::make_error_code(std::errc::io_error);

  if (zStdIsError(ZSTD_CCtx_setParameter(cctx, ZSTD_c_checksumFlag, 1)))
    code = std::make_error_code(std::errc::io_error);
}

sv::utils::zstd_ostream::~zstd_ostream() {
  ZSTD_outBuffer output = {buffOut.data(), buffOut.size(), 0};
  if (zStdIsError(ZSTD_endStream(cctx, &output))) std::abort();
  if (zStdIsError(ZSTD_freeCCtx(cctx))) std::abort();
  out.write(buffOut.data(), output.pos);
  out.close();
}

void sv::utils::zstd_ostream::write_impl(const char *buffIn, size_t size) {
  ZSTD_inBuffer input = {buffIn, size, 0};
  do {
    ZSTD_outBuffer output = {buffOut.data(), buffOut.size(), 0};
    const size_t remaining = ZSTD_compressStream2(cctx, &output, &input, ZSTD_e_continue);
    if (zStdIsError(remaining)) std::abort();
    out.write(buffOut.data(), output.pos);
  } while (input.pos != input.size);
  pos += size;
}

[[nodiscard]] uint64_t sv::utils::zstd_ostream::current_pos() const { return pos; }

std::optional<std::vector<char>> sv::utils::zStdDecompress(const std::string &filename) {
  auto buffer = llvm::MemoryBuffer::getFile(filename);
  if (auto error = buffer.getError(); error) {
    std::cerr << error.message() << std::endl;
    return std::nullopt;
  }

  auto rSize = ZSTD_getFrameContentSize((*buffer)->getBufferStart(), (*buffer)->getBufferSize());
  if (rSize == ZSTD_CONTENTSIZE_ERROR) {
    // Just return it as there's an option to not compress in build
    return std::vector<char>{(*buffer)->getBufferStart(), (*buffer)->getBufferEnd()};
  }

  if (rSize != ZSTD_CONTENTSIZE_UNKNOWN) {
    std::vector<char> result(rSize, 0);
    size_t dSize = ZSTD_decompress(result.data(), rSize, (*buffer)->getBufferStart(),
                                   (*buffer)->getBufferSize());
    if (zStdIsError(dSize)) std::abort();
    if (dSize != rSize) {
      std::cerr << "Zstd invariant failed: decompressed size != frame content size" << std::endl;
      std::abort();
    }
    return result;
  } else {
    ZSTD_DStream *dstream = ZSTD_createDStream();
    if (!dstream) {
      std::cerr << "Failed to create ZSTD_DStream" << std::endl;
      return std::nullopt;
    }

    size_t initResult = ZSTD_initDStream(dstream);
    if (zStdIsError(initResult)) {
      ZSTD_freeDStream(dstream);
      std::abort();
    }

    std::vector<char> inBuffer(ZSTD_DStreamInSize());
    std::vector<char> outBuffer(ZSTD_DStreamOutSize());
    std::vector<char> result;
    ZSTD_inBuffer input = {(*buffer)->getBufferStart(), (*buffer)->getBufferSize(), 0};
    ZSTD_outBuffer output = {outBuffer.data(), outBuffer.size(), 0};
    while (input.pos < input.size) {
      if (zStdIsError(ZSTD_decompressStream(dstream, &output, &input))) {
        ZSTD_freeDStream(dstream);
        std::abort();
      }
      result.insert(result.end(), outBuffer.data(), outBuffer.data() + output.pos);
      output.pos = 0;
    }
    ZSTD_freeDStream(dstream);
    return result;
  }
} // namespace sv::utils