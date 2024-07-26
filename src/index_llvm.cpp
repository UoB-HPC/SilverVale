#include <filesystem>
#include <fstream>
#include <iostream>

#include "sv/cli.h"
#include "sv/compress.h"
#include "sv/exec.h"
#include "sv/glob.h"
#include "sv/index_common.h"
#include "sv/index_llvm.h"
#include "sv/model.h"
#include "sv/par.h"

#include "fmt/core.h"
#include "fmt/std.h"
#include "xxh3.h"

#include "clang/Driver/OffloadBundler.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "llvm/BinaryFormat/Magic.h"
#include "llvm/Object/OffloadBinary.h"
#include "llvm/Support/Program.h"

#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace std::string_literals;
using namespace aspartame;
using namespace llvm;

// Clean-room of https://clang.llvm.org/docs/ClangOffloadBundler.html
// We can't use clang::OffloadBundler::ListBundleIDsInFile because it writes to llvm::outs()
// and we can't easily capture the output when threaded.
struct ClangOffloadBundle {
  static constexpr char BundleMagic[] = "__CLANG_OFFLOAD_BUNDLE__";
  struct Entry {
    uint64_t offset, size, idLength;
    std::string id;
  };
  std::vector<Entry> entries;
  static std::optional<Expected<ClangOffloadBundle>> parse(const std::string &filename) {
    auto fail = [](auto &&s) { return make_error<StringError>(s, inconvertibleErrorCode()); };
    std::ifstream file(filename, std::ios::binary);
    if (!file) return fail(fmt::format("Cannot open {}", filename));

    char magic[sizeof(BundleMagic) - 1];
    if (!(file.read(magic, sizeof(magic)) &&
          (std::strncmp(magic, BundleMagic, sizeof(magic)) == 0))) {
      return {};
    }

    auto read = [&]<typename T>(T *field, auto name) -> std::optional<std::string> {
      if (!file.read(reinterpret_cast<char *>(field), sizeof(T))) {
        return (fmt::format("Cannot read {} bytes for ClangOffloadBundle.{}", sizeof(T), name));
      }
      return {};
    };
    ClangOffloadBundle bundle;
    uint64_t numEntries{};
    if (auto e = read(&numEntries, "numEntries"); e) return fail(*e);
    for (uint64_t i = 0; i < numEntries; ++i) {
      ClangOffloadBundle::Entry entry{};
      if (auto e = read(&entry.offset, "offset"); e) { return fail(*e); }
      if (auto e = read(&entry.size, "size"); e) { return fail(*e); }
      if (auto e = read(&entry.idLength, "idLength"); e) { return fail(*e); }
      entry.id.resize(entry.idLength);
      if (!file.read(entry.id.data(), static_cast<std::streamsize>(entry.idLength))) {
        return fail(fmt::format("Cannot read {} bytes for ClangOffloadBundle.id", entry.idLength));
      } else bundle.entries.push_back(entry);
    }
    return bundle;
  }
};

// Handle TU.bc/TU-$triple.bc bitcode file
// -save-temps has the following convention for BC temps:
//   Host
//      {OUTPUT}.bc
//   CUDA/HIP
//     {OUTPUT}-host-{triple}.bc
//     {OUTPUT}-{cuda|hip}-{triple}.bc
//   OpenMP target:
//     {OUTPUT}-host-{triple}.bc
//     {OUTPUT}-openmp-{triple}.bc
// -emit-llvm uses the same format where:
//   Only CUDA will create a BC file,
//   HIP generates a clang-offload-bundle file
//   OpenMP target generates a normal BC with a 0x10ff10ad prefixed @llvm.embedded.object
//   see https://clang.llvm.org/docs/ClangOffloadPackager.html
static std::vector<sv::LLVMBitcode> collectBitcodeFiles(bool verbose, const std::string &prefix,
                                                        const std::string &name,
                                                        const std::filesystem::path &wd,
                                                        const std::filesystem::path &dest) {
  std::vector<sv::LLVMBitcode> codes;
  auto saveBC = [&, pattern = std::regex("^" + name + "-([a-zA-Z]+)-([a-zA-Z0-9-_]+)\\.bc$")](
                    const std::filesystem::path &src, const std::filesystem::path &dest) {
    auto e = std::error_code{};
    if (!std::filesystem::copy_file(src, dest) || e)
      AGV_WARNF("failed to copy BC {} to {}: {}", src, dest, e.message());
    else {
      auto buffer = MemoryBuffer::getFile(src.string());
      if (!buffer) {
        AGV_WARNF("error reading BC {}: {}", src, buffer.getError().message());
        return;
      }
      auto bufferRef = buffer->get()->getMemBufferRef();
      if (auto magic = identify_magic(bufferRef.getBuffer()); magic != file_magic::bitcode) {
        AGV_WARNF("file {} is not a BC file (llvm::file_magic index={})", src,
                  static_cast<std::underlying_type_t<file_magic::Impl>>(magic));
        return;
      }

      SmallVector<object::OffloadFile> binaries;
      if (auto _ = object::extractOffloadBinaries(bufferRef, binaries)) {
        AGV_WARNF("error reading embedded offload binaries for {}", src);
      }
      binaries | filter([](auto &f) {
        return f.getBinary()->getImageKind() == object::ImageKind::IMG_Bitcode;
      }) | for_each([&](auto &f) {
        auto kind = getOffloadKindName(f.getBinary()->getOffloadKind()).str();
        auto triple = f.getBinary()->getTriple().str();
        auto embeddedName = fmt::format("{}.{}-{}-{}.bc", prefix, name, kind, triple);
        if (verbose)
          AGV_INFOF("adding embedded BC {} (kind={}, triple={})", embeddedName, kind, triple);
        std::error_code embeddedEC;
        llvm::raw_fd_ostream file((dest.parent_path() / embeddedName).string(), embeddedEC);
        file << f.getBinary()->getImage();
        if (embeddedEC) {
          AGV_WARNF("failed to write embedded offload binary {}: {}", embeddedName,
                    embeddedEC.message());
        } else codes.emplace_back(embeddedName, kind, triple);
      });

      auto destName = dest.filename().string();
      std::smatch match;
      auto [_, kind, triple] = std::regex_match(destName, match, pattern)
                                   ? codes.emplace_back(destName, match[1].str(), match[2].str())
                                   : codes.emplace_back(destName, "host", "");
      if (verbose) AGV_INFOF("adding BC: {} (kind={}, triple={})", destName, kind, triple);
    }
  };

  // first walk the wd to discover any existing target BC
  try {
    for (const auto &entry : std::filesystem::directory_iterator(wd)) {
      std::string bcFile = entry.path().filename();
      if (bcFile ^ starts_with(name + "-") && bcFile ^ ends_with(".bc")) {
        saveBC(bcFile, dest / fmt::format("{}.{}", prefix, bcFile));
      }
    }
  } catch (const std::exception &e) {
    AGV_WARNF("failed to traverse working directory {} for BC files:{} ", wd, e);
  }

  // then handle the host BC itself
  std::string hostBCFile;
  if (auto bcFile = wd / fmt::format("{}.bc", name); std::filesystem::exists(bcFile))
    hostBCFile = bcFile; //
  else if (auto oFile = wd / fmt::format("{}.o", name); std::filesystem::exists(oFile))
    hostBCFile = oFile; //
  if (!hostBCFile.empty()) {
    // found a valid host BC, it could be a clang offload bundle: try to unbundle
    // The following drivers calls are equivalent to:
    //   clang-offload-bundler --list     --type bc --input $FILE
    //   clang-offload-bundler --unbundle --type bc --input $FILE --output $OUT --targets $TARGET
    std::vector<std::string> targets;
    auto bundle = ClangOffloadBundle::parse(hostBCFile);
    if (bundle) {
      if (auto e = bundle->takeError())
        AGV_WARNF("cannot list offload bundles for {}: {}", hostBCFile, toString(std::move(e)));
      else targets = bundle->get().entries ^ map([](auto x) { return x.id; });
    }
    auto extracted = targets ^ collect([&](auto &target) -> std::optional<std::string> {
                       auto targetBCFile = fmt::format("{}.{}-{}.bc", prefix, name, target);
                       clang::OffloadBundlerConfig config;
                       config.FilesType = "bc";
                       config.ObjcopyPath = "";
                       config.InputFileNames = {hostBCFile};
                       config.OutputFileNames = {targetBCFile};
                       config.TargetNames = {target};
                       if (auto e = clang::OffloadBundler(config).UnbundleFiles()) {
                         AGV_WARNF("cannot extract target {} from {}", target, hostBCFile);
                         return std::nullopt;
                       }
                       if (verbose)
                         AGV_INFOF("extracted {} from offload bundle {}", targetBCFile, hostBCFile);
                       return {targetBCFile};
                     });
    auto hostBCDest = dest / fmt::format("{}.{}.bc", prefix, name);
    if (targets.empty()) saveBC(hostBCFile, hostBCDest); // not an offload bundle, copy the host BC
    else {
      if (extracted.size() != targets.size()) {
        AGV_WARNF(
            "not all BC extracted, got [{}] targets but extracted only [{}], retaining all BCs",
            targets | mk_string(","), extracted | mk_string(","));
        saveBC(hostBCFile, hostBCDest);
      }
      for (auto &file : extracted) { // copy the extracted targets then delete
        saveBC(file, dest / file);
        if (!std::filesystem::remove(file)) AGV_WARNF("cannot remove extracted temporary {}", file);
      }
    }
  }

  return codes;
}

bool sv::detectClangAndIndex(bool verbose,
                             const sv::CompilationDatabase::Entry &cmd, //
                             const std::filesystem::path &wd,           //
                             const std::filesystem::path &dest,         //
                             const std::unordered_map<std::string, std::string> &programLUT) {

  auto programAndVersion = sv::resolveProgramAndDetect(
      cmd.command[0],
      [](auto &x) {
        return x ^ starts_with("clang ") || x ^ starts_with("Intel(R) oneAPI DPC++/C++");
      },
      programLUT);
  if (!programAndVersion) return false;

  const auto &[program, version] = *programAndVersion;
  const auto prefix = fmt::format("{:08x}", XXH32(cmd.file.data(), cmd.file.size(), {}));
  const auto fileStem = std::filesystem::path(cmd.file).stem();
  const auto iiName = fmt::format("{}.ii", fileStem);
  const auto pchName = fmt::format("{}.pch", fileStem);
  const auto dFile = fmt::format("{}.d", fileStem);

  const auto isOMP = cmd.command ^ exists([](auto x) { return x ^ starts_with("-fopenmp"); });
  const auto noOffloadArch = [&](auto &arg) {
    return !isOMP || !(arg ^ starts_with("--offload-arch"));
  };

  const auto args = sv::stripHeadAndOArgs(cmd.command);
  const auto bcArgs =
      std::vector<std::string>{program, "-emit-llvm"} | concat(args) | to_vector();       //
  const auto pchArgs =                                                                    //
      std::vector<std::string>{program,                                                   //
                               "-emit-ast", "-o" + pchName, "--offload-host-only", "-MD"} //
      | concat(args | filter(noOffloadArch)) | to_vector();                               //
  const auto iiArgs =                                                                     //
      std::vector<std::string>{program,                                                   //
                               "-E", "-o" + iiName, "--offload-host-only"}                //
      | concat(args) | to_vector();

  sv::par_for(std::vector{bcArgs, pchArgs, iiArgs}, [&](auto args, auto) {
    auto line = args ^ mk_string(" ");
    if (verbose) AGV_COUT << line << std::endl;
    if (auto code = sv::exec(line, std::cout); code) {
      if (*code != 0) AGV_WARNF("non-zero return for `{}`", line);
    } else AGV_WARNF("popen failed for `{}`: ", line);
  });

  // For Clang, we set an initial language guess determined by the driver, then select the correct
  // one at codebase load time
  std::string language;
  if (auto driver = std::filesystem::path(program).filename(); driver == "clang") language = "c";
  else if (driver == "clang++") language = "cpp";
  else {
    AGV_WARNF("cannot determine language from driver ({}) for command: {}", driver,
              cmd.command ^ mk_string(" "));
    language = fmt::format("unknown ({})", driver);
  }

  sv::ClangEntry result{.kind = "clang",
                        .language = language,
                        .file = std::filesystem::path(cmd.file).filename(),
                        .command = cmd.command ^ mk_string(" "),
                        .preprocessed = readFile(wd / iiName),
                        .pchFile = fmt::format("{}.{}.zstd", prefix, pchName),
                        .bitcodes = collectBitcodeFiles(verbose, prefix, fileStem, wd, dest),
                        .dependencies = sv::readDepFile(wd / dFile, cmd.file),
                        .attributes = {{"version", version}}};

  { // handle TU.pch CPCH file
    auto pchDest = dest / result.pchFile;
    std::error_code zstdError;
    auto pchStream = sv::utils::zstd_ostream(pchDest, zstdError, 6);
    if (zstdError)
      AGV_WARNF("cannot open compressed stream for PHC {}: {}", pchDest, zstdError.message());
    else {
      auto buffer = MemoryBuffer::getFile((wd / pchName).string());
      if (auto bufferError = buffer.getError())
        AGV_WARNF("cannot open PCH {}: {}", (wd / pchName), bufferError.message());
      else {
        auto ptr = std::move(buffer.get());
        (pchStream << ptr->getBuffer()).flush();
      }
    }
  }

  std::ofstream out(dest / fmt::format("{}.{}.sv.json", prefix, fileStem), std::ios::out);
  out << nlohmann::json(result);
  return true;
}
