# SilverVale

A modern and fully integrated codebase analysis tool that covers the entire spectrum: syntax,
semantics, and low-level IR.

This work has been accepted at P3HPC'24: *A Metric for HPC Programming Model Productivity*.
The paper is still under embargo,
a draft copy may be requested at https://research-information.bris.ac.uk/en/publications/a-metric-for-hpc-programming-model-productivity.

## Supported Compilers Executables

- `clang`/`clang++`
- `gcc`/`g++`/`gfortran`
- `icpx`

> [!IMPORTANT]  
> SilverVale only works if you are able to fully compile your codebase using one of the supported
> compilers listed above.

## Building

```shell
# in project root
> cmake -Bbuild -S.
> cmake --build build  --target driver -j $(nproc)
> build/sv --help 

# check main tool executable
USAGE: build/sv <command> <command options>...

OPTIONS:
  --help       - Display this help; append this after <command> for command specific help

Commands:
  delta        - Compare two or more SV databases
  dump         - Dump entries in an SV database
  index        - Build SV database from compile_commands.json
  script       - Execute Lua scripts with SV objects in scope
  --           - Compiler wrapper mode for building SV database

# check if compiler plugins are built correctly
> ldd build/libuproot-clang-llvm.so 
        linux-vdso.so.1 (0x00007fb3cc58d000)
        libm.so.6 => /lib64/libm.so.6 (0x00007fb3cc11c000)
        libc.so.6 => /lib64/libc.so.6 (0x00007fb3cbf2b000)
        /lib64/ld-linux-x86-64.so.2 (0x00007fb3cc58f000)
 > ldd build/libuproot-gcc.so 
        linux-vdso.so.1 (0x00007fd520650000)
        libm.so.6 => /lib64/libm.so.6 (0x00007fd520544000)
        libc.so.6 => /lib64/libc.so.6 (0x00007fd52000f000)
        /lib64/ld-linux-x86-64.so.2 (0x00007fd520652000)
```

## Fully worked example (CloverLeaf OpenMP v.s Serial)

```shell
# Download and compile the OpenMP variant of CloverLeaf first
> git clone https://github.com/UoB-HPC/CloverLeaf.git 
> coverage="-fprofile-instr-generate -fcoverage-mapping -Wno-everything" # optional: use coverage options
> CXXFLAGS="$coverage" LDFLAGS="$coverage" cmake -Bomp_clover -SCloverLeaf -DMODEL=omp \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DCMAKE_VERBOSE_MAKEFILE=ON \
  -DCMAKE_C_COMPILER="clang" \
  -DCMAKE_CXX_COMPILER="clang++"
> cmake --build omp_clover -j $(nproc) # compile
...

# Proceed to build the serial variant:
> CXXFLAGS="$coverage" LDFLAGS="$coverage" cmake -Bserial_clover -SCloverLeaf -DMODEL=serial \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DCMAKE_VERBOSE_MAKEFILE=ON \
  -DCMAKE_C_COMPILER="clang" \
  -DCMAKE_CXX_COMPILER="clang++"
> cmake --build serial_clover -j $(nproc) # compile

# Optional: run cloverleaf to generate coverage data 
> omp_clover/omp-cloverleaf --file CloverLeaf/InputDecks/clover_bm2_short.in
> serial_clover/serial-cloverleaf --file CloverLeaf/InputDecks/clover_bm2_short.in

# Build OpenMP and serial database, --cov-bin is optional and only works if the binary was executed 
> build/sv index -j $(nproc) -v \
  --build omp_clover --out "db/builds.omp" --cov-bin omp_clover/omp-cloverleaf  

> build/sv index -j $(nproc) -v \
  --build serial_clover --out "db/builds.serial" --cov-bin serial_clover/serial-cloverleaf
  

# Pick our metrics, run `sv delta --help` for a list of documented values and modifiers
> kinds="srclen,srclen+cpp,sloc,sloc+cpp,lloc,lloc+cpp,src,src+cpp,tstree,stree"
> build/sv delta -j $(nproc) --kinds "$kinds" --prefix cloverleaf \
        --merges "*update_halo*.cpp:update_halo.cpp" \
        --merges "*update_tile_halo_kernel*.cpp:update_tile_halo_kernel.cpp" \
        'db/builds.serial:*CloverLeaf/*' 'db/builds.omp:*CloverLeaf/*' --maxGroups 1
...        
2024-10-01 21:14:00.743] [info] Created 680 tasks
[2024-10-01 21:14:00.751] [info] Executing group <stree> of 136 tasks (maxThreads=16, memRatio=0.95 memInterval=100ms)
[2024-10-01 21:14:50.487] [info] Completed 136 tasks in 49s
[2024-10-01 21:14:50.487] [info] Executing group <other> of 544 tasks (maxThreads=32, memRatio=0.95 memInterval=100ms)
[2024-10-01 21:14:50.505] [info] Completed 544 tasks in 0s
[2024-10-01 21:14:50.803] [info] Group 1 completed in 52s
[2024-10-01 21:14:50.803] [info] All tasks completed in 110s
[2024-10-01 21:14:50.804] [info] Wrote cloverleaf.serial.model_max.csv
[2024-10-01 21:14:50.804] [info] Wrote cloverleaf.serial.model_max.total.csv
[2024-10-01 21:14:50.805] [info] Wrote cloverleaf.serial.model_diffs.csv
[2024-10-01 21:14:50.805] [info] Wrote cloverleaf.serial.model_diffs.total.csv        

# Inspect results: serial is the base model so relative divergence metrics (Tsem, Tsrc, etc) 
#   for serial v.s serial will be zero 0
> cat cloverleaf.serial.model_diffs.total.csv  
kind,name,serial,omp
srcLenA+Raw,total,181943,187738
srcLenA+CPP,total,177596,183391
srcR+Raw,total,0,5997
srcR+CPP,total,0,5997
slocA+Raw,total,15417,15591
slocA+CPP,total,13784,13958
llocA+Raw,total,15478,15652
llocA+CPP,total,15253,15427
tstreeR+Raw,total,0,522
streeR+Raw,total,0,10024

# Inspect results: max divergence for absolute measures have no defined value so 0 is reported 
> cat cloverleaf.serial.model_max.total.csv  
kind,name,serial,omp
srcLenA+Raw,total,0,0
srcLenA+CPP,total,0,0
srcR+Raw,total,538226,544021
srcR+CPP,total,500655,506450
slocA+Raw,total,0,0
slocA+CPP,total,0,0
llocA+Raw,total,0,0
llocA+CPP,total,0,0
tstreeR+Raw,total,124536,125058
streeR+Raw,total,59579,49908
```

One may now inspect the database using the Lua scripting feature:

```shell
# First view the Teal (https://github.com/teal-language/tl) types as API documentation or 
#   alternatively, save this definition for IDE support 
> build/sv script --defs 1 
global record Dependency
  modified: (function(Dependency): integer)
  content: (function(Dependency): string)
end
global record Instance
  function: (function(Instance): string)
  lineStart: (function(Instance): integer)
  lineEnd: (function(Instance): integer)
  colStart: (function(Instance): integer)
  colEnd: (function(Instance): integer)
  count: (function(Instance): integer)
end
global record PerFileCoverage
  instances: (function(PerFileCoverage): {string:{Instance}})
end
global record Diff
  apted: (function(Tree, Tree): number)
  diff: (function(string, string): number)
end
... (truncated)

# Create a script to dump the Tsem of calc_dt.cpp
> cat script.lua
print(arg[1])
local db = Codebase.loadDB(arg[1])
local n = 0
for _ in pairs(db:entries()) do n = n + 1 end
print(n .. " entries")
local cb = Codebase.load(db, true, {}, function(s) return not (string.find(s, "calc_dt") == nil) end)
for _, x in ipairs(cb:units()) do
  print("Tree for " .. x:name() .. ":")
  print(x:sTree(1):prettyPrint())
end

# Execute the script 
> build/sv script script.lua db/builds.omp 
db/builds.omp
[2024-10-01 21:38:03.812] [info] Loaded DB db/builds.omp with 34 entries and 0 coverage entries
34 entries
Tree for calc_dt.cpp:
root (:0:0)
├─ toplevel (:0:0)
│  ╰─ Function:  (/home/tom/SilverVale/CloverLeaf/src/omp/calc_dt.cpp:30:6)
│     ├─ ParmVar:  (/home/tom/SilverVale/CloverLeaf/src/omp/calc_dt.cpp:30:25)
│     ├─ ParmVar:  (/home/tom/SilverVale/CloverLeaf/src/omp/calc_dt.cpp:30:36)
│     ├─ ParmVar:  (/home/tom/SilverVale/CloverLeaf/src/omp/calc_dt.cpp:30:47)
...(truncated)

# Scripting works with normal shell scripting use cases, here we look for Clang's OpenMP tokens:    
> build/sv script script.lua db/builds.omp | grep -C 3 OMP                                                                                                                                                                                                 INT ✘  tom@soraws-uk  21:39:25 
│     │  ╰─ FloatingLiteral: 1.1000000000000001 (/home/tom/SilverVale/CloverLeaf/src/omp/calc_dt.cpp:40:23)
│     ├─ Var:  (/home/tom/SilverVale/CloverLeaf/src/omp/calc_dt.cpp:48:10)
│     │  ╰─ DeclRefExpr:  (/home/tom/SilverVale/CloverLeaf/src/omp/calc_dt.cpp:48:24)
│     ├─ OMPParallelForSimdDirective (/home/tom/SilverVale/CloverLeaf/src/omp/calc_dt.cpp:49:1)
│     │  ╰─ CapturedStmt (/home/tom/SilverVale/CloverLeaf/src/omp/calc_dt.cpp:50:3)
│     │     ├─ DeclRefExpr:  (/home/tom/SilverVale/CloverLeaf/src/omp/calc_dt.cpp:50:17)
│     │     ├─ DeclRefExpr:  (/home/tom/SilverVale/CloverLeaf/src/omp/calc_dt.cpp:50:34)

# We can inspect the associated file (calc_dt.cpp:49:1), for example:
> sed -n '47,51p' /home/tom/SilverVale/CloverLeaf/src/omp/calc_dt.cpp  #+1 line before/after 49 for context 
  double dt_min_val0 = dt_min_val;
#pragma omp parallel for simd collapse(2) reduction(min : dt_min_val0)
  for (int j = (y_min + 1); j < (y_max + 2); j++) {
```



