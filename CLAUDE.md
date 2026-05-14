# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a C++17 emulator for the **BESM-6** Soviet computer running the **Dubna monitor system**. It simulates the full 48-bit BESM-6 architecture and supports execution of Fortran, Algol, Pascal, and assembly language programs.

## Build Commands

```bash
make              # Build (RelWithDebInfo by default)
make debug        # Configure debug build in build/
make test         # Run unit tests via ctest
make test-all     # Run all tests including optional CERN library tests
make clean        # Remove build directory
make install      # Install to /usr/local
```

The build uses CMake 3.12+ with strict flags: `-Wall -Werror -Wshadow -std=c++17`. Cppcheck runs automatically during builds.

## Running Tests

```bash
# All tests
make test

# Specific test category
ctest --test-dir build/tests -R "cli.*"
ctest --test-dir build/tests --verbose --output-on-failure

# Single test by name (after building)
./build/tests/unit_tests --gtest_filter="cli.usage"
```

Tests use GoogleTest (auto-fetched at configure time). Expected output for 476 validation files lives in [tests/output/](tests/output/).

## Code Style

Clang-format config: Google style base, 4-space indent, 100-column limit, WebKit brace style, right-aligned pointers. Run `clang-format` on changed files before committing.

## Architecture

### Data Flow

```
main.cpp (CLI parsing)
    ↓
Session  (public API, pImpl idiom)
    ↓
Machine  (simulation orchestrator)
    ├── Processor   (CPU fetch/decode/execute)
    ├── Memory      (32K × 48-bit words, page-based)
    ├── Extracodes  (system calls e50–e77)
    ├── I/O         (Disk, Drum, Puncher, Plotter)
    └── Trace       (debug tracing by mode flags)
```

### Key Components

**`session.cpp/h`** — External API. Manages job file, input file, run control, and debug modes. Uses pImpl to hide `Machine`.

**`machine.cpp/h`** — Orchestrates the simulation loop: drives the processor, handles disk search paths, file mounting, and progress/instruction limits.

**`processor.cpp/h`** + **`arithmetic.cpp`** — 48-bit CPU with 15-bit PC, 48-bit accumulator (ACC), 48-bit RMR, 16 modifier registers (M[0–15]), and RAU mode register. Fetches/decodes instructions and dispatches to ALU or extracodes.

**`memory.cpp/h`** — 32,768-word address space organized as 32 pages × 1024 words. Accessed through the `Machine` by the processor.

**`extracode.cpp/h`**, **`e50.cpp`**, **`e57.cpp`**, **`e64.cpp`** — System call layer (opcodes e50–e77): elementary math functions (e50–e56), filesystem/tape operations (e57), I/O control (e60–e61, e70–e71), job termination (e74), text output (e64).

**`besm6_arch.cpp/h`** + **`assembler.cpp`** — Full opcode table (0–77 octal), mnemonic lookup, and `besm6_asm()` for inline assembly in tests.

**`encoding.cpp/h`**, **`cosy.cpp/h`** — Character encoding: COSY (Dubna text), ISO 8859-5, GOST 10859.

**I/O units**: `disk.cpp` (SIMH-format images, units 030–067 octal), `drum.cpp` (units 0–027 and 070–077 octal), `puncher.cpp`, `plotter.cpp` (Watanabe/Tektronix/Calcomp).

**Embedded tape images** in [tapes/](tapes/): `monsys.9` (Dubna monitor), `librar.12`, `librar.37`, `bemsh.739` (assembler), `b.7` (B compiler).

### pImpl Pattern

`Session` and `Memory` use the pImpl (pointer-to-implementation) idiom — the public headers expose only the interface; internals are in the `.cpp` files. This keeps `Machine` and `Processor` out of client headers.

### Instruction Set Reference

Full per-instruction documentation (opcodes, operands, ω mode effects, stack behavior, kernel restrictions): [doc/Besm6_Instruction_Set.md](doc/Besm6_Instruction_Set.md).

### Instruction Execution Cycle

1. Processor fetches a 48-bit word.
2. Decodes left-half instruction (opcode bits 0–5, address bits 6–20).
3. Applies address modification via M[n] registers if needed.
4. Executes: normal instruction, or dispatches to extracode handler (e50–e77).
5. Optionally executes right-half instruction.
6. Trace system logs if the relevant trace mode is active (`i`, `e`, `p`, `f`, `r`, `m`, `d`).

## Known Issues (see [TODO.md](TODO.md))

- No bounds checking in `memory.h` store/load — potential unbounded access.
- File paths not validated for `../` traversal sequences.
- Some extracodes throw on error; others silently return — inconsistent.
- `machine.cpp:566` has an incomplete `file_mount` function signature.
