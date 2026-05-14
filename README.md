# Dubna BESM-6 Emulator

![build status](https://github.com/besm6/dubna/actions/workflows/c-cpp.yml/badge.svg)

Run programs from a 1970s Soviet supercomputer on your laptop — complete with the original Russian-language output.

## What is this?

The **BESM-6** was the flagship Soviet computer of the 1960s–70s, widely used at research institutes across the USSR. At the [Joint Institute for Nuclear Research](https://www.jinr.ru/posts/about/) in the city of **Dubna**, scientists ran their calculations under the **Dubna monitor system** — a home-grown operating system described in the book [Программирование на БЭСМ-6 в системе "Дубна"](https://www.google.com/books/edition/%D0%9F%D1%80%D0%BE%D0%B3%D0%BC%D0%BC%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5_%D0%BD%D0%B0_%D0%91/oVMWzgEACAAJ) by Gennady Maznyi.

This emulator recreates that system faithfully: you feed it original job files and get the original output — Cyrillic banners, Fortran listings, plotter graphics, and all.

## Quickstart

```bash
make
make install
dubna examples/name.dub
```

```text
Read job 'name.dub'
Mount image '/Users/vak/.besm6/9' as disk 30
Redirect drum 21 to disk 30
------------------------------------------------------------


                                         3  000    00.00
 ЙОКСЕЛ      БЭСМ-6/5     ШИФР-12
 МОНИТОРНАЯ СИСТЕМА  ′Д У Б Н А′  -  20/10/88




         ЖЖЖЖЖ ЖЖЖЖ  Ж   Ж Ж   Ж ЖЖЖЖ  ЖЖЖЖ
         Ж   Ж Ж   Ж Ж   Ж ЖЖ ЖЖ Ж     Ж   Ж
         Ж   Ж Ж   Ж Ж   Ж Ж Ж Ж ЖЖЖ   Ж   Ж
         Ж   Ж Ж   Ж Ж  ЖЖ Ж Ж Ж Ж     Ж   Ж
         Ж   Ж ЖЖЖЖ  Ж Ж Ж Ж   Ж Ж     ЖЖЖЖ
         Ж   Ж Ж     ЖЖ  Ж Ж   Ж Ж     Ж
         Ж   Ж Ж     Ж   Ж Ж   Ж ЖЖЖЖЖ Ж


*NАМЕ ПРИМЕР
*ЕND FILЕ
------------------------------------------------------------
   Elapsed time: 0.007 seconds
      Simulated: 209161 instructions
Simulation rate: 30269320 instructions/sec
```

## What can you run?

The `examples/` directory contains over 100 ready-to-run programs:

- **Languages**: Fortran, Algol, Pascal, BESM-6 assembly, the B language
- **Games**: Adventure, Wumpus, Oregon Trail, Hamurabi, and more
- **Graphics**: Mandelbrot set, 3-D surfaces, parametric curves — rendered as SVG via simulated Calcomp, Tektronix, and Watanabe plotters
- **Curiosities**: Quine programs (self-reproducing code in multiple languages), π calculation, character encoding demos

See [examples/README.md](examples/README.md) for the full catalog with descriptions.

## Installation

### Prerequisites

- C++17 compiler (GCC 7+, Clang 5+)
- CMake 3.12+
- Git (used to fetch the test framework at build time)

### Build

```bash
# Standard build
make
make install

# Debug build
make clean
make debug
make
```

The system tapes (Dubna monitor, standard libraries, assembler) are embedded in the binary — no separate download is needed to run examples.

## Usage

```bash
dubna [options] filename.dub
```

A `.dub` file is a job script for the Dubna monitor system — think of it as a shell script that sets up libraries, specifies the programming language, and provides the source code or data for one run.

### Options

| Flag | Description |
| ---- | ----------- |
| `-h`, `--help` | Show help |
| `--help-libs` | List available libraries |
| `--help-commands` | List available commands |
| `--help-resident` | List resident routines |
| `-V`, `--version` | Print version and exit |
| `-v`, `--verbose` | Verbose output |
| `-r`, `--random` | Disable randomization |
| `-s`, `--system` | Enable system load list |
| `-l NUM`, `--limit=NUM` | Stop after NUM instructions (default: 100 billion) |
| `--keep` | Keep scratch files and raw plotter output |
| `--trace=FILE` | Write trace output to file |
| `-d MODE`, `--debug=MODE` | Enable debug tracing (see below) |

### Debug modes

Pass one or more letters to `-d`:

| Letter | Traces |
| ------ | ------ |
| `i` | Instructions |
| `e` | System calls (extracodes) |
| `p` | Print operations |
| `f` | Instruction fetch |
| `r` | Registers |
| `m` | Memory reads and writes |
| `d` | Dispak-format output to stderr |

Examples:

```bash
# Run with verbose output
dubna -v examples/fortran.dub

# Trace instructions and registers to a file
dubna -d ir --trace=debug.log examples/name.dub

# Stop after one million instructions (useful for debugging loops)
dubna -l 1000000 examples/name.dub
```

## Disk images and file search

The emulator looks for disk images in this order:

1. Current directory
2. `~/.besm6/`
3. `/usr/local/share/besm6/`
4. Directories listed in the `BESM6_PATH` environment variable

Disk images use the SIMH format (the same format used by other vintage computer emulators). The core system tapes — Dubna monitor, Fortran/Algol/Pascal compilers, the BEMSH assembler — are built into the binary and mount automatically.

## Testing

```bash
make test          # 127 tests, ~0.6 seconds
make test-all      # includes optional CERN library tests
```

See [tests/README.md](tests/README.md) for details on test categories and how to run individual tests.

## Architecture (for the curious)

The BESM-6 used a 48-bit word, with 32 768 words of memory organized in 1 024-word pages. The CPU had a 15-bit program counter, a 48-bit accumulator, an auxiliary 48-bit register (RMR), and 15 modifier registers used for address arithmetic. System services — file I/O, math functions, job control — were invoked through a special instruction class called *extracodes* (think system calls). I/O devices included magnetic drums and disks, a punch-card reader/puncher, and pen plotters.

The emulator is written in C++17 and structured as: `Session` (public API) → `Machine` (orchestrator) → `Processor` + `Memory` + I/O devices + extracode handlers.

## Known issues

See [TODO.md](TODO.md) for a list of known issues and planned improvements.

## License

MIT — see [LICENSE](LICENSE).

## Acknowledgments

- **Gennady Maznyi** — author of the original Dubna programming book
- **Mikhail Popov** — keeper of invaluable knowledge about the Dubna operating system
- **Leonid Broukhis** — original emulator development
- **Serge Vakulenko** — current maintainer
- **BESM-6 Community** — historical documentation and software preservation
