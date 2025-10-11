![build status](https://github.com/besm6/dubna/actions/workflows/c-cpp.yml/badge.svg)

# Dubna BESM-6 Emulator

A comprehensive emulator for the BESM-6 (Big Electronic Calculating Machine, model 6) computer system running the Dubna monitor system. This project simulates the Dubna monitor system as described in the book
[Программирование на БЭСМ-6 в системе "Дубна"](https://www.google.com/books/edition/%D0%9F%D1%80%D0%BE%D0%B3%D0%BC%D0%BC%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5_%D0%BD%D0%B0_%D0%91/oVMWzgEACAAJ)
by Gennady Maznyi.

## Overview

The Dubna emulator provides a complete simulation of the BESM-6 computer architecture, including:

- **48-bit word architecture** with 32K words of virtual memory
- **Complete instruction set** including arithmetic, logical, and control operations
- **Extracode system** (e50-e77) for advanced operations like trigonometric functions, I/O, and system services
- **Memory management** with page-based organization (1024 words per page)
- **I/O subsystem** supporting magnetic drums, disks, punch cards, and plotters
- **Dubna OS compatibility** with resident programs and system calls
- **Multiple programming languages** support including Fortran, Algol, Pascal, and assembly

## Architecture Details

### Processor Core
- **Program Counter (PC)**: 15-bit address register
- **Accumulator (ACC)**: 48-bit arithmetic register
- **Register of Minor Ranks (RMR)**: 48-bit auxiliary register
- **Modifier Registers (M[0-15])**: 15-bit address modification registers
- **ALU Mode Register (RAU)**: Controls arithmetic/logical modes

### Memory System
- **32K words** of 48-bit memory organized in pages
- **Page size**: 1024 words (1K)
- **Address space**: 0-32767 (octal 0-77777)
- **Memory protection**: None (full access emulation)

### I/O Devices
- **Magnetic Drums**: Up to 32 units (0-27, 70-77 octal)
- **Magnetic Disks**: Up to 32 units (30-67 octal)
- **Punch Cards**: Input/output via card reader/puncher
- **Plotters**: Support for Watanabe, Tektronix, and Calcomp plotters
- **Terminal I/O**: Console input/output operations

## Key Features

### Extracode System
The emulator implements the complete extracode system (e50-e77):

- **e50-e56**: Elementary functions (sin, cos, arctan, arcsin, log, exp)
- **e57**: File system operations and tape mounting
- **e60-e61**: Punch card and plotter control
- **e63-e65**: System services and hardware access
- **e64**: Text output and formatting
- **e67**: Debug services
- **e70**: Disk/drum I/O operations
- **e71**: Punch card read/write
- **e72-e76**: OS-specific operations
- **e74**: Job termination
- **e75**: Memory write with instruction check bits

### Supported File Formats
- **SIMH disk images**: Standard format for magnetic storage
- **Binary overlays**: Executable programs with shebang support
- **COSY format**: Text encoding for Dubna system
- **ISO encoding**: Character encoding for text files
- **GOST encoding**: Russian character encoding standard

### Programming Language Support
- **Fortran**: Full Fortran-Dubna compiler support
- **Algol**: Algol-60 implementation
- **Pascal**: Pascal compiler support
- **Assembly**: BESM-6 assembly language

## Installation

### Prerequisites
- **C++17 compatible compiler** (GCC 7+, Clang 5+, or MSVC 2017+)
- **CMake 3.12+** or **Meson**
- **Git** (for fetching dependencies)

### Build Options

#### Using CMake (Recommended)
```bash
# Standard build
make
make install

# Debug build
make clean
make debug
make

# Run tests
make test

# Run all tests (including CERN library tests)
make test-all
```

#### Using Meson
```bash
meson setup build
cd build
meson compile
meson install
```

### Build Configuration
The project uses strict compilation flags for code quality:
- `-Wall -Werror -Wshadow`: All warnings treated as errors
- `-std=c++17`: Modern C++ standard
- **Cppcheck integration**: Static analysis enabled by default

### Dependencies
- **GoogleTest**: Automatically fetched for unit tests
- **Embedded tape images**: Included in source (monsys.9, librar.12, librar.37, bemsh.739, b.7)

## Testing

The project includes comprehensive test coverage:

### Test Categories
- **Unit Tests**: Core functionality testing (127 tests)
- **Integration Tests**: End-to-end system testing
- **CLI Tests**: Command-line interface validation
- **Architecture Tests**: BESM-6 instruction set verification
- **ALU Tests**: Arithmetic logic unit operations
- **Memory Tests**: Memory access and management
- **Extracode Tests**: System call implementations
- **Encoding Tests**: Character encoding conversions

### Running Tests
```bash
# Run all tests
make test

# Run specific test categories
ctest --test-dir build/tests -R "cli.*"
ctest --test-dir build/tests -R "unit.*"

# Run with verbose output
ctest --test-dir build/tests --verbose
```

### Expected Test Output
```
ctest --test-dir build/tests
...
  1/127 Test  #88: dubna_session.file_write_read ...............   Passed    0.07 sec
  2/127 Test  #85: dubna_session.grafor_tektronix ..............   Passed    0.07 sec
        Start  64: dubna_session.overflow
        Start  49: cli.besmcat_exe
...
127/127 Test  #81: dubna_session.lib1 ..........................   Passed    0.59 sec

100% tests passed, 0 tests failed out of 118

Total Test time (real) =   0.59 sec
```

# Examples

A fair amount of demos are available in the `examples` directory:

```
$ cd examples
$ dubna name.dub
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

## Usage

### Command Line Interface

The Dubna emulator provides a comprehensive command-line interface:

```bash
dubna [options...] filename
```

#### Options
- `-h, --help`: Display available options
- `--help-libs`: Show available libraries
- `--help-commands`: Show available commands
- `--help-resident`: Print resident routines
- `-V, --version`: Print version number and exit
- `-v, --verbose`: Verbose mode
- `-r, --random`: Disable randomization
- `-s, --system`: Enable system load list
- `-l NUM, --limit=NUM`: Stop after NUM instructions (default 100 billion)
- `--keep`: Keep scratch files and raw plotter output
- `--trace=FILE`: Redirect trace to file
- `-d MODE, --debug=MODE`: Select debug mode (default: irm)

#### Debug Modes
- `i`: Trace instructions
- `e`: Trace extracodes
- `p`: Trace printing (extracode e64)
- `f`: Trace fetch
- `r`: Trace registers
- `m`: Trace memory read/write
- `d`: Trace in dispak format, to stderr

### Example Usage

#### Basic Program Execution
```bash
# Run a Dubna job file
dubna examples/name.dub

# Run with verbose output
dubna -v examples/name.dub

# Run with instruction limit
dubna -l 1000000 examples/name.dub
```

#### Debugging and Tracing
```bash
# Enable instruction tracing
dubna -d i examples/name.dub

# Enable multiple debug modes
dubna -d irm examples/name.dub

# Redirect trace to file
dubna --trace=debug.log -d irm examples/name.dub
```

#### System Information
```bash
# Show version
dubna --version

# Show available libraries
dubna --help-libs

# Show resident routines
dubna --help-resident
```

## File System Support

### Disk Images
The emulator supports various disk image formats:

- **SIMH Format**: Standard format for magnetic storage
- **Binary Files**: Raw binary data files
- **Embedded Images**: Built-in tape images (monsys.9, librar.12, etc.)

### File Operations
- **Mount/Unmount**: Dynamic disk mounting
- **Read/Write**: Full I/O operations
- **Scratch Files**: Temporary file creation
- **File Search**: Directory traversal and file lookup

### Configuration
- **BESM6_PATH**: Environment variable for disk image search path
- **Default Paths**: `~/.besm6`, `/usr/local/share/besm6`
- **Tape Images**: Automatic mounting of system tapes

## Development

### Code Structure
- **Core Emulator**: `machine.cpp`, `processor.cpp`, `memory.cpp`
- **I/O Subsystem**: `disk.cpp`, `drum.cpp`, `puncher.cpp`, `plotter.cpp`
- **Extracodes**: `extracode.cpp`, `e50.cpp`, `e57.cpp`, `e64.cpp`
- **Utilities**: `encoding.cpp`, `cosy.cpp`, `trace.cpp`
- **Architecture**: `besm6_arch.cpp`, `assembler.cpp`

### Contributing
1. Fork the repository
2. Create a feature branch
3. Make changes with proper tests
4. Ensure all tests pass
5. Submit a pull request

### Code Quality
- **Static Analysis**: Cppcheck integration
- **Memory Safety**: AddressSanitizer support
- **Testing**: Comprehensive unit and integration tests
- **Documentation**: Inline code documentation

## Known Issues

See [TODO.md](TODO.md) for a comprehensive list of known issues and planned improvements.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- **Gennady Maznyi**: Author of the original Dubna programming book
- **Mikhail Popov**: The keeper of invaluable knowledge about the Dubna operating system
- **Leonid Broukhis**: Original emulator development
- **Serge Vakulenko**: Current maintainer and enhancements
- **BESM-6 Community**: Historical documentation and software preservation
