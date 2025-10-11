# Dubna BESM-6 Examples

This directory contains a comprehensive collection of examples demonstrating the capabilities of the Dubna BESM-6 emulator. The examples are organized by category and showcase different programming languages, system features, and applications.

## Quick Start

To run any example:
```bash
cd examples
dubna <example-name>.dub
```

## Example Categories

### 🚀 **Basic Examples**
Simple "Hello World" programs demonstrating basic functionality:

- **`name.dub`** - Minimal job with just name and end file
- **`okno.dub`** - Demonstrates OKHO routine call
- **`edit.dub`** - Interactive editing session example
- **`copy.dub`** - File copying demonstration
- **`move.dub`** - File moving demonstration

### 💻 **Programming Languages**

#### Assembly Languages
- **`assem.dub`** - Traditional Madlen assembler "Hello World"
- **`madlen.dub`** - New Madlen assembler (requires additional tape)
- **`bemsh.dub`** - BEMSH assembler "Hello World" (requires additional tape)

#### High-Level Languages
- **`fortran.dub`** - Fortran "Hello World"
- **`algol.dub`** - Algol-60 example
- **`pascal.dub`** - Pascal example
- **`simula.dub`** - Simula example
- **`refal.dub`** - Refal example

#### Specialized Languages
- **`ftn.dub`** - Fortran variant
- **`exfor.dub`** - Extended Fortran
- **`setftn.dub`** - Set-oriented Fortran
- **`dipol.dub`** - DIPOL language example

### 🎮 **Games**
Classic computer games implemented in various languages:

- **`games/adventure1.dub`** - Text adventure game in Pascal
- **`games/adventure2/`** - Advanced adventure game (multiple files)
- **`games/adventure3/`** - Complex adventure game with database
- **`games/delmar.dub`** - Delmar game
- **`games/guess.dub`** - Number guessing game
- **`games/hamurabi.dub`** - Ancient city management game
- **`games/oregon.dub`** - Oregon Trail-style game
- **`games/quinio.dub`** - Quinio game
- **`games/wumpus.dub`** - Hunt the Wumpus game

### 📊 **Graphics and Plotting**
Examples using the Grafor graphics library:

- **`grafor/`** - Complete graphics examples directory
  - **`calcomp.dub`** - Calcomp plotter example
  - **`tektronix.dub`** - Tektronix plotter example
  - **`watanabe.dub`** - Watanabe plotter example
  - **`cardioide.dub`** - Cardioid curve plotting
  - **`mandelbrot.dub`** - Mandelbrot set visualization
  - **`sphere.dub`** - 3D sphere rendering
  - **`surface.dub`** - Surface plotting
  - **`roses.dub`** - Rose curve patterns
  - **`flower.dub`** - Flower pattern generation

### 🔧 **System Operations**

#### File System
- **`file.dub`** - File operations demonstration
- **`libpunch.dub`** - Library punch operations
- **`wrcard.dub`** - Write card operations
- **`pcatalog.dub`** - Punch card catalog
- **`ocatalog.dub`** - Object catalog

#### Memory Management
- **`freemem.dub`** - Free memory demonstration
- **`newlib.dub`** - New library operations

#### System Utilities
- **`dtran.dub`** - Data transfer operations
- **`tidy-relkin.dub`** - Relkin tidying
- **`tidy-si.dub`** - SI tidying
- **`topc.dub`** - Top card operations
- **`tracy.dub`** - Tracy operations
- **`traxy.dub`** - Traxy operations

### 📚 **Libraries**
Library usage examples:

- **`libraries/`** - Library examples directory
  - **`lib1.dub`** - Library 1 example
  - **`lib2.dub`** - Library 2 example
  - **`lib3.dub`** - Library 3 example
  - **`lib26.dub`** - Library 26 example
  - **`library1.dub`** - Library 1 operations
  - **`library2.dub`** - Library 2 operations
  - **`library3.dub`** - Library 3 operations
  - **`library5.dub`** - Library 5 operations
  - **`library12.dub`** - Library 12 operations
  - **`library21.dub`** - Library 21 operations
  - **`library22.dub`** - Library 22 operations
  - **`library23.dub`** - Library 23 operations

### 🎯 **Specialized Examples**

#### Mathematical
- **`pi.dub`** - Pi calculation
- **`spigot6.dub`** - Spigot algorithm for pi
- **`man_or_boy.dub`** - Man or boy test
- **`b/`** - B compiler examples
  - **`fibonacci.dub`** - Fibonacci sequence
  - **`fizzbuzz.dub`** - FizzBuzz problem
  - **`isprime.dub`** - Prime number checking
  - **`mandelbrot.dub`** - Mandelbrot set
  - **`hello.dub`** - Hello World in B
  - **`doors.dub`** - 100 doors problem
  - **`e-2.dub`** - Mathematical constant e

#### Encoding and Text Processing
- **`gost-encoding.dub`** - GOST character encoding
- **`iso-encoding.dub`** - ISO character encoding
- **`grayscale.dub`** - Grayscale processing

#### Advanced Features
- **`introspective.dub`** - Introspective programming
- **`intrsp.dub`** - Introspection example
- **`juggle.dub`** - Juggling algorithm
- **`flodia.dub`** - Flodia operations
- **`flower.dub`** - Flower pattern
- **`bozp-ybyb.dub`** - Bozp-ybyb operations
- **`ch54.dub`** - CH54 operations
- **`aspid.dub`** - Aspid operations
- **`dos.dub`** - DOS operations
- **`sovcatal.dub`** - Soviet catalog
- **`ptime.dub`** - Processing time

### 🔗 **Overlay Programs**
Static binary programs that can run without the monitoring system:

- **`overlay/`** - Overlay examples directory
  - **`hello-algol.dub`** - Algol-ГДP (7.01.82)
  - **`hello-assem.dub`** - Assembler Madlen (1.10.72)
  - **`hello-bemsh.dub`** - Assembler БЭМШ (06/78)
  - **`hello-forex.dub`** - Forex (11.09.85)
  - **`hello-fortran.dub`** - Fortran Dubna (16.07.73)
  - **`hello-ftn.dub`** - Fortran-ГДP (09.07.81)
  - **`hello-madlen.dub`** - Assembler Madlen-3.5 (23/06/79)
  - **`hello-pascal.dub`** - Pascal (24.12.79)

### 🎭 **Quine Programs**
Self-reproducing programs in different languages:

- **`quine/`** - Quine examples directory
  - **`algol.dub`** - Algol quine
  - **`forex.dub`** - Forex quine
  - **`fortran.dub`** - Fortran quine
  - **`ftn.dub`** - FTN quine
  - **`pascal.dub`** - Pascal quine

### 📼 **Tape Operations**
Examples demonstrating tape and disk operations:

- **`tapes/`** - Tape examples directory
  - **`monsys171.dub`** - MONSYS 171 operations
  - **`monsys315.dub`** - MONSYS 315 operations
  - **`monsys316.dub`** - MONSYS 316 operations
  - **`monsys317.dub`** - MONSYS 317 operations
  - **`monsys377.dub`** - MONSYS 377 operations
  - **`monsys434.dub`** - MONSYS 434 operations
  - **`monsys437.dub`** - MONSYS 437 operations
  - **`tape9.dub`** - Tape 9 operations
  - **`tape12.dub`** - Tape 12 operations
  - **`tape37.dub`** - Tape 37 operations

### 🧪 **Test Examples**
Examples for testing specific functionality:

- **`tests/`** - Test examples directory
  - **`bad_e64.dub`** - Bad E64 extracode test
  - **`elem_func.dub`** - Elementary functions test
  - **`overflow.dub`** - Overflow test

## Running Examples

### Basic Usage
```bash
# Run a simple example
dubna name.dub

# Run with verbose output
dubna -v fortran.dub

# Run with debug tracing
dubna -d i assem.dub
```

### Graphics Examples
For graphics examples, you need to specify a plotter:
```bash
# Run with Watanabe plotter
dubna grafor/watanabe.dub

# Run with Tektronix plotter  
dubna grafor/tektronix.dub

# Run with Calcomp plotter
dubna grafor/calcomp.dub
```

### Overlay Programs
Overlay programs can be compiled to binary format:
```bash
# Compile to binary
dubna overlay/hello-pascal.dub

# Run the compiled binary
dubna hello.bin
```

## Example Output

Most examples will produce output similar to:
```
Read job 'fortran.dub'
Mount image '/Users/vak/.besm6/9' as disk 30
Redirect drum 21 to disk 30
------------------------------------------------------------

                                             3  000    00.00
 ЙОКСЕЛ      БЭСМ-6/5     ШИФР-12
 МОНИТОРНАЯ СИСТЕМА  ′Д У Б Н А′  -  20/10/88

Hello, World!

------------------------------------------------------------
   Elapsed time: 0.007 seconds
      Simulated: 209161 instructions
Simulation rate: 30269320 instructions/sec
```

## Graphics Output

Graphics examples automatically generate SVG files that can be viewed in any web browser:
- **Watanabe plots**: `watanabe_output.svg`
- **Tektronix plots**: `tektronix_output.svg`  
- **Calcomp plots**: `calcomp_output.svg`

## Troubleshooting

### Common Issues

1. **Missing tapes**: Some examples require additional tapes
   - `madlen.dub` and `bemsh.dub` ask for additional tapes
   - Ensure required tapes are available in `~/.besm6/` or `/usr/local/share/besm6/`

2. **Graphics not working**: Ensure plotter is specified
   - Add `*call plotter:wx4675,direct` to enable Watanabe plotter
   - Add `*call plotter:tektronix,direct` to enable Tektronix plotter
   - Add `*call plotter:calcomp,direct` to enable Calcomp plotter

3. **File not found**: Check file paths and permissions
   - Ensure examples directory is accessible
   - Check file permissions

### Getting Help

- Check the main [README.md](../README.md) for general usage
- Review [TODO.md](../TODO.md) for known issues
- Use `dubna --help` for command-line options
- Use `dubna --help-commands` for available commands

## Contributing Examples

To add new examples:

1. Create a new `.dub` file in the appropriate category
2. Follow the naming convention (lowercase with hyphens)
3. Add appropriate comments and documentation
4. Test the example thoroughly
5. Update this README.md with the new example

## Historical Context

These examples demonstrate the rich programming environment available on the BESM-6 computer system at the Joint Institute for Nuclear Research in Dubna, Russia. They showcase:

- **Multiple programming languages** supported on a single system
- **Advanced graphics capabilities** with multiple plotter types
- **Sophisticated file and tape management**
- **Interactive games and applications**
- **Mathematical and scientific computing**

The examples preserve the programming techniques and styles used by Soviet computer scientists and programmers in the 1970s and 1980s.