# Dubna BESM-6 Test Suite

This directory contains the comprehensive test suite for the Dubna BESM-6 emulator. The tests ensure correct emulation behavior, validate system functionality, and maintain compatibility with historical BESM-6 software.

## Test Overview

The test suite includes **42 unit tests** covering all major components of the emulator:

- **Unit Tests**: Core functionality testing (42 tests)
- **Integration Tests**: End-to-end system testing
- **CLI Tests**: Command-line interface validation
- **Architecture Tests**: BESM-6 instruction set verification
- **ALU Tests**: Arithmetic logic unit operations
- **Memory Tests**: Memory access and management
- **Extracode Tests**: System call implementations
- **Encoding Tests**: Character encoding conversions

## Test Structure

### Core Test Files

#### Unit Tests
- **`alu_test.cpp`** - Arithmetic Logic Unit tests
  - Multiplication operations
  - Division operations
  - Arithmetic accuracy validation

- **`arch_test.cpp`** - Architecture-specific tests
  - BESM-6 instruction set validation
  - Register operations
  - Memory addressing modes

- **`cli_test.cpp`** - Command-line interface tests
  - Usage help display
  - Version information
  - Command-line argument parsing
  - Error handling

- **`cpu_test.cpp`** - CPU instruction tests
  - Instruction execution
  - Register operations
  - Control flow instructions
  - Memory access instructions

- **`machine_test.cpp`** - Machine emulation tests
  - Trace operations
  - Memory management
  - I/O operations
  - System state validation

- **`session_test.cpp`** - Session management tests
  - Job execution
  - File operations
  - System initialization
  - Resource management

#### Specialized Tests
- **`cosy_test.cpp`** - COSY encoding tests
  - Character encoding validation
  - Text processing accuracy
  - Encoding/decoding operations

- **`e64_test.cpp`** - E64 extracode tests
  - Text output functionality
  - Formatting operations
  - Print control

- **`elem_func_test.cpp`** - Elementary function tests
  - Trigonometric functions (sin, cos, arctan)
  - Logarithmic functions (log, exp)
  - Mathematical accuracy validation

- **`cernlib_test.cpp`** - CERN library tests (optional)
  - Scientific computing validation
  - Mathematical library functions
  - Performance benchmarks

### Test Infrastructure

#### Test Fixtures
- **`fixture_machine.h`** - Machine test fixture
  - Fresh Machine instance for each test
  - Random number generator setup
  - Test environment initialization

- **`fixture_session.h`** - Session test fixture
  - Session management setup
  - File system initialization
  - Test data preparation

#### Utilities
- **`util.h`** / **`util.cpp`** - Test utilities
  - Helper functions for tests
  - Common test operations
  - Test data generation

### Expected Output Files

The test suite includes **476 expected output files** for validation:

#### Text Output Files (`expect_*.txt`)
- **`expect_algol.txt`** - Algol program output
- **`expect_aspid.txt`** - Aspid program output
- **`expect_assem.txt`** - Assembly program output
- **`expect_bemsh.txt`** - BEMSH assembler output
- **`expect_dtran.txt`** - Data transfer output
- **`expect_edit.txt`** - Editor session output
- **`expect_elem_func.txt`** - Elementary functions output
- **`expect_epsilon.txt`** - Epsilon calculation output
- **`expect_file_*.txt`** - File operation outputs
- **`expect_fortran.txt`** - Fortran program output
- **`expect_ftn.txt`** - FTN program output
- **`expect_grafor_*.txt`** - Graphics library outputs
- **`expect_lib*.txt`** - Library operation outputs
- **`expect_madlen.txt`** - Madlen assembler output
- **`expect_okno.txt`** - OKHO routine output
- **`expect_pascal.txt`** - Pascal program output
- **`expect_pcatalog.txt`** - Punch card catalog output
- **`expect_punch.txt`** - Punch card output
- **`expect_sovcatal.txt`** - Soviet catalog output
- **`expect_startjob.txt`** - Job startup output
- **`expect_stdarray.txt`** - Standard array output
- **`expect_tape*.txt`** - Tape operation outputs
- **`expect_whatis_monsys.txt`** - MONSYS information output

#### Binary Output Files (`expect_*.bin`)
- **`expect_calcomp_output.bin`** - Calcomp plotter binary output
- **`expect_cosy.bin`** - COSY encoded binary output
- **`expect_fubar.bin`** - Fubar program binary output
- **`expect_iso.bin`** - ISO encoded binary output
- **`expect_tektronix_output.bin`** - Tektronix plotter binary output

#### Graphics Output Files (`expect_*.svg`)
- **`expect_calcomp.svg`** - Calcomp plotter SVG output
- **`expect_tektronix.svg`** - Tektronix plotter SVG output
- **`expect_watanabe.svg`** - Watanabe plotter SVG output

### Test Libraries

#### CERN Library Tests (`lib1/`, `lib2/`)
- **`lib1/`** - CERN library 1 test files (377 files)
  - Fortran source files (188 *.f)
  - Test data files (186 *.txt)
  - Dubna job files (1 *.dub)

- **`lib2/`** - CERN library 2 test files (474 files)
  - Fortran source files (237 *.f)
  - Test data files (235 *.txt)
  - Sed script files (1 *.sed)

## Running Tests

### Basic Test Execution
```bash
# Run all tests
make test

# Run all tests including CERN library tests
make test-all

# Run tests with verbose output
ctest --test-dir build/tests --verbose
```

### Specific Test Categories
```bash
# Run CLI tests only
ctest --test-dir build/tests -R "cli.*"

# Run unit tests only
ctest --test-dir build/tests -R "unit.*"

# Run architecture tests
ctest --test-dir build/tests -R "arch.*"

# Run ALU tests
ctest --test-dir build/tests -R "alu.*"

# Run memory tests
ctest --test-dir build/tests -R "memory.*"

# Run extracode tests
ctest --test-dir build/tests -R "e64.*"
```

### Test with Debugging
```bash
# Run tests with debug output
ctest --test-dir build/tests --verbose --output-on-failure

# Run specific test with debugging
./build/tests/unit_tests --gtest_filter="cli.usage" --gtest_verbose
```

## Test Framework

### GoogleTest Integration
- **Version**: GoogleTest v1.15.2
- **Framework**: C++ unit testing framework
- **Features**: Assertions, test fixtures, parameterized tests
- **Output**: XML reports, verbose output, failure details

### Test Configuration
- **Build System**: CMake integration
- **Timeout**: 120 seconds per test
- **Repeat**: Single execution per test
- **Dependencies**: Automatic GoogleTest fetching

### Test Environment
- **Test Directory**: `TEST_DIR` environment variable
- **Build Directory**: `BUILD_DIR` environment variable
- **Libraries**: Links with simulator and gtest_main
- **Includes**: Parent directory included for source access

## Test Categories

### Functional Tests
- **Core Emulation**: Machine state, instruction execution
- **Memory Management**: Memory access, allocation, deallocation
- **I/O Operations**: File operations, device interactions
- **System Calls**: Extracode implementation and behavior

### Compatibility Tests
- **Historical Software**: CERN library compatibility
- **Language Support**: Fortran, Algol, Pascal, Assembly
- **File Formats**: SIMH, COSY, ISO, GOST encodings
- **Graphics Output**: Plotter compatibility and SVG generation

### Performance Tests
- **Execution Speed**: Instruction execution rates
- **Memory Usage**: Resource consumption validation
- **I/O Performance**: File operation efficiency
- **System Response**: Overall system performance

### Regression Tests
- **Output Validation**: Expected vs. actual output comparison
- **Behavior Consistency**: Consistent emulation behavior
- **Error Handling**: Proper error detection and reporting
- **Edge Cases**: Boundary condition handling

## Expected Test Output

### Successful Test Run
```
ctest --test-dir build/tests
...
 1/42 Test  #1: cli.usage ........................   Passed    0.01 sec
      Start  2: cli.version
 2/42 Test  #2: cli.version ......................   Passed    0.01 sec
      Start  3: cli.trace_end_file
...
42/42 Test #42: unit.encode_cosy .................   Passed    0.00 sec

100% tests passed, 0 tests failed out of 42

Total Test time (real) =   0.21 sec
```

### Test Categories Breakdown
- **CLI Tests**: Command-line interface validation
- **Unit Tests**: Core functionality testing
- **Integration Tests**: End-to-end system testing
- **Architecture Tests**: BESM-6 instruction set verification
- **ALU Tests**: Arithmetic operations validation
- **Memory Tests**: Memory management testing
- **Extracode Tests**: System call implementation
- **Encoding Tests**: Character encoding validation

## Test Development

### Adding New Tests
1. **Create test file**: Follow naming convention `*_test.cpp`
2. **Include fixtures**: Use appropriate test fixtures
3. **Add to CMakeLists.txt**: Include in TEST_SOURCES
4. **Create expected output**: Add `expect_*.txt` files
5. **Update documentation**: Document new test functionality

### Test Standards
- **Naming Convention**: `TEST_F(fixture_name, test_name)`
- **Assertions**: Use appropriate GoogleTest assertions
- **Documentation**: Clear test descriptions and comments
- **Expected Output**: Include validation files

### Test Data Management
- **Expected Output**: Store in `expect_*.txt` files
- **Test Input**: Use consistent test data
- **Binary Output**: Store in `expect_*.bin` files
- **Graphics Output**: Store in `expect_*.svg` files

## Troubleshooting

### Common Test Issues
- **Test Failures**: Check expected output files
- **Timeout Issues**: Increase timeout in CMakeLists.txt
- **Missing Dependencies**: Ensure all libraries are built
- **Environment Issues**: Check TEST_DIR and BUILD_DIR

### Debugging Tests
- **Verbose Output**: Use `--verbose` flag
- **Single Test**: Run specific test with `--gtest_filter`
- **Debug Mode**: Compile with debug flags
- **Output Comparison**: Compare actual vs. expected output

### Test Maintenance
- **Update Expected Output**: When behavior changes
- **Add New Tests**: For new functionality
- **Remove Obsolete Tests**: When features are removed
- **Performance Monitoring**: Track test execution times

## Historical Context

### Soviet Computing Testing
- **CERN Library**: European physics computing standards
- **Mathematical Functions**: Scientific computing validation
- **System Compatibility**: Historical software preservation
- **Performance Standards**: Original BESM-6 performance targets

### Test Philosophy
- **Comprehensive Coverage**: All major components tested
- **Historical Accuracy**: Faithful emulation validation
- **Regression Prevention**: Change impact assessment
- **Quality Assurance**: Continuous validation

## Contributing

### Test Contributions
1. **Follow Standards**: Use established test patterns
2. **Add Documentation**: Clear test descriptions
3. **Include Expected Output**: Validation files
4. **Test Thoroughly**: Verify test correctness
5. **Update README**: Document new tests

### Test Review Process
1. **Code Review**: Test implementation review
2. **Output Validation**: Expected output verification
3. **Performance Check**: Test execution time
4. **Integration Test**: Full test suite validation
5. **Documentation Update**: README maintenance
