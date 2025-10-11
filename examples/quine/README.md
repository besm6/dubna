# Quine Programs

This directory contains quine programs - self-reproducing programs that output their own source code when executed. These programs demonstrate advanced programming techniques and showcase the capabilities of different languages on the BESM-6 system.

## Available Quines

### Language-Specific Quines
- **`algol.dub`** - Algol-60 quine
  - Self-reproducing Algol program
  - Demonstrates Algol string handling
  - Shows Algol output capabilities

- **`forex.dub`** - Forex quine
  - Self-reproducing Forex program
  - Demonstrates Forex language features
  - Shows Forex output mechanisms

- **`fortran.dub`** - Fortran quine
  - Self-reproducing Fortran program
  - Demonstrates Fortran string operations
  - Shows Fortran formatting capabilities

- **`ftn.dub`** - FTN quine
  - Self-reproducing FTN program
  - Demonstrates FTN language features
  - Shows FTN output handling

- **`pascal.dub`** - Pascal quine
  - Self-reproducing Pascal program
  - Demonstrates Pascal string handling
  - Shows Pascal output capabilities

## Running Quines

### Basic Execution
```bash
# Run Algol quine
dubna algol.dub

# Run Forex quine
dubna forex.dub

# Run Fortran quine
dubna fortran.dub

# Run FTN quine
dubna ftn.dub

# Run Pascal quine
dubna pascal.dub
```

### Expected Output
Each quine should output its own source code:
```
*name quine_example
*language
        [source code of the program]
*execute
*end file
```
