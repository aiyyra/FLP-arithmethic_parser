# Arithmetic Expression Evaluator

A command-line arithmetic expression evaluator written in Haskell with support for basic operations, exponentiation, and mathematical functions.

## Quick Start

```bash
# Clone the repository
git clone <repository-url>
cd Assignment/solution

# Build with Cabal (recommended)
cabal update
cabal build

# Run the evaluator
cabal run evaluator -- "2+3^2"
# Output: 11.0
```

## Features

- **Basic Arithmetic**: Addition (+), Subtraction (-), Multiplication (\*), Division (/)
- **Exponentiation**: Power operator (^) with right-associativity
- **Mathematical Functions**: sin, abs, sqrt
- **Floating-Point Support**: Handles decimal numbers
- **Error Handling**: Division by zero, square root of negative numbers, invalid syntax
- **Proper Precedence**: Follows standard mathematical order of operations

## Getting Started

### Prerequisites

- **GHC** (Glasgow Haskell Compiler) version 9.6.7 or later
- **Cabal** version 3.0 or later (recommended) or manual GHC compilation
- **Git** (for cloning the repository)

Check if you have the required tools:

```bash
ghc --version    # Should show GHC 9.6.7 or later
cabal --version  # Should show cabal-install 3.0 or later
```

If not installed, install via [GHCup](https://www.haskell.org/ghcup/):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

### Clone the Repository

```bash
git clone <repository-url>
cd Assignment
```

## Building and Running

### Method 1: Using Cabal (Recommended)

Cabal provides automatic dependency management and standardized builds.

**1. Navigate to the solution directory:**

```bash
cd solution
```

**2. Update Cabal package index (first time only):**

```bash
cabal update
```

**3. Build the project:**

```bash
cabal build
```

**4. Run the evaluator:**

```bash
cabal run evaluator -- "expression"
```

**Examples:**

```bash
cabal run evaluator -- "2+3"
cabal run evaluator -- "2^3^2"
cabal run evaluator -- "sqrt(abs(-4))"
```

**5. (Optional) Install system-wide:**

```bash
cabal install
```

After installation, you can run directly:

```bash
evaluator "2+3"
```

### Method 2: Manual GHC Compilation

For quick builds without Cabal:

**1. Navigate to the solution directory:**

```bash
cd solution
```

**2. Compile with GHC:**

```bash
ghc --make -o evaluator Main.hs
```

**3. Run the executable:**

```bash
./evaluator "expression"
```

**Examples:**

```bash
./evaluator "2+3"
./evaluator "2^3^2"
./evaluator "sqrt(abs(-4))"
```

### Cleaning Build Artifacts

**With Cabal:**

```bash
cd solution
cabal clean
```

**With Manual GHC:**

```bash
cd solution
rm -f *.hi *.o evaluator
```

### Troubleshooting

**Issue: "cabal: command not found"**

Solution: Install Cabal via GHCup (see Prerequisites section)

**Issue: "Could not resolve dependencies"**

Solution:

```bash
cabal update
cabal build
```

**Issue: Build errors after code changes**

Solution:

```bash
cabal clean
cabal build
```

**Issue: Module not found errors**

Solution: Make sure you're in the `solution/` directory when building

### Important Notes

- **Always quote expressions**: Use quotes to prevent shell interpretation of special characters
- **Cabal vs Manual**: Cabal is recommended for reproducible builds and easier dependency management
- **Build artifacts**: Cabal stores builds in `dist-newstyle/` (ignored by Git)
- **No external dependencies**: Project uses only Haskell's base library

## Usage Examples

**Basic Arithmetic:**

```bash
./evaluator "2+3"
# Output: 5.0

./evaluator "10-4*2"
# Output: 2.0

./evaluator "(2+3)*4"
# Output: 20.0
```

**Exponentiation:**

```bash
./evaluator "2^3"
# Output: 8.0

./evaluator "2^3^2"
# Output: 512.0  (right-associative: 2^(3^2))

./evaluator "2+3^2"
# Output: 11.0  (exponentiation has higher precedence)
```

**Mathematical Functions:**

```bash
./evaluator "sin(0)"
# Output: 0.0

./evaluator "abs(-5)"
# Output: 5.0

./evaluator "sqrt(16)"
# Output: 4.0

./evaluator "sqrt(abs(-4))"
# Output: 2.0  (nested functions)
```

**Complex Expressions:**

```bash
./evaluator "2*sqrt(16)+abs(-3)"
# Output: 11.0

./evaluator "sin(0)+2^3*4"
# Output: 32.0
```

**Error Handling:**

```bash
./evaluator "5/0"
# Output: Evaluation error: Error: Division by zero

./evaluator "sqrt(-1)"
# Output: Evaluation error: Error: Square root of negative number (-1.0)

./evaluator "2+"
# Output: Parse error: Incomplete expression
```

### Supported Operations

| Operation      | Syntax    | Example   | Result |
| -------------- | --------- | --------- | ------ |
| Addition       | `a + b`   | `2 + 3`   | `5.0`  |
| Subtraction    | `a - b`   | `5 - 2`   | `3.0`  |
| Multiplication | `a * b`   | `3 * 4`   | `12.0` |
| Division       | `a / b`   | `10 / 2`  | `5.0`  |
| Exponentiation | `a ^ b`   | `2 ^ 3`   | `8.0`  |
| Sine           | `sin(a)`  | `sin(0)`  | `0.0`  |
| Absolute Value | `abs(a)`  | `abs(-5)` | `5.0`  |
| Square Root    | `sqrt(a)` | `sqrt(4)` | `2.0`  |
| Parentheses    | `(expr)`  | `(2+3)*4` | `20.0` |
| Unary Minus    | `-a`      | `-5`      | `-5.0` |

### Operator Precedence (Highest to Lowest)

1. Functions: `sin()`, `abs()`, `sqrt()`
2. Parentheses: `()`
3. Exponentiation: `^` (right-associative)
4. Multiplication and Division: `*`, `/` (left-associative)
5. Addition and Subtraction: `+`, `-` (left-associative)

### Notes

- **Trigonometric Functions**: The `sin` function expects input in radians
- **Right-Associativity**: `2^3^2` is evaluated as `2^(3^2) = 512`, not `(2^3)^2 = 64`
- **Floating-Point**: All calculations use double-precision floating-point numbers
- **Whitespace**: Spaces in expressions are ignored (e.g., `2 + 3` is the same as `2+3`)

## Project Structure

```
Assignment/
├── README.md                    # This file - project overview and instructions
├── .gitignore                   # Git ignore rules
│
├── solution/                    # Main application code
│   ├── evaluator.cabal          # Cabal build configuration
│   ├── .gitignore               # Solution-specific ignore rules
│   ├── Main.hs                  # CLI entry point
│   ├── Tokenizer.hs             # Lexical analysis module
│   ├── Parser.hs                # Syntax analysis and AST module
│   ├── Evaluator.hs             # Expression evaluation module
│   ├── Tests.hs                 # Test suite
│   ├── Evaluator_OLD.hs         # Original buggy code (reference)
│   └── docs/                    # Comprehensive documentation
│       ├── BugFixesDetailed.md          # All 12 bugs fixed (363 lines)
│       ├── REFACTORING_SUMMARY.md       # FP principles applied (141 lines)
│       ├── FeatureExtensionDetailed.md  # Part 2 extensions (893 lines)
│       ├── PROJECT_OVERVIEW.md          # High-level overview
│       └── ASSIGNMENT_GUIDE.md          # Assignment requirements
│
├── task/                        # Original assignment materials
│   ├── Assignment 1.md          # Assignment requirements
│   └── Evaluator.hs             # Original flawed code
│
└── agent-log/                   # Development logs and guides
    ├── agent.md                 # Project state tracking
    ├── log.md                   # Detailed change log
    ├── Part2.md                 # Part 2 implementation tracking
    └── cabal.md                 # Cabal learning guide (464 lines)
```

## Development

### Running Tests

```bash
cd solution
ghc Tests.hs
./Tests
```

### Adding New Features

1. Edit the appropriate module (Tokenizer.hs, Parser.hs, or Evaluator.hs)
2. Update the .cabal file if adding new modules
3. Rebuild: `cabal build`
4. Test: `cabal run evaluator -- "test expression"`

### Documentation

Comprehensive documentation is available in `solution/docs/`:

- **BugFixesDetailed.md**: Details of all 12 bugs fixed
- **REFACTORING_SUMMARY.md**: Functional programming principles applied
- **FeatureExtensionDetailed.md**: Implementation of exponentiation and functions
- **Cabal Guide**: See `agent-log/cabal.md` for Cabal learning resources

## Contributing

This is a coursework project. For educational purposes only.

## License

This project is part of WID3001 Functional and Logic Programming coursework at University of Malaya.

## Acknowledgments

- Assignment designed for WID3001 course
- Implements functional programming principles from course lectures
- Uses Haskell's base library only (no external dependencies)
