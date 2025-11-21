# Arithmetic Expression Evaluator

A command-line arithmetic expression evaluator written in Haskell with support for basic operations, exponentiation, and mathematical functions.

## Features

- **Basic Arithmetic**: Addition (+), Subtraction (-), Multiplication (\*), Division (/)
- **Exponentiation**: Power operator (^) with right-associativity
- **Mathematical Functions**: sin, abs, sqrt
- **Floating-Point Support**: Handles decimal numbers
- **Error Handling**: Division by zero, square root of negative numbers, invalid syntax
- **Proper Precedence**: Follows standard mathematical order of operations

## How to Run

### Prerequisites

- GHC (Glasgow Haskell Compiler) version 9.6.7 or later
- No additional libraries required (uses base Haskell only)

### Building the Application

Navigate to the `solution/` directory and compile the program:

```bash
cd solution
ghc --make -o evaluator Main.hs
```

This will create an executable named `evaluator`.

### Running the Application

Run the evaluator with an expression as a command-line argument:

```bash
./evaluator "expression"
```

**Important**: Always enclose your expression in quotes to prevent shell interpretation of special characters.

### Examples

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
solution/
├── Main.hs          # CLI entry point
├── Tokenizer.hs     # Lexical analysis
├── Parser.hs        # Syntax analysis and AST
├── Evaluator.hs     # Expression evaluation
├── Tests.hs         # Test suite
└── docs/            # Documentation
    ├── BugFixesDetailed.md
    ├── REFACTORING_SUMMARY.md
    ├── FeatureExtensionDetailed.md
    ├── PROJECT_OVERVIEW.md
    └── ASSIGNMENT_GUIDE.md
```

## License

This project is part of WID3001 Functional and Logic Programming coursework.
