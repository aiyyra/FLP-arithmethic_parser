# Feature Extensions Documentation - Part 2 Implementation

This document details the implementation of the two feature extensions chosen for Part 2 of the assignment.

---

## Extension 1: Exponentiation (^) Operator ✅ COMPLETED

### Overview

Added support for the exponentiation operator (^) with **right-associativity**, as required by mathematical convention. This extension required changes across all three core modules: Tokenizer, Parser, and Evaluator.

### Key Requirements

1. **Operator Support**: Recognize and tokenize the `^` character
2. **Right-Associativity**: `2^3^2` must evaluate as `2^(3^2) = 512`, not `(2^3)^2 = 64`
3. **Correct Precedence**: Exponentiation has higher precedence than multiplication/division
4. **Error Handling**: Handle edge cases like NaN and overflow

---

## Change 1: Tokenizer - Add '^' Operator

### Before:

```haskell
-- Tokenizer.hs (line 16)
tokenize' (c:cs) pos
  | isSpace c = tokenize' cs (pos + 1)
  | c `elem` "+-*/()" = ([c] :) <$> tokenize' cs (pos + 1)
  -- ^ does not include '^'
```

**Problem:** The tokenizer doesn't recognize `^` as a valid operator, so expressions like "2^3" would fail with "Invalid character '^'" error.

### After:

```haskell
-- Tokenizer.hs (line 16-17)
tokenize' (c : cs) pos
  | isSpace c = tokenize' cs (pos + 1)
  | c `elem` "+-*/()^" = ([c] :) <$> tokenize' cs (pos + 1)
  -- ^ now includes '^'
```

**Fix:** Added `^` to the operator character list. The tokenizer now correctly recognizes exponentiation.

**Test:**

```haskell
tokenize "2^3"     -- Result: Right ["2", "^", "3"]
tokenize "2^3^2"   -- Result: Right ["2", "^", "3", "^", "2"]
```

---

## Change 2: Parser - Add Exp Constructor to AST

### Before:

```haskell
-- Parser.hs (lines 5-11)
data Expr = Num Double
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
  deriving (Show, Eq)
```

**Problem:** The AST has no way to represent exponentiation operations.

### After:

```haskell
-- Parser.hs (lines 5-14)
-- EXTENSION: Added Exp constructor for exponentiation
data Expr
  = Num Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Exp Expr Expr  -- NEW: Exponentiation
  deriving (Show, Eq)
```

**Fix:** Added `Exp Expr Expr` constructor to represent exponentiation in the abstract syntax tree.

**Example AST:**

- Expression: `2^3`
- AST: `Exp (Num 2.0) (Num 3.0)`
- Expression: `2^3^2`
- AST: `Exp (Num 2.0) (Exp (Num 3.0) (Num 2.0))` (right-associative!)

---

## Change 3: Parser - Implement Right-Associative Parsing

### Understanding Associativity

**Left-Associative (existing operators: +, -, \*, /):**

```
2 + 3 + 4  =  (2 + 3) + 4  =  5 + 4  =  9
```

**Right-Associative (new operator: ^):**

```
2 ^ 3 ^ 2  =  2 ^ (3 ^ 2)  =  2 ^ 9  =  512
```

### Parsing Hierarchy (Precedence)

```
parseExpr
  └─> parseAddSub      (lowest precedence: +, -)
       └─> parseMulDiv  (medium precedence: *, /)
            └─> parseExp (highest precedence: ^) [NEW]
                 └─> parseFactor (atoms: numbers, parentheses, unary minus)
```

### Before:

```haskell
-- Parser.hs - parseMulDiv called parseFactor directly
parseMulDiv :: [Token] -> (Maybe Expr, [Token])
parseMulDiv ts =
  case parseFactor ts of  -- Direct call to parseFactor
    (Nothing, rest) -> (Nothing, rest)
    (Just left, rest) -> parseMulDiv' left rest
  where
    parseMulDiv' left ("*" : rest) =
      case parseFactor rest of  -- Direct call to parseFactor
        (Just right, rest') -> parseMulDiv' (Mul left right) rest'
        ...
```

**Problem:** No intermediate level for exponentiation. Multiplication and division have the same precedence as factors.

### After:

```haskell
-- Parser.hs - Updated parseMulDiv to call parseExp
parseMulDiv :: [Token] -> (Maybe Expr, [Token])
parseMulDiv ts =
  case parseExp ts of  -- Now calls parseExp instead
    (Nothing, rest) -> (Nothing, rest)
    (Just left, rest) -> parseMulDiv' left rest
  where
    parseMulDiv' left ("*" : rest) =
      case parseExp rest of  -- Now calls parseExp instead
        (Just right, rest') -> parseMulDiv' (Mul left right) rest'
        (Nothing, _) -> (Nothing, rest)
    parseMulDiv' left ("/" : rest) =
      case parseExp rest of  -- Now calls parseExp instead
        (Just right, rest') -> parseMulDiv' (Div left right) rest'
        (Nothing, _) -> (Nothing, rest)
    parseMulDiv' left rest = (Just left, rest)

-- NEW FUNCTION: parseExp with right-associativity
parseExp :: [Token] -> (Maybe Expr, [Token])
parseExp ts =
  case parseFactor ts of
    (Nothing, rest) -> (Nothing, rest)
    (Just base, "^" : rest) ->
      -- RIGHT-ASSOCIATIVITY: recursively parse entire right side
      case parseExp rest of  -- Recursive call to parseExp (not parseFactor!)
        (Just exponent, rest') -> (Just (Exp base exponent), rest')
        (Nothing, _) -> (Nothing, rest)
    (Just base, rest) -> (Just base, rest)
```

**Fix:**

1. Created new `parseExp` function between `parseMulDiv` and `parseFactor`
2. Implemented right-associativity by recursively calling `parseExp` for the exponent
3. Updated `parseMulDiv` to call `parseExp` instead of `parseFactor`

**Why Right-Associativity Works:**

For `2^3^2`:

1. `parseExp` parses `2` as base
2. Sees `^`, so recursively calls `parseExp` on `3^2`
3. Inner `parseExp` parses `3` as base
4. Sees `^`, recursively calls `parseExp` on `2`
5. Returns `Exp (Num 3) (Num 2)` representing `3^2`
6. Outer call returns `Exp (Num 2) (Exp (Num 3) (Num 2))` representing `2^(3^2)`

---

## Change 4: Evaluator - Implement Exponentiation with Error Handling

### Before:

```haskell
-- Evaluator.hs - No case for Exp
eval :: Expr -> Either String Double
eval (Num n) = Right n
eval (Add a b) = (+) <$> eval a <*> eval b
eval (Sub a b) = (-) <$> eval a <*> eval b
eval (Mul a b) = (*) <$> eval a <*> eval b
eval (Div a b) = do
  dividend <- eval a
  divisor <- eval b
  if divisor == 0
    then Left "Error: Division by zero"
    else Right (dividend / divisor)
-- No case for (Exp a b)!
```

**Problem:** The evaluator cannot handle `Exp` expressions. Attempting to evaluate would cause a pattern match failure.

### After:

```haskell
-- Evaluator.hs - Added Exp case with error handling
eval :: Expr -> Either String Double
eval (Num n) = Right n
eval (Add a b) = (+) <$> eval a <*> eval b
eval (Sub a b) = (-) <$> eval a <*> eval b
eval (Mul a b) = (*) <$> eval a <*> eval b
eval (Div a b) = do
  dividend <- eval a
  divisor <- eval b
  if divisor == 0
    then Left "Error: Division by zero"
    else Right (dividend / divisor)
eval (Exp a b) = do
  base <- eval a
  exponent <- eval b
  let result = base ** exponent
  -- Check for NaN (e.g., negative base with fractional exponent)
  if isNaN result
    then Left $ "Error: Invalid exponentiation (" ++ show base ++ " ^ "
                ++ show exponent ++ " results in NaN)"
    -- Check for Infinity (overflow)
    else
      if isInfinite result && result > 0
        then Left $ "Error: Exponentiation overflow (" ++ show base ++ " ^ "
                    ++ show exponent ++ " is too large)"
        else Right result
```

**Fix:**

1. Added pattern match for `Exp a b`
2. Used Haskell's `(**)` operator for floating-point exponentiation
3. Added error handling for edge cases:
   - **NaN detection**: Catches cases like `(-2)^0.5` which produce NaN
   - **Overflow detection**: Catches cases where result is too large (Infinity)

**Edge Cases Handled:**

| Expression | Result     | Explanation                                         |
| ---------- | ---------- | --------------------------------------------------- |
| `0^0`      | `1.0`      | Haskell's default behavior (mathematically debated) |
| `2^0.5`    | `1.414...` | Fractional exponents work (square root)             |
| `(-2)^2`   | `4.0`      | Integer exponents with negative base work           |
| `(-2)^0.5` | Error      | Fractional exponent with negative base → NaN        |
| `10^1000`  | Error      | Overflow to Infinity                                |

---

## Precedence Verification

The implementation correctly handles operator precedence:

### Test Cases:

```bash
$ ./evaluator "2+3^2"
11.0                    # = 2 + 9 (not 25)

$ ./evaluator "2*3^2"
18.0                    # = 2 * 9 (not 36)

$ ./evaluator "10-2^3"
2.0                     # = 10 - 8 (not 64)

$ ./evaluator "2^3*4"
32.0                    # = 8 * 4 (not 4096)

$ ./evaluator "(2+3)^2"
25.0                    # Parentheses override precedence
```

**Precedence Order (highest to lowest):**

1. Parentheses `()`
2. Exponentiation `^` (right-associative)
3. Multiplication `*`, Division `/` (left-associative)
4. Addition `+`, Subtraction `-` (left-associative)

---

## Right-Associativity Verification

The critical requirement for exponentiation is right-associativity:

### Test Cases:

```bash
$ ./evaluator "2^3^2"
512.0                   # = 2^(3^2) = 2^9 = 512 ✓
                        # NOT (2^3)^2 = 8^2 = 64 ✗

$ ./evaluator "2^2^3"
256.0                   # = 2^(2^3) = 2^8 = 256 ✓
                        # NOT (2^2)^3 = 4^3 = 64 ✗

$ ./evaluator "2^3^2^1"
512.0                   # = 2^(3^(2^1)) = 2^(3^2) = 2^9 = 512 ✓
```

**Comparison with Left-Associativity (incorrect):**

| Expression | Right-Associative (Correct) | Left-Associative (Wrong) |
| ---------- | --------------------------- | ------------------------ |
| `2^3^2`    | `2^(3^2) = 2^9 = 512`       | `(2^3)^2 = 8^2 = 64`     |
| `2^2^3`    | `2^(2^3) = 2^8 = 256`       | `(2^2)^3 = 4^3 = 64`     |
| `3^3^2`    | `3^(3^2) = 3^9 = 19683`     | `(3^3)^2 = 27^2 = 729`   |

---

## Comprehensive Test Results

### Basic Exponentiation

```bash
$ ./evaluator "2^3"
8.0                     # ✓

$ ./evaluator "5^2"
25.0                    # ✓

$ ./evaluator "10^0"
1.0                     # ✓

$ ./evaluator "2^-1"
0.5                     # ✓ (negative exponents work)

$ ./evaluator "4^0.5"
2.0                     # ✓ (fractional exponents work)
```

### Complex Expressions

```bash
$ ./evaluator "2^3+4^2"
24.0                    # = 8 + 16 ✓

$ ./evaluator "(2+3)^(1+1)"
25.0                    # = 5^2 ✓

$ ./evaluator "100^0.5*2"
20.0                    # = 10 * 2 ✓

$ ./evaluator "2^3^2-500"
12.0                    # = 512 - 500 ✓
```

### Edge Cases

```bash
$ ./evaluator "0^0"
1.0                     # ✓ (Haskell convention)

$ ./evaluator "(-2)^2"
4.0                     # ✓

$ ./evaluator "(-2)^0.5"
Evaluation error: Error: Invalid exponentiation (-2.0 ^ 0.5 results in NaN)
                        # ✓ (properly caught)

$ ./evaluator "2^0.5"
1.4142135623730951      # ✓ (sqrt(2))
```

---

## Summary of Changes

| File           | Lines Changed | Description                                                         |
| -------------- | ------------- | ------------------------------------------------------------------- |
| `Tokenizer.hs` | 1 line        | Added `^` to operator list                                          |
| `Parser.hs`    | ~20 lines     | Added `Exp` constructor, `parseExp` function, updated `parseMulDiv` |
| `Evaluator.hs` | ~12 lines     | Added `Exp` evaluation with error handling                          |
| **Total**      | **~33 lines** | Complete exponentiation support                                     |

---

## Functional Programming Principles Applied

### 1. **Composability**

The `parseExp` function composes cleanly with existing parser functions, maintaining the recursive descent pattern.

### 2. **Type Safety**

The `Exp` constructor is part of the `Expr` type, ensuring type-safe representation of exponentiation.

### 3. **Error Handling with Either Monad**

Consistent use of `Either String Double` for error propagation, maintaining totality.

### 4. **Immutability**

All functions remain pure; no mutable state introduced.

### 5. **Pattern Matching**

Clean pattern matching on `Exp` constructor in evaluator.

---

## Design Decisions

### Why Right-Associativity?

Mathematical convention dictates that exponentiation is right-associative:

- In mathematics: a^b^c means a^(b^c)
- Most programming languages (Python, Ruby, etc.) follow this convention
- Makes expressions like 2^3^2 = 2^9 = 512 intuitive

### Why Higher Precedence than Multiplication?

Standard mathematical order of operations (PEMDAS/BODMAS):

- **P**arentheses / **B**rackets
- **E**xponents / **O**rders
- **M**ultiplication, **D**ivision
- **A**ddition, **S**ubtraction

### Error Handling Strategy

Rather than allowing NaN or Infinity to propagate silently, we catch these cases and provide clear error messages. This improves user experience and debugging.

---

## Extension 2: Basic Functions (sin, abs, sqrt) ✅ COMPLETED

### Overview

Added support for three basic mathematical functions: **sin** (sine), **abs** (absolute value), and **sqrt** (square root). This extension required implementing multi-character token recognition, extending the AST with unary function constructors, and adding function call parsing.

### Key Requirements

1. **Function Syntax**: Standard mathematical notation `fname(expr)`
2. **Functions Supported**: sin, abs, sqrt
3. **Precedence**: Functions have highest precedence (bind tightly to arguments)
4. **Error Handling**: Validate function arguments (e.g., sqrt of negative numbers)

---

## Change 1: Tokenizer - Add Function Name Recognition

### Before:

```haskell
-- Tokenizer.hs - Only handled single-character operators and numbers
import Data.Char (isDigit, isSpace)

tokenize' (c : cs) pos
  | isSpace c = tokenize' cs (pos + 1)
  | c `elem` "+-*/()^" = ([c] :) <$> tokenize' cs (pos + 1)
  | isDigit c || c == '.' = ...
  | otherwise =
      Left $ "Invalid character '" ++ [c] ++ "' at position " ++ show pos
```

**Problem:** The tokenizer cannot recognize multi-character identifiers like "sin", "abs", "sqrt". Attempting to parse "sin(0)" would fail with "Invalid character 's'" error.

### After:

```haskell
-- Tokenizer.hs - Added function name recognition
import Data.Char (isAlpha, isDigit, isSpace)

tokenize' (c : cs) pos
  | isSpace c = tokenize' cs (pos + 1)
  | c `elem` "+-*/()^" = ([c] :) <$> tokenize' cs (pos + 1)
  | isDigit c || c == '.' = ...
  | isAlpha c =
      -- Parse function names (sin, abs, sqrt)
      let (identifier, rest) = span isAlpha (c : cs)
       in if identifier `elem` ["sin", "abs", "sqrt"]
            then (identifier :) <$> tokenize' rest (pos + length identifier)
            else Left $ "Unknown function '" ++ identifier ++ "' at position " ++ show pos
  | otherwise =
      Left $ "Invalid character '" ++ [c] ++ "' at position " ++ show pos
```

**Fix:**

1. Added `isAlpha` import for alphabetic character detection
2. Added case for alphabetic characters
3. Used `span isAlpha` to collect consecutive letters into identifier
4. Validated identifier against known function names
5. Returns clear error for unknown functions

**Test:**

```haskell
tokenize "sin(0)"           -- Result: Right ["sin", "(", "0", ")"]
tokenize "abs(-5)+sqrt(4)"  -- Result: Right ["abs", "(", "-", "5", ")", "+", "sqrt", "(", "4", ")"]
tokenize "cos(0)"           -- Result: Left "Unknown function 'cos' at position 0"
```

---

## Change 2: Parser - Add Function Constructors to AST

### Before:

```haskell
-- Parser.hs - No representation for functions
data Expr
  = Num Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Exp Expr Expr
  deriving (Show, Eq)
```

**Problem:** The AST has no way to represent function calls.

### After:

```haskell
-- Parser.hs - Added function constructors
-- EXTENSION 2: Added Sin, Abs, Sqrt constructors for basic functions
data Expr
  = Num Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Exp Expr Expr
  | Sin Expr    -- NEW: Sine function
  | Abs Expr    -- NEW: Absolute value
  | Sqrt Expr   -- NEW: Square root
  deriving (Show, Eq)
```

**Fix:** Added three unary function constructors to represent function calls in the AST.

**Why separate constructors instead of generic `Function String Expr`?**

- **Type safety**: Each function has specific semantics
- **Pattern matching**: Exhaustive case analysis in evaluator
- **Error messages**: Can be function-specific
- **Extensibility**: Easy to add more functions later

**Example AST:**

- Expression: `sin(0)`
- AST: `Sin (Num 0.0)`
- Expression: `sqrt(abs(-4))`
- AST: `Sqrt (Abs (Mul (Num (-1.0)) (Num 4.0)))`

---

## Change 3: Parser - Implement Function Call Parsing

### Parsing Hierarchy

Functions are parsed at the **factor level** (highest precedence):

```
parseFactor handles:
  - Numbers: "42"
  - Parentheses: "(expr)"
  - Unary minus: "-expr"
  - Functions: "sin(expr)", "abs(expr)", "sqrt(expr)"  [NEW]
```

### Before:

```haskell
-- Parser.hs - parseFactor only handled numbers, parentheses, unary minus
parseFactor :: [Token] -> (Maybe Expr, [Token])
parseFactor [] = (Nothing, [])
parseFactor ("(" : ts) = ...
parseFactor ("-" : ts) = ...
parseFactor (t : ts)
  | all (\c -> isDigit c || c == '.' || c == '-') t = ...
  | otherwise = (Nothing, t : ts)
```

**Problem:** No handling for function tokens.

### After:

```haskell
-- Parser.hs - Added function call parsing
parseFactor :: [Token] -> (Maybe Expr, [Token])
parseFactor [] = (Nothing, [])
parseFactor ("(" : ts) = ...
parseFactor ("-" : ts) = ...
-- Parse function calls: sin(expr), abs(expr), sqrt(expr)
parseFactor ("sin" : "(" : ts) =
  case parseExpr ts of
    (Just e, ")" : rest) -> (Just (Sin e), rest)
    _ -> (Nothing, ts)
parseFactor ("abs" : "(" : ts) =
  case parseExpr ts of
    (Just e, ")" : rest) -> (Just (Abs e), rest)
    _ -> (Nothing, ts)
parseFactor ("sqrt" : "(" : ts) =
  case parseExpr ts of
    (Just e, ")" : rest) -> (Just (Sqrt e), rest)
    _ -> (Nothing, ts)
parseFactor (t : ts)
  | all (\c -> isDigit c || c == '.' || c == '-') t = ...
  | otherwise = (Nothing, t : ts)
```

**Fix:**

1. Pattern match on function name followed by opening parenthesis
2. Recursively parse the argument expression
3. Expect closing parenthesis
4. Return appropriate constructor (Sin/Abs/Sqrt)

**Error Handling:**

| Input    | Result             | Reason                      |
| -------- | ------------------ | --------------------------- |
| `sin(5)` | `Sin (Num 5.0)`    | Valid ✓                     |
| `sin 5`  | Parse error        | Missing parentheses         |
| `sin(`   | Parse error        | Missing closing parenthesis |
| `sin()`  | Parse error        | Empty argument              |
| `cos(5)` | Tokenization error | Unknown function            |

---

## Change 4: Evaluator - Implement Function Evaluation

### Before:

```haskell
-- Evaluator.hs - No cases for function constructors
eval :: Expr -> Either String Double
eval (Num n) = Right n
eval (Add a b) = (+) <$> eval a <*> eval b
-- ... other operators ...
eval (Exp a b) = ...
-- No cases for Sin, Abs, Sqrt!
```

**Problem:** The evaluator cannot handle function expressions. Pattern match would fail.

### After:

```haskell
-- Evaluator.hs - Added function evaluation
eval :: Expr -> Either String Double
eval (Num n) = Right n
eval (Add a b) = (+) <$> eval a <*> eval b
-- ... other operators ...
eval (Exp a b) = ...
-- Basic functions
eval (Sin a) = do
  arg <- eval a
  return (sin arg)
eval (Abs a) = do
  arg <- eval a
  return (abs arg)
eval (Sqrt a) = do
  arg <- eval a
  if arg < 0
    then Left $ "Error: Square root of negative number (" ++ show arg ++ ")"
    else return (sqrt arg)
```

**Fix:**

1. Added pattern match for `Sin` - uses Haskell's `sin` function (radians)
2. Added pattern match for `Abs` - uses Haskell's `abs` function
3. Added pattern match for `Sqrt` - validates non-negative argument

**Edge Cases Handled:**

| Expression | Result     | Explanation                |
| ---------- | ---------- | -------------------------- |
| `sin(0)`   | `0.0`      | Sine of 0 radians          |
| `abs(-5)`  | `5.0`      | Absolute value             |
| `abs(0)`   | `0.0`      | Absolute value of zero     |
| `sqrt(4)`  | `2.0`      | Square root                |
| `sqrt(0)`  | `0.0`      | Square root of zero        |
| `sqrt(-1)` | Error      | Negative argument rejected |
| `sqrt(2)`  | `1.414...` | Irrational result          |

---

## Precedence Verification

Functions have the **highest precedence** (bind tightly to their arguments):

### Test Cases:

```bash
$ ./evaluator "sin(0)+1"
1.0                     # = 0 + 1 (function evaluated first)

$ ./evaluator "2*abs(-3)"
6.0                     # = 2 * 3 (function evaluated first)

$ ./evaluator "sqrt(4)^2"
4.0                     # = 2 ^ 2 (function evaluated first)

$ ./evaluator "abs(-2)+abs(-3)"
5.0                     # = 2 + 3 (both functions evaluated first)
```

**Precedence Order (highest to lowest):**

1. Functions `sin()`, `abs()`, `sqrt()`
2. Parentheses `()`
3. Exponentiation `^` (right-associative)
4. Multiplication `*`, Division `/` (left-associative)
5. Addition `+`, Subtraction `-` (left-associative)

---

## Nested Expressions and Functions

Functions can contain arbitrary expressions and can be nested:

### Nested Expressions:

```bash
$ ./evaluator "sin(2+1)"
0.1411200080598672      # = sin(3)

$ ./evaluator "abs(-2*3)"
6.0                     # = abs(-6)

$ ./evaluator "sqrt(2^2)"
2.0                     # = sqrt(4)
```

### Nested Functions:

```bash
$ ./evaluator "sqrt(abs(-4))"
2.0                     # = sqrt(4)

$ ./evaluator "abs(sin(-1))"
0.8414709848078965      # = abs(-0.841...)

$ ./evaluator "sin(sqrt(4))"
0.9092974268256817      # = sin(2)
```

### Complex Expressions:

```bash
$ ./evaluator "2+sin(0)*3"
2.0                     # = 2 + (0 * 3)

$ ./evaluator "sqrt(16)/2"
2.0                     # = 4 / 2

$ ./evaluator "abs(-5)^2"
25.0                    # = 5 ^ 2
```

---

## Comprehensive Test Results

### Basic Function Tests

```bash
$ ./evaluator "sin(0)"
0.0                     # ✓

$ ./evaluator "abs(-5)"
5.0                     # ✓

$ ./evaluator "abs(5)"
5.0                     # ✓ (positive input)

$ ./evaluator "abs(0)"
0.0                     # ✓

$ ./evaluator "sqrt(4)"
2.0                     # ✓

$ ./evaluator "sqrt(0)"
0.0                     # ✓

$ ./evaluator "sqrt(2)"
1.4142135623730951      # ✓ (irrational)
```

### Edge Cases

```bash
$ ./evaluator "sqrt(-1)"
Evaluation error: Error: Square root of negative number (-1.0)
                        # ✓ (properly caught)

$ ./evaluator "sqrt(-4)"
Evaluation error: Error: Square root of negative number (-4.0)
                        # ✓ (properly caught)
```

---

## Summary of Changes

| File           | Lines Changed | Description                                              |
| -------------- | ------------- | -------------------------------------------------------- |
| `Tokenizer.hs` | ~7 lines      | Added `isAlpha`, identifier parsing, function validation |
| `Parser.hs`    | ~16 lines     | Added 3 constructors, function call parsing              |
| `Evaluator.hs` | ~12 lines     | Added 3 function evaluations with error handling         |
| **Total**      | **~35 lines** | Complete function support                                |

---

## Functional Programming Principles Applied

### 1. **Type Safety**

Each function has its own constructor, ensuring type-safe representation and exhaustive pattern matching.

### 2. **Composability**

Functions compose naturally with all existing operators and with each other (e.g., `sqrt(abs(-4))`).

### 3. **Error Handling with Either Monad**

Consistent use of `Either String Double` for error propagation, maintaining totality.

### 4. **Immutability**

All functions remain pure; no mutable state introduced.

### 5. **Pattern Matching**

Clean, exhaustive pattern matching on function constructors in evaluator.

---

## Design Decisions

### Why Parse Functions at Factor Level?

Functions bind tightly to their arguments, similar to how parentheses work. This ensures:

- `sin(2)+3` is parsed as `(sin(2)) + 3`, not `sin((2+3))`
- Consistent with mathematical notation
- Highest precedence for function application

### Why Validate sqrt Argument in Evaluator?

- **Separation of concerns**: Parser handles syntax, evaluator handles semantics
- **Flexibility**: Allows expressions like `sqrt(x)` where x is computed at runtime
- **Consistency**: Matches division-by-zero handling pattern

### Why Lowercase Function Names Only?

- Simpler implementation (no case conversion needed)
- Standard mathematical notation
- Easier to type and read

### Why These Three Functions?

- **sin**: Demonstrates trigonometric functions (radians)
- **abs**: Demonstrates simple unary operations
- **sqrt**: Demonstrates domain validation (non-negative)
- Together they cover different categories of mathematical functions

---

## Conclusion

Both Part 2 extensions have been successfully implemented:

1. **Exponentiation (^)**: Right-associative operator with proper precedence
2. **Basic Functions (sin, abs, sqrt)**: Full function call support with error handling

The implementation maintains all functional programming principles from Part 1, adds comprehensive error handling, and integrates seamlessly with existing operators. All test cases pass, demonstrating correct precedence, associativity, and edge case handling.
