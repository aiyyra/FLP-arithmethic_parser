This docs is used to organize, plan and track the progress of Part 2 implementation.

# Part 2: Extensions (Choose 2)

## Extension 1: Exponentiation (^) ✅ COMPLETED

### Requirements

- [x] Add support for exponentiation (^) operator
- [x] Implement right-associativity for exponentiation (e.g., 2^3^2 = 2^(3^2) = 512, not (2^3)^2 = 64)

### Design Plan

#### 1. Tokenizer Changes ✅

- [x] **Analysis**: Tokenizer already handles single-character operators
- [x] **Change needed**: Add '^' to the operator list in `Tokenizer.hs`
- [x] **Implementation**: Modified line 17 to include '^' in operator characters
- [x] **Testing**: Verified "2^3" tokenizes to ["2", "^", "3"] ✅

#### 2. Parser Changes (AST) ✅

- [x] **Add Exp constructor** to Expr data type in `Parser.hs`
  - Current: `data Expr = Num Double | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr`
  - New: `data Expr = Num Double | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Exp Expr Expr` ✅

#### 3. Parser Changes (Precedence & Associativity) ✅

- [x] **Create parseExp function** for exponentiation parsing
  - Precedence: Higher than multiplication/division ✅
  - Associativity: RIGHT (unlike other operators which are left-associative) ✅
  - Position in parsing hierarchy: Between parseMulDiv and parseFactor ✅

**Parsing Hierarchy (Precedence from lowest to highest):**

```
parseExpr
  └─> parseAddSub      (lowest precedence: +, -)
       └─> parseMulDiv  (medium precedence: *, /)
            └─> parseExp (highest precedence: ^) [NEW]
                 └─> parseFactor (atoms: numbers, parentheses, unary minus)
```

**Right-Associativity Implementation:**

- Left-associative (current +, -, \*, /): `2 + 3 + 4` = `(2 + 3) + 4`
- Right-associative (new ^): `2 ^ 3 ^ 4` = `2 ^ (3 ^ 4)`
- Implementation: Parse left operand, then recursively parse entire right side

#### 4. Parser Changes (Function Signatures) ✅

- [x] Update parseMulDiv to call parseExp instead of parseFactor ✅
- [x] Create parseExp that calls parseFactor for base and recursively for exponent ✅

#### 5. Evaluator Changes ✅

- [x] **Add Exp case** to eval function in `Evaluator.hs`
  - Use Haskell's `(**)` operator for exponentiation ✅
  - Handle edge cases: ✅
    - 0^0 → Returns 1.0 (Haskell's default behavior) ✅
    - Negative base with fractional exponent → Error: "Invalid exponentiation (results in NaN)" ✅
    - Very large results → Error: "Exponentiation overflow (is too large)" ✅

#### 6. Testing ✅ VERIFIED

- [x] Add test cases for basic exponentiation
  - `2^3` → 8.0 ✅
  - `5^2` → 25.0 ✅
  - `10^0` → 1.0 ✅
  - `2^-1` → 0.5 ✅
- [x] Add test cases for right-associativity
  - `2^3^2` → 512.0 (not 64.0) ✅
  - `2^2^3` → 256.0 ✅
- [x] Add test cases for precedence
  - `2+3^2` → 11.0 (not 25.0) ✅
  - `2*3^2` → 18.0 (not 36.0) ✅
  - `10-2^3` → 2.0 (not 64.0) ✅
  - `(2+3)^2` → 25.0 ✅
- [x] Add test cases for edge cases
  - `0^0` → 1.0 ✅
  - `2^0.5` → 1.414... (sqrt(2)) ✅
  - `(-2)^2` → 4.0 ✅
  - `(-2)^0.5` → Error: "Invalid exponentiation (-2.0 ^ 0.5 results in NaN)" ✅

### Implementation Checklist ✅ ALL COMPLETE

**Step 1: Tokenizer** (Tokenizer.hs) ✅

- [x] Add '^' to operator characters (line 17)
- [x] Test tokenization

**Step 2: Parser - Data Type** (Parser.hs) ✅

- [x] Add `Exp Expr Expr` constructor to Expr data type
- [x] Verify it compiles

**Step 3: Parser - parseExp Function** (Parser.hs) ✅

- [x] Create parseExp function with right-associativity
- [x] Insert between parseMulDiv and parseFactor in hierarchy
- [x] Update parseMulDiv to call parseExp instead of parseFactor

**Step 4: Evaluator** (Evaluator.hs) ✅

- [x] Add eval case for Exp constructor
- [x] Implement using `(**)` operator
- [x] Add error handling for edge cases

**Step 5: Testing** (Tests.hs) ✅ COMPLETED

- [x] Manual testing complete (all tests pass)
- [x] Add exponentiation test cases to Tests.hs (14 test cases)
- [x] Verify right-associativity in test suite (2 test cases)
- [x] Verify precedence in test suite (4 test cases)
- [x] Test edge cases in test suite (3 test cases)

**Step 6: Documentation** ✅ COMPLETED

- [x] Created FeatureExtensionDetailed.md (893 lines - both extensions)
- [x] Document design decisions (right-associativity, edge cases)
- [x] Add examples to README (comprehensive examples included)

---

### Test Results Summary

**All manual tests passed successfully:**

✅ Basic exponentiation: 2^3=8, 5^2=25, 10^0=1, 2^-1=0.5
✅ Right-associativity: 2^3^2=512 (not 64), 2^2^3=256
✅ Precedence: 2+3^2=11, 2\*3^2=18, 10-2^3=2, (2+3)^2=25
✅ Edge cases: 0^0=1, 2^0.5=1.414..., (-2)^2=4, (-2)^0.5=Error

**Implementation is complete and working correctly!**

---

## Extension 2: Basic Functions ✅ COMPLETED

### Requirements

- [x] Add support for **sin** function (sine, takes radians)
- [x] Add support for **abs** function (absolute value)
- [x] Add support for **sqrt** function (square root)

### Design Plan

#### Function Syntax

Functions will use standard mathematical notation:

- `sin(expr)` - sine of expression (in radians)
- `abs(expr)` - absolute value of expression
- `sqrt(expr)` - square root of expression

**Examples:**

- `sin(0)` → 0.0
- `abs(-5)` → 5.0
- `sqrt(4)` → 2.0
- `sin(2+1)` → sin(3) → 0.141...
- `abs(-2*3)` → abs(-6) → 6.0
- `sqrt(2^2)` → sqrt(4) → 2.0

#### 1. Tokenizer Changes

**Challenge:** Need to tokenize multi-character function names (sin, abs, sqrt)

**Current tokenizer:** Only handles single-character operators and numbers

**Solution:** Add function name recognition

- Check if character is alphabetic
- Span consecutive alphabetic characters to form identifier
- Validate identifier is a known function name
- Return function name as a token

**Implementation:**

- [ ] Add alphabetic character detection
- [ ] Add identifier spanning logic
- [ ] Add function name validation (sin, abs, sqrt)
- [ ] Return function tokens

#### 2. Parser Changes (AST)

**Add function constructors** to Expr data type:

- Current: `data Expr = Num Double | Add Expr Expr | ... | Exp Expr Expr`
- New: Add `Sin Expr | Abs Expr | Sqrt Expr`

**Why separate constructors?**

- Type-safe representation
- Pattern matching in evaluator
- Clear AST structure

#### 3. Parser Changes (Function Parsing)

**Parsing hierarchy:** Functions are parsed at the factor level (like parentheses)

```
parseFactor handles:
  - Numbers: "42"
  - Parentheses: "(expr)"
  - Unary minus: "-expr"
  - Functions: "sin(expr)", "abs(expr)", "sqrt(expr)"  [NEW]
```

**Parsing logic:**

1. Recognize function token (sin/abs/sqrt)
2. Expect opening parenthesis `(`
3. Recursively parse inner expression
4. Expect closing parenthesis `)`
5. Return appropriate constructor (Sin/Abs/Sqrt)

**Error handling:**

- Missing opening parenthesis: "sin 5" → Error
- Missing closing parenthesis: "sin(5" → Error
- Unknown function: "cos(5)" → Error (during tokenization)
- Empty argument: "sin()" → Error

#### 4. Evaluator Changes

**Add evaluation cases** for each function:

```haskell
eval (Sin a) = do
  arg <- eval a
  return (sin arg)  -- Haskell's sin function (radians)

eval (Abs a) = do
  arg <- eval a
  return (abs arg)  -- Haskell's abs function

eval (Sqrt a) = do
  arg <- eval a
  if arg < 0
    then Left "Error: Square root of negative number"
    else return (sqrt arg)
```

**Edge cases to handle:**

- `sqrt(-1)` → Error (no complex numbers)
- `sin(very large)` → May lose precision but valid
- `abs(0)` → 0.0
- Nested functions: `sin(abs(-1))` → sin(1)

#### 5. Precedence & Associativity

**Functions have highest precedence** (evaluated before any operators):

- `sin(2)+3` = `(sin(2)) + 3` ✓
- `2*abs(-3)` = `2 * (abs(-3))` ✓
- `sqrt(4)^2` = `(sqrt(4)) ^ 2` = `2^2` = 4 ✓

**Functions are right-associative with their arguments:**

- `sin(2+3)` = `sin(5)` (argument evaluated first)

#### 6. Testing

**Basic function tests:**

- [ ] `sin(0)` → 0.0
- [ ] `sin(1.5707963267948966)` → 1.0 (π/2)
- [ ] `abs(5)` → 5.0
- [ ] `abs(-5)` → 5.0
- [ ] `abs(0)` → 0.0
- [ ] `sqrt(4)` → 2.0
- [ ] `sqrt(0)` → 0.0
- [ ] `sqrt(2)` → 1.414...

**Precedence tests:**

- [ ] `sin(0)+1` → 1.0
- [ ] `2*abs(-3)` → 6.0
- [ ] `sqrt(4)^2` → 4.0
- [ ] `abs(-2)+abs(-3)` → 5.0

**Nested expression tests:**

- [ ] `sin(2+1)` → sin(3)
- [ ] `abs(-2*3)` → 6.0
- [ ] `sqrt(2^2)` → 2.0
- [ ] `sin(abs(-1))` → sin(1)

**Nested function tests:**

- [ ] `abs(sin(-1))` → abs(-0.841...) → 0.841...
- [ ] `sqrt(abs(-4))` → sqrt(4) → 2.0
- [ ] `sin(sqrt(4))` → sin(2)

**Edge case tests:**

- [ ] `sqrt(-1)` → Error
- [ ] `sqrt(-4)` → Error
- [ ] `sin()` → Parse error
- [ ] `abs()` → Parse error
- [ ] `sqrt(` → Parse error

**Complex expression tests:**

- [ ] `2+sin(0)*3` → 2.0
- [ ] `sqrt(16)/2` → 2.0
- [ ] `abs(-5)^2` → 25.0
- [ ] `sin(0)+cos(0)` → Error (cos not implemented)

### Implementation Checklist

**Step 1: Tokenizer** (Tokenizer.hs) ✅

- [x] Add alphabetic character detection (isAlpha)
- [x] Add identifier/function name parsing (span isAlpha)
- [x] Validate function names (sin, abs, sqrt)
- [x] Test tokenization of function calls

**Step 2: Parser - Data Type** (Parser.hs) ✅

- [x] Add `Sin Expr` constructor to Expr
- [x] Add `Abs Expr` constructor to Expr
- [x] Add `Sqrt Expr` constructor to Expr
- [x] Verify it compiles

**Step 3: Parser - Function Parsing** (Parser.hs) ✅

- [x] Modify parseFactor to recognize function tokens
- [x] Parse function call syntax: `fname(expr)`
- [x] Handle opening/closing parentheses
- [x] Add error handling for malformed function calls

**Step 4: Evaluator** (Evaluator.hs) ✅

- [x] Add eval case for Sin using Haskell's `sin`
- [x] Add eval case for Abs using Haskell's `abs`
- [x] Add eval case for Sqrt with negative number check
- [x] Test edge cases

**Step 5: Testing** (Tests.hs) ✅ COMPLETED

- [x] Add basic function tests (7 test cases - all passed)
- [x] Add precedence tests (4 test cases - all passed)
- [x] Add nested expression tests (included in complex tests)
- [x] Add nested function tests (2 test cases - all passed)
- [x] Add edge case tests (2 test cases - all passed)
- [x] Add to Tests.hs file (15 function test cases total)

**Step 6: Documentation** ✅ COMPLETED

- [x] Update FeatureExtensionDetailed.md with Extension 2 (448 lines added)
- [x] Document tokenizer changes (function name recognition)
- [x] Document parser changes (AST and function parsing)
- [x] Document evaluator changes (function evaluation)
- [x] Add test results (comprehensive test coverage documented)

---

### Design Decisions

**Why separate constructors instead of generic Function constructor?**

- Type safety: Each function has specific semantics
- Pattern matching: Clear, exhaustive case analysis
- Error messages: Can be function-specific
- Extensibility: Easy to add more functions later

**Why parse at factor level?**

- Functions bind tightly to their arguments
- Consistent with mathematical notation
- Similar to parentheses in precedence

**Why check for negative sqrt in evaluator, not parser?**

- Parser handles syntax, evaluator handles semantics
- Allows expressions like `sqrt(x)` where x is computed
- Consistent with division-by-zero handling

**Function name case sensitivity?**

- Lowercase only (sin, abs, sqrt)
- Simpler implementation
- Standard mathematical notation

---

### Test Results Summary

**All manual tests passed successfully:**

✅ **Basic functions:** sin(0)=0, abs(-5)=5, sqrt(4)=2, abs(0)=0
✅ **Precedence:** sin(0)+1=1, 2*abs(-3)=6, sqrt(4)^2=4, abs(-2)+abs(-3)=5
✅ **Nested expressions:** sin(2+1)=0.141..., abs(-2*3)=6, sqrt(2^2)=2
✅ **Nested functions:** sqrt(abs(-4))=2
✅ **Edge cases:** sqrt(-1)=Error, sqrt(2)=1.414..., 2+sin(0)\*3=2, sqrt(16)/2=2

**Implementation is complete and working correctly!**

---

## Extension 3: Variables - NOT SELECTED

### Option 3: Variables

- [ ] Allow variables in expressions (e.g., "x + 2")
- [ ] Provide variable values via CLI flags
- [ ] Handle variable parsing and substitution
