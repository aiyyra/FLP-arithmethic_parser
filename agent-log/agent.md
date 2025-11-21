This is an instruction files for agent.

- Append and log every change in `agent-log/log.md`.

# Task Overview

**WID3001 – Functional and Logic Programming Assignment 1**
**Arithmetic Expression Evaluator**

## Part 1: Debug & Refactor ✅ COMPLETED

### 1.1 Bug Fixes (Minimum 8) ✅ EXCEEDED - Fixed 12 bugs

- [x] Identify and fix at least 8 runtime errors or edge cases in the flawed code
  - [x] Bug 1: Operator precedence & left-to-right associativity broken
  - [x] Bug 2: Division by zero not handled
  - [x] Bug 3: `tokensUsed` function fundamentally broken (removed entirely)
  - [x] Bug 4: Negative numbers only work at start of expression
  - [x] Bug 5: Parentheses parsing fails due to tokensUsed bug
  - [x] Bug 6: No empty input validation
  - [x] Bug 7: No floating-point number support
  - [x] Bug 8: Invalid character errors lack details
  - [x] Bug 9: Unused accumulator parameter in parseAddSub
  - [x] Bug 10: parseExpr passes meaningless empty list parameter
  - [x] Bug 11: SECRET_MODIFIER applied without user awareness
  - [x] Bug 12: No decimal point validation (accepts "2.3.4")

### 1.2 Code Organization ✅ COMPLETED - 4 modules

- [x] Organize code into multiple logically separated modules
  - [x] Tokenizer.hs - Lexical analysis (27 lines)
  - [x] Parser.hs - Syntax analysis (85 lines)
  - [x] Evaluator.hs - Expression evaluation (18 lines)
  - [x] Main.hs - CLI/main orchestration (46 lines)

### 1.3 Functional Programming Design Improvements (Minimum 3) ✅ EXCEEDED - 6 principles

- [x] Design improvement 1: **Totality** - Either String a instead of error/crashes
- [x] Design improvement 2: **Immutability** - Thread tokens through parser, no hidden state
- [x] Design improvement 3: **Separation of Concerns** - 4 focused modules
- [x] Design improvement 4: **Type Safety** - Types encode errors (Either monad)
- [x] Design improvement 5: **Composability** - Applicative functors (<$>, <\*>)
- [x] Design improvement 6: **Transparency** - Explicit side effect notification

## Part 2: Extensions (Choose 2) ✅ COMPLETED

### Option 1: Exponentiation ✅ COMPLETED

- [x] Add support for exponentiation (^) operator
- [x] Implement right-associativity for exponentiation

### Option 2: Basic Functions ✅ COMPLETED

- [x] Add support for **sin** function
- [x] Add support for **abs** function
- [x] Add support for **sqrt** function

### Option 3: Variables ❌ NOT SELECTED

- [ ] Allow variables in expressions (e.g., "x + 2")
- [ ] Provide variable values via CLI flags
- [ ] Handle variable parsing and substitution

## Deliverables

### Code Deliverable ✅ COMPLETED

- [x] Create fixed and refactored Haskell code
- [x] Organize into proper module structure
- [x] Implement Part 2 extensions (exponentiation + functions)
- [ ] Prepare zip file with all code

### Test Cases Deliverable ⚠️ PARTIAL - Not using HUnit

- [x] Create Tests.hs file (103 lines)
- [x] Write at least 10 unit tests (12+ test cases included)
- [x] Include tests for fixed bugs (edge cases)
- [x] Manual testing for new features (Part 2 extensions - all passed)
- [x] Tests compile and run successfully
- ⚠️ **ISSUE**: Tests use custom test runner, NOT HUnit library as required
- [ ] Add tests for exponentiation extension
- [ ] Add tests for function extension

### Documentation Deliverable ✅ EXCEEDED

- [x] Document bugs fixed and how they were fixed (BugFixesDetailed.md - 363 lines)
- [x] Explain refactoring choices (REFACTORING_SUMMARY.md - 141 lines)
- [x] Document Part 2 extensions (FeatureExtensionDetailed.md - 893 lines)
- [ ] Include 3-5 screenshots of CLI in action
  - [ ] Screenshot: Basic arithmetic operations
  - [ ] Screenshot: Error handling
  - [ ] Screenshot: Exponentiation feature
  - [ ] Screenshot: Function features
  - [ ] Screenshot: Edge cases
- [ ] Acknowledge AI tool usage (if any)
- [x] Additional docs: PROJECT_OVERVIEW.md, ASSIGNMENT_GUIDE.md
- [x] Total documentation: 1,807 lines across 5 comprehensive documents

## Implementation Plan

1. **Setup Phase** ✅ COMPLETED

   - [x] Set up proper Haskell project structure (manual, no Cabal/Stack)
   - [ ] Install HUnit testing library ⚠️ NOT DONE
   - [x] Create module structure (4 modules)

2. **Debug Phase** ✅ COMPLETED

   - [x] Analyze and document all bugs (12 bugs identified)
   - [x] Fix bugs one by one (all 12 fixed)
   - [x] Test each fix (verified working)

3. **Refactor Phase** ✅ COMPLETED

   - [x] Split code into modules (4 modules)
   - [x] Apply FP design principles (6 principles)
   - [x] Improve code quality and readability

4. **Extension Phase** ✅ COMPLETED

   - [x] Choose 2 extensions to implement (Exponentiation + Functions)
   - [x] Implement chosen extensions
   - [x] Integrate with refactored code

5. **Testing Phase** ⚠️ PARTIAL

   - [x] Write comprehensive unit tests (12+ test cases)
   - [x] Test edge cases (all bugs tested)
   - [x] Test new features (Part 2 - manual testing complete)
   - [x] Ensure all tests pass (verified)
   - ⚠️ **NOT using HUnit as required** (need to convert)

6. **Documentation Phase** ⚠️ PARTIAL
   - [ ] Take screenshots of working CLI (0/5 done)
   - [x] Write explanation document (1,807 lines of docs)
   - [ ] Prepare final submission package

---

## ⚠️ Issues & Gaps Identified

### Critical Issues

1. **Part 2 Extensions NOT Implemented** ✅ RESOLVED

   - ~~Assignment requires choosing and implementing 2 extensions~~
   - ~~Current solution only completes Part 1~~
   - **COMPLETED**: Implemented both Exponentiation (^) and Basic Functions (sin, abs, sqrt)
   - All tests pass, fully integrated with existing code

2. **Tests NOT Using HUnit Library** 🔴 STILL PENDING
   - Assignment explicitly requires HUnit library
   - Current Tests.hs uses custom test runner
   - Need to rewrite tests using HUnit's `Test`, `TestCase`, `assertEqual`, etc.

### Minor Issues

3. **No Screenshots**

   - Assignment requires 3-5 screenshots of CLI in action
   - Need to capture and include in documentation

4. **No Project Build Configuration**
   - No Cabal or Stack configuration
   - Makes it harder for others to build/run
   - Consider adding .cabal file

### Questionable Design Choices

5. **Bug Counting May Be Inflated**

   - Bugs #9 and #10 (unused parameters) are code quality issues, not runtime errors
   - Assignment asks for "runtime errors or edge cases"
   - May want to reclassify or find additional runtime bugs

6. **Excessive Documentation**
   - 914 lines of documentation may be overkill
   - Assignment asks for "short explanation (PDF)"
   - Current docs are very comprehensive but may need condensing for PDF

---

## Recommendations for Completion

### High Priority (Required)

1. ✅ ~~Implement 2 extensions from Part 2~~ **COMPLETED**
2. 🔴 Rewrite Tests.hs using HUnit library **PENDING**
3. 🔴 Take 3-5 screenshots of CLI **PENDING**

### Medium Priority (Recommended)

4. Add Cabal configuration for easier building
5. Condense documentation into PDF format
6. Add README.md in solution/ directory

### Low Priority (Nice to Have)

7. Add more edge case tests for Part 2 extensions
8. Consider adding QuickCheck property tests
9. Add build/run instructions

---

## Summary of Part 2 Implementation

### Extension 1: Exponentiation (^) ✅

**Changes:**

- Tokenizer: Added '^' to operator list
- Parser: Added `Exp` constructor, implemented right-associative parsing
- Evaluator: Added exponentiation with NaN/overflow error handling
- **Total: ~33 lines**

**Test Results:**

- Basic: 2^3=8, 5^2=25, 10^0=1, 2^-1=0.5 ✅
- Right-associativity: 2^3^2=512 (not 64) ✅
- Precedence: 2+3^2=11, 2\*3^2=18 ✅
- Edge cases: 0^0=1, (-2)^0.5=Error ✅

### Extension 2: Basic Functions (sin, abs, sqrt) ✅

**Changes:**

- Tokenizer: Added function name recognition (isAlpha, span)
- Parser: Added `Sin`, `Abs`, `Sqrt` constructors, function call parsing
- Evaluator: Added function evaluation with sqrt validation
- **Total: ~35 lines**

**Test Results:**

- Basic: sin(0)=0, abs(-5)=5, sqrt(4)=2 ✅
- Precedence: sin(0)+1=1, 2\*abs(-3)=6, sqrt(4)^2=4 ✅
- Nested: sin(2+1)=0.141, sqrt(abs(-4))=2 ✅
- Edge cases: sqrt(-1)=Error ✅

**Total Implementation:**

- ~68 lines of code added
- 893 lines of documentation (FeatureExtensionDetailed.md)
- All manual tests passed
- Fully integrated with Part 1 code
