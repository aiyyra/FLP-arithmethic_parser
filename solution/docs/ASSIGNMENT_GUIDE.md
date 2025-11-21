# Quick Reference: Assignment Submission Guide

## 📋 What You're Submitting

### Core Files (Required)
1. **Main.hs** - Entry point and I/O handling
2. **Tokenizer.hs** - Lexical analysis module
3. **Parser.hs** - Syntax analysis module
4. **Evaluator.hs** - Expression evaluation module
5. **Tests.hs** - Test suite demonstrating all bug fixes

### Documentation Files (Recommended)
6. **README.md** - Project overview and usage guide
7. **BugFixesDetailed.md** - Detailed before/after for each bug
8. **MODULE_ARCHITECTURE.md** - Module design explanation
9. **REFACTORING_SUMMARY.md** - Executive summary

### Reference
10. **Evaluator_OLD.hs** - Original buggy code (for comparison)

---

## 🎯 Assignment Requirements Checklist

### ✅ Task 1: Debug - Fix 8+ Runtime Errors/Edge Cases
We fixed **12 bugs** total:

**Critical Runtime Errors (5):**
- [x] Bug #1: Operator precedence & left-to-right associativity
- [x] Bug #2: Division by zero not handled
- [x] Bug #3: tokensUsed function fundamentally broken
- [x] Bug #4: Negative numbers only work at start
- [x] Bug #5: Parentheses parsing fails

**Input Validation (3):**
- [x] Bug #6: No empty input validation
- [x] Bug #7: No floating-point number support
- [x] Bug #8: Invalid character errors lack details

**Design & Architecture (4):**
- [x] Bug #9: Unused accumulator parameter
- [x] Bug #10: parseExpr passes meaningless parameter
- [x] Bug #11: SECRET_MODIFIER applied without user awareness
- [x] Bug #12: No decimal point validation


---

### ✅ Task 2: Organize into Multiple Modules
We created **4 logically separated modules**:

1. **Tokenizer.hs** - Lexical analysis
   - Converts strings to tokens
   - Handles invalid characters
   - Validates number formats

2. **Parser.hs** - Syntax analysis
   - Converts tokens to AST
   - Manages operator precedence
   - Handles parentheses and negation

3. **Evaluator.hs** - Semantic analysis
   - Evaluates expressions
   - Handles division by zero
   - Pure computation only

4. **Main.hs** - I/O orchestration
   - Command-line interface
   - Error reporting
   - SECRET_MODIFIER handling

---

### ✅ Task 3: Apply 3+ FP Design Improvements
We applied **6 functional programming principles**:

1. **Totality** (Total Functions)
   - Before: `tokenize :: String -> [String]` (crashes on errors)
   - After: `tokenize :: String -> Either String [String]`
   - **Lecture**: *05 IO and side effects.pdf*

2. **Immutability** (No Hidden State)
   - Before: `tokensUsed` tries to track state externally
   - After: Thread remaining tokens through parser
   - **Lecture**: *02 Lambda Calculus FP Basics.pdf*

3. **Separation of Concerns** (Module System)
   - Before: 85 lines in 1 file
   - After: 4 focused modules
   - **Lecture**: *01 Introduction to Haskell.pdf*

4. **Type Safety** (Types Encode Errors)
   - Before: Runtime exceptions with `error`
   - After: `Either String a` for safe errors
   - **Lecture**: *01 Introduction to Haskell.pdf*

5. **Composability** (Higher-Order Functions)
   - Before: Manual error checking everywhere
   - After: Applicative style `(+) <$> eval a <*> eval b`
   - **Lecture**: *04 Higher Order Functions.pdf*

6. **Transparency** (Explicit Side Effects)
   - Before: SECRET_MODIFIER silently changes results
   - After: Explicit notification to user
   - **Lecture**: *05 IO and side effects.pdf*

---

## 📝 How to Present Each Bug

For each bug, include:

### 1. Bug Description
```
Bug #X: [Name]
Category: [Critical/Validation/Design]
Severity: [High/Medium/Low]
```

### 2. Before Code
```haskell
-- Show the buggy code
```

### 3. Problem Explanation
```
What goes wrong and why
Include test case showing failure
```

### 4. After Code
```haskell
-- Show the fixed code
```

### 5. Lecture Citation
```
Reference: "[Lecture PDF name]" - [Specific concept]
```

---

## 🧪 Test Execution Guide

### Compile and Run
```bash
# Compile main program
ghc --make -o evaluator Main.hs

# Test individual bugs
./evaluator "2+3*4"           # Bug #1: Precedence → 14.0
./evaluator "5/0"             # Bug #2: Div by zero → Error
./evaluator "(2+3)*4"         # Bug #5: Parentheses → 20.0
./evaluator "2.5+3.5"         # Bug #7: Floats → 6.0
./evaluator "5+-3"            # Bug #4: Negatives → 2.0
./evaluator ""                # Bug #6: Empty input → Error
./evaluator "2+3&4"           # Bug #8: Error message → Detailed
./evaluator "2+3+4"           # Bug #1: Associativity → 9.0

# Test SECRET_MODIFIER transparency (Bug #11)
$env:SECRET_MODIFIER='2.0'
./evaluator "5+5"             # Shows notification + 20.0
Remove-Item Env:\SECRET_MODIFIER

# Compile and run full test suite
ghc --make -o tests Tests.hs
./tests
```

---

## 📊 Presentation Structure Suggestion

### Introduction
- Brief overview of what the program does
- Mention it's an arithmetic expression evaluator

### Part 1: Bug Fixes (Choose 8)
For each bug:
1. Show buggy code
2. Explain the problem with test case
3. Show fixed code
4. Cite lecture reference
5. Demo the fix working

### Part 2: Modular Organization
- Show original monolithic structure
- Explain 4-module separation
- Discuss benefits (testing, maintenance, reusability)
- **Cite**: *01 Introduction to Haskell.pdf* - Module system

### Part 3: FP Design Improvements (Choose 3)
1. **Total Functions with Either Monad**
   - Show before/after
   - Explain type safety
   - **Cite**: *05 IO and side effects.pdf*

2. **Explicit State Threading**
   - Show tokensUsed problem
   - Show parser returning (Maybe Expr, [Token])
   - **Cite**: *02 Lambda Calculus FP Basics.pdf*

3. **Separation of Concerns**
   - Show module dependency graph
   - Explain single responsibility
   - **Cite**: *01 Introduction to Haskell.pdf*

### Conclusion
- Summary of improvements
- Benefits gained
- Professional-grade architecture

---

## 💡 Key Points to Emphasize

1. **Systematic Approach**: Found bugs through code analysis, not just trial and error
2. **Type-Driven Development**: Used types to prevent errors (Either, Maybe)
3. **FP Principles**: Applied concepts from lectures throughout
4. **Professional Quality**: Modular design matches industry standards
5. **Comprehensive Testing**: Every bug has test case verification

---

## 🎓 Lecture References Summary

| Bug/Improvement | Lecture Reference | Concept |
|----------------|-------------------|---------|
| Precedence & Parsing | 01 Introduction | Recursion, Pattern matching |
| Division by Zero | 05 IO | Error handling, Either monad |
| tokensUsed Removal | 02 Lambda Calculus | Pure functions, State threading |
| Negative Numbers | 04 HOFs | Function composition |
| Module Separation | 01 Introduction | Module system |
| Total Functions | 05 IO | Totality, Type safety |
| Immutability | 02 Lambda Calculus | No side effects |
| Composability | 04 HOFs | Applicative functors |

---

## ✅ Final Checklist Before Submission

- [ ] All 4 module files compile without errors
- [ ] Tests.hs runs and demonstrates bug fixes
- [ ] README.md explains project structure
- [ ] BugFixesDetailed.md documents all fixes
- [ ] Each bug has lecture citation
- [ ] At least 8 bugs fixed
- [ ] At least 3 FP improvements documented
- [ ] Code is properly commented
- [ ] Original buggy code included for comparison

---

**You're ready to submit! Good luck with your assignment! 🎉**
