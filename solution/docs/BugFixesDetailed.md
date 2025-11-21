# Bug Fixes Documentation - Before & After Comparison

## Bug #1: Broken Operator Precedence & Associativity

### Before:
```haskell
parseAddSub :: [String] -> [String] -> Maybe Expr
parseAddSub ts acc = do
  e <- parseMulDiv ts
  let remaining = drop (tokensUsed e) ts  -- WRONG: uses broken tokensUsed
  case remaining of
    [] -> Just e
    (op:rest) | op `elem` ["+", "-"] -> 
      let next = parseAddSub rest (op:acc)  -- Ignores accumulator!
      in case next of
        Just e' -> if op == "+" then Just (Add e e') else Just (Sub e e')
        Nothing -> Just e
    _ -> Just e
```

**Problem:** 
- Uses broken `tokensUsed` function
- Doesn't properly handle left-to-right associativity
- Expression "2 + 3 + 4" would parse incorrectly

### After:
```haskell
parseAddSub :: [String] -> (Maybe Expr, [String])
parseAddSub ts = 
  case parseMulDiv ts of
    (Nothing, rest) -> (Nothing, rest)
    (Just left, rest) -> parseAddSub' left rest
  where
    parseAddSub' :: Expr -> [Token] -> (Maybe Expr, [Token])
    parseAddSub' left [] = (Just left, [])
    parseAddSub' left ("+":rest) = 
      case parseMulDiv rest of
        (Just right, rest') -> parseAddSub' (Add left right) rest'
        (Nothing, _) -> (Nothing, rest)
    parseAddSub' left ("-":rest) = 
      case parseMulDiv rest of
        (Just right, rest') -> parseAddSub' (Sub left right) rest'
        (Nothing, _) -> (Nothing, rest)
    parseAddSub' left rest = (Just left, rest)
```

**Fix:** Proper left-associative parsing with explicit token threading

---

## Bug #2: Division by Zero Not Handled

### Before:
```haskell
eval :: Expr -> Double
eval (Div a b) = eval a / eval b  -- No check!
```

**Problem:** Expression "5 / 0" produces `Infinity` instead of error

### After:
```haskell
eval :: Expr -> Either String Double
eval (Div a b) = do
  dividend <- eval a
  divisor <- eval b
  if divisor == 0
    then Left "Error: Division by zero"
    else Right (dividend / divisor)
```

**Fix:** Either monad for error handling, explicit zero check

---

## Bug #3: tokensUsed Function Fundamentally Broken

### Before:
```haskell
tokensUsed :: Expr -> Int
tokensUsed (Num _) = 1
tokensUsed (Add _ _) = 3  -- WRONG: doesn't recurse!
tokensUsed (Sub _ _) = 3
tokensUsed (Mul _ _) = 3
tokensUsed (Div _ _) = 3
```

**Problem:** 
- For `Add (Num 2) (Mul (Num 3) (Num 4))` returns 3, should be 5
- Non-recursive counting

### After:
```haskell
-- REMOVED ENTIRELY
-- Instead, parser returns remaining tokens:
parseMulDiv :: [Token] -> (Maybe Expr, [Token])
```

**Fix:** Eliminated the function, threading tokens through parser instead

---

## Bug #4: Negative Numbers Only Work at Start

### Before:
```haskell
tokenize s@(c:cs)
  | c == '-' && not (null cs) && isDigit (head cs) = 
      let (num, rest) = span isDigit cs in ("-" ++ num) : tokenize rest
```

**Problem:** "5 + -3" tokenizes as ["5", "+", "-", "3"] causing parse error

### After:
```haskell
-- In tokenizer: treat - as operator
tokenize' (c:cs) pos
  | c `elem` "+-*/()" = ([c] :) <$> tokenize' cs (pos + 1)

-- In parser: handle unary minus
parseFactor :: [Token] -> (Maybe Expr, [Token])
parseFactor ("-":ts) = 
  case parseFactor ts of
    (Just e, rest) -> (Just (Mul (Num (-1)) e), rest)
    _ -> (Nothing, ts)
```

**Fix:** Unary minus handled in parser, not tokenizer

---

## Bug #5: Parentheses Parsing Broken

### Before:
```haskell
parseFactor ("(":ts) = do
  e <- parseExpr ts
  let rest = drop (tokensUsed e) ts  -- Uses broken tokensUsed!
  case rest of
    (")":rest') -> Just e
    _ -> Nothing
```

**Problem:** "(2 + 3) * 4" fails because can't find closing parenthesis

### After:
```haskell
parseFactor :: [Token] -> (Maybe Expr, [Token])
parseFactor ("(":ts) = 
  case parseExpr ts of
    (Just e, ")":rest) -> (Just e, rest)
    _ -> (Nothing, ts)
```

**Fix:** Properly consumes tokens and returns remaining

---

## Bug #6: No Empty Input Validation

### Before:
```haskell
main = do
  args <- getArgs
  if null args
    then putStrLn "Usage: evaluator \"expression\""
    else do
      let tokens = tokenize (head args)  -- No check for empty string!
```

**Problem:** `evaluator ""` crashes with confusing error

### After:
```haskell
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: evaluator \"expression\""
    (expr:_) -> do
      let trimmed = filter (not . isSpace) expr
      if null trimmed
        then putStrLn "Error: Empty expression"
        else evaluateExpression trimmed modifier
```

**Fix:** Explicit validation before processing

---

## Bug #7: SECRET_MODIFIER Applied Silently

### Before:
```haskell
main = do
  secret <- catch (getEnv "SECRET_MODIFIER")
                (\(_ :: SomeException) -> return "1.0")
  -- ... later ...
  let result = eval expr * read secret  -- Silent!
  print result
```

**Problem:** User expects "5 + 5" = 10, gets 20 with no explanation

### After:
```haskell
main :: IO ()
main = do
  modifier <- catch (fmap read (getEnv "SECRET_MODIFIER") :: IO Double)
                   (\(_ :: SomeException) -> return 1.0)
  -- ... after evaluation ...
  Right result -> do
    when (modifier /= 1.0) $
      putStrLn $ "Note: Result modified by SECRET_MODIFIER (" 
                 ++ show modifier ++ ")"
    print (result * modifier)
```

**Fix:** Transparent notification when modifier is active

---

## Bug #8: No Floating-Point Support

### Before:
```haskell
tokenize s@(c:cs)
  | isDigit c = let (num, rest) = span isDigit s in num : tokenize rest
  -- Stops at decimal point!
```

**Problem:** "2.5 + 3.5" tokenizes as ["2", ".", "5", "+", "3", ".", "5"]

### After:
```haskell
tokenize' (c:cs) pos
  | isDigit c || c == '.' = 
      let (num, rest) = span (\x -> isDigit x || x == '.') (c:cs)
          dotCount = length (filter (== '.') num)
      in if dotCount > 1
           then Left $ "Invalid number '" ++ num ++ "' (multiple decimal points)"
           else (num :) <$> tokenize' rest (pos + length num)
```

**Fix:** Include decimal points in number spans, with validation

---

## Bug #9: Unused Accumulator Parameter

### Before:
```haskell
parseAddSub :: [String] -> [String] -> Maybe Expr
parseAddSub ts acc = do  -- 'acc' never used!
  e <- parseMulDiv ts
  -- ... acc is passed but never read ...
```

**Problem:** Dead code, confusing interface

### After:
```haskell
parseAddSub :: [String] -> (Maybe Expr, [String])
parseAddSub ts = 
  case parseMulDiv ts of
    (Nothing, rest) -> (Nothing, rest)
    (Just left, rest) -> parseAddSub' left rest
```

**Fix:** Removed unused parameter entirely

---

## Bug #10: Meaningless Empty List Parameter

### Before:
```haskell
parseExpr :: [String] -> Maybe Expr
parseExpr [] = Nothing
parseExpr ts = parseAddSub ts []  -- What does [] mean?
```

**Problem:** Passes meaningless empty list to unused parameter

### After:
```haskell
parseExpr :: [String] -> Maybe Expr
parseExpr [] = Nothing
parseExpr ts = fst (parseAddSub ts)
```

**Fix:** Clean call without mysterious parameters

---

## Bug #11: Generic Error Messages

### Before:
```haskell
tokenize s@(c:cs)
  | otherwise = error "Invalid character"  -- Which one?
```

**Problem:** "2 + 3 & 4" gives "Invalid character" with no context

### After:
```haskell
tokenize' (c:cs) pos
  | otherwise = 
      Left $ "Invalid character '" ++ [c] ++ "' at position " ++ show pos
```

**Fix:** Detailed error with character and position

---

## Bug #12: No Decimal Point Validation

### Before:
```haskell
-- If we naively added decimal support:
tokenize s@(c:cs)
  | isDigit c = let (num, rest) = span (\x -> isDigit x || x == '.') s 
                in num : tokenize rest
  -- Would accept "2.3.4"!
```

**Problem:** Malformed numbers like "2.3.4" would be tokenized, then crash on `read`

### After:
```haskell
tokenize' (c:cs) pos
  | isDigit c || c == '.' = 
      let (num, rest) = span (\x -> isDigit x || x == '.') (c:cs)
          dotCount = length (filter (== '.') num)
      in if dotCount > 1
           then Left $ "Invalid number '" ++ num ++ "' (multiple decimal points)"
           else (num :) <$> tokenize' rest (pos + length num)
```

**Fix:** Validates exactly one decimal point per number

---

## Summary of Changes

| Bug | Category | Severity | Fix Approach |
|-----|----------|----------|--------------|
| #1 | Parsing | HIGH | Rewrite parser with proper recursion |
| #2 | Safety | HIGH | Add Either monad for errors |
| #3 | Design | HIGH | Remove function, thread tokens |
| #4 | Parsing | MEDIUM | Move unary minus to parser |
| #5 | Parsing | HIGH | Fix token consumption |
| #6 | Validation | MEDIUM | Add input validation |
| #7 | UX | MEDIUM | Add transparency notification |
| #8 | Features | MEDIUM | Extend tokenizer for floats |
| #9 | Code Quality | LOW | Remove dead parameter |
| #10 | Code Quality | LOW | Clean up interface |
| #11 | UX | MEDIUM | Detailed error messages |
| #12 | Validation | MEDIUM | Validate decimal points |

All fixes maintain functional programming principles: totality, immutability, composability, and type safety.
