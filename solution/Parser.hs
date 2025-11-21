module Parser (parse, Expr (..)) where

import Tokenizer (Token)

-- Expression data type
-- EXTENSION 1: Added Exp constructor for exponentiation
-- EXTENSION 2: Added Sin, Abs, Sqrt constructors for basic functions
data Expr
  = Num Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Exp Expr Expr
  | Sin Expr
  | Abs Expr
  | Sqrt Expr
  deriving (Show, Eq)

-- Parse tokens into an expression tree
-- FIXES: Bug #1 (precedence), Bug #3 (tokensUsed), Bug #4 (negatives),
--        Bug #5 (parentheses), Bug #9 (unused accumulator), Bug #10 (meaningless parameter)
parse :: [Token] -> Either String Expr
parse [] = Left "Empty expression"
parse ts = case parseExpr ts of
  (Just expr, []) -> Right expr
  (Just _, remaining) -> Left $ "Unexpected tokens: " ++ unwords remaining
  (Nothing, _) -> Left "Parse error"

-- Top-level expression parser
parseExpr :: [Token] -> (Maybe Expr, [Token])
parseExpr = parseAddSub

-- Parse addition and subtraction (lowest precedence)
-- Thread remaining tokens through for proper left-to-right associativity
parseAddSub :: [Token] -> (Maybe Expr, [Token])
parseAddSub ts =
  case parseMulDiv ts of
    (Nothing, rest) -> (Nothing, rest)
    (Just left, rest) -> parseAddSub' left rest
  where
    parseAddSub' :: Expr -> [Token] -> (Maybe Expr, [Token])
    parseAddSub' left [] = (Just left, [])
    parseAddSub' left ("+" : rest) =
      case parseMulDiv rest of
        (Just right, rest') -> parseAddSub' (Add left right) rest'
        (Nothing, _) -> (Nothing, rest)
    parseAddSub' left ("-" : rest) =
      case parseMulDiv rest of
        (Just right, rest') -> parseAddSub' (Sub left right) rest'
        (Nothing, _) -> (Nothing, rest)
    parseAddSub' left rest = (Just left, rest)

-- Parse multiplication and division (higher precedence)
-- EXTENSION: Updated to call parseExp instead of parseFactor for exponentiation support
parseMulDiv :: [Token] -> (Maybe Expr, [Token])
parseMulDiv ts =
  case parseExp ts of
    (Nothing, rest) -> (Nothing, rest)
    (Just left, rest) -> parseMulDiv' left rest
  where
    parseMulDiv' :: Expr -> [Token] -> (Maybe Expr, [Token])
    parseMulDiv' left [] = (Just left, [])
    parseMulDiv' left ("*" : rest) =
      case parseExp rest of
        (Just right, rest') -> parseMulDiv' (Mul left right) rest'
        (Nothing, _) -> (Nothing, rest)
    parseMulDiv' left ("/" : rest) =
      case parseExp rest of
        (Just right, rest') -> parseMulDiv' (Div left right) rest'
        (Nothing, _) -> (Nothing, rest)
    parseMulDiv' left rest = (Just left, rest)

-- Parse exponentiation (highest precedence, right-associative)
-- EXTENSION: New function for exponentiation with right-associativity
-- Right-associative means: 2^3^2 = 2^(3^2) = 512, not (2^3)^2 = 64
parseExp :: [Token] -> (Maybe Expr, [Token])
parseExp ts =
  case parseFactor ts of
    (Nothing, rest) -> (Nothing, rest)
    (Just base, "^" : rest) ->
      -- Right-associativity: recursively parse the entire right side
      case parseExp rest of
        (Just exponent, rest') -> (Just (Exp base exponent), rest')
        (Nothing, _) -> (Nothing, rest)
    (Just base, rest) -> (Just base, rest)

-- Parse factors: numbers, parentheses, unary minus, and functions
-- EXTENSION 2: Added function parsing (sin, abs, sqrt)
parseFactor :: [Token] -> (Maybe Expr, [Token])
parseFactor [] = (Nothing, [])
parseFactor ("(" : ts) =
  case parseExpr ts of
    (Just e, ")" : rest) -> (Just e, rest)
    _ -> (Nothing, ts)
parseFactor ("-" : ts) =
  case parseFactor ts of
    (Just e, rest) -> (Just (Mul (Num (-1)) e), rest)
    _ -> (Nothing, ts)
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
  | all (\c -> isDigit c || c == '.' || c == '-') t =
      case reads t :: [(Double, String)] of
        [(n, "")] -> (Just (Num n), ts)
        _ -> (Nothing, t : ts)
  | otherwise = (Nothing, t : ts)
  where
    isDigit c = c >= '0' && c <= '9'
