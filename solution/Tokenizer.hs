module Tokenizer (tokenize, Token) where

import Data.Char (isAlpha, isDigit, isSpace)

type Token = String

-- Tokenize the input string into tokens with proper error handling
-- FIXES: Bug #8 (floating-point support), Bug #11 (detailed errors), Bug #12 (decimal validation)
-- EXTENSION 1: Added support for exponentiation operator (^)
-- EXTENSION 2: Added support for function names (sin, abs, sqrt)
tokenize :: String -> Either String [Token]
tokenize s = tokenize' s 0
  where
    tokenize' :: String -> Int -> Either String [Token]
    tokenize' [] _ = Right []
    tokenize' (c : cs) pos
      | isSpace c = tokenize' cs (pos + 1)
      | c `elem` "+-*/()^" = ([c] :) <$> tokenize' cs (pos + 1)
      | isDigit c || c == '.' =
          let (num, rest) = span (\x -> isDigit x || x == '.') (c : cs)
              dotCount = length (filter (== '.') num)
           in if dotCount > 1
                then Left $ "Invalid number '" ++ num ++ "' (multiple decimal points) at position " ++ show pos
                else
                  if null num || num == "."
                    then Left $ "Invalid number format at position " ++ show pos
                    else (num :) <$> tokenize' rest (pos + length num)
      | isAlpha c =
          -- Parse function names (sin, abs, sqrt)
          let (identifier, rest) = span isAlpha (c : cs)
           in if identifier `elem` ["sin", "abs", "sqrt"]
                then (identifier :) <$> tokenize' rest (pos + length identifier)
                else Left $ "Unknown function '" ++ identifier ++ "' at position " ++ show pos
      | otherwise =
          Left $ "Invalid character '" ++ [c] ++ "' at position " ++ show pos
