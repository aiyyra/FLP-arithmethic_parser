module Evaluator (eval) where

import Parser (Expr (..))

-- Evaluate the expression with proper error handling
-- FIXES: Bug #2 (division by zero)
-- EXTENSION 1: Added support for exponentiation
-- EXTENSION 2: Added support for basic functions (sin, abs, sqrt)
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
    then Left $ "Error: Invalid exponentiation (" ++ show base ++ " ^ " ++ show exponent ++ " results in NaN)"
    -- Check for Infinity (overflow)
    else
      if isInfinite result && result > 0
        then Left $ "Error: Exponentiation overflow (" ++ show base ++ " ^ " ++ show exponent ++ " is too large)"
        else Right result
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
