{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Char (isSpace)
import Evaluator qualified as Eval
import Parser (parse)
import System.Environment (getArgs, getEnv)
import Tokenizer (tokenize)

-- Main CLI
-- FIXES: Bug #6 (empty input validation), Bug #7 (SECRET_MODIFIER transparency)
main :: IO ()
main = do
  -- Get optional modifier with better error handling
  modifier <-
    catch
      (fmap read (getEnv "SECRET_MODIFIER") :: IO Double)
      (\(_ :: SomeException) -> return 1.0)

  args <- getArgs

  case args of
    [] -> putStrLn "Usage: evaluator \"expression\""
    (expr : _) -> do
      let trimmed = filter (not . isSpace) expr

      -- Validate non-empty input
      if null trimmed
        then putStrLn "Error: Empty expression"
        else evaluateExpression trimmed modifier

-- Helper function to evaluate an expression
evaluateExpression :: String -> Double -> IO ()
evaluateExpression expr modifier = do
  case tokenize expr of
    Left err -> putStrLn $ "Tokenization error: " ++ err
    Right tokens -> case parse tokens of
      Left err -> putStrLn $ "Parse error: " ++ err
      Right ast -> case Eval.eval ast of
        Left err -> putStrLn $ "Evaluation error: " ++ err
        Right result -> do
          -- Notify user if result is being modified
          when (modifier /= 1.0) $
            putStrLn $
              "Note: Result modified by SECRET_MODIFIER (" ++ show modifier ++ ")"
          print (result * modifier)
