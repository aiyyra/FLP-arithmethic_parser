module Main where

import Data.List (isInfixOf)
import Evaluator qualified as Eval
import Parser (parse)
import Test.HUnit
import Tokenizer (tokenize)

-- Helper function to evaluate an expression string
evalExpr :: String -> Either String Double
evalExpr expr = do
  tokens <- tokenize expr
  ast <- parse tokens
  Eval.eval ast

-- Helper function to convert character to lowercase
toLowerChar :: Char -> Char
toLowerChar c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- Test Suite 1: Bug Fixes - Basic Arithmetic
testBasicArithmetic :: Test
testBasicArithmetic =
  TestList
    [ TestCase $ assertEqual "Addition" (Right 5.0) (evalExpr "2+3"),
      TestCase $ assertEqual "Subtraction" (Right 1.0) (evalExpr "5-4"),
      TestCase $ assertEqual "Multiplication" (Right 12.0) (evalExpr "3*4"),
      TestCase $ assertEqual "Division" (Right 2.5) (evalExpr "5/2")
    ]

-- Test Suite 2: Bug Fixes - Operator Precedence
testPrecedence :: Test
testPrecedence =
  TestList
    [ TestCase $ assertEqual "Multiplication before addition" (Right 11.0) (evalExpr "2+3*3"),
      TestCase $ assertEqual "Division before subtraction" (Right 8.0) (evalExpr "10-4/2"),
      TestCase $ assertEqual "Left-to-right for same precedence" (Right 2.0) (evalExpr "10-4-4"),
      TestCase $ assertEqual "Parentheses override precedence" (Right 20.0) (evalExpr "(2+3)*4")
    ]

-- Test Suite 3: Bug Fixes - Division by Zero
testDivisionByZero :: Test
testDivisionByZero =
  TestList
    [ TestCase $
        assertBool
          "Division by zero should fail"
          ( case evalExpr "5/0" of
              Left err -> "zero" `elem` words (map toLowerChar err)
              Right _ -> False
          ),
      TestCase $
        assertBool
          "Division by zero in complex expression"
          ( case evalExpr "2+3/0" of
              Left _ -> True
              Right _ -> False
          )
    ]

-- Test Suite 4: Bug Fixes - Floating Point Support
testFloatingPoint :: Test
testFloatingPoint =
  TestList
    [ TestCase $ assertEqual "Decimal numbers" (Right 3.5) (evalExpr "1.5+2.0"),
      TestCase $ assertEqual "Decimal multiplication" (Right 7.5) (evalExpr "2.5*3.0"),
      TestCase $ assertEqual "Decimal division" (Right 2.5) (evalExpr "5.0/2.0")
    ]

-- Test Suite 5: Bug Fixes - Negative Numbers
testNegativeNumbers :: Test
testNegativeNumbers =
  TestList
    [ TestCase $ assertEqual "Negative number at start" (Right (-5.0)) (evalExpr "-5"),
      TestCase $ assertEqual "Negative in expression" (Right (-2.0)) (evalExpr "3+(-5)"),
      TestCase $ assertEqual "Negative multiplication" (Right (-15.0)) (evalExpr "-5*3"),
      TestCase $ assertEqual "Double negative" (Right 5.0) (evalExpr "-(-5)")
    ]

-- Test Suite 6: Bug Fixes - Parentheses
testParentheses :: Test
testParentheses =
  TestList
    [ TestCase $ assertEqual "Simple parentheses" (Right 15.0) (evalExpr "(3+2)*3"),
      TestCase $ assertEqual "Nested parentheses" (Right 14.0) (evalExpr "((2+3)*2)+4"),
      TestCase $ assertEqual "Multiple parentheses" (Right 25.0) (evalExpr "(2+3)*(4+1)")
    ]

-- Test Suite 7: Bug Fixes - Empty Input Validation
testEmptyInput :: Test
testEmptyInput =
  TestList
    [ TestCase $
        assertBool
          "Empty string should fail"
          ( case evalExpr "" of
              Left _ -> True
              Right _ -> False
          )
    ]

-- Test Suite 8: Extension 1 - Exponentiation Basic
testExponentiationBasic :: Test
testExponentiationBasic =
  TestList
    [ TestCase $ assertEqual "2^3" (Right 8.0) (evalExpr "2^3"),
      TestCase $ assertEqual "5^2" (Right 25.0) (evalExpr "5^2"),
      TestCase $ assertEqual "10^0" (Right 1.0) (evalExpr "10^0"),
      TestCase $ assertEqual "2^-1" (Right 0.5) (evalExpr "2^-1"),
      TestCase $ assertEqual "4^0.5" (Right 2.0) (evalExpr "4^0.5")
    ]

-- Test Suite 9: Extension 1 - Exponentiation Right-Associativity
testExponentiationAssociativity :: Test
testExponentiationAssociativity =
  TestList
    [ TestCase $ assertEqual "2^3^2 should be 2^(3^2)=512" (Right 512.0) (evalExpr "2^3^2"),
      TestCase $ assertEqual "2^2^3 should be 2^(2^3)=256" (Right 256.0) (evalExpr "2^2^3")
    ]

-- Test Suite 10: Extension 1 - Exponentiation Precedence
testExponentiationPrecedence :: Test
testExponentiationPrecedence =
  TestList
    [ TestCase $ assertEqual "2+3^2 should be 2+9=11" (Right 11.0) (evalExpr "2+3^2"),
      TestCase $ assertEqual "2*3^2 should be 2*9=18" (Right 18.0) (evalExpr "2*3^2"),
      TestCase $ assertEqual "10-2^3 should be 10-8=2" (Right 2.0) (evalExpr "10-2^3"),
      TestCase $ assertEqual "(2+3)^2 should be 25" (Right 25.0) (evalExpr "(2+3)^2")
    ]

-- Test Suite 11: Extension 1 - Exponentiation Edge Cases
testExponentiationEdgeCases :: Test
testExponentiationEdgeCases =
  TestList
    [ TestCase $ assertEqual "0^0 should be 1" (Right 1.0) (evalExpr "0^0"),
      TestCase $ assertEqual "(-2)^2 should be 4" (Right 4.0) (evalExpr "(-2)^2"),
      TestCase $
        assertBool
          "(-2)^0.5 should fail (NaN)"
          ( case evalExpr "(-2)^0.5" of
              Left err -> let lowerErr = map toLowerChar err in "nan" `isInfixOf` lowerErr
              Right _ -> False
          )
    ]

-- Test Suite 12: Extension 2 - Basic Functions
testBasicFunctions :: Test
testBasicFunctions =
  TestList
    [ TestCase $ assertEqual "sin(0)" (Right 0.0) (evalExpr "sin(0)"),
      TestCase $ assertEqual "abs(-5)" (Right 5.0) (evalExpr "abs(-5)"),
      TestCase $ assertEqual "abs(5)" (Right 5.0) (evalExpr "abs(5)"),
      TestCase $ assertEqual "abs(0)" (Right 0.0) (evalExpr "abs(0)"),
      TestCase $ assertEqual "sqrt(4)" (Right 2.0) (evalExpr "sqrt(4)"),
      TestCase $ assertEqual "sqrt(0)" (Right 0.0) (evalExpr "sqrt(0)"),
      TestCase $
        assertBool
          "sqrt(2) should be ~1.414"
          ( case evalExpr "sqrt(2)" of
              Right x -> abs (x - 1.414213562373095) < 0.0001
              Left _ -> False
          )
    ]

-- Test Suite 13: Extension 2 - Function Precedence
testFunctionPrecedence :: Test
testFunctionPrecedence =
  TestList
    [ TestCase $ assertEqual "sin(0)+1" (Right 1.0) (evalExpr "sin(0)+1"),
      TestCase $ assertEqual "2*abs(-3)" (Right 6.0) (evalExpr "2*abs(-3)"),
      TestCase $ assertEqual "sqrt(4)^2" (Right 4.0) (evalExpr "sqrt(4)^2"),
      TestCase $ assertEqual "abs(-2)+abs(-3)" (Right 5.0) (evalExpr "abs(-2)+abs(-3)")
    ]

-- Test Suite 14: Extension 2 - Nested Functions
testNestedFunctions :: Test
testNestedFunctions =
  TestList
    [ TestCase $ assertEqual "sqrt(abs(-4))" (Right 2.0) (evalExpr "sqrt(abs(-4))"),
      TestCase $
        assertBool
          "abs(sin(-1)) should be ~0.841"
          ( case evalExpr "abs(sin(-1))" of
              Right x -> abs (x - 0.8414709848078965) < 0.0001
              Left _ -> False
          )
    ]

-- Test Suite 15: Extension 2 - Function Edge Cases
testFunctionEdgeCases :: Test
testFunctionEdgeCases =
  TestList
    [ TestCase $
        assertBool
          "sqrt(-1) should fail"
          ( case evalExpr "sqrt(-1)" of
              Left err -> "negative" `elem` words (map toLowerChar err)
              Right _ -> False
          ),
      TestCase $
        assertBool
          "sqrt(-4) should fail"
          ( case evalExpr "sqrt(-4)" of
              Left _ -> True
              Right _ -> False
          )
    ]

-- Test Suite 16: Complex Expressions
testComplexExpressions :: Test
testComplexExpressions =
  TestList
    [ TestCase $ assertEqual "2*sqrt(16)+abs(-3)" (Right 11.0) (evalExpr "2*sqrt(16)+abs(-3)"),
      TestCase $ assertEqual "sin(0)+2^3*4" (Right 32.0) (evalExpr "sin(0)+2^3*4"),
      TestCase $ assertEqual "sqrt(16)/2" (Right 2.0) (evalExpr "sqrt(16)/2"),
      TestCase $ assertEqual "abs(-5)^2" (Right 25.0) (evalExpr "abs(-5)^2"),
      TestCase $ assertEqual "(2+3)^2-sqrt(16)" (Right 21.0) (evalExpr "(2+3)^2-sqrt(16)")
    ]

-- Test Suite 17: Tokenization Error Cases
testTokenizationErrors :: Test
testTokenizationErrors =
  TestList
    [ TestCase $
        assertBool
          "Invalid character should fail"
          ( case evalExpr "2+@3" of
              Left _ -> True
              Right _ -> False
          ),
      TestCase $
        assertBool
          "Unknown function should fail"
          ( case evalExpr "cos(0)" of
              Left err -> "unknown" `elem` words (map toLowerChar err) || "function" `elem` words (map toLowerChar err)
              Right _ -> False
          )
    ]

-- Test Suite 18: Parse Error Cases
testParseErrors :: Test
testParseErrors =
  TestList
    [ TestCase $
        assertBool
          "Incomplete expression should fail"
          ( case evalExpr "2+" of
              Left _ -> True
              Right _ -> False
          ),
      TestCase $
        assertBool
          "Mismatched parentheses should fail"
          ( case evalExpr "(2+3" of
              Left _ -> True
              Right _ -> False
          )
    ]

-- Combine all test suites
allTests :: Test
allTests =
  TestList
    [ TestLabel "Basic Arithmetic" testBasicArithmetic,
      TestLabel "Operator Precedence" testPrecedence,
      TestLabel "Division by Zero" testDivisionByZero,
      TestLabel "Floating Point Support" testFloatingPoint,
      TestLabel "Negative Numbers" testNegativeNumbers,
      TestLabel "Parentheses" testParentheses,
      TestLabel "Empty Input Validation" testEmptyInput,
      TestLabel "Exponentiation Basic" testExponentiationBasic,
      TestLabel "Exponentiation Right-Associativity" testExponentiationAssociativity,
      TestLabel "Exponentiation Precedence" testExponentiationPrecedence,
      TestLabel "Exponentiation Edge Cases" testExponentiationEdgeCases,
      TestLabel "Basic Functions" testBasicFunctions,
      TestLabel "Function Precedence" testFunctionPrecedence,
      TestLabel "Nested Functions" testNestedFunctions,
      TestLabel "Function Edge Cases" testFunctionEdgeCases,
      TestLabel "Complex Expressions" testComplexExpressions,
      TestLabel "Tokenization Errors" testTokenizationErrors,
      TestLabel "Parse Errors" testParseErrors
    ]

-- Main function to run all tests
main :: IO ()
main = do
  putStrLn "Running HUnit Tests for Arithmetic Expression Evaluator"
  putStrLn "========================================================\n"
  counts <- runTestTT allTests
  putStrLn ""
  putStrLn "========================================================"
  putStrLn $ "Tests run: " ++ show (cases counts)
  putStrLn $ "Failures: " ++ show (failures counts)
  putStrLn $ "Errors: " ++ show (errors counts)
  if failures counts + errors counts == 0
    then putStrLn "\n✓ All tests passed!"
    else putStrLn "\n✗ Some tests failed."
