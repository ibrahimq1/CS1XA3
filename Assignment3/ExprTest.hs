module ExprTest where

import           ExprDiff
import           ExprParser
import           ExprPretty
import           ExprType

import qualified Data.Map.Strict as Map
import           Test.QuickCheck


{-
  Module: ExprTest
  Tests
  Description : Contains Tests for the methods.
  Copyright : (c) Quazi Rafid Ibrahim @2018
  License : WTFPL
  Maintainer : ibrahimq@mcmaster.ca
  Stability : experimental
  Portability : POSIX
-}

-- | Takes 2 expressions 
sampleExpr1 :: Expr Double
sampleExpr1 = ((var "x") !+ (var "y")) 


-- | Retrives from the list
listToExpr1 :: [Double] -> Expr Double
listToExpr1 [x]    = Const x
listToExpr1 (x:xs) = Add (Const x) (listToExpr1 xs)
listToExpr1 []     = error "Not list to expression for empty"


-- | Tests A+B
evalProp1 :: Double -> Double -> Bool 
evalProp1 a b = eval (Map.fromList [("x",a),("y",b)]) (Add (Var "x") (Var "y")) == a + b
testEvalProp1 = quickCheck evalProp1

-- | Tests A * B 
evalProp2 :: Double -> Double -> Bool 
evalProp2 a b = eval (Map.fromList [("x",a),("y",b)]) (Mult (Var "x") (Var "y")) == a * b
testEvalProp2 = quickCheck evalProp2

-- | Tests Cos(a) 
evalProp6 :: Double -> Bool 
evalProp6 a = eval (Map.fromList [("x",a)]) (Cos (Var "x")) == cos(a)
testEvalProp6 = quickCheck evalProp6

-- | Tests Sin(a) 
evalProp7 :: Double -> Bool
evalProp7 a = eval (Map.fromList [("x",a)]) (Sin (Var "x")) == sin(a)
testEvalProp7 = quickCheck evalProp7

