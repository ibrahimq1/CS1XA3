module ExprType where

import Data.List


{-
  Module: ExprType
  Expression Types
  Description : Contains a type types and methods.
  Copyright : (c) Quazi Rafid Ibrahim @2018
  License : WTFPL
  Maintainer : ibrahimq@mcmaster.ca
  Stability : experimental
  Portability : POSIX

-}

-- | This is the data types thing
data Expr a = Add (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)
            | Cos (Expr a) -- | This constructor represents the cosine of an expression of type 'Expr a'
            | Sin (Expr a) -- | This constructor represents the sin of an expression of type 'Expr a'
            | Exp (Expr a) (Expr a) -- | This constructor represents the exponent of two expressions of type 'Expr a'. First expression to the power of second expression.
            | NatExp (Expr a)
            | Const a
            | Var String
  deriving Eq

{-- Getting the Vars --}

-- | Get vars
getVars :: Expr a -> [String]

getVars (Add e1 e2)  = getVars e1 ++ getVars e2
-- ^ Additions
getVars (Mult e1 e2) = getVars e1 ++ getVars e2
-- ^ Multiplication
getVars (Cos e1) = getVars e1
-- ^ Cosine
getVars (Sin e1) = getVars e1
-- ^ Sine
getVars (NatExp e1) = getVars e1
-- ^ Natural Exponents e^x
getVars (Exp e1 e2) = getVars e1 ++ getVars e2
-- ^ Exponents x^n
getVars (Const _)    = []
getVars (Var ident)  = [ident]