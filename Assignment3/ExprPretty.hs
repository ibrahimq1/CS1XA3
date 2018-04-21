module ExprPretty where

import ExprType

{-
  Module: ExprPretty
  Expression Types
  Description : Makes the code Pretty.
  Copyright : (c) Quazi Rafid Ibrahim @2018
  License : WTFPL
  Maintainer : ibrahimq@mcmaster.ca
  Stability : experimental
  Portability : POSIX


  Expression Datatype
  -------------------
  Binds with brackets to make the output look organized.
-}

-- | Add parenthesis on the begining and end. 
parens :: String -> String
parens ss = "(" ++ ss ++ ")" 

-- | Shows Add,Mul,Cos,Sin,Var etc 
instance Show a => Show (Expr a) where
  show (Add e1 e2) = parens (show e1) ++ " !+ " ++ parens (show e2) 
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)  
  show (Cos x)  = "Cos" ++ parens (show x)   
  show (Sin x) = "Sin" ++ parens (show x) 
  show (Var ss)     = parens $ "Var " ++ "\"" ++ ss ++ "\"" 
  show (Const x)    = parens $ "Val. " ++ show x  
  show (NatExp x) = "e^" ++ parens (show x) 
  show (Exp e1 e2) = parens (show e1) ++ " !^ " ++ parens (show e2)  