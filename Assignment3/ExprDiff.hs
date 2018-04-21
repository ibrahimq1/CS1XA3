{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-
  Module: Differentiable Expressions
  Description : Contains a type class and instances for
  differentiable expressions
  Copyright : (c) Quazi Rafid Ibrahim @2018
  License : WTFPL
  Maintainer : ibrahimq@mcmaster.ca
  Stability : experimental
  Portability : POSIX
  ----------------------------------------------

  The DiffExpr class cointains methods over the 
  Expr datatype tht helps with making and
  evaluating diferentiable expressions.

  ----------------------------------------------

  Methods:
    eval: Takes a dictionary of variable-identifiers
          and values, and uses it to compute the Expr.

    simplify: Takes a dictionary (complete or incomplete)
              and uses it to reduce Expr as much as
              possible.

              e1 = x + y
              e2 = y + x
              e1 == e2

              Add (Add (Var "x") (Const 1) (Add (Const 2) (Var "y")))
              => Add (Const 3) (Add (Var "x") (Var "y"))
    partDiff: Given a var identifier, differentiate
          in terms of that identifier
    Default Methods:
      !+,!*,var,val: are function wrappers for Expr
                     constructors that perform
                     additional simplification
-}

module ExprDiff where

import ExprType

import qualified Data.Map as Map

-- | Default Methods and the DiffExpr Class
class DiffExpr a where
  eval :: Map.Map String a -> Expr a -> a
  simplify :: Map.Map String a -> Expr a -> Expr a
  partDiff :: String -> Expr a -> Expr a
  

  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  (!^) :: Expr a -- ^ First Expression given
          -> Expr a -- ^ Second Expression given
          -> Expr a -- ^ Resulting simplified Exp Expression
  e1 !^ e2 = simplify (Map.fromList []) $ Exp e1 e2
  -- | corresponds to the Cos type wrappers to optimize Cos type. tries to simplify the Expr
  mycos :: Expr a -- ^ Expression given
          -> Expr a -- ^ Resulting simplified Cos Expression
  mycos e1 = simplify (Map.fromList []) $ Cos e1 
  -- | corresponds to the Sin type wrappers to optimize Sin type. tries to simplify the Expr
  mysin :: Expr a -- ^ Expression given
          -> Expr a -- ^ Resulting simplified Sin Expression
  mysin e1 = simplify (Map.fromList []) $ Sin e1

  -- | corresponds to the NatExp type wrappers to optimize Natexp type. tries to simplify the Expr
  natexp :: Expr a -- ^ Expression given
            -> Expr a -- ^ Resulting simplified NatExp Expression
  natexp e1 = simplify (Map.fromList []) $ NatExp e1   

  val :: a -> Expr a
  val x = Const x
  var :: String -> Expr a
  var x = Var x

  
  
-- | Evaluation 
instance (Eq a, Floating a) => DiffExpr a where
  -- ^ Evaluates The Parsed Addition Expression
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2 
  -- ^ Evaluates The Parsed Mul Expression
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2 
  -- ^ Evaluates The Parsed Expression
  eval vrs (Exp e1 e2) = eval vrs e1 ** eval vrs e2
  -- ^ Evaluates The Parsed Cos Expression 
  eval vrs (Cos x) =  cos (eval vrs x)
  -- ^ Evaluates The Parsed Sin Expression 
  eval vrs (Sin x) = sin (eval vrs x) 
  -- ^ Evaluates The Parsed e^x Expression
  eval vrs (NatExp x) = exp (eval vrs x) 
  -- ^ Evaluates The Parsed Const Expression
  eval vrs (Const x) = x 
  -- ^ Evaluates The Parsed Var Expression
  eval vrs (Var x) = case Map.lookup x vrs of 
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"


  -- **Part Diff 
  
  partDiff t (Var x) | x == t = Const 1 
                     | otherwise = (Const 0)
  -- | Part Diff The Parse Variable Expression
  partDiff _ (Const _) = Const 0
  partDiff t (Add e1 e2) =(Add (partDiff t e1) (partDiff t e2))
  partDiff t (Mult e1 e2) = (Add (Mult (partDiff t e1) e2) (Mult e1 (partDiff t e2)))
  partDiff t (Cos x) =(Mult (Const (-1) ) (Mult (partDiff t x) (Sin x)))
  partDiff t (Sin x) = (Mult (partDiff t x) (Cos x))
  partDiff t (NatExp x) = Mult (NatExp x) (partDiff t x)

  -- ***Simplify --
  -- | Simplify The Parsed Constant Expression
  simplify vrs (Const x) = (Const x) 
  simplify vrs (Mult (Const 0) e1) = Const 0
  simplify vrs (Mult e1 (Const 0)) = Const 0
  simplify vrs (Mult e1 (Const 1)) = simplify vrs e1
  simplify vrs (Mult (Const 1) e1) =simplify vrs e1
  simplify vrs (Add e1 (Const 0)) = simplify vrs e1
  simplify vrs (Add (Const 0) e1) = simplify vrs e1
  simplify vrs (Exp (e1) (Const 0)) = Const 1
  simplify vrs (Exp (Const 0) e1) = Const 0 


  simplify vrs (Var x) = case Map.lookup x vrs of 
                          Just v -> Const v
                          Nothing -> Var x

  simplify vrs (Add e1 (Var x)) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Add (Const v) (simplify vrs e1))
                                    Nothing -> Add (simplify vrs e1) (Var x)

  simplify vrs (Add (Var x) e1) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Add (simplify vrs e1) (Const v) )
                                    Nothing -> Add (Var x) (simplify vrs e1)


  simplify vrs (Mult (Var x) e1) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Mult (simplify vrs e1) (Const v) )
                                    Nothing -> Mult (Var x) (simplify vrs e1)                

  simplify vrs (Mult e1 (Var x)) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Mult (Const v) (simplify vrs e1) )
                                    Nothing -> Mult (simplify vrs e1) (Var x) 


  simplify vrs (Add e1 e2) = case ((simplify vrs e1),(simplify vrs e2)) of 
                             (Const x , Const y) -> Const (x + y)
                             _ -> Add (simplify vrs e1)  (simplify vrs e2)
  


  simplify vrs (Mult e1 e2) = case ((simplify vrs e1) ,(simplify vrs e2)) of 
                             (Const x,Const y)->(Const (x*y) ) 
                             _ -> Mult (simplify vrs e1)  (simplify vrs e2)



  simplify vrs (Exp (Var x) e1) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Exp (Const v) (simplify vrs e1) )
                                    Nothing -> Exp (Var x) (simplify vrs e1)                

  simplify vrs (Exp e1 (Var x)) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Exp (simplify vrs e1)  (Const v) )
                                    Nothing -> Exp (simplify vrs e1) (Var x)
  simplify vrs (Cos (Var x)) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Cos (Const v) )
                                    Nothing -> Cos (Var x) 
  simplify vrs (Sin (Var x)) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (Sin (Const v) )
                                    Nothing -> Sin (Var x)

  simplify vrs (Exp e1 e2) = case ((simplify vrs e1) ,(simplify vrs e2)) of 
                             (Const x,Const y)->(Const (x**y) ) 
                             _ -> Exp (simplify vrs e1)  (simplify vrs e2)
  simplify vrs (Cos e1) = case (simplify vrs e1) of 
                             Const x -> Const (cos x)
                             _-> Cos (simplify vrs e1)
  simplify vrs (Sin e1) = case (simplify vrs e1) of 
                             Const x -> Const (sin x)
                             _-> Sin (simplify vrs e1)

  simplify vrs (NatExp (Var x)) = case Map.lookup x vrs of 
                                    Just v ->  simplify vrs (NatExp (Const v) )
                                    Nothing -> NatExp (Var x) 