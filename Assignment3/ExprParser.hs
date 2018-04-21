module ExprParser (parseExprD,parseExprF) where

import ExprType
import Text.Parsec
import Text.Parsec.String
import ExprPretty


{-
  Module: ExprParser
  Expression Types
  Description : Contains the parse method and algorithm.
  Copyright : (c) Quazi Rafid Ibrahim @2018
  License : WTFPL
  Maintainer : ibrahimq@mcmaster.ca
  Stability : experimental
  Portability : POSIX



  Expression Datatype
  -------------------
  Wraps different operations in a expression tree
  Ops:
    Add - standard binary addition
    Mult - standard binary multiplication
    Const - wrapper for a simple values
    Var - string identifier for variables
-}


-- | Parse Expression for Doubles
parseExprD :: String -> Expr Double 
parseExprD ss = case parse exprD "" ss of
                  Left err -> error $ show err
                  Right expr -> expr
-- | Parse Expression for Floats
parseExprF :: String -> Expr Float  
parseExprF ss = case parse exprF "" ss of
                  Left err -> error $ show err
                  Right expr -> expr

-- | Double Expression types
exprD :: Parser (Expr Double) 
exprD = exprVar <|> exprConstD <|> exprTrigD <|> exprOpD
-- | Float Expression types
exprF :: Parser (Expr Float) 
exprF = exprVar <|> exprConstF <|> exprTrigF <|> exprOpF


-- !!!!!! EXPR PARSING DOUBLES !!!!!! --



-- | Add or Mul Reader for Parse
exprOpD :: Parser (Expr Double) 
exprOpD = do {
                s <- symbol "Add" <|> symbol "Mult";
                ss <- between (symbol "(") (symbol ")") (exprVar <|> exprConstD <|> exprOpD <|> exprOpD);
                ss' <- between (symbol "(") (symbol ")") (exprVar <|> exprConstD <|> exprOpD <|> exprOpD);

                if s == "Add" then
                  return (Add ss ss'); 
                else 
                  return (Mult ss ss');
              }
-- | Cos or Sin Reader for Parse
exprTrigD :: Parser (Expr Double) 
exprTrigD = do {
               s <- symbol "Cos" <|> symbol "Sin";
               ss <- exprD;

               if s == "Cos" then
                 return (Cos ss);
               else
                 return (Sin ss);
             }



-- | Const reader for parse
exprConstD :: Parser (Expr Double) 
exprConstD = do {
               symbol "Const";
               ss <- double;
               return (Const ss);
             }


-- !!!!!! EXPR PARSING FLOATS !!!!!! --

{-
  exprTrigD is a function which deals with the parsing
  of trigonometic functions.
  More specifically, it is able to parse:
    - Sine   (Sin)
    - Cosine (Cos)
-}

-- | Float reader for parse
exprOpF :: Parser (Expr Float) 
exprOpF = do {
                s <- symbol "Add" <|> symbol "Mult";
                ss <- between (symbol "(") (symbol ")") (exprVar <|> exprConstF <|> exprOpF);
                ss' <- between (symbol "(") (symbol ")") (exprVar <|> exprConstF <|> exprOpF);

                {-
                  This parser can only have the option of
                  Adding or Multiplying, so it's necessary
                  to check whether the initial input was
                  "Add" or "Mult", and return the correct
                  result accordingly
                -}
                
                
                if s == "Add" then
                  return (Add ss ss'); 
                else 
                  return (Mult ss ss')
              }
-- | Cos or Sign Expression for floats
exprTrigF :: Parser (Expr Float) 
exprTrigF = do {
               s <- symbol "Cos" <|> symbol "Sin";
               ss <- between (symbol "(") (symbol ")") (exprF);

               if s == "Cos" then
                 return (Cos ss);
               else
                 return (Sin ss);
             }


-- | Constant Expression for float
exprConstF :: Parser (Expr Float) 
exprConstF = do {
               symbol "Const";
               ss <- float;
               return (Const ss);
             }




-- !!!!!! EXPR PARSING GENERALIZED !!!!!! --
-- | Abstracted and available for use whether for float values or double
exprVar :: Parser (Expr a) 
exprVar = do {
               symbol "Var";
               ss <- many1 letter;
               return (Var ss);
             }


{- Utility Combinators -}

parens :: Parser a -> Parser a
parens p = do { char '(';
               cs <- p;
               char ')';
               return cs }

symbol :: String -> Parser String
symbol ss = let
 symbol' :: Parser String
 symbol' = do { spaces;
                ss' <- string ss;
                spaces;
                return ss' }
 in try symbol'

removeRight (Right ss) = ss

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { neg <- symbol "-";
                dig <- digits;
                return (neg ++ dig) }

integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits

decimalDigits :: Parser String
decimalDigits = do { d <- char '.';
                     rm <- digits;
                     return $ d:rm }

decimalDigits' :: Parser String
decimalDigits' = do { ds <- try negDigits <|> digits;
                   rs <- try decimalDigits <|> return "";
                   return $ ds ++ rs }

double :: Parser Double
double = fmap read $ decimalDigits'

float :: Parser Float
float = fmap read $ decimalDigits'