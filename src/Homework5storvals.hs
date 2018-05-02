{-# LANGUAGE FlexibleInstances #-}

module Homework5storvals where

import qualified Data.Map as M
import Homework5 (Expr, lit, mul, add)

class HasVars a where
  var :: String -> a

------------

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

------------

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n = \m -> Just n
  add f g = \m -> (+) <$> (f m) <*> (g m)
  mul f g = \m -> (*) <$> (f m) <*> (g m)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer

withVars vs exp = exp $ M.fromList vs

ex1 = withVars [("x", 6)] $ add (lit 3) (var "x")
ex2 = withVars [("x", 6)] $ add (lit 3) (var "y")
ex3 = withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
