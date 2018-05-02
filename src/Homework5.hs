module Homework5 where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Mul x y) = eval x * eval y
eval (Add x y) = eval x + eval y

evalStr :: String -> Maybe Integer
evalStr = fmap eval . (parseExp Lit Add Mul)

class Expr expr where
  lit :: Integer -> expr
  add :: expr -> expr -> expr
  mul :: expr -> expr -> expr

instance Expr ExprT where
  lit n = Lit n
  add x y = Add x y
  mul x y = Mul x y

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `rem` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * 7) `rem` 7)

testExp :: Expr a => Maybe a
testExp     = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7
