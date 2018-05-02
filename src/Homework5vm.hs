-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Homework5vm where

import Homework5 (Expr, lit, mul, add)
-- import ExprT
import StackVM
import Parser

instance Expr Program where
  lit n   = [PushI n]
  mul x y = x ++ y ++ [StackVM.Mul]
  add x y = x ++ y ++ [StackVM.Add]

compile :: String -> Maybe Program
compile x = parseExp lit add mul x

example = case compile "4*5+1" of
  Just program -> stackVM program
  Nothing -> Left "Could not parse"