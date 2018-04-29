{-# LANGUAGE ScopedTypeVariables #-}

module Homework1 where

import Data.Functor
import Data.Monoid
import Data.Foldable
import Control.Monad

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 9 = [n]
  | otherwise =
    (n `div` maxPowerOfTen) : toDigits (n `mod` maxPowerOfTen)
      where
      maxPowerOfTen :: Integer = 10 ^ (floor $ logBase 10.0 (fromIntegral n))

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = (x:[])
doubleEveryOther (x:y:zs) = (x * 2) : y : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits xs = getSum $ mconcat $ fmap Sum $ join (map toDigits xs)

validate :: Integer -> Bool
validate n = (sumDigits $ doubleEveryOther $ toDigits n) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

-- exercise 6 (optional)
-- TODO