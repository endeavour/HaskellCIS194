module Homework4 where

import Data.Monoid

fun1 :: [Integer] -> Integer
fun1 xs = getProduct $ mconcat $ map (\x -> Product (x - 2)) $ filter even xs

--TODO: simplify this...
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)
