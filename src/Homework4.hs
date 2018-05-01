module Homework4 where

--import Data.List
import Data.Monoid

fun1 :: [Integer] -> Integer
fun1 xs = getProduct $ mconcat $ map (\x -> Product (x - 2)) $ filter even xs

-- https://en.wikipedia.org/wiki/Collatz_conjecture
-- Key insight to solving this is figuring out what this function actually does...
-- That is, given a Collatz sequence beginning at n, what is the sum of all the even values in the sequence?
fun2 :: Integer -> Integer
fun2 =
  sum . filter even . takeWhile ((/=) 1) . iterate collatz
  where
    collatz n
      | even n = n `div` 2
      | otherwise = 3*n + 1

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree xs =
  foldr insertNode Leaf xs
  where
    depth Leaf = -1
    depth (Node n _ _ _) = n
    insertNode x Leaf = Node 0 Leaf x Leaf
    insertNode x (Node height left val right)
      | depth left <= depth right =
        let new_left = (insertNode x left)
            new_height = depth new_left + 1
        in Node new_height new_left val right
      | otherwise =
        let new_right = (insertNode x right)
            new_height = depth new_right + 1
        in Node new_height left val new_right

xor :: [Bool] -> Bool
xor xs = foldl (/=) False xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\next state -> f next : state) []

-- TODO
-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr ...

-- Not quite a cartesian product as such... not sure what the mathematical name for this is?
cartProd :: Integer -> [(Integer, Integer)]
cartProd n = [(i,j) | i <- [1..n], j <- [i..n]]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let prods    = map (\(i,j) -> i + j + 2*i*j) $ cartProd n
      leftover = filter (\x -> not $ elem x prods) [1..n]
  in map (\n -> 2*n + 1) leftover
