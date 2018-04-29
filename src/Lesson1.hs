module Lesson1 where


-- Compute the sum of the integers from 1 to n.
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

hailstone :: Integer -> Integer
-- these are called guards
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0             = 0
  | n `mod` 17 == 2   = -43
  | otherwise         = n + 3

p :: (Int, Char)
p = (3, 'x')

sumPair :: (Int,Int) -> Int
sumPair (x,y) = x + y

f :: Int -> Int -> Int -> Int
f x y z  = x + y + z

nums, range, range2 :: [Integer]
nums    = [1,2,3,19]
range   = [1..100]
range2  = [2,4..100]

hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']

hello2 :: String
hello2 = "hello"

helloSame = hello1 == hello2

emptyList = []
ex18 = 1 : []
ex19 = 3 : (1 : [])
ex20 = 1 : 2 : 3 : 4 : []

-- Generate the sequence of hailstone iterations from a starting number.
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- Compute the length of a list of Integers.
intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (x : xs) = 1 + (intListLength xs)

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo (x:[]) = [x]
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs