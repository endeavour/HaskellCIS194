module Homework6 where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = fib <$> [0..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons h t) = h:streamToList t

instance Show a => Show (Stream a) where
  show stream = show $ take 20 $ streamToList stream

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons h t)= Cons (f h) (streamMap f t)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f $ f x)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons h1 t1) s2 = Cons h1 (interleaveStreams s2 t1)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)