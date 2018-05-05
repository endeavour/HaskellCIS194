{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Prelude
import Sized
import Homework7

-- Homework 7 --

instance Arbitrary a => Arbitrary (JoinList Size a) where
  arbitrary =
    let
      empty = do
        return Empty
      single = do
        a <- arbitrary
        return (Single (Size 1) a)
      append = do
        a <- arbitrary
        b <- arbitrary
        return (a +++ b)
    in do
      jl <- oneof [empty, single, append]
      return jl


prop_indexingMatchesListBehaviour :: Int -> JoinList Size Int -> Bool
prop_indexingMatchesListBehaviour i jl = (indexJ i jl) == (jlToList jl !!? i)

prop_dropBehaviourMatchesList :: Int -> JoinList Size Int -> Bool
prop_dropBehaviourMatchesList n jl = jlToList (dropJ n jl) == drop n (jlToList jl)

prop_takeBehaviourMatchesList :: Int -> JoinList Size Int -> Bool
prop_takeBehaviourMatchesList n jl = jlToList (takeJ n jl) == take n (jlToList jl)

main :: IO ()
main = do
  quickCheck prop_indexingMatchesListBehaviour
  quickCheck prop_dropBehaviourMatchesList
  quickCheck prop_takeBehaviourMatchesList
