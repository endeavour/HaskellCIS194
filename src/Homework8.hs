{-# LANGUAGE ScopedTypeVariables #-}
module Homework8 where
import Employee
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons (e@Emp {empFun = empFun}) (GL es glFun) = GL (e:es) (empFun + glFun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2) = if f1 >= f2 then gl1 else gl2

-- Note: This is actually part of the standard library now... (named foldTree)
treeFold :: (s -> a -> s) -> s -> Tree a -> s
treeFold f initialState tree =
  foldl f initialState flattenedTree
  where
    flattenedTree = flatten tree

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel employee guestlists = undefined

maxFun :: Tree Employee -> GuestList
maxFun = undefined

