{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Homework7 where
import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) x@(Single m1 _) y@(Single m2 _) = Append (m1 <> m2) x y
(+++) x@(Single m1 a1) y@(Append m2 _ _) = Append (m1 <> m2) x y
(+++) x@(Append m1 _ _) y@(Single m2 _) = Append (m1 <> m2) x y
(+++) x@(Append m1 _ _) y@(Append m2 _ _) = Append (m1 <> m2) x y

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n _ | n < 0 = Nothing
indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ a) = Nothing
indexJ n Append x y = ...