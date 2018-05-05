{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Homework7 where
import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _ ) = Nothing
indexJ i (Append _ x y)
  | i < (getSize $ size $ tag x)  = indexJ i x
  | otherwise                     = indexJ (i - (getSize $ size $ tag x)) y

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl
dropJ _ Empty = Empty
dropJ n (Single _ _) = Empty
dropJ n jl | n >= (getSize $ size $ tag jl) = Empty
dropJ n (Append m x y)
  | n > (getSize $ size $ tag x) = dropJ (n - (getSize $ size $ tag x)) y
  | otherwise = (dropJ n x) +++ y

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl | n <= 0 = Empty
takeJ _ Empty = Empty
takeJ _ x@(Single _ _) = x
takeJ n jl | n >= (getSize $ size $ tag jl) = jl
takeJ n (Append m x y)
  | n <= (getSize $ size $ tag x) = takeJ n x
  | otherwise = x +++ (takeJ (n - (getSize $ size $ tag x)) y)

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

instance Buffer (JoinList (Score, Size) String)
  where
  -- | Convert a buffer to a String.
  toString = unlines . jlToList

  -- | Create a buffer from a String.
  fromString = fromLines . lines where
    fromLines [] = Empty
    fromLines (l:[]) = Single (scoreString l, Size 1) l
    fromLines ls = fromLines (take half ls) +++
                   fromLines (drop half ls) where
                     half = length ls `div` 2
  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line = indexJ

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine _ _ Empty = Empty
  replaceLine n _ buf | n < 0 || n >= value buf = buf
  replaceLine n ln buf =
    takeJ n buf +++ fromString ln +++ dropJ (n + 1) buf

  numLines = getScore . fst . tag
  value = getSize . snd . tag

main =
  runEditor editor initialBuffer
  where
    initialBuffer :: JoinList (Score, Size) String = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]