module Homework3 where

import Data.List (take, drop, map, sort)
import Data.List.Index (imap)
import Data.HashMap.Strict (fromList, lookupDefault)
import Data.List.Split (chunksOf)
import Data.List.NonEmpty (group, head)

every :: Int -> [a] -> [a]
every n [] = []
every n xs =
  case drop (n-1) xs of
  [] -> []
  x:xs -> x:every n xs

skips :: [a] -> [[a]]
skips xs = imap (\i x -> every (i+1) xs) xs

windowed :: Int -> [a] -> [[a]]
windowed n [] = []
windowed n l@(x:xs) = take n l : if n < length l then windowed n xs else []

localMaxima :: [Integer] -> [Integer]
localMaxima xs =
  concatMap (\ls ->
    case ls of
    x:y:z:_ | y > x && y > z -> [y]
    _ -> []) (windowed 3 xs)

histogram :: [Integer] -> String
histogram xs =
  let frequencies        = map (\x -> (Data.List.NonEmpty.head x, length x)) $ group $ sort xs
      graphHeight        = maximum $ map (\(_, freq) -> freq) frequencies
      freqMap            = fromList frequencies
      graphChar n height = if lookupDefault 0 n freqMap >= height then '*' else ' '
      graphRows          = [graphChar num row | row <- [graphHeight , graphHeight - 1 .. 1],
                                                num <- [0..9]]
      axis               = "=========="
      labels             = "0123456789"
  in unlines $ chunksOf 10 $ graphRows ++ axis ++ labels
