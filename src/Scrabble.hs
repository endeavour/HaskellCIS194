{-# OPTIONS_GHC -Wall #-}            -- Shows all warnings
{-# LANGUAGE ViewPatterns #-}        -- Neat, let's us do something similar to F# Active Patterns :)

module Scrabble where

import Data.Monoid
import Data.Char

type Score = Sum Int

getScore :: Score -> Int
getScore (Sum n) = n

score :: Char -> Score
score (toLower -> c) | elem c ['a', 'e', 'i', 'o', 'u', 'l', 'n', 's', 't', 'r'] = Sum 1
score (toLower -> c) | elem c ['d', 'g'] = Sum 2
score (toLower -> c) | elem c ['b', 'c', 'm', 'p'] = Sum 3
score (toLower -> c) | elem c ['f', 'h', 'v', 'w', 'y'] = Sum 4
score (toLower -> c) | elem c ['k'] = Sum 5
score (toLower -> c) | elem c ['j', 'x'] = Sum 8
score (toLower -> c) | elem c ['q', 'z'] = Sum 10
score _ = Sum 0

scoreString :: String -> Score
scoreString str = mconcat $ score <$> str