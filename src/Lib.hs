module Lib
    ( someFunc
    ) where

import Data.Functor
import Data.Monoid
import Lesson1
import Homework1
import Homework2
import Log

someFunc :: IO ()
someFunc = do
--  putStrLn "Hello"
--  putStrLn "World"
--  putStrLn (show $ sumtorial 10)
--  putStrLn (foldMap show (fmap hailstone [1..10]))
--  putStrLn (show $ foo 38)
--  putStrLn (show $ sumPair (3,4))
  putStrLn (show $ doubleEveryOther [1,3,8,6])
  putStrLn (show $ toDigits 1234)
  putStrLn (show $ toDigitsRev 1234)
  putStrLn (show $ sumDigits [16,7,12,5])
  putStrLn (show $ validate 4012888888881881)
  putStrLn (show $ validate 4012888888881882)
  putStrLn (show $ hanoi 2 "a" "b" "c")

  -- xs <- testParse parse 1000000 "homework2/error.log"
  -- mapM_ (putStrLn . show) xs

  wrong <- testWhatWentWrong parse whatWentWrong "homework2/sample.log"

  mapM_ (putStrLn . show) wrong
