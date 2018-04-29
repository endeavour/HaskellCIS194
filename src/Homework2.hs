{-# OPTIONS_GHC -Wall #-}            -- Shows all warnings
{-# LANGUAGE ViewPatterns #-}        -- Neat, let's us do something similar to F# Active Patterns :)

module Homework2 where
import Log
import Text.Regex.PCRE
import Data.Maybe

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

rgroups :: String -> String -> [String]
rgroups regex text = result
  where
    matchResult :: (String, String, String, [String])
    matchResult = text =~ regex
    (_, _, _, result) = matchResult

parseMessage :: String -> LogMessage
parseMessage (rgroups "^I\\ (\\d+)\\ (.*)"
              -> [(readMaybe -> Just timestamp), msg])                             = LogMessage Info timestamp msg

parseMessage (rgroups "^W\\ (\\d+)\\ (.*)"
              -> [(readMaybe -> Just timestamp), msg])                             = LogMessage Warning timestamp msg

parseMessage (rgroups "^E\\ (\\d+)\\ (\\d+)\\ (.*)"
              -> [(readMaybe -> Just errNo), (readMaybe -> Just timestamp), msg])  = LogMessage (Error errNo) timestamp msg

parseMessage str                                                                   = Unknown str

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert (LogMessage _ _ _) tree@(Node _ (Unknown _) _) = tree
insert msg@(LogMessage _ _ _) Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ tsNew _) (Node left node@(LogMessage _ tsExisting _) right) =
  if tsNew <= tsExisting then Node (insert msg left) node (right) else Node left node (insert msg right)

build :: [LogMessage] -> MessageTree
build xs = foldl (\tree msg -> insert msg tree) Log.Leaf xs

inOrder :: MessageTree -> [LogMessage]
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)
inOrder Leaf = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs =
  mapMaybe onlySevere $ inOrder $ build xs
  where
    onlySevere msg =
                      case msg of
                      LogMessage (Error n) _ txt | n > 50 -> Just txt
                      _ -> Nothing
