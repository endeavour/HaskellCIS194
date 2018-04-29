{-# OPTIONS_GHC -Wall #-}            -- Shows all warnings
{-# LANGUAGE ViewPatterns #-}        -- Neat, let's us do something similar to F# Active Patterns :)

module Homework2 where
import Log
import Text.Regex.PCRE

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