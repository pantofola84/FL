module WhatWentWrong where

import Log
import Parse

-- true if LogMessage is an Error with priority >= 50
errorToKeep :: LogMessage -> Bool
errorToKeep (LogMessage (Error p) _ _) = p >= 50
errorToKeep _ = False

-- extracts the string from a LogMessage
extractString :: LogMessage -> String
extractString (Unknown _) = ""
extractString (LogMessage _ _ s) = s

-- takes an unsorted list of LogMessages and returns a list of Strings. Each string belongs to an Error with priority >= 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessageList = map (\ lm -> extractString lm) (filter (\ lm -> errorToKeep lm) ((inOrder.build) logMessageList))
