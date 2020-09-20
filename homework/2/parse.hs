-- compiler will show all warnings
{-# OPTIONS_GHC -Wall #-}


-- create a new module called Parse
module Parse where


-- import module Log (custom datatypes and tests)
import Log
-- import module Data.Char (isDigit function)
import Data.Char
-- import module Text.Read (readMaybe function)
import Text.Read


-----------------------------------------     PARSE    -----------------------------------------------------------------------------------------------------------------------


-- checks if a trimmed string has the correct length, has a flag to know if it's dealing with an error
validLength :: [String] -> Bool -> Bool
validLength t True 	= length t >= 4
validLength t False 	= length t >= 3


-- given a string, returns true if it's a valid timestamp
isValidTimeStamp :: String -> Bool
isValidTimeStamp [] 		= False
isValidTimeStamp [c] 		= isDigit c
isValidTimeStamp (f : r) 	= isValidTimeStamp [f] && isValidTimeStamp r


-- given a Maybe, is true if it represents a valid error priority
readAndValidate :: (Ord a, Num a) => Maybe a -> Bool
readAndValidate x = case x of
 Nothing 	-> False
 Just a 	-> (a < 101) && (a > 0)


-- given a trimmed string, returns true if it is a valid Info LogMessage
isInfo :: [String] -> Bool
isInfo s = ((s !! 0) == "I") && (isValidTimeStamp (s !! 1))


-- given a trimmed string, returns true if it is a valid Warning LogMessage
isWarning :: [String] -> Bool
isWarning s = ((s !! 0) == "W") && (isValidTimeStamp (s !! 1))


-- given a trimmed string, returns true if it is a valid Error LogMessage
isError :: [String] -> Bool
isError s = ((s !! 0) == "E")	&&
 (isValidTimeStamp (s !! 2))	&&
 readAndValidate (readMaybe (s !! 1)::Maybe Int)


-- given a string, returns the corresponding LogMessage
parseMessage :: String -> LogMessage
parseMessage s
 | validLength trimmedS True && isError trimmedS		= LogMessage (Error (read (trimmedS !! 1)::Int)) (read (trimmedS !! 2)::Int) (unwords (drop 3 trimmedS))
 | validLength trimmedS False && isWarning trimmedS		= LogMessage Warning (read (trimmedS !! 1)::Int) (unwords (drop 2 trimmedS))
 | validLength trimmedS False && isInfo trimmedS		= LogMessage Info (read (trimmedS !! 1)::Int) (unwords (drop 2 trimmedS))
 | otherwise							= Unknown s
 where trimmedS = words s
 
-- parse given a String representing the whole Log file, parses every line into a LogMessage
parse :: String -> [LogMessage]
parse text = map parseMessage (lines text)

-----------------------------------------     PARSE    -----------------------------------------------------------------------------------------------------------------------



-----------------------------------------     MESSAGETREE    -----------------------------------------------------------------------------------------------------------------------

-- extract the TimeStamp
extractTimeStamp :: LogMessage -> TimeStamp
extractTimeStamp (LogMessage _ timeStamp _) 	= timeStamp
extractTimeStamp (Unknown _) 			= -1


-- insert a LogMessage into an existing MessageTree. If the LogMessage is Unknown then do nothing
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree 					= tree
insert logMessage Leaf 						= Node Leaf logMessage Leaf
insert logMessage (Node leftTree nodeLogMessage rightTree) 	= if (extractTimeStamp logMessage) < (extractTimeStamp nodeLogMessage)
 then Node (insert logMessage leftTree) nodeLogMessage rightTree
 else Node leftTree nodeLogMessage (insert logMessage rightTree)


-- build a MessageTree from a list of LogMessages, starting with a Leaf (the empty tree)
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (f : r) = insert f (build r)


-- takes a MessageTree and generates a list of its LogMessages sorted by TimeStamp
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree lm rightTree) = inOrder leftTree ++ [lm] ++ inOrder rightTree


-----------------------------------------     MESSAGETREE    -----------------------------------------------------------------------------------------------------------------------
