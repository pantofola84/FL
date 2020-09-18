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


-- given a string, returns true if it's a valid timestamp
isValidTimeStamp :: String -> Bool
isValidTimeStamp [] = False
isValidTimeStamp [c] = isDigit c
isValidTimeStamp (f : r) = isValidTimeStamp [f] && isValidTimeStamp r


-- given a Maybe, is true if it represents a valid error priority
readAndValidate :: Maybe a -> Maybe a -> Bool
readAndValidate inf_constr sup_constr = do
 i <- inf_constr
 s <- sup_constr

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
 readAndValidate (readMaybe (s !! 1)::Int) (readMaybe (s !! 1)::Int)


-- given a string, returns the corresponding LogMessage
parse :: String -> LogMessage
parse s
 | isError trimmedS 	= LogMessage (Error (read (trimmedS !! 1)::Int)) (read (trimmedS !! 2)::Int) (unwords (drop 3 trimmedS))
 | isWarning trimmedS 	= LogMessage Warning (read (trimmedS !! 1)::Int) (unwords (drop 2 trimmedS))
 | isInfo trimmedS 	= LogMessage Info (read (trimmedS !! 1)::Int) (unwords (drop 2 trimmedS))
 | otherwise		= Unknown s
 where trimmedS = words s


