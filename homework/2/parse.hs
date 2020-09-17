-- compiler will show all warnings
{-# OPTIONS_GHC -Wall #-}

-- create a new module called Parse
module Parse where

-- import module Log (custom datatypes and tests)
-- import Log

main :: IO ()
main = do
 content <- readFile "sample.log"
 mapM_ print (lines content)
