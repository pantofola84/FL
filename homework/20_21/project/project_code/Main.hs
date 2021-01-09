module Main where


import Parse
import ParseProg


main :: IO ()
main = do
 l <- getLine
 print (parse parseProg l)
