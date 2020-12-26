module Main where


import Balance


--The "main" function reads a line from input and tells us if parentheses in the received string are balanced or not. It
-- invokes the function "repeatForNTimes" passing as parameter the length of the line, exploiting the fact that balance 
--behaves as the identity function whenever it assess that the input string is not balanced
main :: IO ()
main = do
 l <- getLine
 print (app (repeatForNTimes (length l)) ([], l))
