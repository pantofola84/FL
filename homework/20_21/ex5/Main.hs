module Main where

import Transmitter


--Reads a string of numbers (to be parsed) and a string that has to be transmitted. Transmit the string and prints the r
--esult
main :: IO ()
main = do
 numberString <- getLine
 stringToTransmit <- getLine
 print (transmit (map read (words numberString)::[Int]) stringToTransmit)
