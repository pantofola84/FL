module Main where

import Validate


--This function takes an accumulator list, then reads a sequence of credit card numbers and prints a list of boolean val
--ues. Every boolean in the list tells if the correspondent credit card number is valid or not.
--The I/O input is in the form:
--	n1
--	n2
--	n3
--	end
--INPUT:
--	an accumulator for the list to be validated and then printed
readValidateAndPrint :: [Int] -> IO ()
readValidateAndPrint a = do
 creditCardNumber <- getLine
 if creditCardNumber == "end" 
 then putStrLn $ show (map validate a) 
 else readValidateAndPrint (a ++ [read creditCardNumber::Int])


--Reads a sequence of credit card numbers and prints a list of booleans which indicates if numbers are valid or not
main :: IO ()
main = readValidateAndPrint []
