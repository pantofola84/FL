module Mai where


import Merge


--This function reads a sequence of numbers, calls msort to sort them, and then prints 'em
--The I/O input is in the form:
--	n1 n2 n3
main :: IO ()
main = do
 stringOfNumbers <- getLine
 putStrLn $ (show (msort (map read (words stringOfNumbers)::[Int])))
