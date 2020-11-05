module Validate where


--Takes a number and converts it into a list of digits
--INPUT:
--	the number to be transformed
--OUTPUT:
--	the list of digits that compose the number
number2Digits :: Int -> [Int]
number2Digits n 
 | n < 10 	= [n]
 | otherwise 	= number2Digits (n `div` 10) ++ [n `mod` 10]


--Takes a number and creates a reversed list of its digits
--INPUT:
--	the number of which we want to obtain the reversed list of digits
--OUTPUT:
--	the reversed list of digits composing the number
rNumber2Digits :: Int -> [Int] 
rNumber2Digits n = reverse(number2Digits n)


--Doubles a number and substract 9 from it if it's greater than 9
--INPUT:
--	the number to normalize
--OUTPUT:
--	the normalized number
normalize :: Int -> Int
normalize n
 | double > 9 	= double - 9
 | otherwise	= double
 where double = n * 2


--Takes a list of number and normalizes every element which has an odd index
--INPUT:
--	a list of numbers
--OUTPUT:
--	the list of number in which every element at an odd position has been normalized
normalizeEverySecondElement :: [Int] -> [Int]
normalizeEverySecondElement []	 	= []
normalizeEverySecondElement (x:[]) 	= [x]
normalizeEverySecondElement (f:s:r) = [f] ++ [normalize(s)] ++ normalizeEverySecondElement r 


--Takes a credit card number and check if it's valid
--INPUT:
--	a credit card number
--OUTPUT:
--	returns true if the credit card number is valid, false otherwise
validate = (\n -> n `mod` 10 == 0).sum.normalizeEverySecondElement.rNumber2Digits
