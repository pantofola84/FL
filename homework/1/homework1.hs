module Validate where

-- toDigits
-- converts positive Integers into a list of digits
-- E: toDigits 123 = [1, 2, 3]
-- E: toDigits 0 = []

-- NOTE: the negative argument MUST be passed in brackets, because (-) is a function
-- E: toDigits (-23) = []

toDigits :: Integer -> [Integer]
toDigits n
 | n > 0 = toDigits (n `div` 10) ++ [n `mod` 10]
 | otherwise = []


-- toDigitsRev
-- should do the same, but with digits reversed
-- E: toDigitsRev 317 = [7, 1, 3]

toDigitsRev :: Integer -> [Integer]
toDigitsRev a = reverse (toDigits a)


-- doubleAlternate
-- doubles values in the list on the base of the parameter passed
-- E: doubleAlternate 1 [1, 2, 3] = [1, 4, 3]
-- E: doubleAlternate 2 [1, 4] = [2, 4]

doubleAlternate :: (Eq a, Num a, Num b) => a -> [b] -> [b]
doubleAlternate _ [] = []
doubleAlternate 1 (f : r) = [f] ++ doubleAlternate 2 r
doubleAlternate 2 (f : r) = [2 * f] ++ doubleAlternate 1 r 


-- doubleEveryOther
-- should double every number at a pair index, beginning to count from the right
-- E: doubleEveryOther [2, 4, 5, 2] = [4, 4, 10, 2]
-- E: doubleEveryOther [] = []
-- E: doubleEveryOther [2, 4, 6] = [2, 8, 6]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l
 | ((length l) `mod` 2) == 0 = doubleAlternate 2 l
 | otherwise = doubleAlternate 1 l


-- sumDigits
-- should return the sum of the whole list. Numbers composed by two or more digits are decomposed i
-- n single digits
-- E: sumDigits [2, 23, 3] = 10

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (f : r) = sum (toDigits f) + sumDigits r


-- validate
-- should return true if an integer is a valid credit card number, false otherwise
-- E: validate 4012888888881881 = True
-- E: validate 4012888888881882 = False

validate :: Integer -> Bool
validate i = if ((sumDigits.doubleEveryOther.toDigits) i) `mod` 10 == 0 then True else False
