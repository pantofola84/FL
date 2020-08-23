module Validate where

-- toDigits
-- converts positive Integers into a list of digits
-- E: toDigits 123 = [1, 2, 3]
-- E: toDigits 0 = []

-- NOTE: the negative argument MUST be passed in brackets, because (-) is a function
-- E: toDigits (-23) = []

toDigits :: Integral a => a -> [a]
toDigits n
 | n > 0 = toDigits (n `div` 10) ++ [n `mod` 10]
 | otherwise = []

-- toDigitsRev
-- should do the same, but with digits reversed
-- E: toDigitsRev 317 = [7, 1, 3]

toDigitsRev :: Integral a => a -> [a]
toDigitsRev a = reverse (toDigits a)
