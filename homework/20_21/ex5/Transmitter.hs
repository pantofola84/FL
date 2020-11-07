module Transmitter where

import Data.Char


type Bit = Int


--Given function that converts a list of Bit into the corresponding Int
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0


--Given function that converts an Int into the corresponding sequence of Bits
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n mod 2 : int2bin (n div 2)


--Takes a list of Bits and prepends to it the correct parity bit
--INPUT
--	a list of 8 bits
--OUTPUT
--	a list of 9 bits. The first bit is the correct parity bit for the following byte
prependParityBit :: [Bit] -> [Bit]
prependParityBit xs
 | ( (sum xs) `mod` 2 ) == 0 = [0] ++ xs
 | otherwise = [1] ++ xs


--Given function that possibly fills a list of bit to create the corresponding byte.
--It simply adds zeros to the less significative positions
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)


--Takes a string and converts it into the correct list of Bits
--INPUT
--	the string to be encoded
--OUTPUT
--	the correct list of Bits that represents the encoding of the given string
encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)


--Takes a list of Bits and splits it into a list of lists of 9 Bits
--INPUT:
--	a list of Bits
--OUTPUT:
--	the corresponding list of lists of 9 Bits
chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)


--The list of Bits representing the character "x"
xInBits :: [Bit]
xInBits = drop 1 (encode "x")


--Takes a list of Bits and checks the parity bit:
--	if it's correct, removes it and returns the resulting list of Bits;
--	otherwise returns the byte representing the character "x".
--INPUT:
--	a list of Bits
--OUTPUT:
--	a byte with the parity bit removed of the byte corresponding to the character "x"
checkParity :: [Bit] -> [Bit] 
checkParity (x:xs)
 | x == ((sum xs) `mod` 2) = xs
 | otherwise = xInBits


--Takes a list of lists of 9 bits, and for each one checks and handles errors, if any
--INPUT
--	a list of lists of 9 Bits
--OUTPUT
--	a list of lists of 8 Bits
checkParityInList :: [[Bit]] -> [[Bit]]
checkParityInList xs = map checkParity xs


--Takes a list of Bits and decodes it
--INPUT:
--	a list of Bits
--OUTPUT:
--	the decoded string
decode :: [Bit] -> String
decode = map (chr . bin2int) . checkParityInList . chop9


--
flipBit :: Bit -> Bit
flipBit i = abs (i - 1)

listOfSum :: [Int] -> [Int]
listOfSum l = [sum (take i l) | i <- [1,2..length l]] 

modify :: Int -> [Bit] -> [Bit]
modify i bs = take i bs ++ [flipBit (bs !! i)] ++ drop (i + 1) bs

flipByIndex :: [Int] -> [Bit] -> [Bit]
flipByIndex [] bs = bs
flipByIndex i [] = []
flipByIndex (i:is) bs = flipByIndex is (modify i bs)

channel :: [Int] -> [Bit] -> [Bit]
channel ns xs = flipByIndex (listOfSum ns) xs

transmit :: [Int] -> String -> String
transmit ns = decode .(channel ns). encode
