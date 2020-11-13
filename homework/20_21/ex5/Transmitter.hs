module Transmitter where

import Data.Char


type Bit = Int


--Given function that converts a list of Bit into the corresponding Int
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0


--Given function that converts an Int into the corresponding sequence of Bits
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)


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
encode = concat . map (prependParityBit . make8 . int2bin . ord)


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


--Takes a Bit and inverts it
--INPUT:
--	a single Bit
--OUTPUT:
--	the inverted Bit
flipBit :: Bit -> Bit
flipBit i = abs (i - 1)


--Takes a list and computes the required list of indexes
--INPUT:
--	a list of Int [i0, i1, ..., ik]
--OUTPUT:
--	a list of indexes [i0, i0 + i1, i0 + i1 + i2, ..., i0 + ... + ik]
listOfIndexes :: [Int] -> [Int]
listOfIndexes l = [sum (take i l) | i <- [1,2..length l]] 


--Takes an index and a list of Bits, and inverts the Bit at the specified index
--INPUT:
--	the index of the Bit that has to be inverted
--	a list of Bits
--OUTPUT:
--	the original list, but now the Bit at the specified index is inverted
flipBitOnList :: Int -> [Bit] -> [Bit]
flipBitOnList i bl = take i bl ++ [flipBit (bl !! i)] ++ drop (i + 1) bl


--Takes a list of indexes and a list of Bits, inverts the corresponding Bits
--INPUT:
--	a list of indexes
--	a list of Bits
--OUTPUT:
--	a list of Bits: Bits located at specified indexes are now inverted
flipBitsOnListByIndex :: [Int] -> [Bit] -> [Bit]
flipBitsOnListByIndex [] bl = bl
flipBitsOnListByIndex i [] = []
flipBitsOnListByIndex (i:is) bl = flipBitsOnListByIndex is (flipBitOnList i bl)


--Takes a list of Int and a list of Bits, computes indexes and inverts the corresponding Bits
--INPUT:
--	a list of Int from which the indexes will be computed
--	a list of Bits
--OUTPUT:
--	the correct list with specified Bits inverted
channel :: [Int] -> [Bit] -> [Bit]
channel nl bl = flipBitsOnListByIndex (listOfIndexes nl) bl


--Simulates the transmission on the channel
--INPUT:
--	a list of Int from which the indexes will be computed
--OUTPUT:
--	the string obtained after the encoding, the transmission and subsequent decoding
transmit :: [Int] -> String -> String
transmit nl = decode .(channel nl). encode
