module Merge where

--Takes two sorted lists and merges 'em in a big single sorted list
--INPUT
--	a sorted list of elements
--	another sorted list of elements
--OUTPUT
--	a sorted list of elements created by merging the two parameter lists
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] 		= []
merge l []		= l
merge [] r		= r
merge (l:ls) (r:rs)	= if l < r
 then l:merge ls (r:rs)
 else r:merge (l:ls) rs


--Takes a list and splits it in a half (at most one element of difference if the cardinality of the list is odd)
--INPUT
--	a list of elements
--OUTPUT
--	a couple of lists, obtained by splitting the original list in a half
halve :: [a] -> ([a], [a])
halve l = (take ((length l) `div` 2 + (length l) `mod` 2) l, drop ((length l) `div` 2 + (length l) `mod` 2) l)


--Haskell implementation of merge sort
--INPUT
--	a (possibly) unsorted list of elements
--OUTPUT
--	the same list as before, but sorted
msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort l = merge (msort firstSortedList) (msort secondSortedList) 
 where
 firstSortedList = fst (halve l)
 secondSortedList = snd (halve l)
