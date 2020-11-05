module Ex where


--Takes a list of elements and returns the last one. Not defined for the empty list
--INPUT:
--	a list of elements
--OUTPUT:
--	the last element of the list
last' :: [a] -> a
last' (x:[]) = x
last' (x:xs) = last' xs


--Takes a list and removes the last element
--INPUT:
--	a list of elements
--OUTPUT:
-- 	the same list as before except for the last element, which has been removed
init' :: [a] -> [a]
init' [] = []
init' [x] = []
init' (x:xs) = [x] ++ init' xs


--Retrieves the (n + 1)-th element of a list. Returns -1 for the empty list
--INPUT:
--	a list of elements
--OUTPUT:
--	the element whose index is n
pick :: Num a => Int -> [a] -> a
pick _ [] = -1
pick 0 (x:xs) = x
pick n (x:xs) = pick (n-1) xs


--Takes the first i elements of a list
--INPUT
--	the number of elements which we want to take
--	a list of elements
--OUTPUT
--	the list composed by the first i elements
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' i (x:xs) = [x] ++ take' (i-1) xs


--Drops the first i elements of a list
--INPUT:
--	the number of elements to drop
--	a list of elements
--OUTPUT:
--	the remaining of the original list
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' i (x:xs) = drop' (i-1) xs
