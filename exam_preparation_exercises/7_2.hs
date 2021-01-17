--Write definitions for the following functions

all1 :: (a -> Bool) -> [a] -> Bool

any1 :: (a -> Bool) -> [a] -> Bool

takeWhile1 :: (a -> Bool) -> [a] -> [a]

dropWhile1 :: (a -> Bool) -> [a] -> [a]


all1 p xs = foldr (\fe rv -> rv && (p fe)) True xs

any1 p xs = foldr (\fe rv -> rv || (p fe)) False xs

takeWhile1 p [] = []
takeWhile1 p (x:xs)
 | p x = [x] ++ takeWhile1 p xs
 | otherwise = []

dropWhile1 p [] = []
dropWhile1 p l@(x:xs)
 | p x = dropWhile1 p xs
 | otherwise = l
