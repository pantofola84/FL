-- pyt
-- return a list of Pythagorean triples in which the max value is AT MOST n

pyt :: Int -> [(Int, Int, Int)]
pyt n = [(x, y, z) | z <- [0..n], x <- [0..n], y <- [0..n], (x ^ 2) + (y ^ 2) == (z ^ 2)]
