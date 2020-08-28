-- perfect
-- return a list of perfect numbers whose max is n

perfect :: Int -> [Int]
perfect n = [x | x <- [1..n], (\ a -> (sum ((\ b -> [z | z <- [1..b], b `mod` z == 0]) a) - a) == a ) x == True]
