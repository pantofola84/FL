mine_sum :: Num a => [a] -> a
mine_sum [] = 0
mine_sum (f : r) = f + sum r
