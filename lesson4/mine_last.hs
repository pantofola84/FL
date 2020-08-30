mine_last :: [a] -> a
mine_last [x] = x
mine_last (f : r) = mine_last r
