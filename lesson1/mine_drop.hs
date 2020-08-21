mine_drop :: (Num a, Eq a) => a -> [b] -> [b]

mine_drop _ [] = []

mine_drop 0 l = l

mine_drop e (x : r) = mine_drop (e - 1) r
