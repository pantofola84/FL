mine_pick::(Num a, Eq a) => [b] -> a -> b

mine_pick (x : r) 0 = x

mine_pick (x : r) i = mine_pick r (i - 1)
