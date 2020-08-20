mine_take :: (Eq a, Num a) => a -> [b] -> [b]
mine_take 0 _ = []
mine_take _ [] = []
mine_take q (x : r) = [x] ++ mine_take (q-1) r
