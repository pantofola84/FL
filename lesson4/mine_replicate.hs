-- mine_replicate
-- E: mine_replicate 3 True = [True, True, True]

mine_replicate :: Int -> a -> [a]
mine_replicate 0 _ = []
mine_replicate n x = [x] ++ mine_replicate (n-1) x
