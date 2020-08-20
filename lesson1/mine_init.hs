nit :: [a] -> [a]
mine_init [x] = []
mine_init (x : r) = [x] ++ mine_init r
