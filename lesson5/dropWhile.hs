mine_takeWhile :: (a -> Bool) -> [a] -> [a]
mine_takeWhile p [] = []
mine_takeWhile p (f : r) = if p f then f:mine_takeWhile p r else []

mine_dropWhile :: (a -> Bool) -> [a] -> [a]
mine_dropWhile p l = drop (length (mine_takeWhile p l)) l
