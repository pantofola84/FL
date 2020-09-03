-- filter function implemented using foldr function
mine_filter :: (a -> Bool) -> [a] -> [a]
mine_filter p l = foldr (\ f -> \ rec -> if p f then f:rec else rec) [] l
