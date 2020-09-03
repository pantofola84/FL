--dec2int implemented using foldl
dec2int :: [Int] -> Int
dec2int l = foldl (\ rec -> \ f -> (rec * 10) + f) 0 l
