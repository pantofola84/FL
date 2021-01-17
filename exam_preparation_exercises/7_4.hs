--Define a function
dec2int :: [Int] -> Int
--that converts a decimal number into an integer

dec2int = foldl (\acc v -> (acc * 10) + v) 0
