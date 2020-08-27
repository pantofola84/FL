-- safetail with conditional expression
safetail1 l = if null l then [] else tail l

-- safetail with guarded conditions
safetail2 l
 | null l == True = []
 | otherwise = tail l

-- safetail with pattern matching
safetail3 [] = []
safetail3 l = tail l
