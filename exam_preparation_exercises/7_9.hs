applyf2 :: (a -> b) -> (a -> b) -> [a] -> [b]
applyf2 _ _ [] = []
applyf2 f1 f2 (x: xs) = [f2 x] ++ applyf1 f1 f2 xs


applyf1 :: (a -> b) -> (a -> b) -> [a] -> [b]
applyf1 _ _ [] = []
applyf1 f1 f2 (x: xs) = [f1 x] ++ applyf2 f1 f2 xs


altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 l = applyf1 f1 f2 l
