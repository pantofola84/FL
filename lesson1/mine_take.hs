-- mine_take takes a formal parameter "a" that must:
-- 1. belong to the type class Num because we make a recursive call in which there is the expression "a - 1"
-- 2. belong to the type class Eq because we need to compare it to 0  
mine_take :: (Eq a, Num a) => a -> [b] -> [b]

mine_take 0 _ = []

mine_take _ [] = []

mine_take q (x : r) = [x] ++ mine_take (q-1) r
