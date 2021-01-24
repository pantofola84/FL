--We have the following binary tree type:


data Tree a = Leaf
 | Node (Tree a) a (Tree a) deriving Show


--and we want to write the correct version of the following functions (examples refer to lists)


--repeat :: a -> [a]
--repeat x = xs where
-- xs = x:xs

--repeat v returns the infinite Tree whose nodes contains the value v
repeat' :: a -> Tree a
repeat' v = Node (repeat' v) v (repeat' v)


--Returns the correct subdivision of an integer in a "breadth-first traversal" optic
balance :: Int -> Int -> (Int, Int)
balance p n
 | (n - s) < 0 = let diff = s - (2 ^ p) in (diff, n - diff)
 | (n - s) == 0 = (s, 0)
 | otherwise = balance (p + 1) n
 where
      s = (sum.(map (2^))) [1..p]


--take :: Int -> [a] -> [a]
--take 0 _ = []
--take (n + 1) [] = []
--take (n + 1) (x:xs) = x : take n xs
take' :: Int -> Tree a -> Tree a
take' 0 _ = Leaf
take' n Leaf = Leaf
take' n (Node l v r) = Node (take' ((division `div` 2) + rest) l) v (take' (division `div` 2) r) where
 division = fst result
 rest = snd result
 result = balance 1 (n - 1)


--replicate :: Int -> a -> [a]
--replicate n = (take n).repeat

replicate' :: Int -> a -> Tree a
replicate' n = (take' n).repeat'  
