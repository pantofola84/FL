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


--take :: Int -> [a] -> [a]
--take 0 _ = []
--take (n + 1) [] = []
--take (n + 1) (x:xs) = x : take n xs

--NOTE: the sum "int_part + mod_part" handles the case in which n is odd
take' :: Int -> Tree a -> Tree a
take' 0 _ = Leaf
take' n Leaf = Leaf
take' n (Node l v r) = Node (take' (int_part + mod_part) l) v (take' int_part r) where
 int_part = n `div` 2
 mod_part = n `mod` 2


--replicate :: Int -> a -> [a]
--replicate n = (take n).repeat

replicate' :: Int -> a -> Tree a
replicate' n = (take' n).repeat'  
