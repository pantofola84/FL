unfold p h t x
 | p x = []
 | otherwise = h x : unfold p h t (t x)


type Bit = Int

--Takes a single list of bits and transforms it into a list of bytes
chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (==[]) (take 8) (drop 8)


map1 :: (a -> b) -> [a] -> [b]
map1 f = unfold (\x -> (length x) == 0) (f.head) (tail)


iterate1 :: (a -> a) -> a -> [a]
iterate1 f = unfold (\x -> False) (\x -> x) (f)
