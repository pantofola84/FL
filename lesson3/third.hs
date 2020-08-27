-- third with head and tail
third1 l = (head.tail.tail) l

-- third with !!
third2 l = l !! 2i

-- third with pattern matching
third3 (a : b : c : _) = c
