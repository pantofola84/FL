--Takes a function on pairs and transforms it into a curried function
curry1 :: ((a, b) -> c) -> a -> b -> c
curry1 f x y = f (x, y)

--Takes a curried function and transforms it into a function on pairs 
uncurry1 :: (a -> b -> c) -> ((a, b) -> c)
uncurry1 f = \ (x, y) -> f x y
