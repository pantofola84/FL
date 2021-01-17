--Rewrite the following function
fun1 f p xs = [f x | x <- xs, p x]
--in terms of "map" and "filter"


fun2 f p xs = map f (filter p xs)
