-- define type: mine_last takes a list of elements of *any* type (parametric polymorphism) and returns one of such elements 
mine_last :: [a] -> a

-- if there is just one element in the list, then is simply returned
mine_last [x] = x

-- if the list has more than one element, call recursively mine_last on the rest of the list
mine_last (x : r) = mine_last r
