-- rewrite the original expression using two comprehensions which use a single generator

lesson4_7_original = [(x,y) | x <-[1,2], y <-[3,4]]

lesson4_7_mine = zip [x | x <- [1,1,2,2]] [y | y <- [3,4,3,4]]
