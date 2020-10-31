--Recursive definition for Fibonacci sequence
--f(1) = 0
--f(2) = 1
--f(n) = f(n - 1) + f(n - 2)


fibs :: [Integer]

fibs = [x | x <- [0..], (x == 0 || x == 1 || )]  
