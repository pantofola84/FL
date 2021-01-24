--Recursive definition for Fibonacci sequence
--f(1) = 0
--f(2) = 1
--f(n) = f(n - 1) + f(n - 2)


--Simple implementation of Fibonacci
fibonacciFun :: Integer -> Integer
fibonacciFun 0 = 0
fibonacciFun 1 = 1
fibonacciFun n = fibonacciFun (n - 1) + fibonacciFun (n - 2)


--Infinite list of Fibonacci numbers
fibs :: [Integer]
fibs = [fibonacciFun x | x <- [0..]]


--fib returns the nth Fibonacci number
fib :: Int -> Integer
fib n = fibs !! (n - 1)


--firstGreaterThan1k returns the first Fibonacci number that is greater than 1000
firstGreaterThan1k :: Integer
firstGreaterThan1k = (head.dropWhile (<=1000)) fibs  
