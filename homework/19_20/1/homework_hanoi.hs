-- "type" is the Haskell analogous to C++ "typedef"
type Peg = String
type Move = (Peg, Peg)

-- hanoi
--
-- takes as input:
-- 1. The number of disks to move
-- 2. The starting Peg
-- 3. The goal Peg
-- 4. The stack Peg
--
-- returns a list of Moves to perform
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 start goal stack = [(start, goal)]
hanoi n start goal stack =  hanoi (n-1) start stack goal ++ [(start, goal)] ++ hanoi (n-1) stack goal start  
