type Peg = String


--Represents a move from a Peg to another
type Move = (Peg, Peg)


--A Conf represents the status of the game in any given moment
--For instance, the initial configuration is [[1,2,3,4], [], []], 

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
