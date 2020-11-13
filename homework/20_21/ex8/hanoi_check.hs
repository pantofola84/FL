module Hanoi_Check where


type Peg = String


--Represents a move from a Peg to another
type Move = (Peg, Peg)


--A Conf represents the status of the game in any given moment.
--For instance, the initial configuration is ([1, 2, 3, 4], [], []), while the final one is ([], [], [1, 2, 3, 4]).
--1 represents the smallest Peg, while 4 stands for the biggest one
type Conf = ([Int], [Int], [Int])
 

--A Report is simply a type of data that models success or failure
--The fact that derives the type class Show implies that it can be directly printed
data Report = Bad | Ok deriving Show


--Takes the number of disks and three Pegs and returns the minimal correct list of Moves needed to win the game
--INPUT:
--	the number of disks to move
--	the starting Peg
--	the stack Peg
--	the goal Peg
--OUTPUT:
--	the minimal list of Moves needed to win the game
hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 start stack goal = [(start, goal)]
hanoi n start stack goal =  hanoi (n-1) start goal stack ++ [(start, goal)] ++ hanoi (n-1) stack start goal



--Maps a String to an element of a triple in a conventional way
--INPUT:
--	the String "a", "b" or "c"
--OUTPUT:
--	the first, second or third element of the triple respectively
s2e :: String -> (a, a, a) -> a
s2e "a" (a, b, c) = a
s2e "b" (a, b, c) = b
s2e "c" (a, b, c) = c


--Takes a Move and checks that only the specific Pegs "a", "b" and "c" are used
--INPUT:
--	the move that has to be checked
--OUTPUT:
--	true iff the Move uses only Pegs "a" or "b" or "c", false otherwise
pegNameCheck :: Move -> Bool
pegNameCheck (p1, p2) = (p1 == "a" || p1 == "b" || p1 == "c") && (p2 == "a" || p2 == "b" || p2 == "c") 


--Takes a configuration and a Move and checks that the Move is not referring to an empty Peg
--INPUT:
--	a configuration
--	a Move
--OUTPUT:
--	true iff nobody is trying to move a disk from an empty Peg
emptyPegCheck :: Conf -> Move -> Bool
emptyPegCheck (a, b, c) move
 | fst move == "a" = a /= []
 | fst move == "b" = b /= []
 | fst move == "c" = c /= []


--Takes a configuration and a Move, then checks for a size error: that is to say that nobody is trying to place a bigger
-- disk on top of a smaller one
--INPUT:
--	a configuration
--	a Move
--OUTPUT:
--	true iff nobody is trying to put a bigger disk on top of a smaller one
sizeCheck :: Conf -> Move -> Bool
sizeCheck configuration move
 | goalPeg == [] = True
 | otherwise = head startingPeg < head goalPeg
 where startingPeg = s2e (fst move) configuration
       goalPeg = s2e (snd move) configuration


--Takes a configuration and a Move, then checks if the Move is valid or not. For a Move to be valid, it must abide to t
--he following rules:
--	no disk can be moved from an empty Peg
--	no disk can be placed on top of a smaller one
--	it only uses peg "a", "b", "c"
--INPUT:
--	the current configuration
--	a Move whose validity has to be checked
--OUTPUT:
--	a boolean that indicates if the Move is valid or not
isValid :: Conf -> Move -> Bool
isValid configuration m = pegNameCheck m && emptyPegCheck configuration m && sizeCheck configuration m


--Takes a configuration and a Move and returns a new configuration modified accordingly
--INPUT:
--	a configuration
--	a Move
--OUTPUT:
--	the configuration obtained by applying the Move
performMove :: Conf -> Move -> Conf

performMove (a, b, c) ("a", "b") = (tail a, [head a] ++ b, c)
performMove (a, b, c) ("a", "c") = (tail a, b, [head a] ++ c)

performMove (a, b, c) ("b", "a") = ([head b] ++ a, tail b, c)
performMove (a, b, c) ("b", "c") = (a, tail b, [head b] ++ c)

performMove (a, b, c) ("c", "a") = ([head c] ++ a, b, tail c)
performMove (a, b, c) ("c", "b") = (a, [head c] ++ b, tail c)


--Takes the current configuration of the game, the number of moves executed so far and a list of Moves to check, then re
--turns a tuple containing:
--	a Report that indicates if a forbidden Move was performed
--	the total number of moves executed
--	a list that is empty if there were no wrong moves, or filled with the first wrong Move encountered
--	the final configuration reached
--INPUT:
--	the current configuration
--	the number of moves executed so far
--	the list of Moves to check
--OUTPUT:
--	a tuple that indicates if there were any errors, the overall number of moves performed, a list that possibly con
--	tains the first wrong Move and the final reached configuration
--
--NOTE: it follows from this contract that the input configuration represents always a valid game
check :: Conf -> Int -> [Move] -> (Report, Int, [Move], Conf)
--If the list of Moves to check is empty then there is actually nothing to do
check configuration movesSoFar [] = (Ok, movesSoFar, [], configuration)
--If the list of Moves contains at least one Move, we need to check that Move. Now:
--	the Move is invalid =>	we return a tuple containing a "Bad" Report, the total number of moves (aka the previous 
--				one), a list containing our bad guy and the final configuration reached
--	the Move is valid =>	we call check recursively with updated inputs
check configuration movesSoFar (m:ms)
 | isValid configuration m = check (performMove configuration m) (movesSoFar + 1) ms
 | otherwise = (Bad, movesSoFar, [m], configuration)
