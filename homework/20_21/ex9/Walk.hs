module Walk where


--Couple of Int representing the number of birds on the left and the right side of the pole
--NOTE: such numbers cannot be negative
type Conf = (Int, Int)


--Type representing a generic Move: for instance the Move (1, 'R') indicates that a bird has landend on the right side o
--f the pole, while the Move (-3, 'L') tells us that 3 birds left the left side
type Move = (Int, Char)


--Takes a Move and a Conf and returns the modified configuration
--INPUT:
--	a Move to be performed
--	a configuration
--OUTPUT:
--	the modified configuration obtained by performing the Move
performMove :: Move -> Conf -> Conf
performMove (arrival, side) (leftside, rightside) = case side of
 'L' -> if (abs (leftside + arrival - rightside)) > 4 then error "troppi uccelli" else (leftside + arrival, rightside)
 'R' -> if (abs (rightside + arrival - leftside)) > 4 then error "troppi uccelli" else (leftside, rightside + arrival)


--Takes a Move and a list of Conf and applies the Move to the last configuration
--INPUT:
--	a Move to be performed
--	a list of configurations
--OUTPUT:
--	the updated list of configurations
performMoveOnList :: Move -> [Conf] -> [Conf]
performMoveOnList m cl = cl ++ [performMove m (last cl)]


--Takes a list of Moves and returns a function that:
--	takes a configuration and returns the list of configurations obtained by executing the Moves passed as input
--INPUT:
--	a list of Moves
--OUTPUT:
--	a function that takes a Conf and applies sequentially the Moves passed. It returns the list of Conf thus obtaine
--	d
--NOTE1: the function should be defined using foldr
--NOTE2: the function must handle failure by using the built-in type Error initialized with the String "troppi uccelli"

--Let's remember the type of foldr
--foldr :: (a -> b -> b) -> b -> [a] -> b
--Let's instantiate a to Move and b to [Conf]

play1 :: [Move] -> (Conf -> [Conf])
play1 movesList = \c -> foldr performMoveOnList [c] movesList
