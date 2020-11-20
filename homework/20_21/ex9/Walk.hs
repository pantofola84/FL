module Walk where


--Couple of Int representing the number of birds on the left and the right side of the pole
--NOTE: such numbers cannot be negative
type Conf = (Int, Int)


--Type representing a generic Move: for instance the Move (1, 'R') indicates that a bird has landend on the right side o
--f the pole, while the Move (-3, 'L') tells us that 3 birds left the left side
type Move = (Int, Char)


---------------------------PLAY1----------------------------------------------------------------------------------------


--It behaves like the normal summation if both operands are positive. If the second operand is negative and the summatio
--n leads to a negative value, it returns zero
--INPUT:
--	the left operand, positive or equal to zero by assumption
--	the right operand, might be negative
--OUTPUT:
--	if the normal sum is positive it returns the same value, otherwise it returns 0
safePlus :: Int -> Int -> Int
safePlus a b
 | (a + b) > 0 = a + b
 | otherwise = 0


--Takes three parameters a, b and c, and returns True if the diffence between (a + b) and c is greater than 4
--INPUT:
--	'a', first sum operand
--	'b', the second sum operand
--	'c', the value that has to be subtracted
--OUTPUT:
--	True iff abs (a + b - c) > 4, False otherwise
isDifferenceGreaterThan4 :: Int -> Int -> Int -> Bool
isDifferenceGreaterThan4 a b c = abs (a + b - c) > 4


--Takes a Move and a Conf and returns the modified configuration
--INPUT:
--	a Move to be performed
--	a configuration
--OUTPUT:
--	the modified configuration obtained by performing the Move
performMove1 :: Move -> Conf -> Conf
performMove1 (arrival, side) (left, right) = case side of
 'L' -> if isDifferenceGreaterThan4 arrival left right then error "troppi uccelli" else (safePlus left arrival, right)
 'R' -> if isDifferenceGreaterThan4 arrival right left then error "troppi uccelli" else (left, safePlus right arrival)


--Takes a Move and a list of Conf and applies the Move to the last configuration
--INPUT:
--	a Move to be performed
--	a list of configurations
--OUTPUT:
--	the updated list of configurations
performMoveOnList1 :: Move -> [Conf] -> [Conf]
performMoveOnList1 m cl = cl ++ [performMove1 m (last cl)]


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
play1 movesList = \c -> tail (foldr performMoveOnList1 [c] (reverse movesList))


---------------PLAY1----------------------------------------------------------------------------------------------------




--------------PLAY2-----------------------------------------------------------------------------------------------------


--Takes a Move and a Conf and tells if the acrobat will stay on the rope or will fall
--INPUT:
--	a Move
--	a configuration
--OUTPUT:
--	True if the acrobat is going to stay on the rope, False if he'll fall
checkStayingCondition :: Move -> Conf -> Bool
checkStayingCondition (arrival, side) (left, right) = case side of
 'L' -> abs (left + arrival - right) <= 4
 'R' -> abs (right + arrival - left) <= 4


--Takes a Move and a configuration and applies the Move to the configuration
--INPUT:
--	a Move
--	a Conf
--OUTPUT:
--	the configuration obtained by applying the Move
performMove2 :: Move -> Conf -> Conf
performMove2 (arrival, side) (left, right) = case side of
 'L' -> (safePlus left arrival, right)
 'R' -> (left, safePlus right arrival)


--Takes a Move and a Maybe containing a list of Conf and applies the Move to the last configuration
--INPUT:
--	a Move to be performed
--	a Maybe containing a list of configurations
--OUTPUT:
--	a Maybe containing the updated list of configurations, or Nothing
performMoveOnList2 :: Move -> Maybe [Conf] -> Maybe [Conf]
performMoveOnList2 m Nothing = Nothing
performMoveOnList2 m (Just cl)
 | checkStayingCondition m (last cl) = Just (cl ++ [performMove2 m (last cl)])
 | otherwise = Nothing 


--The change with respect to play1 is that the function returned by play2 is such that for some start configuration may 
--return Nothing, corresponding to sequence of moves that cause the acrobat to fall down
--INPUT:
--	a list of Moves
--OUTPUT:
--	a function that takes a Conf and applies sequentially the Moves passed. It returns a Maybe that contains the lis
--	t of Conf thus obtained
--NOTE1: the function should be defined using foldr
--NOTE2: the function must handle failure by returning Nothing

--Let's remember the type of foldr
--foldr :: (a -> b -> b) -> b -> [a] -> b
--Let's instantiate a to Move and b to Maybe [Conf]

play2 :: [Move] -> (Conf -> Maybe [Conf])
play2 movesList = \c -> do
 confList <- foldr performMoveOnList2 (Just [c]) (reverse movesList)
 return (tail confList)


---------------------------PLAY2----------------------------------------------------------------------------------------




--------------------------PLAY3-----------------------------------------------------------------------------------------




--------------------------PLAY3-----------------------------------------------------------------------------------------
