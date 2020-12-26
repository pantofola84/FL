module Balance where


import Control.Monad
import Control.Applicative


--1.Create the basic structure of the program: required types and so on						DONE
--2.Implement the pushdown automata via the function "balance"							DONE
--3.Instance CheckPar to the class Monad (also Functor and Applicative, otherwise we have building errors)	DONE
--4.Combine the function "balance" with the function "repeatForNTimes"						DONE
--5.Write the "main" function which performs IO and invokes the function "repeatForNTimes"			DONE 


--The type State is a couple of Strings which represent the following things:
--	the first string of the couple is the stack used to remember the open parentheses found so far;
--	the second element is the remaining string that has to be checked yet.
type State = (String, String)


--The type CheckPar is the function that allows us to make stateful computations. In fact it is our equivalent of the St
--ate Monad.
--NOTE: adding curly brackets allows us to name the characteristic function (like a field in an object)
newtype CheckPar a = CP {runCheckPar :: State -> (a, State)}


--Let's instantiate CheckPar to the Monad type class
instance Monad CheckPar where
--With the "return" operator we want to embed a value into a CheckPar "box" (that is really a function), and to do this 
--we create a CheckPar that returns a couple containing our value and the state accepted as parameter
 return x = CP $ \s -> (x, s)
--The bind operator has the following type:
--	m a -> (a -> m b) -> m b
--it takes a "box" containing a certain thing of type a, a function that takes a parameter of type a and returns a "box"
--containing an object of type b (a -> m b), and returns something of type m b. To do this, we first run the CheckPar fu
--nction on the parameter s to obtain a couple (v, s1) where v is the result of the first stateful computation and s1 is
--the new state. Secondly, we obtain the new CheckPar with the expression "f v". In the end we run the new CheckPar (whi
--ch is a function) in order to get our final value-state couple 
 c >>= f = CP $ \s -> let (v, s1) = runCheckPar c s in runCheckPar (f v) s1


--Once we have the Monad instantiation, Applicatives and Functors come automatically


--Let's instantiate CheckPar to the Applicative type class
instance Applicative CheckPar where
 pure = return
 (<*>) = ap


--Let's instantiate CheckPar to the Functor type class
instance Functor CheckPar where
 fmap = liftM


--CheckPar needs an application function to "run" the stateful computation
--INPUT:
--	a CheckPar
--	an initial State
--OUTPUT:
--	the couple (a, State) resulting from the stateful computation
app :: CheckPar a -> State -> (a, State)
app = runCheckPar


--balance implements the pushdown automata illustrated in the file "ex10.jpg".
--balance is an instantiation of the type CheckPar with the type bool
balance :: CheckPar Bool
balance = CP $ balanceImpl


-------------------------------UTILITY FUNCTIONS------------------------------------------------------------------------

isEmpty :: [a] -> Bool
isEmpty l = length l == 0

--True if the character is not "(" or ")" or "[" or "]", false otherwise
isOtherCharacter :: Char -> Bool
isOtherCharacter c = not (c == '[' || c == ']' || c == ')' || c == '(')

isOpenBracket :: Char -> Bool
isOpenBracket c = c == '(' || c == '['

isClosedBracket :: Char -> Bool
isClosedBracket c = c == ')' || c == ']'

stackAndStringNotEmpty :: String -> String -> Bool
stackAndStringNotEmpty stack string = not (isEmpty stack) && not (isEmpty string)

areCorrespondingBrackets :: String -> String -> Bool
areCorrespondingBrackets stack string = 
 (head string == ')' && head stack == '(') || (head string == ']' && head stack == '[')

------------------------------------------------------------------------------------------------------------------------


--balanceImpl is the actual implementation of the function. It has not been defined inline in order to use syntactic con
--structs like pattern matching and so on
balanceImpl :: State -> (Bool, State)
balanceImpl (stack, string)
--stack and string are empty: parentheses are balanced
 | isEmpty stack && isEmpty string = (True, ([], []))
--length of stack > length of string, therefore parentheses cannot be balanced (there are not sufficient remaining chara
--cters
 | (length stack) > (length string) = (False, (stack, string))
--the stack is empty, and the remaining string is not. We distinguish three cases:
--the first character of the string is not a parentheses: the only thing to do is to consume it
--the first character of the string is a closed bracket: we can conclude right now that the string is not balanced
--the first character of the string is an open bracket: we consume it and we add it on top of the stack
 | isEmpty stack && not (isEmpty string) && isOtherCharacter (head string) = (True, (stack, tail string))
 | isEmpty stack && not (isEmpty string) && isClosedBracket (head string) = (False, (stack, string))
 | isEmpty stack && not (isEmpty string) && isOpenBracket (head string) = (False, ([head string] ++ stack, tail string))
--the stack and the remaining string are not empty. We distinguish three cases:
--the first character of the string is not a parentheses: the only thing to do is to consume it
--the first character of the string is an open bracket: we consume it and we add it on top of the stack
--the first character of the string is a closed bracket: we expand further our function by checking two other cases:
--	the first character of the stack is not a matching bracket, therefore our string is not balanced
--	the first character of the stack is a matching bracket: we consume both the stack and the string
 | stackAndStringNotEmpty stack string && isOtherCharacter (head string) = (False, (stack, tail string))
 | stackAndStringNotEmpty stack string && isOpenBracket (head string) = (False, ([head string] ++ stack, tail string))
 | stackAndStringNotEmpty stack string && isClosedBracket (head string) = 
  if areCorrespondingBrackets stack string then (True, (tail stack, tail string)) else (False, (stack, string))


--"repeatForNTimes" combines the function "balance" n times
repeatForNTimes :: Int -> CheckPar Bool
repeatForNTimes 0 = return True
repeatForNTimes 1 = balance
repeatForNTimes n = do {balance; repeatForNTimes (n - 1)}
