module Balance where


--The type State is a couple of Strings which represent the following things:
--	the first string of the couple is the stack used to remember the open parentheses found so far;
--	the second element is the remaining string that has to be checked yet.
type State = (String, String)


--The type CheckPar is the function that allows us to make stateful computations. In fact it is our equivalent of the St
--ate Monad.
--NOTE: adding curly brackets allows us to name the characteristic function
newtype CheckPar a = CP {runCheckPar :: State -> (a, State)}


--CheckPar needs an application function to "run" the stateful computation
--INPUT:
--	a CheckPar
--	an initial State
--OUTPUT:
--	the couple (a, State) resulting from the stateful computation
app :: CheckPar a -> State -> (a, State)
app = runCheckPar
