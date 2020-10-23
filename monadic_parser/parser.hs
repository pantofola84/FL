--Polymorphic type that represents a Parser of type a, e.g. Parser Int is a parser for type Int.
--A parser is essentially a function that takes a String: a prefix of the String is parsed and a couple in the form
--(a, String) is returned, where the first element is the result of the parsing, and the second element is what remains
--of the String to be parsed
newtype Parser t = Parser (String -> [(t, String)])


--The type Parser is a Monad:
--
--return :: t -> Parser t
--return function "encapsulates" a value inside a Parser, that is, the value is immediately returned as result;
--
--`bind` :: Parser p -> (p -> Parser q) -> Parser q 
--the bind operator is implemented in the following way:
--1. 	the input string is given to the parser par: the result is a list of couples that we iterate with a list
-- 	generator
--2.	every result res is then given to the function fun which in turn results in a parser, aka a function that
--	takes a string
--3.	we are in the body of our lambda, so we are interested in the result: we apply our newly obtained parser to the
--	string str' and we get a list of couples
--4.	so now we have a list of lists of couples, that we flatten out with concat
--
--LAWS OF MONADS
--
--1.	return val >>= fun		=	fun val
--2.	par >>= return			=	par
--3.	par >>= (\t -> (f t >>= g)	=	(par >>= (\t -> f t)) >>= g
--
--Laws 1 and 2 tell us that return is the unit for the bind (>>=) operator
--Law 3 tells us that the bind operator is associative, that is to say that we can remove brackets and result will not
--change
--These laws must be valid for every instance of Monad
instance Monad Parser where
 return v 	= Parser $ (\s -> (v, s))
 par >>= fun 	= Parser $ (\str -> concat [(fun res) str' | (res, str') <- par str])


--item is a Parser for Char
--item takes a String and behaves in the following way:
--	if s is empty then there is nothing to parse
--	if s is not empty then item parses the first character and returns the rest of the String
item :: Parser Char
item = Parser $ (\s -> case s of
 "" = []
 (ch:chs) = [(ch, chs)])

--One can imagine a Parser as a "little box" that takes a specific value and parses it.
--The "do" notation is the magic spell that enables us to parse strings:
--Example:	do {item; c <- item; return c}
--The code above can be interpreted in the following way:
--1.	parse the first character of the string;
--2.	parse the second character of the string and give it the name of c;
--3.	return the generic Parser c (imagine it as a box that parses always the character c for whatever string we pass
--	to it)

--CONDITIONAL PARSERS

--Now we make Parser an instance of MonadZero and MonadPlus
--This means we equip it with a binary operator "plus" (++) and a "zero" element
--The "zero" element and the operator ++ satisfy:
--1.	zero ++ p 	= p
--2.	p ++ zero 	= p
--3.	p ++ (q ++ r)	= (p ++ q) ++ r
--That is to say that "zero" element is the identity element with respect to ++, and the plus operator is associative
--These properties are also valid:
--1.	zero >>= fun			= zero
--2.	par >>= const zero		= zero
--3.	(p ++ q) >>= fun		= (p >>= fun) ++ (q >>= fun)
--4.	par >>= (\v -> f v ++ g v)	= (par >>= f) ++ (par >>= g)
--"zero" is the right and left zero element for the operator >>=
--(>>=) distributes through (++) both on the right and on the left
instance MonadZero Parser where
 zero = Parser $ (\s -> [])
--The plus operator combines the results from the two parsers involved
instance MonadPlus Parser where
 p ++ q = Parser $ (\s -> p s ++ q s) 


--The operator (+++) allow us to parse a string in two different ways. It then returns the first available result (if
--there is one)
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ (\s -> case (p ++ q) of
 [] -> []
 (f:r) -> [f])


--sat takes a predicate on characters as its input and returns:
--	the parsed char if it satisfies pred
--	the Parser "zero" (aka failure) otherwise
--one can now define specific parsers by passing the right predicate as a parameter
--Example: sat (== c) is the Parser for the specific character c 
sat :: (Char -> Bool) -> Parser Char
sat pred = do
 ch <- item
 if pred ch then return ch else zero


--Parser for a specific character
char :: Char -> Parser Char
char ch = sat (== ch)


--RECURSION COMBINATORS


--string is a Parser for a specific String
string :: String -> Parser String
