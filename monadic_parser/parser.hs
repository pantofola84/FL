--Polymorphic type that represents a Parser of type a, e.g. Parser Int is a parser for Int.
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


--Now we make
