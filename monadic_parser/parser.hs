import Control.Monad		--Library for monads
import Data.Char		--Contains the function "isSpace"
import Control.Applicative	--Library for applicatives


--NOTE: we have to define Parser to be an Applicative a Functor and an Alternative too. This is due to the so-called AMP
-- (Applicative Monad Proposal, https://wiki.haskell.org/Functor-Applicative-Monad_Proposal): it makes sense because, ma
--thematically speaking, every monad is also an applicative and a functor. The Alternative stuff is caused by a change i
--n the hierarchy of types 


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


--LAWS OF MONADS


--1.	return val >>= fun		=	fun val
--2.	par >>= return			=	par
--3.	par >>= (\t -> (f t >>= g)	=	(par >>= (\t -> f t)) >>= g
--
--Laws 1 and 2 tell us that return is the unit for the bind (>>=) operator
--Law 3 tells us that the bind operator is associative, that is to say that we can remove brackets and result will not
--change
--These laws must be valid for every instance of Monad


--apply is a function that effectively applies a Parser to a String
--NOTE: why do we need "apply"? Parser a itself isn't a function, so we need an explicit statement to extract and apply
--the encapsulated function
apply :: Parser a -> String -> [(a, String)]
apply (Parser f) = f

instance Monad Parser where
 return v 	= Parser $ (\s -> [(v, s)])
 par >>= fun 	= Parser $ (\str -> concat [apply (fun res) str' | (res, str') <- (apply par str)])


--Here we give generic but inefficient instantiation for applicative, functors and Alternative Monad
--See https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/7.10
instance Functor Parser where
 fmap = liftM


instance Applicative Parser where
 pure 	= return
 (<*>) 	= ap


instance Alternative Parser where
 (<|>)	= mplus
 empty	= mzero

--item is a Parser for Char
--item takes a String and behaves in the following way:
--	if s is empty then there is nothing to parse
--	if s is not empty then item parses the first character and returns the rest of the String
item :: Parser Char
item = Parser $ (\s -> case s of
 "" -> []
 (ch:chs) -> [(ch, chs)])


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
--The plus operator combines the results from the two parsers involved
instance MonadPlus Parser where
 mplus p q = Parser $ (\s -> apply p s ++ apply q s)
 mzero = Parser $ (\s -> [])


--The operator (+++) allow us to parse a string in two different ways. It then returns the first available result (if
--there is one)
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ (\s -> case apply (mplus p q) s of
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
 if pred ch then return ch else mzero


--Parser for a specific character
char :: Char -> Parser Char
char ch = sat (== ch)


--RECURSION COMBINATORS


--string is a Parser for a specific string
--The idea here is the following: parse the string using the tools defined previously and then return the
--corresponding Parser:
--	for the empty string there is nothing to parse, so just return the Parser;
--	otherwise parse it with a simple call to the Parser char and a recursive (take notice) call to the Parser
--	string, the return the Parser for the whole string
string :: String -> Parser String
string "" 	= return ""
string (c:cs) 	= do
 char c
 string cs
 return (c:cs)


--The many combinator applies a Parser t zero or more times to obtain a Parser [t]
--it tries to apply one or more time using the combinator many1 and if it fails it falls back to the Parser
--representing failure
many :: Parser t -> Parser [t]
many p = many1 p +++ return []


--The many1 combinator applies a Parser t one or more times to get a Parser [t]
--the first (sure) application of the Parser p is hardcoded, while the following zero or more are obtained by using the
--combinator many. It then returns the Parser for the list
many1 :: Parser t -> Parser [t]
many1 p = do
 v <- p
 vs <- Main.many p
 return (v:vs) 


--sepby takes a Parser a and a Parser b (the "separator"), then applies the auxiliary function "sepby_aux" to parse a
--string. If the parsing of the string fails it return the failure Parser
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby_aux` sep) +++ return []


--sepby_aux takes a Parser a and a Parser b (the "separator" or sep), then follows these steps:
--1.	applies p once;
--2.	within the do statement applies sep once, but doesn't retain the result, then returns p;
--3.	applies the newly obtained parser zero or more times (parse the separator and throws away its result, then
--	parses the string with p, then parse the separator again, and so on...);
--4.	composes results in a Parser for lists and returns it.
sepby_aux :: Parser a -> Parser b -> Parser [a]
p `sepby_aux` sep = do
 v <- p
 vs <- Main.many ( do { sep; p } )
 return (v:vs)


--chainl takes:
--1.	p: a Parser a
--2.	op: a Parser for a binary operator closed on a
--3.	a: a fallback value
--It returns a Parser that parses a string in which values are composed by using an operator that associates to the
--left: these values are parsed and combined by using the operator
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl_aux` op) +++ return a


--chainl_aux does the following things:
--1.	applies p once to obtain the value v;
--2.	applies the local defined function rest v:
--	a.	parses the binary function into f;
--	b.	parses the second value v2 applying p again;
--	c.	calls itself recursively using as a parameter f(v, v2);
--	d. 	if the parsing fails uses the parameter v as a fallback value.
--What happens is that the string "2+2" is parsed in the following way;
--1.	the character 2 gets parsed;
--2.	the first application of "rest" parses the rest of the string "+ 2" and calls itself recursively with 4 as a par
--	ameter;
--3.	the second application of "rest" fails because there is no string to parse;
--4.	"rest" falls back into the call "return 4" which is also the result for the whole "chainl_aux" function
chainl_aux :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl_aux` op = do {v <- p; rest v}
 where
  rest v = ( do { f <- op; v2 <- p; rest (f v v2) } ) +++ return v


--LEXICAL COMBINATORS
--Here we use combinators to define Parsers that handle the lexical aspect of the parsing process


--space is a Parser for strings of special characters, like strings of spaces or tabulation characters
--"isSpace" is an Haskell predicate that returns true if the character is a special character
--"sat isSpace" is a customized Parser that parses special characters
--this special Parser is applied zero or more times by using the combinator "many"
space :: Parser String
space = Main.many ( sat isSpace )


--token is a Parser that parses a substring and throws away any trailing string composed by special characters 
token :: Parser a -> Parser a
token p = do
 v <- p
 space
 return v


--symb parses a symbolic token
--In other words, it accepts a string, for example "ciao", and parses a token regardless of every trailing spaces whats
--oever, as in the string "ciao      "
symb :: String -> Parser String
symb s = token ( string s )


--The name is pretty explicative, innit?
eatLeadingSpaces :: Parser a -> Parser a
eatLeadingSpaces p = do
 space
 p
