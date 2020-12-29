module Main where


import Control.Applicative
import Control.Monad
import Data.Char


--1.	Copy all Parser definitions and various utilities (as explained on chapter 13 of the book).	DONE
--2.	Define the "wrong" Parser.									DONE


-----------------------PARSER DEFINITIONS AND UTILITIES-----------------------------------------------------------------


--Parser type definition
--A parser is essentially a function that takes a string, parses (possibly) a prefix of that same string and returns a l
--ist of couples composed by the parsed espression and by the remaining string. Conventionally the list contains only on
--e couple if the parsing operation was allright, otherwise the list is empty
newtype Parser a = P {parse :: String -> [(a, String)]}


--Item is the building block of the whole machinery
--Item parses the first character of the string and fails if the string is empty
item :: Parser Char
item = P $ (\s -> case s of
 [] -> []
 (c:cs) -> [(c, cs)])


--Parser is a Functor
instance Functor Parser where

-- fmap :: (a -> b) -> Parser a -> Parser b
 fmap f p = P $ (\s -> case parse p s of
  [] -> []
  [(v, s1)] -> [(f v, s1)])


--Parser is an Applicative Functor
instance Applicative Parser where

-- pure :: a -> Parser a
 pure v = P $ \s -> [(v, s)]

-- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
 pf <*> pa = P $ (\s -> case parse pf s of
  [] -> []
  [(f, s1)] -> parse (fmap f pa) s1)


--Parser is a Monad
instance Monad Parser where

-- return :: a -> Parser a
 return = pure

-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
 pa >>= f = P $ (\s -> case parse pa s of
  [] -> []
  [(v, s1)] -> parse (f v) s1)


--Let's instantiate Parser to the Alternative type class
instance Alternative Parser where

--The "empty" operator returns the Parser that always fails
-- empty :: Parser a
 empty = P $ \s -> []

--The (<|>) operator returns the result of the first successfull Parser for the input
-- (<|>) :: Parser a -> Parser a -> Parser a
 f <|> se = P $ (\s -> case parse f s of
  [] -> parse se s
  [(v, s1)] -> [(v, s1)])


--"sat" is a function that returns a Parser that fails if the specified condition is false
sat :: (Char -> Bool) -> Parser Char
sat p = do
 v <- item
 if p v then return v else empty


digit :: Parser Char
digit = sat isDigit


char :: Char -> Parser Char
char c = sat (== c)


--"string" is a function that returns the Parser for a specific String
string :: String -> Parser String
string [] = return []
string (c: cs) = do
 char c
 string cs
 return (c: cs)


--"nat" is a Parser for natural numbers
nat :: Parser Int
nat = do
 xs <- some digit
 return (read xs)


--"space" is a Parser that consumes zero or more space characters
space :: Parser ()
space = do
 many (sat isSpace)
 return ()


--"int" exploits the fact that a Parser is also an Alternative to obtain a Parser for integers
int :: Parser Int
int = (do {char '-'; n <- nat; return (-n)}) <|> nat


--"token" is a function that takes a Parser and allows it to ignore any space character placed before and/or after the a
--ctual token
token :: Parser a -> Parser a
token p = do 
 space
 v <- p
 space
 return v


--Now we can exploit the "token" function just defined

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol s = token (string s)

 
------------------------------------------------------------------------------------------------------------------------


--The most immediate way to write a Parser for our grammar (following close what is done in the reference book) is this:
--expr :: Parser Int
--expr = (do {e <- expr; symbol "-"; n <- natural; return (e - n)}) <|> natural
