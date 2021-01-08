module ParseProg where


import Parse


------------TYPE DEFINITIONS FOR THE CORE LANGUAGE----------------------------------------------------------------------


--"Name" is an alias for the type String
type Name = String


--"Def" is an alias for definitions in a let(rec) construct
--The first component of the couple is the binder, while the second is the bound expression
type Def a = (a, Expr a)

type CoreDef = Def Name


--"Alter" is an alias for alternatives within a case statement
--The first Int is the tag of the Pack constructor, then we have a list of binders associated with the constructor, and 
--at the end the expression to evaluate
type Alter a = (Int, [a], Expr a)

type CoreAlt = Alter Name


--"isRec" is a simple alias for the boolean type
type isRec = Bool

recursive :: isRec
recursive = True

nonRecursive :: isRec
nonRecursive = False


--Expr represents the expression production of the Core Language
--It is parameterized by the type of its binders, that is to say, the Haskell type of the formal variables in a "let" or
-- lambda abstraction
--1.	EVar represents the variable type: the formal parameter represents the name of the variable
--2.	ENum represents a numeric value
--3.	EConstr is the Pack constructor: the first integer is the tag while the second is the arity
--4.	EAp represents the application of an expression to another one
--5.	ELet has three parameters:
--		a.	The first one is really a boolean that enables us to distinguish between Let and LetRec
--		b.	Then we have a list of definitions, in other words couples composed by binders and the associate
--			d expression
--		c.	The actual expression to evaluate
--6.	ECase represents the case statements: it has an expression to evaluate and scrutinize and a list of Alternatives
--	which are triples in the form (tag, [binder], associated expression)
--7.	ELam represents the lambda abstraction, with a list of binders and an associated expression
data Expr a = EVar Name
 | ENum Int
 | EConstr Int Int
 | EAp (Expr a) (Expr a)
 | ELet isRec [Def a] (Expr a)
 | ECase (Expr a) [Alter a]
 | ELam [a] (Expr a)
 deriving Show


--The parameterized type for Expr will always be Name, so here we make a little alias
type CoreExpr = Expr Name


--A supercombinator is a triple in the form (name of the supercombinator, [binders], an Expr, the body of the supercombi
--nator)
--It is parameterized by the type of its binders
type ScDefn a = (Name, [a], Expr a)

type CoreScDefn = ScDefn Name


--A program is simply a list of supercombinators definitions
type Program a = [ScDefn a]

type CoreProgram = Program Name


------------------------------------------------------------------------------------------------------------------------



--------------FUNCTION AND OTHER UTILITIES FOR THE CORE LANGUAGE--------------------------------------------------------


--"bindersOf" takes a list of couples in the form binder-expression and returns the list of binders
bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]


--"rhssOf" takes a list of couples in the form binder-expression and returns the list of expressions
rhssOf :: [(a, b)] -> [b]
rhssOf defns = [name | (name, rhs) <- defns]


--"isAtomicExpr" is a simple function that recognizes expressions with no internal structure (namely variables and numbe
--rs)
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr _ = False


------------------------------------------------------------------------------------------------------------------------



---------------------LEXICAL ANALYSIS-----------------------------------------------------------------------------------


--Before the actual parsing of the program, we want to perform a text pre-processing operation called "tokenization" (se
--e "https://en.wikipedia.org/wiki/Lexical_analysis#Tokenization"). Tokenization helps to divide the main task of parsin
--g the program into sub-tasks, and in this way enables us to reduce the overall complexity of the actual parser


--"Token" is a simple alias for the String type
type Token = String


--Returns true if the character is a white space, a tabulation character or a newline character, false otherwise
isWhiteSpace :: Char -> Bool
isWhiteSpace c = c 'elem' " \t\n"


--Returns true if the character is an alphanumeric character or an underscore, false otherwise
isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || (c == '_')


--Returns true if the parameter String starts with a comment
isComment :: String -> Bool
isComment (c: []) = False
isComment (c1: c2: cs)
 | c1 == '|' && c2 == '|' = True
 | otherwise = False


--A list of all binary operators
twoCharOps :: [String]
twoCharOps = ["==", "~=", "<=", ">=", "->"]


--Returns true if the string starts with a binary operator, false otherwise
isBinaryOperator :: String -> Bool
isBinaryOperator (c: []) = False
isBinaryOperator (c1: c2: cs) = [c1, c2] `elem` twoCharOps


--"clex" is the function that performs the lexical analysis. It accepts a String and returns a list of Tokens. The behav
--iour of "clex" is the following:
--	- throws away white spaces and comments (a comment starts with "||" and continues 'til the end of the line)
--	- recognizes a number as a single Token
--	- recognizes a binary operator as a single Token
--	- recognizes variables and identifiers (in the form of an alphabetic letter followed by zero or more alphanumeri
--	  c characters and/or the character '_') as a single Token
--	- if none of the above cases succeeds, it returns a Token composed by a single character
--	- if the input String is empty, it returns the empty list of Tokens
clex :: String -> [Token]
clex [] = []
clex (c: cs)
 | isWhiteSpace c = clex cs
 | isBinaryOperator (c: cs) = [c, head cs] : clex (tail cs)
 | isComment (c: cs) = (clex.tail.dropWhile (/= '\n')) cs
 | isDigit c = num_token : clex rest_cs
  where num_token = c : takeWhile isDigit cs
  rest_cs = dropWhile isDigit cs
 | isAlpha c = var_tok : clex rest_cs
  where var_tok = c : takeWhile isIdChar cs
  rest_cs = dropWhile isIdChar cs
 | otherwise = [c] : clex cs


------------------------------------------------------------------------------------------------------------------------
