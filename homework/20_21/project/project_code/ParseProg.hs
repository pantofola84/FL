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



-------------------------------------------PROJECT PART 1---------------------------------------------------------------


--Smaller utility parsers



--We need to write the following Parsers

parseExpr :: Parser CoreExpr

parseAExpr :: Parser CoreExpr

parseDef :: Parser CoreDef

--An alternative is in the form:
--	<num> var1 ... varn -> expr
parseAlt :: Parser CoreAlt
parseAlt = do
 symbol "<"
 tag <- natural
 symbol ">"
 bs <- many parseVar
 symbol "->"
 body <- parseExpr
 return (tag, bs, body) 


------------------------------------------------------------------------------------------------------------------------



-----------------------------------------PROVIDED PARSERS---------------------------------------------------------------


parseProg :: Parser CoreProgram
parseProg = do
 scd <- parseScDef
 (do {symbol ";"; scds <-parseProg; return (scd: scds)} <|> return [scd])


parseScDef :: Parser CoreScDefn
parseScDef = do
 n <- parseVar
 vs <- many parseVar
 symbol "="
 body <- parseExpr
 return (n, vs, body)
  

------------------------------------------------------------------------------------------------------------------------
