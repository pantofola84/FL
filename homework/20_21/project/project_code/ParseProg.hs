module ParseProg where


import Parse
import Control.Applicative


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


--"isRec" tells us if we are in front of a "let" expression or a "letrec" expression
data IsRec = Recursive | NonRecursive deriving Show


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
 | ELet IsRec [Def a] (Expr a)
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



-------------------------------------------UTILITY FUNCTIONS------------------------------------------------------------


--A list of all Core Language keywords that can be mistaken as variables
keywordsList :: [String]
keywordsList = ["let", "in", "letrec", "case", "of"]


--Parser for EVar
--If the parsed identifier belongs to the list "keywordsList" it means that we have just read a keyword of the Core Lang
--uage: we can't create a variable with the same name of a keyword, so we make the Parser fail
parseEVar :: Parser CoreExpr
parseEVar = do
 v <- identifier
 if (v `elem` keywordsList) then empty else return (EVar $ v)


--Parser for ENum
parseENum :: Parser CoreExpr
parseENum = do
 n <- natural
 return (ENum $ n)


--Parser for EConstr
parseEConstr :: Parser CoreExpr
parseEConstr = do
 symbol "Pack{"
 tag <- natural
 symbol ","
 arity <- natural
 symbol "}"
 return (EConstr tag arity)


--Parser for parenthesized expressions
parseParenthesizedExpr :: Parser CoreExpr
parseParenthesizedExpr = do
 symbol "("
 expr <- parseExpr
 symbol ")"
 return expr


--Takes a (Parser t) and a String and returns a (Parser [t]). It looks for a sequence of one or more tokens separated by
-- the provided String
someSeparatedBy :: Parser a -> String -> Parser [a]
someSeparatedBy p s = do
 v <- p
 ((do {symbol s; vs <- someSeparatedBy p s; return (v: vs)}) <|> return [v])


--Parser for "let" expressions
parseLet :: Parser CoreExpr
parseLet = do
 symbol "let"
 defns <- someSeparatedBy parseDef ";"
 symbol "in"
 expr <- parseExpr
 return (ELet NonRecursive defns expr)


--Parser for "letrec" expressions
parseLetRec :: Parser CoreExpr
parseLetRec = do
 symbol "letrec"
 defns <- someSeparatedBy parseDef ";"
 symbol "in"
 expr <- parseExpr
 return (ELet Recursive defns expr)


--Parser for "let" and "letrec" expressions
parseLocal :: Parser CoreExpr
parseLocal = parseLet <|> parseLetRec


--Parser for "case" expressions
parseCase :: Parser CoreExpr
parseCase = do
 symbol "case"
 expr <- parseExpr
 symbol "of"
 alts <- someSeparatedBy parseAlt ";"
 return (ECase expr alts)


--Parser for lambda abstractions
parseLambda :: Parser CoreExpr
parseLambda = do
 symbol "\\"
 vs <- some identifier
 symbol "."
 expr <- parseExpr
 return (ELam vs expr) 
 

--Takes a list of CoreExpr and:
--	if it is a singleton, simply extract the expression from the list
--	otherwise combines all the various expression with the EAp constructor, associating to the left
makeAp :: [CoreExpr] -> CoreExpr
makeAp (ae: [])= ae
makeAp (ae1: ae2: aes) = foldl EAp (EAp ae1 ae2) aes


--Parser for an expression in the form:
--	aexpr1 ... aexprn (n >= 1)
--If n = 1 this case is the same as the one in which expr is a simple aexpr
--If n > 1 it's the application
parseAexprSeq :: Parser CoreExpr
parseAexprSeq = fmap makeAp (some parseAExpr)


--Takes an operator and two expressions and creates the corresponding CoreExpr
composeOp :: String -> CoreExpr -> CoreExpr -> CoreExpr
composeOp op e1 e2 = EAp (EAp (EVar op) (e1)) (e2)


--Parser for operators with a precedence level of 1
parse1 :: Parser CoreExpr
parse1 = (do {e2 <- parse2; symbol "|"; e1 <- parse1; return (composeOp "|" e2 e1)}) <|> parse2


--Parser for operators with a precedence level of 2
parse2 :: Parser CoreExpr
parse2 = (do {e3 <- parse3; symbol "&"; e2 <- parse2; return (composeOp "&" e3 e2)}) <|> parse3


--Parser for operators with a precedence level of 3
parse3 :: Parser CoreExpr
parse3 = (do
 e4a <- parse4
 op <- (((symbol "==" <|> symbol "~=") <|> symbol "<=") <|> ((symbol ">=" <|> symbol "<") <|> symbol ">"))
 e4b <- parse4
 return (composeOp op e4a e4b)) <|> parse4


--Parser for operators with precedence level of 4
parse4 :: Parser CoreExpr
parse4 = ((do {e5 <- parse5; symbol "+"; e4 <- parse4; return (composeOp "+" e5 e4)})
 <|> (do {e5a <- parse5; symbol "-"; e5b <- parse5; return (composeOp "-" e5a e5b)})) <|> parse5


--Parser for operators with precedence level of 5
parse5 :: Parser CoreExpr
parse5 = ((do {o1 <- parseAexprSeq; symbol "*"; o2 <- parse5; return (composeOp "*" o1 o2)})
 <|> (do {o1 <- parseAexprSeq; symbol "/"; o2 <- parseAexprSeq; return (composeOp "/" o1 o2)})) <|> parseAexprSeq


------------------------------------------------------------------------------------------------------------------------



--------------------------------------------PROJECT PARSERS-------------------------------------------------------------


--We need to write the following Parsers

--For the first part of the project we parse only the last four productions for Expr, namely:
--	a "let" expression in the form:	let defns in expr
--	a "letrec" expression in the form:	letrec defns in expr
--	a "case" expression in the form:	case expr of alts
--	a lambda expression in the form:	\ var1 ... varn . expr
--An Expr can also be an atomic expression AExpr
parseExpr :: Parser CoreExpr
parseExpr = (parseLocal <|> parseCase) <|> (parseLambda <|> parse1)


--An atomic expression can be in the form:
--	variable (we already have the Parser "identifier")
--	number (we already have the Parser "natural")
--	Pack{num, num}
--	(expr)
--"parseAExpr" becomes simply a list of the corresponding four utility parsers
parseAExpr :: Parser CoreExpr
parseAExpr = (parseEVar <|> parseENum) <|> (parseEConstr <|> parseParenthesizedExpr)


--A definition is in the form:
--	var = expr
parseDef :: Parser CoreDef
parseDef = do
 v <- identifier
 symbol "="
 expr <- parseExpr
 return (v, expr)


--An alternative is in the form:
--	<num> var1 ... varn -> expr
parseAlt :: Parser CoreAlt
parseAlt = do
 symbol "<"
 tag <- natural
 symbol ">"
 bs <- many identifier
 symbol "->"
 body <- parseExpr
 return (tag, bs, body) 


------------------------------------------------------------------------------------------------------------------------



-----------------------------------------PROVIDED FUNCTIONS-------------------------------------------------------------


comp :: [(CoreProgram, Name)] -> CoreProgram
comp [] = error "no parse"
comp [(cp, "")] = cp
comp [(_, rs)] = error ("doesn't use all input" ++ rs)


parseProg :: Parser CoreProgram
parseProg = do
 scd <- parseScDef
 (do {symbol ";"; scds <-parseProg; return (scd: scds)} <|> return [scd])


parseScDef :: Parser CoreScDefn
parseScDef = do
 n <- identifier
 vs <- many identifier
 symbol "="
 body <- parseExpr
 return (n, vs, body)
  

------------------------------------------------------------------------------------------------------------------------
