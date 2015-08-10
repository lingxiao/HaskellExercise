{-
	While Parser
	Created: April 22nd

	lecture and hw source: http://www.seas.upenn.edu/~cis194/lectures.html
	http://www.cs.tufts.edu/~nr/cs257/archive/doaitse-swierstra/combinator-parsing-tutorial.pdf

	resources:
	state monad documentation: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2




-}

import Control.Monad
import Control.Monad.State
import Test.HUnit
import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
import qualified Text.PrettyPrint.HughesPJ as PP

import Parser
import ParserCombinator
import While_Interpreter


----------------
-- Data Types -- 
----------------

-- Tokens for Lexing --

data Token = 
     TokVar String       -- variables
   | TokVal Value        -- primitive values
   | TokBop Bop          -- binary operators
   | Keyword String      -- keywords        
      deriving (Eq, Show)

-- | a lexer parses a of lists of Chars into a list of Tokens

type Lexer = Parser Char [Token]


-----------
-- Utils --
-----------

-- gets rid of trailing whitespace after running parser p
wsP :: Parser Char b -> Parser Char b
wsP p = do
	v <- p 
	spaces
	return v

-- string w/ trailing whitespace
wstring :: String -> Parser Char [Char]
wstring = wsP . string 

-- parse between whitespace
betweenS :: Parser Char b -> Parser Char b
betweenS p = between spaces p spaces


-----------------------
-- Parsing Constants -- 
-----------------------

-- begin valueP --

-- main function to be used
valueP :: Parser Char Value
valueP = intP <|> boolP

-- parse ints
intP :: Parser Char Value
intP = liftM IntVal int

-- parse booleans
boolP :: Parser Char Value
boolP = constP "true" (BoolVal True) <|> constP "false" (BoolVal False)

-- end valueP --

-- parse binary operators
opP :: Parser Char Bop 
opP = choice . liftM (\(a,b) -> constP a b ) $ [("+",Plus),("-",Minus),("*",Times),("/",Divide),(">=",Ge),("<=",Le),(">",Gt),("<",Lt)]

-- a util function to map String to Value
constP :: String -> a -> Parser Char a
constP str a = string str >> return a

-- | parse a variable, which is a string of cap alphas
varP :: Parser Char Variable
varP = many1 upper


------------
-- Lexing --
------------

-- Lexing functions -- 

keywords :: [ Parser Char Token ]
keywords = map (\x -> constP x (Keyword x)) 
             [ "(", ")", ":=", ";", "if", "then", "else", "endif", "while", "do", "endwhile", "skip" ]

lexer :: Lexer 
lexer = sepBy1 (liftM TokVal valueP <|> liftM TokVar varP <|> liftM TokBop opP <|> choice keywords) (many space)


-- ID Tokens --
isTokVar, isTokVal, isTokBop, isKeyWord :: Token -> Bool

isTokVar (TokVar _ )   = True
isTokVar _ 		       = False

isTokVal (TokVal _ )   = True
isTokVal _ 			   = False

isTokBop (TokBop _)    = True
isTokBop _ 			   = False


-- Keyword Parser --
keyword :: String -> Parser Token Token
keyword t = satisfy (== Keyword t)


--------------------------------------
-- Parsing Expressions from [Token] --
--------------------------------------

-- | parse an whole expression
exprP :: Parser Token Expression
exprP = chainl1 ( tparens <|> tvalP <|> (liftM Var tvarP) ) topP


tparens :: Parser Token Expression
tparens = between ( keyword "(" ) exprP ( keyword ")" )


tvalP :: Parser Token Expression
tvalP = satisfy isTokVal >>= \(TokVal a) -> return $ Val a 

topP :: Parser Token (Expression -> Expression -> Expression)
topP = satisfy isTokBop >>= \(TokBop a) -> return $ Op a


tvarP :: Parser Token String
tvarP = satisfy isTokVar >>= \(TokVar a) -> return a


-----------------------------------
-- Parsing Statements of [Token] --
-----------------------------------

statementP :: Parser Token Statement
statementP  = chainl1 ps $ keyword ";" >> return Sequence 
	where ps = skipmnt <|> assignmnt <|> ifmnt <|> whilemnt


skipmnt :: Parser Token Statement
skipmnt = keyword "skip" >> return Skip


assignmnt :: Parser Token Statement
assignmnt = do
	var <- tvarP
	keyword ":="
	val <- exprP
	return $ Assign var val

ifmnt :: Parser Token Statement
ifmnt = do
	keyword "if"
	e  <- exprP
	keyword "then"
	s1 <- statementP
	keyword "else"
	s2 <- statementP
	return $ If e s1 s2

whilemnt :: Parser Token Statement
whilemnt = between ( keyword "while" ) p $ keyword "endwhile"
	where p = do
		e <- exprP
		keyword "do"
		s <- statementP
		return $ While e s


------------------
-- Parser Tests --
------------------

-- test exprP -- 
me1 = "10"              -- Val $ IntVal 10
me2 = "X"               -- Val $ Var "X"
me3 = "1+1"             -- Op Plus ( Val IntVal 1) ( Val IntVal 1)
me4 = "X<5"             -- Op Lt ( Var "X" ) ( Val IntVal 5 )
me5 = "X>=6"            -- Op Ge ( Var "X" ) ( Val IntVal 6 )
me6 = "X+12"            -- Op Add ( Var "X" ) ( Val IntVal 12 )
me7 = "X + 12 - 13"     -- Op Add ( Var "X" ) ( Op Minus (Val IntVal 12) (Val IntVal 13) )
me8 = "((1 + 2) - 3) + (1 + 3)"  

{-
me8 S-expression
	Op Plus 
		(Op Minus 
			(Op Plus 
				(Val (IntVal 1)) 
				(Val (IntVal 2))
			) 
			(Val (IntVal 3))) 
		(Op Plus 
			(Val (IntVal 1)) 
			(Val (IntVal 3))
		)
-}


mes = [me1,me2,me3,me4,me5,me6,me7, me8] :: [[Char]]


-- test statementP --
st1 = "X := 1"		 							-- Assign "X" ( Val $ IntVal 1)
st2 = "X := Y"		 							-- Assign "X" ( Var "Y" )
st3 = "if X < 0 then X := 1 else X := Y"  		-- If (Op Gt (Var "X") ( Val $ IntVal 0) (st1) (Skip) )
st4 = "while X > 0 do X := X - 1 endwhile"		-- While (Op Gt (...) (...))
st5 = "skip"
st6 = st3 ++ "; " ++ st4					    -- Sequence (...) (...)
st7 = "X := 1; Y := X; Z := true"               -- Sequence (Assign "X" (...) ) ( Sequence (Assign "Y" (...)) (Assign "Z" (...)))

sts = [st1,st2,st3,st4,st5,st6,st7] :: [[Char]]


-- provided test
t11 :: Test
t11 = TestList ["s1" ~: succeed (parse exprP' "1 "),
                "s2" ~: succeed (parse exprP' "1  + 2") ] where
  succeed (Left _)  = assert False
  succeed (Right _) = assert True













-------------------------------------- Parsing without Lexing ---------------------------------------------------------------


-------------------------------------
-- Parsing Expressions from [Char] --
-------------------------------------

-- | parse an whole expression
exprP' :: Parser Char Expression
exprP' = chainl1 ps $ liftM Op $ wsP opP
	where
		ps      = parensP <|> (wsP . liftM Val $ valueP)  <|> (wsP . liftM Var $ varP)
		parensP = between ( betweenS $ char '(' ) exprP' ( betweenS $ char ')' )


	

----------------------------------
-- Parsing Statements of [Char] --
----------------------------------

statementP' :: Parser Char Statement
statementP'  = chainl1 ps $ wstring ";" >> return Sequence 
	where ps = skipmnt' <|> assignmnt' <|> ifmnt' <|> whilemnt'


skipmnt' :: Parser Char Statement
skipmnt' = wstring "skip" >> return Skip


assignmnt' :: Parser Char Statement
assignmnt' = do
	var <- wsP varP
	wstring ":="
	val <- exprP'
	return $ Assign var val


ifmnt' :: Parser Char Statement
ifmnt' = do 
	wstring "if" 
	e  <- exprP'
	wstring "then"
	s1 <- statementP'
	wstring "else"
	s2 <- statementP'
	return $ If e s1 s2


whilemnt' :: Parser Char Statement
whilemnt' = between ( wstring "while" ) p $ wstring "endwhile"
	where p = do
		e <- exprP'
		wstring "do"
		s <- statementP'
		return $ While e s



-- Some more examples of WHILE syntax --

{-
	X := ((1 + 2) - 3) + (1 + 3);
	Y := 0;
	while X > 0 do
	  Y := Y + X;
	  X := X - 1
	endwhile

	N := 2;
	F := 1;
	while N > 0 do
	  X := N;
	  Z := F;
	  while X > 1 do
	    F := Z + F;
	    X := X - 1
	  endwhile; 
	  N := N - 1
	endwhile

	X := 0 - 3;
	if X < 0 then
	  X := 0 - X
	else
	  skip
	endif

	X := 10;
	Y := 3;
	Z := 0;
	while X > 0 do
	  Z := Z + Y;
	  X := X - 1
	endwhile

-}



