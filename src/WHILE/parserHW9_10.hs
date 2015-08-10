{- 
	Learn How to use applicatives as a model of computation
	Created: April 13th 

	lecture and hw source: http://www.seas.upenn.edu/~cis194/lectures.html
	http://www.cs.tufts.edu/~nr/cs257/archive/doaitse-swierstra/combinator-parsing-tutorial.pdf

	fyi: applicative.alternative implementation:
	http://www.haskell.org/ghc/docs/latest/html/libraries/base/src/Control-Applicative.html#Alternative
	http://www.haskell.org/ghc/docs/latest/html/libraries/base/src/Control-Applicative.html

	monad example from online: http://rosettacode.org/wiki/S-Expressions#Haskell
-}

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Array
import Data.List
import Data.Char


----------------
-- Data Types --
----------------

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }


-- declare toy data types to be parsed --
type Name     = String
type Phone    = String
data Employee = Employee { name :: Name, phone :: Phone } deriving ( Show )


---------------------------------------------------------------
-- Build complex parsers: functors, applicative, alternative --
---------------------------------------------------------------

-- | Note by mapping a functiong g over the fst item in tuple, we map it to a
-- | different value or a partially applied function
instance Functor Parser where
	fmap g ( Parser p ) = Parser $ \xs0 -> p xs0 >>= \(x,xs) -> return ( g x, xs )


-- | Note x1 below must be a partially applied function, taking in x2 as a parameter
instance Applicative Parser where
	pure a 					    = Parser $ \xs0 -> Just ( a, xs0 )
	(Parser p1) <*> (Parser p2) = Parser $ \xs0 -> do
		(x1, xs1) <- p1 xs0
		(x2, xs2) <- p2 xs1
		return ( x1 x2, xs2 )


instance Alternative Parser where
	empty						 = Parser $ const Nothing
	(Parser p1) <|> (Parser p2)  = Parser $ \ss -> let ss1 = p1 ss in case ss1 of
		Nothing  -> p2 ss
		_        -> ss1


{-
	Note these elegant implementations:

	inParser f = Parser . f . runParser

	first :: (a -> b) -> (a,c) -> (b,c)
	first f (x,y) = (f x, y)

	instance Functor Parser where
		fmap = inParser . fmap . fmap . first

	instance Applicative Parser where
		pure a = Parser (\s -> Just (a, s))
		(Parser fp) <*> xp = Parser $ \s ->
			case fp s of
			  Nothing     -> Nothing
			  Just (f,s') -> runParser (f <$> xp) s'

	instance Alternative Parser where
		empty = Parser (const Nothing)
		Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2


-}


-----------------------
-- Parser Components --
-----------------------

-- | Parser for first char in a stream
satisfy :: ( Char -> Bool ) -> Parser Char
satisfy g = Parser $ \stream -> case stream of
	(x:xs) | g x -> Just ( x, xs )
	otherwise    -> Nothing

-- | Parser for postive ints
posInt :: Parser Int
posInt = Parser $ \stream -> let ( s, ss ) = span isDigit stream in
	if null s then Nothing else Just ( read s, ss ) 

-- test satisfy --
testsat1 = runParser ( satisfy $ \x -> x == 'a' ) "ahello" :: Maybe ( Char, String )
testsat2 = runParser ( satisfy $ \_ -> False ) "ahello"    :: Maybe ( Char, String )

-- test posInt --
testPos1 = runParser posInt "hello"    :: Maybe ( Int, String )
testPos2 = runParser posInt "123hello" :: Maybe ( Int, String )


------------------------------------------
-- Test parsers built using applicative --
------------------------------------------


-- test fmap --
g :: Int -> Int
g a   = a + a
tmap1 = runParser ( fmap g posInt ) "123hello" :: Maybe ( Int, String )
tmap2 = runParser ( fmap g posInt ) "hello"    :: Maybe ( Int, String )


-- test applicative --


-- | a parser for name only
parseName :: Parser Name
parseName = Parser $ \stream -> let ( s, ss ) = span ( not . isDigit ) stream in
	if null s then Nothing else Just ( s, ss )


-- | a parser for phone only
parsePhone :: Parser Phone
parsePhone = Parser $ \stream -> let ( s, ss ) = span isDigit stream in 
	if null s then Nothing else Just ( s, ss )


-- | a parser for employee, built from phone and name parser
-- | Note same as parseEmp = fmap Employee parseName <*> parsePhone
-- | Note Employee takes two params: Name and Phone, and build Employee type
parseEmp :: Parser Employee
parseEmp = Employee <$> parseName <*> parsePhone


-- test parsers Constructed using Applicative -- 

-- | Build some other parsers using applicatives only
-- | note (,) is (\a b -> (a,b)) 
abParser :: Parser ( Char, Char )
abParser = (,) <$> ( satisfy $ \x -> x =='a' ) <*> ( satisfy $ \x -> x =='b'  )


abParser_ :: Parser ()
abParser_ = (\a b -> ()) <$> ( satisfy $ \x -> x =='a' ) <*> ( satisfy $ \x -> x =='b'  )


intPair :: Parser [Int]
intPair = (\a b c -> [a,c] ) <$> posInt <*> ( satisfy $ \x -> x == ' ' ) <*> posInt

-- test parseEmp -- 
tparse = runParser ( parseEmp ) "PatBateman5552155678" :: Maybe (Employee, String)


-- test abParser --
tab1 = runParser abParser "abcdef"  -- Just ((’a’,’b’),"cdef")
tab2 = runParser abParser "aebcdf"  -- Nothing


-- test abParser_ --
tab3 = runParser abParser_ "abcdef"  -- Just ((),"cdef")
tab4 = runParser abParser_ "aebcdf"  -- Nothing


-- test intPair --
tint1 = runParser intPair "12 34"     -- Just ([12,34],"")
tint2 = runParser intPair "hel34"     -- Nothing


-- test parsers Constructed using Alternative -- 

intOrUppercase :: Parser () 
intOrUppercase = let f a = () in ( f <$> posInt ) <|> ( f <$> satisfy isUpper )


iut1 = runParser intOrUppercase "342abcd"   -- Just ((), "abcd")
iut2 = runParser intOrUppercase "XYZ"       -- Just ((), "YZ")
iut3 = runParser intOrUppercase "foo"       -- Nothing


-- Slightly more sophisticated parsers --

-- | parse all letter conforming criteria
-- | short-circut with Nothing if first letter in stream does not match criteria 
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []


-- | parse all letter conforming criteria, return empty string if first letter does fail criteria
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p


-- test oneOrMore --
ot1 = runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"    -- Just ("ABC","dEfgH")
ot2 = runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"    -- Nothing

-- test zeroOrMore
zt1 = runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"   -- Just ("ABC","dEfgH")
zt2 = runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh"   -- Just ("","abcdeFGh")


-- | parse consecutive lists of spaces
spaces :: Parser [Char]
spaces = zeroOrMore $ satisfy isSpace


-- | parse nonempty seq of letters and digits, but cannot start with digit
ident :: Parser [Char]
ident = (++) <$> (oneOrMore $ satisfy isAlpha) <*> (zeroOrMore $ satisfy isAlphaNum) 


-- test ident
idt1 = runParser ident "foobar baz"    --- Just ("foobar"," baz")
idt2 = runParser ident "foo33fA"       --- Just ("foo33fA","")
idt3 = runParser ident "2bad"          --- Nothing
idt4 = runParser ident ""    		   --- Nothing



-- Putting it all together: Parser for an S-Expression --

-- | data type representing parsed s-expressions
type Ident = String
data Atom  = N Int | I Ident deriving ( Show )
data SExpr = A Atom | Comb [ SExpr ] deriving ( Show )
 

-- parser
parseSExpr :: Parser SExpr
parseSExpr =  Comb . join <$> many unit
	where unit = spaces *> ( (\a b -> [A (I a)] ++ b) <$> ident ) <* spaces <*> ( (\a -> [A (N a)]) <$> posInt <|> pure [] ) <* spaces


--Comb <$> parser
--parser = (++) <$> unit <*> parser <|> pure []


-- | test parseSExpr
p1 = "5"
p2 = "(bar (foo) 3 5 874)"
p3 = "(((lambda x (lambda y (plus x y))) 3) 5)"
p4 = "( lots of ( spaces in ) this ( one ) )"


----------------------------
-- Case study: Calculator --
----------------------------

-- primitive components --

-- parse operation


-- parse numbers



















