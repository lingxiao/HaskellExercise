{- 
	Learn How to use Monads as a model of computation
	
	Created: April 17th 

	based on lecture:    http://www.seas.upenn.edu/~cis552/12fa/lectures/stub/Parsers.html
	parsec source:       http://code.haskell.org/parsec3/Text/Parsec/
	combinator examples: http://www.haskell.org/haskellwiki/Combinator_pattern

	Problems encountered:

	1. Combinator logic is blowing my mind and I have no mental model to reason functions built from it
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

-- | A parser parses a stream and return a list of possible results
-- | Each result is a tuple of parsed structureData, and remainder of stream
-- | Note it resembles a state monad
newtype Parser a = Parser { runParser :: String -> [ (a, String) ] } 


-----------------------
-- Parser as a Monad --
-----------------------

instance Functor Parser where
	fmap g p = Parser $ \xs0 -> liftM (\(a,b) -> (g a, b)) $ runParser p xs0 


instance Applicative Parser where
	pure a 	  = Parser $ \xs0 -> [ ( a, xs0 ) ]
	p1 <*> p2 = Parser $ \xs0 -> [ (a1 a2, xs2 ) | (a1, xs1) <- runParser p1 xs0, (a2, xs2) <- runParser p2 xs1 ]


instance Alternative Parser where
	empty	  = Parser $ const []
	p1 <|> p2 = Parser $ \ss -> runParser p1 ss ++ runParser p2 ss


-- | alternative implementation of >>= : join . fmap (\(a,xs1) -> runParser ( g a ) xs1 ) $ runParser p1 xs0
instance Monad Parser where
	return a = Parser $ \xs0 -> [ (a, xs0) ]
	p1 >>= g = Parser $ \xs0 -> [ (a2,xs2) | (a1, xs1 ) <- runParser p1 xs0, (a2, xs2) <- runParser ( g a1 ) xs1 ]



------------------------
-- Parser Combinators --
------------------------

-- | run both first and second parser
andP :: Parser a -> Parser a -> Parser a
p1 `andP` p2 = Parser $ \xs -> runParser p1 xs ++ runParser p2 xs


-- | run first parser, if it fails then run second
orP :: Parser a -> Parser a -> Parser a
p1 `orP` p2 = Parser $ \xs0 -> case runParser p1 xs0 of 
	(x:xs) -> x:xs
	[]     -> runParser p2 xs0

-- | run first parser, if it fails then run second
continueP :: Parser a -> Parser a -> Parser a
p1 `continueP` p2 = Parser $ \xs0 -> case runParser p1 xs0 of 
	(x:xs) -> runParser p2 xs0
	[]     -> []



-- | return all possible ways to parse a stream
manyP :: Parser a -> Parser [a]
manyP p = liftM2 (:) p ( manyP p ) `andP` return []

-- | return parsed stream of maximal length
mmanyP :: Parser a -> Parser [a]
mmanyP p = liftM2 (:) p ( mmanyP p ) `orP` return []



-- | check to see if stream empty before parsing
many1 :: Parser a -> Parser [a]
many1 p = ( liftM (\a -> [a]) p ) `continueP` mmanyP p


failP :: Parser a 
failP = Parser $ \_ -> []


satP :: ( Char -> Bool ) -> Parser Char
satP g = Parser $ \xs0 -> case xs0 of
	(x:xs) | g x -> [( x, xs )]
	otherwise    -> []


--------------------
-- Simple parsers --
--------------------

alphaChar :: Parser Char
alphaChar = satP isAlpha

digitChar :: Parser Char
digitChar = satP isDigit 

-- | In applicatve form: pure (\a -> ord a - ord '0' ) <*> digitChar
-- | In de-sugared monad form: digitChar >>= \c -> return $ ord c - ord '0'
digitInt :: Parser Int
digitInt  = do 
  c <- digitChar
  return $ ord c - ord '0'

char :: Char -> Parser Char
char c = satP $ (==) c


------------------------
-- Simple calculator  --
------------------------

parseOp :: Parser (Int -> Int -> Int )
parseOp = plus `andP` minus `andP` multiply `andP` divide
	where
		plus     = char '+' >> return (+) 
		minus    = char '-' >> return (-) 
		multiply = char '*' >> return (*) 
		divide   = char '/' >> return div


calculator :: Parser Int
calculator = do
	x  <- digitInt
	op <- parseOp
	y  <- digitInt
	return $ op x y


------------------------------------------------------
-- Parse recurring chars using recurive combinators --
------------------------------------------------------

-- | explicit recursion -> we got rid of the need for this using recursive combinators
string :: String -> Parser String
string ""     = return ""
string (c:cs) = char c >>= \_ -> string cs >>= \_ -> return $ c:cs


-- | here we demonstrate use of fmap or liftM to change output, and many1, a recursive combinator
oneNum :: Parser Int
oneNum = liftM read $ many1 digitChar 

-------------------------
-- Complex calculator  --
-------------------------


-- | a calculator that ops on arb long numbers w/o respect to order of operation -- 

calc1 :: Parser Int
calc1 = unit `orP` oneNum 
	where unit = do
		x  <- oneNum 
		op <- parseOp
		y  <- calc1
		return $ op x y

calc1' :: Parser Int 
calc1' = ( oneNum >>= \x -> parseOp >>= \op -> calc1' >>= \y -> return $ op x y ) `orP` oneNum



-- Stratify Parsers to respect order of operation --

-- component parser add/subtract
addOp :: Parser ( Int -> Int -> Int )
addOp = plus `andP` minus
	where
		plus  = char '+' >> return (+)
		minus = char '-' >> return (-)

-- component parser mult/divide
mulOp :: Parser ( Int -> Int -> Int )
mulOp = mult `andP` divide
	where
		mult   = char '*' >> return (*)
		divide = char '/' >> return div


sumE :: Parser Int
sumE = addE `orP` prodE
	where addE = do
		x  <- prodE
		op <- addOp
		y  <- sumE
		return $ op x y


prodE :: Parser Int
prodE = multE `orP` factorE
	where multE = do
		x  <- factorE
		op <- mulOp
		y  <- prodE
		return $ op x y


factorE :: Parser Int
factorE = parenE `orP` oneNum
	where parenE = do
		char '('
		n <- sumE
		char ')'
		return n









{-

sumE :: Parser Int 
sumE = addE `orP` prodE
	where addE = prodE >>= \x -> addOp >>= \op -> sumE >>= \y -> return $ op x y


prodE :: Parser Int
prodE = undefined


factorE :: Parser Int
factorE = undefined
-}



















