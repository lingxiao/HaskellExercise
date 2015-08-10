{- 
	Learn How to use applicatives as a model of computation to build a calculator
	Created: April 20th 

	lecture and hw source: http://www.seas.upenn.edu/~cis194/lectures.html
	http://www.cs.tufts.edu/~nr/cs257/archive/doaitse-swierstra/combinator-parsing-tutorial.pdf

-}

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Char

----------------
-- Data Types --
----------------

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }


---------------------------------------
-- Functor, Applicative, Alternative --
---------------------------------------

-- | Note by mapping a function g over the fst item in tuple, we map it to a
-- | different value or a partially applied function
instance Functor Parser where
	fmap g p = Parser $ \xs0 -> runParser p xs0 >>= \(x,xs) -> return ( g x, xs )


-- | Appliative's app function abstract the action: apply outcome of first parser to outcome of second parser
instance Applicative Parser where
	pure a     = Parser $ \xs0 -> Just ( a, xs0 )
	p1 <*> p2  = Parser $ \xs0 -> do
		(g, xs1) <- runParser p1 xs0
		(x, xs2) <- runParser p2 xs1
		return ( g x, xs2 )


instance Alternative Parser where
	empty	  = Parser $ const Nothing
	p1 <|> p2 = Parser $ \ss -> runParser p1 ss <|> runParser p2 ss


-- | Monad's bind function abstract the action: build new parser based on the result of previously parsed expression
instance Monad Parser where
	return   = pure
	p1 >>= g = Parser $ \xs0 -> do 
		(a,xs1) <- runParser p1 xs0
		runParser ( g a ) xs1


---------------------------
-- Primitive Combinators --
---------------------------

-- | parse all letter conforming criteria, short-circuit with empty string
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- | parse all letter conforming criteria, short-circut with Nothing
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p


-- a combinator that abstract away a notion of 'foldl' over the stream of parsed values
-- note this is possible w/ the monad interface
chainl :: Parser b -> Parser (b -> b -> b) -> Parser b
p `chainl` pop  = p >>= rest
   where 
   		rest x = next x <|> return x 
		next x = do 
			o <- pop
			y <- p
			rest $ x `o` y 


--------------------------
-- Primitive Components --
--------------------------

-- | Parser for first char in a stream
satisfy :: ( Char -> Bool ) -> Parser Char
satisfy g = Parser $ \stream -> case stream of
	(x:xs) | g x -> Just ( x, xs )
	otherwise    -> Nothing

alphaChar :: Parser Char
alphaChar = satisfy isAlpha

digitChar :: Parser Char
digitChar = satisfy isDigit 

digitInt :: Parser Int
digitInt  = pure (\a -> read [a]) <*> digitChar

char :: Char -> Parser Char
char c = satisfy $ (==) c

-- | parse consecutive lists of spaces
spaces :: Parser [Char]
spaces = zeroOrMore $ satisfy isSpace


stripSpace :: Parser a -> Parser a
stripSpace p = spaces *> p <* spaces


---------------------------
-- Calculator Components --
---------------------------

-- arithmetic operation
add, sub, mult, divide :: Parser ( Int -> Int -> Int )
add    = stripSpace $ char '+' *> pure (+) 
sub    = stripSpace $ char '-' *> pure (-) 
mult   = stripSpace $ char '*' *> pure (*) 
divide = stripSpace $ char '/' *> pure div 


-- parse arithmetic operation that are the same precedants
intAdd :: Parser ( Int -> Int -> Int )
intAdd = add <|> sub
		

intMult :: Parser ( Int -> Int -> Int )
intMult = mult <|> divide

-- parse all arithmetic operation regardless of precedants
intOp :: Parser ( Int -> Int -> Int )
intOp = intAdd <|> intMult


-- parse string of numbers
ints :: Parser Int
ints = read <$> ( spaces *> zeroOrMore digitChar <* spaces )


----------------------
-- Some Calculators --
----------------------

-- a simple calculator, parse one pairwise operation --
calc0 :: Parser Int 
calc0 = ints <**> intOp <*> ints


-- parse n-wise operations, w/o respect to orders of operation --
-- does not parse parens
calc1 :: Parser Int
calc1 = unit <*> calc1 <|> ints
	where unit  = ints <**> intOp


-- parse nwise operation wrt orders of operation and parens, but is left associative --
-- this is just an alias for calcSum
calc2 :: Parser Int
calc2 = calcSum


-- | this is a 'gateway' into the parsing machine
calcSum :: Parser Int
calcSum = sumed <|> calcProd
	where sumed = calcProd <**> intAdd <*> calcSum


calcProd :: Parser Int
calcProd = prod <|> calcParens
	where prod = calcParens <**> intMult <*> calcProd


calcParens :: Parser Int
calcParens = char '(' *> calcSum <* char ')' <* spaces <|> ints


-- parse nwise operation wrt orders of operation and parens, and is right associative --
calc3 :: Parser Int
calc3 = calcSum1


-- use chainl combinator to combine parsers and reduce boilerplate
calcSum1 :: Parser Int
calcSum1 = calcProd1 `chainl` intAdd

calcProd1 :: Parser Int
calcProd1 = calcParens1 `chainl` intMult

calcParens1 :: Parser Int
calcParens1 = char '(' *> calcSum1 <* char ')' <* spaces <|> ints


-- | manually write a calcSum function that is left associative w/o using logic that was rolled into the `chainl` combinator
-- | calc parses an int and then build a new parser from the int
-- | the new parser is either next, or just returns the int in min context
-- | next parses next op, next int, and then apply op on ints, and recurse
calcSum1' :: Parser Int
calcSum1' = ints >>= rest
	where 
		rest x = next x <|> return x
		next x = intAdd >>= \op -> ints >>= \y -> rest $ op x y


------------------------------
-- Context-sensitive Parser --
------------------------------

parseFile :: Parser [[Int]]
parseFile = many parseLine

-- | note a new parser is built using result of previous parser
parseLine :: Parser [Int]
parseLine = digitInt >>= \i -> replicateM i digitInt 



-- semicolon-chained sentences, a mockup for WHILE --

string :: Parser [Char]
string = zeroOrMore alphaChar

-- semicolon and spaces
semic :: Parser [Char]
semic = (\a -> [a] ) <$> char ';' <* spaces

-- statement is a string followed by a semicolon
statement :: Parser [Char]
statement = string <* semic 

-- a sentence is either a statement, many statements, or a string of numbers
sentence :: Parser [[Char]]
sentence = pure <$> (oneOrMore digitChar) <|> many statement <|> pure <$> statement













