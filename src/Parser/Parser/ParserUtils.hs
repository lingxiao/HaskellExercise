
module ParserUtils (

	  alpha
	, digit
	, upper
	, lower
	, space

	, int
	, char
	, string
	, word

	, spaces
	, wsP
	, betweenS


) where


import Parser
import ParserCombinator
import Control.Monad
import Control.Applicative
import Data.Char


--------------------------
------ Primitives --------
--------------------------

alpha, digit, upper, lower, space :: Parser Char Char
alpha  = satisfy isAlpha
digit  = satisfy isDigit            
upper  = satisfy isUpper
lower  = satisfy isLower
space  = satisfy isSpace

-- | succeed only if the input is a (positive or negative) integer
int :: Parser Char Int
int = do 
  n <- string "-" <|> return []
  s <- many1 digit  
  return $ (read (n ++ s) :: Int)


------------------------------
-- Chars, Strings and Words --
------------------------------

-- | Parses and returns the specified character        
-- succeeds only if the input is exactly that character
char :: Char -> Parser Char Char
char c = satisfy (c ==)   

-- | Parses and returns the specified string. 
-- Succeeds only if the input is the given string
string :: String -> Parser Char String
string = mapM char


-- | TODO: complete this
-- | parse any combination of letters from alphabet 
word :: Parser Char String
word = do 
	w <- many1 alpha 
	spaces
	return w


------------
-- spaces --
------------

spaces :: Parser Char [Char]
spaces = many space

-- | gets rid of trailing whitespace after running parser p
wsP :: Parser Char b -> Parser Char b
wsP p = do
	v <- p 
	spaces
	return v

-- | parse between whitespace
betweenS :: Parser Char b -> Parser Char b
betweenS p = between spaces p spaces


-------------------------
-- terminal-words test --
-------------------------

-- these should fail right now
w1, w2 :: String
w1 = "the"
w2 = "theological"








