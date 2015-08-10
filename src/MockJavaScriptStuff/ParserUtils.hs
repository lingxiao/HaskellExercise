{-# LANGUAGE RankNTypes #-} {-# LANGUAGE ImpredicativeTypes #-} 

module ParserUtils (

	  alpha
	, digit
	, upper
	, lower
	, space

	, int
	, char
	, name
	, letter
	, string
	, capLetter
	, lowerLetter

	, wsP
	, spaces
	, betweenS

	, commaSpc
	, parens
	, bracket


) where


import ParserT
import ParserCombinator
import Control.Monad
import Control.Applicative
import Data.Char


--------------------------
-- Primitive Components --
--------------------------

alpha, digit, upper, lower, space :: Monad m => ParserT Char m Char
alpha  = satisfy isAlpha
digit  = satisfy isDigit            
upper  = satisfy isUpper
lower  = satisfy isLower
space  = satisfy isSpace

-- | succeed only if the input is a (positive or negative) integer
int :: Monad m => ParserT Char m Int
int = do 
  n <- string "-" <|> return []
  s <- many1 digit  
  return $ (read (n ++ s) :: Int)


------------------------------
-- Chars and List of Chars ---
------------------------------

-- | Parses and returns the specified character        
-- succeeds only if the input is exactly that character
char :: Char -> Monad m => ParserT Char m Char
char c = satisfy (c ==)   

-- | Parses and returns the specified string. 
-- Succeeds only if the input is the given string
string :: String -> Monad m => ParserT Char m String
string = mapM char

-- * parse any capitalized and uncapped letters
letter :: Monad m => ParserT Char m Char
letter = lowerLetter <|> capLetter

-- * parse uncapped letters only
lowerLetter :: Monad m => ParserT Char m Char
lowerLetter = letters ls

-- * parse capped letters only
capLetter :: Monad m => ParserT Char m Char
capLetter = letters ls'

-- * some string that beings with a capped letter, followed by one or more uncapped letters
name :: Monad m => ParserT Char m String
name = (:) <$> capLetter <*> many1 lowerLetter


------------
-- spaces --
------------

spaces :: Monad m => ParserT Char m [Char]
spaces = many space

-- | gets rid of trailing whitespace after running parser p
wsP :: Monad m => ParserT Char m b -> ParserT Char m b
wsP p = do
	v <- p 
	spaces
	return v

-- | parse between whitespace
betweenS :: Monad m => ParserT Char m b -> ParserT Char m b
betweenS p = between spaces p spaces


------------
-- symbols --
------------

commaSpc :: Monad m => ParserT Char m String
commaSpc = string ", "

parens :: Monad m => ParserT Char m b -> ParserT Char m b 
parens p = between (char '(') p $ char ')'

bracket :: Monad m => ParserT Char m b -> ParserT Char m b
bracket p = between (char '[') p $ char ']'

------------
-- Utils --
------------

letters :: Monad m => [Char] -> ParserT Char m Char
letters = choice . fmap char 

ls, ls' :: [Char]
ls  = ['a'..'z']
ls' = toUpper <$> ls





