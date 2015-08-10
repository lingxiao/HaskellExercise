{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

-- The basic definition of the parsing monad as developed in lecture.
-- Operations for building sophisticated parsers are in the module
-- ParserCombinators.

module Parser_o (Parser,                  
                   get,
                   choose,
                   (<|>),
                   satisfy,
                   runParser,  
                   ) where


---------------
-- Data Type --
---------------


newtype Parser a = P { runParser :: String -> [(a, String)] }


--------------------
-- Functor, Monad --
--------------------

instance Functor Parser where
  fmap g p = P $ \xs0 -> fmap (\(a,b) -> (g a, b)) $ runParser p xs0 


instance Monad Parser where
  return x  = P $ \cs -> [ (x, cs) ]
  p1 >>= g  = P $ \cs -> do 
    (a,cs') <- runParser p1 cs 
    runParser (g a) cs'
  fail _    = P $ \_ ->  []

-------------------------
-- Primitive Functions --
-------------------------

-- | Return the next character
-- (this was called 'oneChar' in lecture)
get :: Parser Char
get = P $ \cs -> case cs of 
  (x:xs) -> [ (x,xs) ]
  []     -> []


-- | Return the next character if it satisfies the given predicate
-- (this was called satP in lecture)
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do 
  c <- get
  if (p c) then return c else fail "End of input"

-- | Combine two parsers together in parallel, producing all 
-- possible results from either parser.                 
choose :: Parser a -> Parser a -> Parser a
p1 `choose` p2 = P $ \cs -> runParser p1 cs ++ runParser p2 cs

-- | Combine two parsers together in parallel, but only use the 
-- first result. This means that the second parser is used only 
-- if the first parser completely fails. 
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = P $ \cs -> case runParser p1 cs of 
  (x:xs) -> x:xs
  []     -> runParser p2 cs

































