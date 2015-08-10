{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}


module ParserCombinator where


import Parser
import Control.Monad
import Control.Applicative
import System.IO


type ParseError = String

 
---------------------------
-- Primitive Combinators --
--------------------------


-- | given a parser, apply it as many times as possible,
-- but at least once.
many1 :: Parser a b -> Parser a [b]
many1 p = do 
  x  <- p 
  xs <- many p 
  return $ x:xs


-- | Combine all parsers in the list (sequentially)
choice :: [Parser a b] -> Parser a b
choice = foldr (<|>) (fail "")


-- | @chainl p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /left/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainl :: Parser a b -> Parser a (b -> b -> b) -> b -> Parser a b
chainl p op x = chainl1 p op <|> return x


-- | Like 'chainl', but parses one or more occurrences of @p@.
chainl1 :: Parser a b -> Parser a (b -> b -> b) -> Parser a b
p `chainl1` pop = p >>= rest
  where 
    rest x = next x <|> return x 
    next x = do 
      o <- pop
      y <- p
      rest $ x `o` y 


-- | given a parser, apply it as many times as possible                         
-- | and return the answer in a list
-- | reimplementation of many in control.Applicative
--many :: Parser a b -> Parser a [b]
--many p = many1 p <|> return []


--------------------------------
-- 'Phrase' Level Combinators --
--------------------------------

-- | @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is returned.
between :: Parser a open -> Parser a b -> Parser a close -> Parser a b
between open p close = do 
  _ <- open
  x <- p
  _ <- close
  return x

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: Parser a b -> Parser a sep -> Parser a [b]
sepBy p sep = sepBy1 p sep <|> return []


-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
-- | Returns a list of values returned by @p@.
-- | example: runP ( sepBy int ( char 'a') )  "1a1a1a1a"    // [([1,1,1,1], "a")]
sepBy1 :: Parser a b -> Parser a sep -> Parser a [b]
sepBy1 p sep = liftM2 (:) p (many (sep >> p))


----------------
-- Misc Utils --
----------------

-- | Use a parser for a particular string. Note that this parser
-- combinator library doesn't support descriptive parse errors.
-- However, for compatibility with Parsec, we give this function 
-- the same type.
parse :: Parser Char a -> String -> Either ParseError a
parse parser str = case (runP parser str) of 
    []      -> Left  "No parses"
    [(a,_)] -> Right a
    _       -> Left  "Multiple parses"
    
parseFromFile :: Parser Char a -> String -> IO (Either ParseError a)
parseFromFile parser filename = do 
  handle <- openFile filename ReadMode 
  str    <- hGetContents handle
  return $ parse parser str  








