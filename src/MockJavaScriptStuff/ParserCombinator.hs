{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
{-# LANGUAGE RankNTypes #-} {-# LANGUAGE ImpredicativeTypes #-}


module ParserCombinator where


import ParserT
import Control.Monad
import Control.Applicative

type ParseError = String

---------------------------
-- Primitive Combinators --
--------------------------

-- | given a parser, apply it as many times as possible, but at least once.
many1 :: Monad m => ParserT a m b -> ParserT a m [b]
many1 p = do 
  x  <- p 
  xs <- many p 
  return $ x:xs


-- | Combine all parsers in the list (sequentially)
choice :: Monad m => [ParserT a m b] -> ParserT a m b
choice = foldr (<|>) (failP "All parsers failed")


-- | @chainl p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /left/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainl :: Monad m => ParserT a m b -> ParserT a m (b -> b -> b) -> b -> ParserT a m b
chainl p op x = chainl1 p op <|> return x


-- | Like 'chainl', but parses one or more occurrences of @p@.
chainl1 :: Monad m => ParserT a m b -> ParserT a m(b -> b -> b) -> ParserT a m b
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
between :: Monad m => ParserT a m open -> ParserT a m b -> ParserT a m close -> ParserT a m b
between open p close = do 
  _ <- open
  x <- p
  _ <- close
  return x

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: Monad m => ParserT a m b -> ParserT a m sep -> ParserT a m [b]
sepBy p sep = sepBy1 p sep <|> return []


-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
-- | Returns a list of values returned by @p@.
-- | example: runP ( sepBy int ( char 'a') )  "1a1a1a1a"    // [([1,1,1,1], "a")]
sepBy1 :: Monad m => ParserT a m b -> ParserT a m sep -> ParserT a m [b]
sepBy1 p sep = liftM2 (:) p (many (sep >> p))





