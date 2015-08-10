{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Recreate Parser using Monad.List and Monad.State 
-- | Created: April 30th

------------------------------------------------------------------------------
------------------------------------------------------------------------------



module ParserTrans (GenParser, Parser, 
                   getC,
                   choose,
                   (<|>),
                   satisfy,
                   runParser,  
                   ) where

import Control.Monad.Identity
import Control.Monad.List
import Control.Monad.State 


-----------------
-- Parser Type --
-----------------


type GenParser a b = StateT [a] ( ListT Identity ) b

runGenP :: GenParser a b -> [a] -> [(b,[a])]
runGenP m st = runIdentity . runListT $ runStateT m st


type Parser b = GenParser Char b 

runParser :: Parser b -> [Char] -> [(b,[Char])]
runParser = runGenP


-------------------------
-- Primitive Functions --
-------------------------

-- | Combine two parsers together in parallel, producing all 
-- | possible results from either parser.                 
choose :: GenParser e a -> GenParser e a -> GenParser e a
p1 `choose` p2 = undefined


-- | Combine two parsers together in parallel, but only use the 
-- | first result. This means that the second parser is used only 
-- | if the first parser completely fails. 
(<|>) :: GenParser e a -> GenParser e a -> GenParser e a
p1 <|> p2 = undefined


-- | Return the next character
getC :: GenParser e e 
getC = do
  (x:xs) <- get
  put xs
  return x


-- | Return the next character if it satisfies the given predicate
-- | (this was called satP in lecture)
satisfy :: (e -> Bool) -> GenParser e e 
satisfy p = do 
  c <- getC
  if (p c) then return c else fail "End of input"






















