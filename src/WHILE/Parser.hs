{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

-- The basic definition of the parsing monad as developed in lecture.
-- Operations for building sophicsicated parsers are in the module
-- ParserCombinators.



module Parser (Parser,                  
  getC,  
  satisfy,
  (<|>),
  choose,
  runParser
  ) where


-----------------
-- Parser Type --
----------------- 

-- | note 'a' denote type of elements inside the list
newtype Parser a b = P { runParser :: [a] -> [(b, [a])] }

--------------------
-- Functor, Monad --
--------------------

instance Functor (Parser cs) where
  fmap g p = P $ \xs0 -> fmap (\(a,b) -> (g a, b)) $ runParser p xs0 


instance Monad (Parser cs) where
  return x  = P $ \xs0 -> [ (x, xs0) ]
  
  p1 >>= g  = P $ \xs0 -> do 
    (a,xs1) <- runParser p1 xs0
    runParser (g a) xs1
  
  fail _    = P $ \_   ->  []

-------------------------
-- Primitive Functions --
-------------------------

-- | Combine two parsers together in parallel, but only use the 
-- first result. This means that the second parser is used only 
-- if the fircs parser completely fails. 
(<|>) :: Parser a b -> Parser a b -> Parser a b
p1 <|> p2 = P $ \cs -> case runParser p1 cs of 
  (x:xs) -> x:xs
  []     -> runParser p2 cs

  
-- | Combine two parsers together in parallel, producing all 
-- possible results from either parser.                 
choose :: Parser a b -> Parser a b -> Parser a b
p1 `choose` p2 = P $ \cs -> runParser p1 cs ++ runParser p2 cs


-- | Return the next character
getC :: Parser a a
getC = P $ \cs -> case cs of 
  (x:xs) -> [ (x,xs) ]
  []     -> []

-- | Return the next character if it satisfies the given predicate
satisfy :: (a -> Bool) -> Parser a a
satisfy p = do 
  c <- getC
  if (p c) then return c else fail "End of input"



















































