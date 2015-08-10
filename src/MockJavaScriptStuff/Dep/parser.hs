{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE UnicodeSyntax #-}



module Parser (
    Parser
  , runP
  , getC
  , satisfy
  ) where

import Control.Monad
import Control.Applicative 
import Data.Monoid
import Data.Char


---------------
-- Data Type --
--------------- 

-- * TODO: add monadic effect to parser 
newtype Parser a b = P { runP :: [a] -> [(b, [a])] }


---------------
-- Typeclass --
---------------

instance Functor (Parser a) where
  fmap g p = P $ \as -> (\(a,b) -> (g a, b)) <$> runP p as 


instance Applicative (Parser a) where
  pure a     = P $ \as -> [(a,as)]
  p1 <*> p2  = P $ \as -> do
    (g, as')  <- runP p1 as
    (x, as'') <- runP p2 as'
    return ( g x, as'' )


instance Monad (Parser a) where
  return x  = P $ \as -> [ (x, as) ]
  
  p1 >>= g  = P $ \as -> do 
    (a,as') <- runP p1 as
    runP (g a) as'
  
  fail _    = P $ \_   ->  []


instance Alternative (Parser a) where
  empty     = P $ const []
  p1 <|> p2 = P $ \as -> case runP p1 as of 
    (x:xs) -> x:xs
    []     -> runP p2 as


-- | mappend is taking over for choose
instance Monoid (Parser a b) where
  mempty = empty
  p1 `mappend` p2 = P $ \as -> runP p1 as ++ runP p2 as


-------------------------
-- Primitive Functions --
-------------------------

-- | Return the next character
getC :: Parser a a
getC = P $ \as -> case as of 
  (x:xs) -> [(x,xs)]
  []     -> []

-- | Return the next character if it satisfies the given predicate
satisfy :: (a -> Bool) -> Parser a a
satisfy p = do 
  c <- getC
  if (p c) then return c else fail "End of input"


