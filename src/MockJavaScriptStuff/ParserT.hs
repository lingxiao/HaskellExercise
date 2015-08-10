{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-} 

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Homebrew Parser
-- | Creator: Xiao Ling
-- | Created: November 19th
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
module ParserT (

    ParserT
  , Parser

  , runPt
  , runP
  , evalP

  , getC
  , satisfy
  , failP

  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Applicative 
import Data.Monoid
import Data.Char


---------------
-- Data Type --
--------------- 

data ParserT a m b = Pt { runPt :: [a] -> m (Either String (b,[a])) }

type Parser a b    = forall m. Monad m => ParserT a m b

runP :: Parser a b -> [a] -> Either String (b,[a])
runP p = runIdentity . runPt p 

evalP :: Parser a b -> [a] -> b
evalP p = either error fst . runP p

---------------
-- Typeclass --
---------------

instance Monad m => Functor (ParserT a m) where 
  fmap g p = Pt $ \as -> h `liftM` runPt p as where 
    h (Left s)       = Left s 
    h (Right (b,as)) = Right (g b, as)

instance Monad m => Applicative (ParserT a m) where 
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ParserT a m) where 
  return a = Pt $ \as -> return . Right $ (a,as)
  p >>= g  = Pt $ \as -> runPt p as >>= \r -> case r of 
    Left s        -> return . Left $ s
    Right (b,as') -> runPt (g b) as'


instance Monad m => Alternative (ParserT a m) where 
  empty     = Pt . const . return . Left $ []
  p1 <|> p2 = Pt $ \as -> do
    r <- runPt p1 as
    case r of 
      Left _ -> runPt p2 as
      _      -> return r

instance Monad m => MonadPlus (ParserT a m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans (ParserT a) where
  lift m = Pt $ \as -> m >>= \a -> return . Right $ (a,as)

instance (Monad m, MonadPlus m) => Monoid (ParserT a m b) where
  mempty          = empty
  p1 `mappend` p2 = Pt $ \as -> runPt p1 as `mplus` runPt p2 as

-------------------------
-- Primitive Functions --
-------------------------

failP :: Monad m => String -> ParserT a m b
failP s = Pt $ \_ -> return . Left $ s

-- | Return the next character
getC :: Monad m => ParserT a m a
getC = Pt $ \as -> case as of 
  (x:xs) -> return . Right $ (x,xs)
  []     -> return . Left $ "No more characters"

-- | Return the next character if it satisfies the given predicate
satisfy :: Monad m => (a -> Bool) -> ParserT a m a
satisfy p = do 
  c <- getC
  if (p c) then return c else failP "character does not satisfy predicate"

















