{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleContexts #-}  
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}


-----------------------------------------------------------------------------------------------------------------
------------------------------------------------ Recreate ListTM -------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

module ListT where 

  --  ListT
  --, runListT
  --, runList
  --, liftList
  --, joinT

import Prelude hiding (foldr, foldl)

import Control.Monad
import Control.Comonad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Identity
import Control.Applicative

import Data.Foldable
import Data.Monoid





-- | tutorial: http://www.haskell.org/haskellwiki/ListTM_done_right
-- | alternative: http://www.haskell.org/haskellwiki/ListTM_done_right_alternative


-------------------------------------------------------------------------------------
---------------------- Nodeterministic Functor & Instances --------------------------
-------------------------------------------------------------------------------------

data ListTM m a = NilT | a `ConsT` m (ListTM m a)


instance Monad m => Functor (ListTM m) where
  fmap g as = case as of 
    NilT      -> NilT
    ConsT a m -> ConsT (g a) $ m >>= return . fmap g


-------------------------------------------------------------------------------------
-------------------------- ListT Monad Transformer ----------------------------------
-------------------------------------------------------------------------------------

newtype ListT m a = ListT { unl :: m (ListTM m a) } 


instance Monad m => Functor (ListT m) where
  fmap g (ListT m) = ListT $ liftM ap m where 
    ap NilT        = NilT
    ap (ConsT a m) = ConsT (g a) $ m >>= return . fmap g 


instance Monad m => Applicative (ListT m) where
  pure  = return 
  (<*>) = ap

instance Monad m => Monad (ListT m) where
  return   = encase . memptyM
  lt >>= g = joinT . fmap g $ lt


instance MonadTrans ListT where
  lift = ListT . liftM memptyM
  

instance Monad m => MonadPlus (ListT m) where
  mzero = liftList []
  (ListT xs) `mplus` (ListT ys) = ListT $ xs `mappendM` ys


instance Monad m => Monoid (ListT m a) where
  mempty          = encase NilT
  ls `mappend` lt = ListT $ unl ls >>= \lsm -> case lsm of 
    NilT       -> unl lt
    ConsT a m' -> return . ConsT a $ unl $ ListT m' `mappend` lt

-- | foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
instance Monad m => Foldable (ListT m) where
  foldr g b (ListT m) = undefined


--------------------------------------------------------------------------------
------------------------------- Run ListT --------------------------------------
--------------------------------------------------------------------------------

runListT :: Monad m => ListT m a -> m [a]
runListT (ListT m) = m >>= \ltm -> case ltm of
   NilT       -> return []
   ConsT a m' -> (a:) `liftM` (runListT . ListT $ m')


runList :: ListT Identity a -> [a]
runList = runIdentity . runListT


--------------------------------------------------------------------------------
--------------------------- ListT Operations -----------------------------------
--------------------------------------------------------------------------------


liftList :: Monad m => [a] -> ListT m a 
liftList = encase . tol where 
  tol []     = NilT
  tol (a:as) = a `ConsT` return (tol as)


joinT :: Monad m => ListT m (ListT m a) -> ListT m a
joinT (ListT m) = ListT $ runLTM m >>= unl . foldr (<>) mempty


--------------------------------------------------------------------------------
--------------------------------- Utils ----------------------------------------
--------------------------------------------------------------------------------


-- * analogous to `mappend` and `mempty`, but `Monoid` instance cannot be implemented for `ListTM`
-- * does not satisfy the monoid laws

memptyM :: Monad m => a -> ListTM m a
memptyM = flip ConsT $ return NilT

-- * ConsT a (return NilT) -- ? isn't this "pure" over ListTM??

mappendM :: Monad m => m (ListTM m a) -> m (ListTM m a) -> m (ListTM m a)
mappendM mls mlt = mls >>= \ls -> case ls of 
  NilT         -> return NilT
  ConsT a mls' -> mls' >>= \ls' -> case ls' of 
    NilT -> return $ ConsT a mlt
    _    -> return . ConsT a $ mappendM mls' mlt
 

-- * convert `ListTM` from and to list 

liftLTM :: Monad m => [a] -> ListTM m a
liftLTM []     = NilT
liftLTM (x:xs) = ConsT x $ return . liftLTM $ xs


runLTM :: Monad m => m (ListTM m a) -> m [a]
runLTM m = m >>= \ltm -> case ltm of 
  NilT         -> return []
  a `ConsT` m' -> (a:) `liftM` runLTM m'


encase :: Monad m => ListTM m a -> ListT m a
encase = ListT . return


-------------------------------------------------------------
---------------------- Depricated  --------------------------
-------------------------------------------------------------

-- * just outputs the first outcome 
runListT' :: Monad m => ListT m a -> m (Maybe a)
runListT' (ListT m) = m >>= \ltm -> case ltm of 
  NilT      -> return Nothing
  ConsT a m -> return . Just $ a

















