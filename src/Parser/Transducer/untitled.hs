{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleContexts #-}  
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}




-- * Gameplan:

-- * 1. build trivial conversion from plan to coroutine
  -- * most important bit of logic: how to convert a plan to coroutine
  -- * what to do at Fail and Done

{-

-- | Compile a machine to a model.
construct :: Monad m => PlanT k o m a -> MachineT m k o
construct m = MachineT $ runPlanT m
  (const (return Stop))
  (\o k -> return (Yield o (MachineT k)))
  (\f k g -> return (Await (MachineT . f) k (MachineT g)))
  (return Stop)
-}








---------------------------------------------------------------------------------------------------------------------------
------------------------------------------------ Bidirectional Coroutines -------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------

module Coroutine2 where 

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Identity
import Control.Applicative

import Data.Monoid


-------------------------------------------------------------------------------
---------------------- Suspension Functor & Instances --------------------------
--------------------------------------------------------------------------------

{-

     +---------+
     |         |
 a' <==       <== b'
     |         |
 a  ==>       ==> b
     |    |    |
     +----|----+
          v
          r

-}

data Step a a' b b' r c 
  = Done r 
  | Request a' (a -> c)       
  | Respond b  (b' -> c)      
  | Fail 


instance Functor (Step a a' b b' r) where
  fmap g s = case s of 
    Done r       -> Done r
    Request a' k -> Request a' $ g . k 
    Respond b h  -> Respond b $ g . h
    Fail         -> Fail


--------------------------------------------------------------------------------
------------------- Internal Coroutine Representation --------------------------
--------------------------------------------------------------------------------

-- * Internal coroutine representation parametrizing over values of r in `Done r`
-- * This representation makes monad implementation possible 

data Cor a a' b b' m r = Cor { resume :: m (Step a a' b b' r (Cor a a' b b' m r)) }


instance Monad m => Functor (Cor a a' b b' m) where
  fmap g (Cor m) = Cor $ liftM ap m where 
      ap s = case s of 
        Done v       -> Done $ g v
        Request a' k -> Request a' $ (fmap g) . k 
        Respond b h  -> Respond b $ (fmap g) . h 
        Fail         -> Fail


instance Monad m => Monad (Cor a a' b b' m) where
  return        = Cor . return . Done
  (Cor m) >>= g = Cor $ m >>= \step -> case step of 
    Done v        -> resume $ g v
    Request a' k  -> return . Request a' $ (>>=g) . k
    Respond b h   -> return . Respond b $ (>>=g) . h
    Fail          -> return Fail

instance Monad m => Applicative (Cor a a' b b' m) where
  pure  = return 
  (<*>) = ap


instance MonadTrans (Cor a a' b b') where
  lift = Cor . liftM Done


--------------------------------------------------------------------------------
------------------ Exported Representation And Synonyms ------------------------
--------------------------------------------------------------------------------

-- * In principle, a coroutine is never in state `Done r`, thus the r should be hidden

-- * Problem: CoroutineT is NOT a monad!

-- * so we again...need the indirection where a non-monadic computation CoroutineT is constructed
-- * from a a monadic chain of actions Cor 

data CoroutineT a a' b m b' = forall r. Cot (Cor a a' b b' m r)

type Session m b = CoroutineT () () b m ()


--------------------------------------------------------------------------------
---------------------------------- Run -----------------------------------------
--------------------------------------------------------------------------------

-- * In practice, if some other monad tranformer `t` is defined over coroutine and `t` needs to return a value
-- * then it is returned here

runT :: Monad m => Session m b -> m ([b],r)
runT (Cot co) = resume co >>= \s -> case s of 
  Done r      -> undefined
  Fail        -> error "coroutine failed"
  Request _ k -> runT . Cot . k $ ()
  Respond b h -> (\(bs,r) -> (b:bs,r)) `liftM` (runT . Cot . h $ ())


runT' :: Monad m => Session m b -> m [b]
runT' (Cot co) = resume co >>= \s -> case s of 
  Done r      -> return []
  Fail        -> return []
  Request _ k -> runT' . Cot . k $ ()
  Respond b h -> (b:) `liftM` (runT' . Cot . h $ ())

--------------------------------------------------------------------------------
--------------------------------- Compose --------------------------------------
--------------------------------------------------------------------------------

-- * composition of exported representation


-- * composition of MonadTrans t => t (coroutine) and coroutine


-- * composition of MonadTrans t => t (coroutine) and t (coroutine)



--------------------------------------------------------------------------------
------------------------------- Primitives -------------------------------------
--------------------------------------------------------------------------------


request :: Monad m => a' -> CoroutineT a a' b m b'
request a' = encase . Request a' $ \a -> return a


respond :: Monad m => b -> CoroutineT a a' b m b'
respond b = encase . Respond b $ \b' -> return b'


exit :: Monad m => CoroutineT a a' b m b'
exit = encase . Done $ ()


-- * consider depricating this
crash :: Monad m => CoroutineT a a' b m b'
crash = encase Fail



--------------------------------------------------------------------------------
--------------------------------- Utils ----------------------------------------
--------------------------------------------------------------------------------

encase :: Monad m => Step a a' b b' r (Cor a a' b b' m r) -> CoroutineT a a' b m b'
encase = Cot . Cor . return













