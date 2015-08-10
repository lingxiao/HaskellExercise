{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleContexts #-}  
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}


---------------------------------------------------------------------------------------------------------------------------
------------------------------------------------ Bidirectional Coroutines -------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------


import Prelude hiding ((.),id)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import qualified Control.Monad.Product as P
import Control.Monad.Identity
import Control.Applicative
import Control.Category
import Control.Comonad

import Data.Maybe
import Data.Monoid
import Data.Bifunctor
import Data.Profunctor
import Data.Functor.Contravariant

import Utils


-------------------------------------------------------------------------------
---------------------- Suspension Functor & Instances --------------------------
--------------------------------------------------------------------------------


{-
  
  Here request does not pass a parameter up stream, but do await values from upstream, ditto Respond.
     +---------+
     |         |
 () <==       <== ()
     |         |
 a  ==>       ==> b
     |    |    |
     +----|----+
          v
          r

-}

-- Conjecture: using type-class foo, can flicker this thing between pull and pushed based
-- but for sanity not both w/i same monad transformer
-- and not a mixed of pull and push ... could it?


-- | Note in both `Request` and `Respond` state, the coroutine "push" some information to other coroutines 
-- | and then wait for feedback, before next step is to occur
data PullPush a b v r 
  = Done v 
  | Request (a -> r)         -- * pull data from upstream
  | Respond (b -> r)         -- * push data downstream
  | Fail 


-- | push based
data Push a b x r 
  = Done x 
  | Yield b r       -- push
  | Await (a -> r)  -- being pushed
  | Fail


-- | goal: using only two parametes, make a pull based suspension functor

-- | pull based 
-- | to pull something... you need to send parameter upstream... which gets evaluated??
data Pull a b v r 
  = Done v 
  | Request a' (a -> r)    -- * send value a' upstream for request, await param a
  | Respond b r            -- * send value b downstream, do not get any value back



-- constrast:

push_a = do 
  x <- await
  yield x


pull_a = do
  x <- request y
  respond x     -- * this actually looks push based.. since it says: forall type of request, respond with x

--------------------------------------------------------------------------------
-------------------------- Coroutine & Instances -------------------------------
--------------------------------------------------------------------------------

--data CoroutineT a a' b b' m v = CoT { resume :: m (Step a a' b b' v (CoroutineT a a' b b' m v)) }

--instance Monad m => Functor (CoroutineT a a' b b' m) where
--  fmap g (CoT m) = CoT $ liftM ap m where 
--      ap s = case s of 
--        Done v       -> Done $ g v
--        Request a' k -> Request a' $ (fmap g) . k 
--        Respond b h  -> Respond b $ (fmap g) . h 
--        Fail         -> Fail


--instance Monad m => Monad (CoroutineT a a' b b' m) where
--  return = CoT . return . Done
--  (CoT m) >>= g = CoT $ m >>= \step -> case step of 
--    Done v        -> resume $ g v
--    Request a' k  -> return . Request a' $ (>>=g) . k
--    Respond b h   -> return . Respond b $ (>>=g) . h
--    Fail          -> return Fail


--instance MonadTrans (CoroutineT a a' b b') where
--  lift = CoT . liftM Done


--------------------------------------------------------------------------------
----------------------------------- Run ----------------------------------------
--------------------------------------------------------------------------------


-- | Problem: do not know where the information contained in a' and b goes
-- | if the seession is closed, then a' and b should be (), thus no info is lost
-- | but the rules for a coroutine in the middle is not so clear

--runT_ :: (Monoid v, Monad m) => CoroutineT a a' b b' m v -> [a] -> [b'] -> m ([b], [a'], v)
--runT_ m as bs' = resume m >>= \s -> let mem = (mempty,mempty,mempty) in case s of 
--  Done v        -> return ([],[],v) 
--  Fail          -> return mem
--  Request a' k  -> case as of         -- * where do I send this a', putting it in a list makes no sense..
--    []     -> return mem                   
--    (x:xs) -> (\(bs,as',v) -> (bs,a':as',v)) `liftM` runT_ (k x) xs bs'
--  Respond b h -> case bs' of          
--    []     -> return mem
--    (y:ys) -> (\(bs,as',v) -> (b:bs,as',v)) `liftM` runT_ (h y) as ys


--run_ :: Monoid v => CoroutineT a a' b b' Identity v -> [a] -> [b'] -> ([b], [a'], v)
--run_ m as = runIdentity . runT_ m as

--------------------------------------------------------------------------------
--------------------------------- Compose --------------------------------------
--------------------------------------------------------------------------------

{-
  c1 :: CoroutineT a a' b b' m v
  c2 :: CoroutineT b b' c c' m u 

     +---------+          +---------+
     |         |          |         |
 a' <==       <==== b' =====       <== c'
     |   C1    |          |    C2   |
 a  ==>       ===== b  ====>       ==> c
     |         |          |         |
     +----|----+          +----|----+

---}
--(~>) :: Monad m => CoroutineT a a' b b' m v -> CoroutineT b b' c c' m u -> CoroutineT a a' c c' m ()
--m1 ~> m2 = CoT $ resume m2 >>= \s2 -> case s2 of 
--  Fail          -> return Fail
--  Done _        -> return . Done $ ()
--  Respond c f   -> return . Respond c $ \c' -> m1 ~> (f c')
 
--  Request b' g -> resume m1 >>= \s1 -> case s1 of 
--    Respond b k   -> resume $ (k b') ~> (g b)
--    Request a' h  -> return . Request a' $ \a -> (h a) ~> (encase . Request b' $ \b -> g b)
--    Fail          -> return Fail
--    Done _        -> return . Done $ ()

--------------------------------------------------------------------------------
------------------------------- Primitives -------------------------------------
--------------------------------------------------------------------------------


--request :: Monad m => a' -> CoroutineT a a' b b' m a
--request a' = encase . Request a' $ \a -> return a


--respond :: Monad m => b -> CoroutineT a a' b b' m b'
--respond b = encase . Respond b $ \b' -> return b'


--stop :: Monad m => CoroutineT a a' b b' m v
--stop = encase Fail



--------------------------------------------------------------------------------
---------------------------------- Test ----------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--------------------------------- Utils ----------------------------------------
--------------------------------------------------------------------------------

--encase :: Monad m => Step a a' b b' v (CoroutineT a b m v) -> CoroutineT a b m v
--encase = CoT . return





































