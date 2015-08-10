{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleContexts #-}  
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}


-- http://stackoverflow.com/questions/14192018/passs-3-0-non-linear-topologies
-- GADTs: http://vimeo.com/12208838

module Coroutine (

    CoroutineT
  , Coroutine
  , Source

  , runT
  , run
  , (~>)

  , source
  , yield
  , await
  , pass
  , stop
  , err
  , sat 

) where 

import Prelude 

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Applicative

import Data.Monoid


--------------------------------------------------------------------------------
---------------------- Suspension Functor & Instances --------------------------
--------------------------------------------------------------------------------

{-
     +---------+
     |         |
 a  ==>   x   ==> b
     |    |    |
     +----|----+
          r

-}

data Step a b x r 
  = Done x 
  | Yield b r
  | Await (a -> r)
  | Fail String

instance Functor (Step a b x) where
  fmap g s = case s of 
    Done x    -> Done x
    Yield b r -> Yield b (g r)
    Await k   -> Await $ g . k
    Fail s    -> Fail s

--------------------------------------------------------------------------------
-------------------------- Coroutine & Instances -------------------------------
--------------------------------------------------------------------------------

-- | note the monad type needs to parameterize over type of `x` in `Done x`
data CoroutineT a b m x = CoT { resume :: m (Step a b x (CoroutineT a b m x)) }

type Coroutine a b      = forall m. Monad m => CoroutineT a b m ()

-- | existential makes source composable with any Coroutine b c m x w/o exposing a in the type signature
type Source b m         = forall a. CoroutineT a b m ()

instance Monad m => Functor (CoroutineT a b m) where
  fmap g (CoT m) = CoT $ liftM ap m where 
    ap (Done x   ) = Done $ g x
    ap (Yield b r) = Yield b (fmap g r) 
    ap (Await k  ) = Await $ (fmap g) . k
    ap (Fail s   ) = Fail s


instance Monad m => Applicative (CoroutineT a b m) where 
  pure  = return 
  (<*>) = ap


-- * does an anlternative instance make sense?
instance Monad m => Alternative (CoroutineT a b m) where
  empty     = err "<Empty Coroutine>"
  c1 <|> c2 = CoT $ resume c1 >>= \step -> case step of 
    Done x    -> return . Done $ x
    Yield b r -> return . Yield b $ r
    Await k   -> return . Await $ k
    Fail s    -> resume c2 >>= \step' -> case step' of 
      Done x    -> return . Done $ x
      Yield b r -> return . Yield b $ r
      Await k   -> return . Await $ k
      Fail s'   -> return . Fail $ s ++ " and " ++ s'

instance Monad m => Monad (CoroutineT a b m) where
  return = CoT . return . Done
  (CoT m) >>= g = CoT $ m >>= \step -> case step of 
    Done x    -> resume $ g x
    Yield b r -> return . Yield b $ r >>= g
    Await k   -> return . Await $ (>>=g) . k   
    Fail s    -> return $ Fail s

-- | lift :: m a -> t m a
-- | Given Monad m => m a, send a to Done a 
instance MonadTrans (CoroutineT a b) where
  lift = CoT . liftM Done

--------------------------------------------------------------------------------
------------------------------------- Run --------------------------------------
--------------------------------------------------------------------------------

-- | observe how this function "goes inside" some monad m and runs the bits of logic
-- | associated with Coroutine only, so it'll work w/ any arb product monad
-- | this function doesn't output the return value
runT :: (Monoid x, Monad m) => CoroutineT a b m x -> m [b]
runT c = resume c >>= \s -> case s of 
  Done x    -> return []
  Fail s    -> error s
  Yield b r -> (b:) `liftM` runT r
  Await k   -> error "the stream is not properly capped"   

-- | run a pure machine
run :: Monoid x => CoroutineT a b Identity x -> [b]
run = runIdentity . runT

--------------------------------------------------------------------------------
--------------------------------- Compose --------------------------------------
--------------------------------------------------------------------------------

infixl 9 ~>

-- | Note if the Coroutine is in state "Done a", the value of a is discarded
-- | Really both x and y should be ()
(~>) :: Monad m => CoroutineT a b m x -> CoroutineT b c m y -> CoroutineT a c m ()
c1 ~> c2 = CoT $ resume c2 >>= \s2 -> case s2 of 
  Done _    -> return . Done $ ()
  Fail s    -> return $ Fail s
  Yield c r -> return . Yield c $ c1 ~> r
  Await k   -> resume c1 >>= \s1 -> case s1 of 
    Fail s    -> return $ Fail s
    Done _    -> return . Done $ ()
    Yield b r -> resume $ r ~> (k b)
    Await h   -> return . Await $ \a -> h a ~> encase (Await $ \b -> k b)

--------------------------------------------------------------------------------
------------------------------ Primitives --------------------------------------
--------------------------------------------------------------------------------

source :: Monad m => [b] -> Source b m
source []     = return ()
source (b:bs) = yield b >> source bs

yield :: Monad m => b -> CoroutineT a b m () 
yield b = encase . Yield b $ return ()

await :: Monad m => CoroutineT a b m a
await   = encase . Await $ \a -> return a

stop  :: Monad m => CoroutineT a b m ()
stop    = encase . Done $ ()

err   :: Monad m => String -> CoroutineT a b m x
err     = CoT . return . Fail

pass  :: Monad m => CoroutineT b b m ()
pass    = await >>= \x -> yield x

-- * throw some error if input does not satisfy predicate
-- * note this is not very helpful if you want to know more information about how `a` caused the error
sat :: Monad m => (a -> Bool) -> String -> CoroutineT a a m ()
sat p e = await >>= \a -> if p a then yield a else err e

--------------------------------------------------------------------------------
--------------------------------- Utils ----------------------------------------
--------------------------------------------------------------------------------


encase :: Monad m => Step a b x (CoroutineT a b m x) -> CoroutineT a b m x
encase = CoT . return 

