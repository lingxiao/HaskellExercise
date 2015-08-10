-----------------------------------------------------------------------------
-- | Module : SubroutineT.hs
-----------------------------------------------------------------------------   

{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleContexts #-}  
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}


module SubroutineT where 

import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans.Writer
import Control.Applicative
import Control.Comonad
import Data.Monoid


import Coroutine 
import ParserT


-- * END OF DAY GOAL: *finish* first iteration of subroutine, able to interop with some other program
-- * which would act as superoutine.

-- * Gameplan:

-- 1. Grok monad transformer tutorial, make toy examples to make sure
-- 2. Peruse monad transformer source code
-- 3. Grok S.O. answer and make toy examples to be sure

-- 4. see if it's even applicable or is better soln to current problem



----------------- 1. Intermission with Monad Transfomrer -----------------------------


-- * Problem 1. Composing `m Identity a` with `t m a`

-- * The gist: some stock monad `m` has underlying monad `m'` that is incompatible with 
-- * the larger monad that we want to use. 
-- * To make the types work, the underlying `m'` has to be mapped to some generic monad `n`,

-- * some monad 
-- * alternate signature :: StateT Int Identity ()
tick :: State Int ()
tick = modify (+1)

-- * want to use `tick` in larger `StateT Int IO` monad
-- * since any "pure" monad is just `MonadT Identity`, we can map to to `forall m. Monad m => MonadT m` with:

fromId :: Monad m => Identity a -> m a
fromId = return . runIdentity 


--hoist :: (Monad m, MFunctor t) => (forall a. m a -> n a) -> t m b -> t n b

hoistGen :: (Monad m, MFunctor t) => t Identity a -> t m a
hoistGen = hoist fromId


-- * now use in some bigger monad
tock :: StateT Int IO ()
tock = do 
  hoist fromId $ tick          --- * :: (Monad m)      => StateT Int m ()
  lift . putStrLn $ "tock!!"   --- * :: (MonadTrans t) => t         IO ()


-- * my example
toke :: StateT Int (State String) ()
toke = do
  str <- lift get
  modify (+ length str)
  hoist fromId $ tick     -- * <- note the use case
  return ()


toke' = runIdentity . flip runStateT "hi" . flip runStateT 0 $ toke


-- * Problem 2. Composing `t m a` with `s m a`

-- * combine `tick` tock with this below:

save :: StateT Int (Writer [Int]) ()
save = do 
  n <- get 
  lift . tell $ [n]


-- * `tock :: StateT (IO), but save :: StateT (Writer)`
-- * tock has no writerT layer, and save have Identity base monad

-- * hoist lift :: (Monad m, MonadTrans t1, MFunctor t) => t m b -> t (t1 m) b

saveTock :: StateT Int (WriterT [Int] IO) ()
saveTock = do 
  hoist lift tock           :: MonadTrans t => StateT Int (t IO) ()
  hoist (hoist fromId) save :: Monad m      => StateT Int (WriterT [Int] m) ()


------------------------------------- 2. Grok SO answer --------------------------------------




-- * Pieces needed:
{-
  
  type class such that any act as subroutine or superoutine

  Describe the computation, since the features may already exist

  super routine -> 
             recurse it self over and over
             possibly taking in new parameters
             possibly have side effects, ie, persisting state
             call subroutine >= 0 on each iteration
             does not necesarily terminate subroutine on each iteration

  subroutine  -> 
            called by superroutine >= 0 times foreach super routine interation
            sub may run >= 1 times, no guarantee of exit by itself. 
            sub may have side effects, ie persistant state
            sub terminates with value only when superoutine ends it
            sub may fail to terminate when instructed
            sub is not visible outside of the top routine, 
            so no `runSub . runSuper $ super`, just `runSuper super`

  The reason subroutine is not just another coroutine is because:
    1. coroutines never return, just respond/yield
    2. superoutine may have different applicative/monad implemetation than `Coroutine`
    3. in general, want to hide coroutine details *alltogether* sometimes

-}


-- * what if we just made a parser that returned ()



p :: (Monad m, Monad (MachineT m k)) => PT a (MachineT m k) [a]
p = do 
  c <- getC 
  return [c]



























-- * Note this might be a blind alley
-- * Note this may be able to be combined w/ Coroutine.hs


-- * Difference Between coroutine and SubroutineT:
-- * flow of control: coroutine await/request control from upstream, and respond/yield to downsteam
--                    SubroutineT awaits control from upstream, runs to completion and either yiedl result or run again?
--                    all states persits



-- * FLOW OF COMPUTATION:

-- * the super-routine `yields` parameters to SubroutineTs for computation
-- * between resumptions, the SubroutineT maintains its state
-- * the super-routine controls when the SubroutineT should `terminate`
-- * when the SuperRoutineT calls `terminate`, the SubroutineT `yield` its current value, which is the final value of computation 

-- * Use case: 
-- * parser = parserSource <= (buffer ~> transducer ~> builder) :: Parser Char FeatureMatrix

-- * parserSource is superoutine, (..) is SubroutineT. But (<=) is not taking two `SubroutineT` types,
-- * but rather two different types, and sExiting it the type of the first parameter

{-

  closure: does not satisfy closure

  associative:
  (a <= b ) <= c :: a
  a <= (b <= c)  :: a

  identity: 
  id <= a :: a
  a <= id :: a

  problem: what is identity?

-}

-- * the other choice is to make a SuperRoutineT instance, that interops with arbitrary coroutines 

-- * Remember the fixed point of the problem:
-- * call :: superoutine -> subroutine -> superoutine

{-

-- * note now both the param of function and whether to terminate is passed on the same channel
via a product type

this is weird looking and will be improved

super :: (a -> Bool) -> Parser a (..) b 
super p = do 
  a    <- getC 
  lift . send $ (a,False) 
  case p a of 
    False -> sourceParse
    True  -> do 
      b <- lift . send (a,True) 
      return b


sub :: FST a b -> Lexer a (..) b
sub f = do 
  (a,end) <- receive 
  case end of 
    False -> (lift . modify $ (++ f a)) >> sub f
    _     -> do 
      b <- lift get
      exit b


-}












----------------- Attempt one: explicty make both types -----------------------------



-- * class (Monad m, Monad m') => SubroutineT where 
--      input     :: CoroutineT a a' b b' m m v -> ParserT a m b
--      terminate :: CoroutineT a a' b b' m m v -> ParserT a m b



-- * first make super and SubroutineTs and make them interop


-- * note superroutine takes value yielded by subroutine b, and sExit it to v 
data Super a a' b v r 
  = Yield a (a' -> r)
  | Terminate (b -> r)
  | Done v


data Sub a a' b r
  = Resume a' (a -> r)
  | Exit b


-- * now ideally these two should be the same type, how do I make it so?

data SuperRoutineT a a' v m b = Supr { sup :: m (Super a a' b v (SuperRoutineT a a' v m b)) }

data SubroutineT a a' m b = Subr { sub :: m (Sub a a' b (SubroutineT a a' m b)) }


-- * composition, is it more like closure?

call :: Monad m => SuperRoutineT a a' v m b -> SubroutineT a a' m b -> SuperRoutineT a a' v m b
a@(Supr m) `call` b@(Subr m') = Supr $ m >>= \s -> case s of 
  Done v    -> return . Done $ v
  Yield a k -> m' >>= \s' -> case s' of 
    Resume a' h -> sup $ (k a') `call` (h a) 
    Exit b      -> error "subroutine crashed"
  Terminate k -> m' >>= \s' -> case s' of 
    Resume a' h -> error "subroutine indefinitely awaiting"
    Exit b      -> sup $ k b


-- * run 


-- * This design is bad, and you should feel bad
































