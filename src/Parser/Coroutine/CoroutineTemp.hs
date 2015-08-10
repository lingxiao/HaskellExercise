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

module Coroutine (

    CoroutineT
  , Coroutine
  , MachineT
  , Machine

  , Session
  , Source
  , Client
  , Sink

  , (~>)
  , runT
  , run
  , runT_
  , run_
  , run_'

  , request
  , respond
  , await
  , yield
  , stop

  , source
  , client
  , sink

  , o
  , runTs
  , runTs'

  ) where


import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Applicative

import Data.Monoid


-- * PROBLEM!!!! `fmap g $ source [1..]` does not fmap g onto the yielded values, like it does in machines
-- * fun read: http://eventuallyconsistent.net/2013/08/12/messaging-as-a-programming-model-part-1/


infixl 8 ~>



-- * Game plan for today for pipes 
--    0. figure out the fmap yield thing                    --> have to ask edward later
--    1. reason through if a circular toplogy makes sense
--    2. make rough version of circular toplogy
--    3. test it on simple stuff
--    4. test it on parser ~> buffer ~> transducer ~> builder 

--    5. maybe tighten up the abstraction a bit and REDUCE the amount of type synonyms


-- * what a circular toplogy might look like:


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

-- | Note in both `Request` and `Respond` state, the coroutine "push" some information to other coroutines 
-- | and then wait for feedback, before next step is to occur
data Step a a' b b' v r 
  = Done v 
  | Request a' (a -> r)       -- * send type a' upstream to request value of type a, build new r from a
  | Respond b  (b' -> r)      -- * push value of type b downstream, then wait for any requests from downstream of type b'
  | Fail 
  | Exit                      -- * for now put this here, makes it compatible with programs that do "return"
                              -- * this could be a state after coroutine is compiled to a machine

instance Functor (Step a a' b b' v) where
  fmap g s = case s of 
    Done v       -> Done v
    Request a' k -> Request a' $ g . k 
    Respond b h  -> Respond b $ g . h
    Fail         -> Fail

--------------------------------------------------------------------------------
-------------------------- Coroutine & Instances -------------------------------
--------------------------------------------------------------------------------

data CoroutineT a a' b b' m v = CoT { resume :: m (Step a a' b b' v (CoroutineT a a' b b' m v)) }

type Coroutine a a' b b' = forall v. forall m. Monad m => CoroutineT a a' b b' m v

-- | an pushed-based coroutine, or a machine
type MachineT a b m v = CoroutineT a () b () m v
type Machine a b      = forall v. forall m. Monad m => MachineT a b m v


instance Monad m => Functor (CoroutineT a a' b b' m) where
  fmap g (CoT m) = CoT $ liftM ap m where 
      ap s = case s of 
        Done v       -> Done $ g v
        Request a' k -> Request a' $ (fmap g) . k 
        Respond b h  -> Respond b $ (fmap g) . h 
        Fail         -> Fail


instance Monad m => Monad (CoroutineT a a' b b' m) where
  return = CoT . return . Done
  (CoT m) >>= g = CoT $ m >>= \step -> case step of 
    Done v        -> resume $ g v
    Request a' k  -> return . Request a' $ (>>=g) . k
    Respond b h   -> return . Respond b $ (>>=g) . h
    Fail          -> return Fail


instance Monad m => Applicative (CoroutineT a a' b b' m) where
  pure  = return 
  (<*>) = ap


instance MonadTrans (CoroutineT a a' b b') where
  lift = CoT . liftM Done



-- * note if coroutines was implemented such that it never returns anything, 
-- * then it could be made into a monoid where (~>) is `<>` and mempty is `echo = forever $ await >>= \x -> yield x`

-- * could also be made into a catagory, where `.` is (~>) and id is `echo`. which is what this is, fancy function composition
-- * except the flow of computation is "going the wrong way" 


--------------------------------------------------------------------------------
----------------------------------- Run ----------------------------------------
--------------------------------------------------------------------------------

-- | run a session that yields results [b]
-- | Note if the session is properly closed, then `Request` must output `()`, and take in `()` create a new server
-- | Respond, however, may output arbitrary value of `b`, but must also take in `()` to create new client
runT :: Monad m => Session m b -> m [b]
runT m = resume m >>= \s -> case s of 
  Done _       -> return []
  Fail         -> return []
  Request _ k  -> runT $ k ()                 
  Respond b h  -> (b:) `liftM` (runT $ h ())  

run :: Session Identity b -> [b]
run = runIdentity . runT

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

-}
(~>) :: Monad m => CoroutineT a a' b b' m v -> CoroutineT b b' c c' m u -> CoroutineT a a' c c' m ()
m1 ~> m2 = CoT $ resume m2 >>= \s2 -> case s2 of 
  Fail          -> return Fail
  Done _        -> return . Done $ ()
  Respond c f   -> return . Respond c $ \c' -> m1 ~> (f c')
  Request b' g -> resume m1 >>= \s1 -> case s1 of 
    Respond b k   -> resume $ (k b') ~> (g b)
    Request a' h  -> return . Request a' $ \a -> (h a) ~> (encase . Request b' $ \b -> g b)
    Fail          -> return Fail
    Done _        -> return . Done $ ()




-- * alternate typeclass design to avoid type hell

o :: Monad m => forall a1. CoroutineT a a' b b' m u -> CoroutineT b b' c c' m a1 -> CoroutineT a a' c c' m a1
o m1 m2 = CoT $ resume m2 >>= \s2 -> case s2 of 
  Fail          -> return Fail
  Done _        -> error "coroutines are never done" 
  Respond c f   -> return . Respond c $ \c' -> m1 `o` (f c')
  Request b' g -> resume m1 >>= \s1 -> case s1 of 
    Respond b k   -> resume $ (k b') `o` (g b)
    Request a' h  -> return . Request a' $ \a -> (h a) `o` (encase . Request b' $ \b -> g b)
    Fail          -> return Fail
    Done _        -> error "coroutines are never done" 


runTs :: (Monad m, Monoid a, Monoid b') => forall v. CoroutineT a a' b b' m v -> m [b]
runTs m = resume m >>= \s -> case s of 
  Done _       -> return []
  Fail         -> return []
  Request _ k  -> runTs $ k mempty                 
  Respond b h  -> (b:) `liftM` (runTs $ h mempty)  



runTs' :: (Monad m, Monoid a, Monoid b') => forall v. CoroutineT a a' b b' m v -> m ([b],v)
runTs' m = resume m >>= \s -> case s of 
  Done v       -> return ([],v)
  Fail         -> error "fail"
  Request _ k  -> runTs' $ k mempty                 
  Respond b h  -> (\(bs,v) -> (bs ++ [b],v)) `liftM` (runTs' $ h mempty)  


--------------------------------------------------------------------------------
------------------------------- Primitives -------------------------------------
--------------------------------------------------------------------------------

request :: Monad m => a' -> CoroutineT a a' b b' m a
request a' = encase . Request a' $ \a -> return a

respond :: Monad m => b -> CoroutineT a a' b b' m b'
respond b = encase . Respond b $ \b' -> return b'

stop :: Monad m => CoroutineT a a' b b' m v
stop = encase Fail


-- | By setting a' and b' to (), we recover a "push-based" coroutine

await :: Monad m => MachineT a b m a
await = encase . Request () $ \a -> return a

yield :: Monad m => b -> MachineT a b m ()
yield b = encase . Respond b $ \_ -> return ()



--------------------------------------------------------------------------------
----------------------- Distinguished Coroutines -------------------------------
--------------------------------------------------------------------------------

-- | spend some time here and think about what each abstraction is suppose to represent, 
-- | its use case, semantics w/i different domain, etc


{-
  Proposed Session layout: 
    
     +---------+     +---------+     +---------+
     |         |     |         |     |         |
 () <==       <== b'===       <== c'====     <=== ()
     |  source |     |  Proxy  |     |  Client |
 () ==>       === b ==>       ==  c ===>     ==== d
     |         |     |         |     |         |
     +----|----+     +----|----+     +----|----+  

-}
type Session m b = CoroutineT () () b () m ()

type Server b b' = forall a. forall a'.            forall m. Monad m => CoroutineT a a' b b' m a
type Source b    = forall a. forall a'. forall b'. forall m. Monad m => CoroutineT a a' b b' m a
type Sink a'     = forall a. forall b. forall b'.  forall m. Monad m => CoroutineT a a' b b' m a
type Client a a' = forall b'. forall v.            forall m. Monad m => CoroutineT a a' a b' m v


-- | should a server also do some generic things with received values? But what?
server :: [b] -> Source b
server = undefined


source :: [b] -> Source b
source []     = stop
source (b:bs) = respond b >> source bs



-- | Consider depricating These

-- | client feeds [a'] upstream, and receives [a], which it yields
client :: [a'] -> Client a a'
client []       = stop
client (a':as') = request a' >>= \a -> respond a >> client as'


-- | push [a'] upstream
sink :: [a'] -> Sink a'
sink []       = stop
sink (a':as') = request a' >> sink as'



--------------------------------------------------------------------------------
--------------------------------- Utils ----------------------------------------
--------------------------------------------------------------------------------

encase :: Monad m => Step a a' b b' v (CoroutineT a a' b b' m v) -> CoroutineT a a' b b' m v
encase = CoT . return


-- * For Unit Tests 

-- | This Function is used for testing each CoroutineT unit
-- | Problem: do not know where the information contained in a' and b goes
-- | if the seession is closed, then a' and b should be (), thus no info is lost
-- | but the rules for a coroutine in the middle is not so clear

runT_ :: (Monoid v, Monad m) => CoroutineT a a' b b' m v -> [a] -> [b'] -> m ([b], [a'], v)
runT_ m as bs' = resume m >>= \s -> let mem = (mempty,mempty,mempty) in case s of 
  Done v        -> return ([],[],v) 
  Fail          -> return mem
  Request a' k  -> case as of         -- * where do I send this a', putting it in a list makes no sense..
    []     -> return mem                   
    (x:xs) -> (\(bs,as',v) -> (bs,a':as',v)) `liftM` runT_ (k x) xs bs'
  Respond b h -> case bs' of          
    []     -> return mem
    (y:ys) -> (\(bs,as',v) -> (b:bs,as',v)) `liftM` runT_ (h y) as ys

run_ :: Monoid v => CoroutineT a a' b b' Identity v -> [a] -> [b'] -> ([b], [a'], v)
run_ m as = runIdentity . runT_ m as

run_' :: Monoid v => CoroutineT a a' b b' Identity v -> [a] -> [b'] -> [b]
run_' m as bs = let (a,_,_) = run_ m as bs in a






