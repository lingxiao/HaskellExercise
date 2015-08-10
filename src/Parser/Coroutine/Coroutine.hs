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
  , exit

  , source
  , client
  , sink

  , o
  , runTs
  , runTs'

  -- * current interface

  , (<~>)
  , runA

  ) where


import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Applicative

import Data.Monoid

import Control.Monad.State
import Control.Monad.Morph




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
  | Request a' (a  -> r)      -- * send type a' upstream to request value of type a, build new r from a
  | Respond b  (b' -> r)      -- * push value of type b downstream, then wait for any requests from downstream of type b'
  | Fail 


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


-- * consider monadPlus instance


-- * consider category instance

-- * note if coroutines was implemented such that it never returns anything, 
-- * then it could be made into a monoid where (~>) is `<>` and mempty is `echo = forever $ await >>= \x -> yield x`

-- * could also be made into a category, where `.` is (~>) and id is `echo`. which is what this is, fancy function composition
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


--------------------------------------------------------------------------------
------------------------------- Primitives -------------------------------------
--------------------------------------------------------------------------------


-- * CRITICISM: these function signatures are retarded, and throws entire application into typeclass hell.

-- * how do I get rid of them???

-- * option1: this is the plan, construct a pipeline out of plan
-- * pros: get rid of param r constraint
-- * cons: how do I return a  value for pipeline if done is no longer a state

-- * Option2: no distrinction between plan and pipeline.
-- * instead, 

request :: Monad m => a' -> CoroutineT a a' b b' m a
request a' = encase . Request a' $ \a -> return a

respond :: Monad m => b -> CoroutineT a a' b b' m b'
respond b = encase . Respond b $ \b' -> return b'

stop :: Monad m => CoroutineT a a' b b' m v
stop = encase Fail


-- * challenge, what to make this existentially quaitifed
exit :: forall a. forall a'. forall b. forall b'. forall v. Monoid v => forall m. Monad m => CoroutineT a a' b b' m v
exit = return mempty 


-- | By setting a' and b' to (), we recover a "push-based" coroutine

await :: Monad m => MachineT a b m a
await = encase . Request () $ \a -> return a

yield :: Monad m => b -> MachineT a b m ()
yield b = encase . Respond b $ \_ -> return ()


--type GenCo = forall a. forall a'. forall b. forall b'. forall v. Monoid v => forall m. Monad m => CoroutineT a a' b b' m v


--p1 :: Monad m => CoroutineT String String String String m String
--p1 = respond "a" >> respond "b" >> temp


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




-- ************************************ Try some new things ************************************


-- * alternate composition sigature to avoid typelcass hell

o :: Monad m => forall a1. CoroutineT a a' b b' m u -> CoroutineT b b' c c' m a1 -> CoroutineT a a' c c' m a1
o m1 m2 = CoT $ resume m2 >>= \s2 -> case s2 of 
  Fail          -> return Fail
  Done _        -> error "coroutine m2 is never done" 
  Respond c f   -> return . Respond c $ \c' -> m1 `o` (f c')
  Request b' g -> resume m1 >>= \s1 -> case s1 of 
    Respond b k   -> resume $ (k b') `o` (g b)
    Request a' h  -> return . Request a' $ \a -> (h a) `o` (encase . Request b' $ \b -> g b)
    Fail          -> return Fail
    Done _        -> error "coroutine m1 is never done" 



p :: Monad m => CoroutineT a a' b b' m t -> CoroutineT b b' c c' m r -> CoroutineT a a' c c' m r
p m1 m2 = CoT $ resume m2 >>= \s2 -> case s2 of 
  Fail          -> return Fail
  Done _        -> error "coroutine m2 is never done" 
  Respond c f   -> return . Respond c $ \c' -> m1 `p` (f c')
  Request b' g -> resume m1 >>= \s1 -> case s1 of 
    Respond b k   -> resume $ (k b') `p` (g b)
    Request a' h  -> return . Request a' $ \a -> (h a) `p` (encase . Request b' $ \b -> g b)
    Fail          -> return Fail
    Done _        -> error "coroutine m1 is never done" 



-- * something weird: returning a list means transformer over coroutine cannot properly return
-- * returning a tuple of list and v does

runTs :: (Monad m, Monoid a, Monoid b') => forall v. CoroutineT a a' b b' m v -> m [b]
runTs m = resume m >>= \s -> case s of 
  Done _       -> return []
  Fail         -> return []
  Request _ k  -> runTs $ k mempty                 
  Respond b h  -> (b:) `liftM` (runTs $ h mempty)  



runTs' :: (Monad m, Monoid a, Monoid b') => forall v. Monoid v => CoroutineT a a' b b' m v -> m ([b],v)
runTs' m = resume m >>= \s -> case s of 
  Done v       -> return ([],v)
  Fail         -> return ([], mempty)
  Request _ k  -> runTs' $ k mempty                 
  Respond b h  -> (\(bs,v) -> (b:bs,v)) `liftM` (runTs' $ h mempty)  



-- * another run candidate
runA :: Monad m => CoroutineT () a' b () m v -> m ([b],v)
runA m = resume m >>= \s -> case s of 
  Done v       -> return ([],v)
  Fail         -> error "Fail state"
  Request _ k  -> runA $ k ()                 
  Respond b h  -> (\(bs,v) -> (b:bs,v)) `liftM` (runA $ h ())  


-- * discard yielded values
execA :: Monad m => CoroutineT () a' b () m v -> m v
execA m = resume m >>= \s -> case s of 
  Done v       -> return v
  Fail         -> error "Fail state"
  Request _ k  -> execA $ k ()                 
  Respond b h  -> execA $ h ()





-- * compose pure untransformed coroutines

-- * observe typeclass hell: since action ends with 
-- * `respond :: b -> CoroutineT a a' b b' m b'` 
-- * return value has to be b', which in this case is ()
fill0 :: Monad m => CoroutineT Int Bool Int () m ()
fill0 = request True >>= \x -> respond x


fill1 :: Monad m => CoroutineT () () Int Bool m a
fill1 = undefined


fill2 :: Monad m => CoroutineT Int Bool Int () m r
fill2 = undefined

fill01 :: Monad m => CoroutineT () () Int () m ()
fill01 = fill1 `p` fill0




wrap :: Monad m => StateT String (CoroutineT () () Int Bool m) Bool
wrap = undefined



-- * NOTE: hoist type signature puts restrictions on what return value of coroutine can be
-- * compose transformed coroutine with untransformed one
bur :: Monad m => StateT String (CoroutineT () () Int () m) Bool
bur = hoist (\fill -> fill `p` fill2) wrap

u

-- * make hoist toy example to show #haskell
-- * conclusion: it works

hoistSig :: (Monad m, MFunctor t) => (forall a. m a -> n a) -> t m b -> t n b
hoistSig  = undefined


-- * composition
g :: Monad m => forall b. m a -> m b -> m b
g = undefined


-- * case 1, m parameterize over a and b
ma :: (Monad m) => m a
ma = undefined

tma :: (Monad m, MFunctor t) => t m a
tma = undefined

tmaa :: (Monad m, MFunctor t) => t m b
tmaa = hoist (\ma' -> g ma ma') tma

-- * case 2, m parameterize over a and () 

mb :: (Monad m) => m ()
mb = undefined

mab :: Monad m => m ()
mab = g ma mb


tmb :: (Monad m, MFunctor t) => t m ()
tmb = undefined

tmab :: (Monad m, MFunctor t) => t m ()
tmab = hoist (\mb -> g ma mb) tmb

tmba :: (Monad m, MFunctor t) => t m ()
tmba = hoist (\mb -> g mb ma) tmb


-- ********************************** Restart from function signatures ************************************


-- * now replicate signature of coroutine so everything checks out
-- * proof that they do work as intended

-- * now add back the implementation

infixl 8 <~>

-- * perhapse final version of compose, no forall r. like before

-- * there is som ting won with the logic here anyways, the "return" value of the first coroutine is
-- * discarded.. which makes no sense\

(<~>) :: Monad m => CoroutineT a a' b b' m t -> CoroutineT b b' c c' m r -> CoroutineT a a' c c' m r
m1 <~> m2 = CoT $ resume m2 >>= \s2 -> case s2 of 
  Fail          -> return Fail
  Done r        -> return . Done $ r
  Respond c f   -> return . Respond c $ \c' -> m1 <~> (f c')
  Request b' g -> resume m1 >>= \s1 -> case s1 of 
    Respond b k   -> resume $ (k b') <~> (g b)
    Request a' h  -> return . Request a' $ \a -> (h a) <~> (encase . Request b' $ \b -> g b)
    Fail          -> return Fail
    Done _        -> return Fail    -- * m2 request value from m1 but m1 is Done, thus computation fails


-- * test hoist with arbitary m a

mm :: Monad m => m a -> m b -> m b
mm = undefined

mmstr :: Monad m => m a
mmstr = undefined


mmint :: Monad m => m Int 
mmint = undefined


tmma :: (Monad m, MFunctor t) => t m a
tmma = undefined


comp = hoist (\ma -> ma `mm` mmstr) tmma 



-- * test hoist with Coroutine with arbitrary function signatures


ct :: Monad m => CoroutineT a a' b b' m t
ct = undefined


cr :: Monad m => CoroutineT b b' c c' m r
cr = undefined


ctr :: Monad m => CoroutineT a a' c c' m r
ctr = ct <~> cr


tcb' :: Monad m => StateT b (CoroutineT a a' b b' m) b
tcb' = undefined

tcb :: Monad m => StateT b (CoroutineT b b' c c' m) c
tcb = undefined


test1 :: Monad m => StateT b (CoroutineT a a' c c' m) b
test1 = hoist (\cb -> cb <~> cr) tcb'


test2 :: Monad m => StateT b (CoroutineT a a' c c' m) c
test2 = hoist (\cb -> ct <~> cb ) tcb


-- * test compose with arb function signature but fixed `a` in `m a`

cs :: Monad m => CoroutineT a a' b b' m String
cs = undefined

ci :: Monad m => CoroutineT b b' c c' m String
ci = undefined

call :: Monad m => CoroutineT b b' c c' m a
call = undefined


tcs :: (Monad m, MFunctor t) => t (CoroutineT a a' b b' m) r
tcs = undefined


-- * a hypothetical `map` function in category of monads, where underlying monad may
-- * change form and parameter

-- * note: m a -> n c is not a monad homorphism so you don't get path independance
hoist' :: (Monad m, MFunctor t) => (m a -> n c) -> t m b -> t n b
hoist' = undefined


{-
  note how type of hoist says only underlying monad may change, but its parameter may not


  need, hoist' :: (Monad m, MFunctor t) => (m a -> n c) -> t m b -> t n b


  hoist :: (Monad m, MFunctor t) => (forall a. m a -> n a) -> t m b -> t n b

  Expected type: CoroutineT b0 b'0 c c' m a
  Actual type:   CoroutineT b0 b'0 c c' m Int

-}
tcsi = hoist' (\cs -> cs <~> ci ) tcs


tcsall = hoist (\cs -> cs  <~> call) tcs


-- * try using mapStateT
-- * note map state has the same problem with parameters
scsi = mapStateT (\cs -> cs <~> call) scs 
  where scs = undefined :: Monad m => StateT c (CoroutineT a a' b b' m) r


-- * try switching order, note the param of o, a, is universally quantified
three = hoist (\o -> two <~> o) one where 
  one = undefined :: (Monad m, MFunctor t) => t (CoroutineT () () () () m) r
  two = undefined :: Monad m => CoroutineT () () () () m Int


-- * try some two stage, monad morphims, then send return value to what's needed? this makes no sense though...

-- * problem: compose is not order invariant, thus cannot flip order of composition

-- * setting a dummy parameter for last thing might help, it'll also get rid of a lot of other problems..., like 
-- * composition. Though it is a bit hacky and would make it a bogus functor, monad 


-- * Enumerate type class problems:

{-

1. primitives returns values that constrains last value 
  solved by: forever, return a value

2. hoisting contrains the monad morphimsm to be of form (m a -> n a), when in fact we just want (m a -> m b)
    solution: some parser specific function (m a -> m b) -> Parser m c -> Parser m c

    where `m a -> m b` is not a monad morphism

  but ... we can dempmose things...
  
  (m a -> n a) -> Parser m c -> Parser n c -> ( a -> b ) -> Parser n c

    
    soln pt 1. do non monad morphism for state monad


-}


-- * a NON monad morphism 

swap :: (Monad m) => (forall a. forall b. m a -> m b) -> StateT c m r -> StateT c m r
swap g t = StateT $ \s -> g $ runStateT t s


-- * unfortunately, this is incorrect, the error:
-- * Expected type: CoroutineT b b' b b' m b1
-- * Actual type: CoroutineT b b' b b' m Int

-- * meaning type of b has to be generic, not specifc
correct = swap (\a -> a <~> b) ta where 
  ta = undefined :: Monad m => StateT String (CoroutineT a a' b b' m) r
  b  = undefined :: Monad m => CoroutineT b b' c c' m e



-- * problem limiting progress and ability to create new things: don't really understand universal
-- * quantifier and what it does






-- * test compose with specific function signatures, but unparametrized return value of coroutine
-- * the pitfall is specific implementations might not satisfy constraints

mt :: Monad m => CoroutineT () () Int Bool m t
mt = undefined


mr :: Monad m => CoroutineT Int Bool Int () m r
mr = undefined


mtr :: Monad m => CoroutineT () () Int () m r
mtr = mt <~> mr



scb :: Monad m => StateT b (CoroutineT () () Int Bool m) b 
scb = undefined


scb' :: Monad m => StateT b (CoroutineT Int Bool Int () m) c
scb' = undefined



test3 :: Monad m => StateT Int (CoroutineT () () Int () m) Int
test3 = hoist (\cb -> cb <~> mr) scb


test3' = runA . flip runStateT 0 $ test3



test4 :: Monad m => StateT String (CoroutineT () () Int () m) Int
test4 = hoist (\cb' -> mt <~> cb') scb'


test4' = runA . flip runStateT [] $ test4



-- * test compose with specific function signatures, but paramterize return value of coroutine


-- * note we discovered a second problem where respond's signature constrains what mint could be
-- * if respond is used by itself
mint :: Monad m => CoroutineT () () Int Bool m Int
mint = undefined


-- * since this `m a` has `a :: String`, this throws off hoist
mstr :: Monad m => CoroutineT Int Bool Int () m String
mstr = undefined


-- * regular composition work as expected
mintstr :: Monad m => CoroutineT () () Int () m String
mintstr = mint <~> mstr


-- * assert hoisted composition will not work

tmint :: (Monad m, MFunctor t) => t (CoroutineT () () Int Bool m) Int
tmint = undefined


-- *  typeclass error from purgatory:
-- *  Expected type: CoroutineT Int Bool Int () m a
-- *  Actual type:   CoroutineT Int Bool Int () m String
--tmintstr = hoist (\mint -> mint <~> mstr) tmint



-- * observe where return value is put

-- * without stop, this does not compile since return value of `respond` must be input value of respond
-- * with stop, the type checks out but computation fails
me :: Monad m => CoroutineT () () Int () m String
me = respond 12 >> stop


-- * ([12,30],"ok")
-- * return value is just return value of coroutine
mt' :: Monad m => CoroutineT () () Int () m [Char]
mt' = respond 12 >> respond 30 >> return "ok"


-- * ([12,30],("state","more"))
-- * return value is value of outermost computation: stateT
mt'' :: Monad m => StateT String (CoroutineT () () Int () m) String
mt'' = (lift . respond $ 12) >> (lift . respond $ 30) >> modify (++"more") >> return "state"


-- * (([12,303],"coroutine return"),"more")
-- * return value of coroutine is just coroutine's return value
-- * inner monad state is on outermost layer, its return value is (bs, coroutineReturnValue)
mt''' :: CoroutineT () () Int () (State String) String
mt''' = respond 12 >> respond 303 >> (lift . modify $ (++"more")) >> return "coroutine return"




-- *************************************** some other stuff ***************************************



-- * CoT (State String ( Step a a' b b' (State Int) r ) String)

-- * return state: CoT (State String (Step a a' b b' (State Int) r) String )

c :: CoroutineT Int () Int () (State Int) String
c = do                       
  x <- lift get
  y <- request ()
  respond x 
  lift . modify $ (+y)
  x' <- lift get
  respond x'
  lift . return $ "hello"     -- * note both return have to be of same time, since it's the same monadic action
  return $ show x'            -- * however, neither return value is anywhere to be found


-- * ([0,100],100), the underlying state and yielded values are returned
cc1 = flip runState 0 . runT $ source [1..3] ~> c


-- * (([0,100],()), 100)
-- * note the return value above is no where to be found, replaced by (), where did () come from??
-- * this fails for `forever c`, not sure why though
cc2 = flip runState 0 . runA $ source [1..3] ~> c


-- * state on the outside
d :: StateT Int (CoroutineT () () Int () (State String)) String
d = do 
  modify (+400)
  x <- get
  lift . respond $ x
  modify (+4)
  x' <- get
  lift . respond $ x'
  lift . lift . modify $ (++"inner monad state")
  lift . lift . return $ ("inner monad return value")
  lift . return $ ("coroutine return value")
  return "state monadT return"


{-
  
  return value of inner state monad is the computation wrapping it

  it is of form ( overlaying compuation, inner monad state )

  return value of coroutine is what Done v, and that is the overlaying compuatation of StateT

  which is (stateT return, stateT state)

  inner monad runs last, so its result is on the outermost layer, 
  then coroutine is run, its result is second layer,
  finally outer state is run last, its result is on inner most layer


  ( 
    ( [400,404], ("state monadT return",404) ),
    "inner monad state"
  )

-}
d' = flip runState "" . runA . flip runStateT 0 $ d


-- * when the return value of coroutine is disgarded, the overlaying computation is also disgarded
-- * thus  `Done r` absolutely must be a reachable state if coroutine is to be transformed by some other monad 

-- * ([400,404],"inner monad state")
d''' = flip runState [] . runTs . flip runStateT 0 $ d


-- * (("state monadT return",404),"inner monad state")
de = runIdentity . flip runStateT "" . execA . flip runStateT 0 $ d

-- * ([400,404], "inner monad state")
d'' = flip runState "" . runTs . flip runStateT 0 $ d































