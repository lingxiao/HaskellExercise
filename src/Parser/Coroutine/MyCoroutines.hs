{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleContexts #-}  
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}


--http://stackoverflow.com/questions/14192018/pipes-3-0-non-linear-topologies
-- GADTs: http://vimeo.com/12208838

import Prelude hiding ((.),id)


import Control.Lens
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


-- * Enumerate all the problems:

-- * 1. no sensible way to return x at all, when used with forever  -> get rid of return x


-- * 2. no sensible way to return x under compositions              -> get rid of return x
  -- * Some options for solving this:
  -- * if they're not the same type, take product all returns types -> how?
  -- * if they're the same type, take monoids


-- * 3. no sensible way to keep separate effects

  -- * progress: know how to do Product of states using Monad.Product
  -- * but: when running a coroutine, how do you run the effects?
  -- * but: how do you combine them using ~>>

  -- * problem: current combo using ~>> throws exception, why is this?

-- * 4. runT' `Await k` case should never be reached, but have no way to enforce it using type system
  
-- * 5. still havn't gotten to pipes


-- * plan for sunday:

-- * figure out combination using product monads, however ugly it looks
-- * prelim intro to pipes' bi-directional flow

-- * make rough version of website for megs: 
  -- * dynamic aspec >> simple static version >> dynamic && static together


unp  = P.runProduct


--------------------------------------------------------------------------------
---------------------- Suspension Functor & Instances --------------------------
--------------------------------------------------------------------------------

-- * input a
-- * output b 
-- * maps over r
-- * when done, return x
data Step a b x r 
  = Done x 
  | Yield b r
  | Await (a -> r)
  | Fail

-- | fmap :: Step a b x r -> (r -> r') -> Step a b x r'
instance Functor (Step a b x) where
  fmap g s = case s of 
    Done x    -> Done x
    Yield b r -> Yield b (g r)
    Await k   -> Await $ g . k
    Fail      -> Fail


--------------------------------------------------------------------------------
-------------------------- Coroutine & Instances -------------------------------
--------------------------------------------------------------------------------


-- | note the monad type needs to parameterize over type of `x` in `Done x`
data CoroutineT a b m x = CoT { resume :: m (Step a b x (CoroutineT a b m x)) }


data Coroutine a b x where
  Co :: Monad m => CoroutineT a b m x -> Coroutine a b x


instance Monad m => Functor (CoroutineT a b m) where
  fmap g (CoT m) = CoT $ liftM ap m where 
    ap (Done x)    = Done $ g x
    ap (Yield b r) = Yield b (fmap g r) 
    ap (Await k)   = Await $ (fmap g) . k
    ap Fail        = Fail


instance Monad m => Monad (CoroutineT a b m) where
  return = CoT . return . Done
  (CoT m) >>= g = CoT $ m >>= \step -> case step of 
    Done x    -> resume $ g x
    Yield b r -> return . Yield b $ r >>= g
    Await k   -> return . Await $ (>>=g) . k   
    Fail      -> return Fail


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
  Fail      -> return []
  Yield b r -> (b:) `liftM` runT r
  Await k   -> error "the stream is not properly capped"   


-- | run a pure machine
run :: Monoid x => CoroutineT a b Identity x -> [b]
run = runIdentity . runT


--------------------------------------------------------------------------------
--------------------------------- Compose --------------------------------------
--------------------------------------------------------------------------------

infixl 9 ~>
infixl 9 ~>>

-- | Note if the Coroutine is in state "Done a", the value of a is discarded
-- | Really both x and y should be ()
(~>) :: Monad m => CoroutineT a b m x -> CoroutineT b c m y -> CoroutineT a c m ()
c1 ~> c2 = CoT $ resume c2 >>= \s2 -> case s2 of 
  Done _    -> return . Done $ ()
  Fail      -> return Fail
  Yield c r -> return . Yield c $ c1 ~> r
  Await k   -> resume c1 >>= \s1 -> case s1 of 
    Fail      -> return Fail
    Done _    -> return . Done $ ()
    Yield b r -> resume $ r ~> (k b)
    Await h   -> return . Await $ \a -> h a ~> encase (Await $ \b -> k b)


--------------------------------------------------------------------------------
------------------------------ Primitives --------------------------------------
--------------------------------------------------------------------------------


-- | existential makes source composable with any Coroutine b c m x w/o exposing a in the type signature
type Source b m = forall a. CoroutineT a b m ()

source :: Monad m => [b] -> Source b m
source []     = encase Fail
source (b:bs) = yield b >> source bs


yield :: Monad m => b -> CoroutineT a b m () 
yield b = encase . Yield b $ return ()


await :: Monad m => CoroutineT a b m a
await = encase . Await $ \a -> return a


stop :: Monad m => CoroutineT a b m ()
stop = encase . Done $ ()



--------------------------------------------------------------------------------
-------------------------------- Tests -----------------------------------------
--------------------------------------------------------------------------------


ex1 :: CoroutineT Int Int Identity String
ex1 = CoT . return . Await $ \i -> 
  CoT . return . Yield (i + i) $ ex1 


ex2 :: CoroutineT Int Int Identity String
ex2 = CoT . return . Await $ \i -> case (i > 4) of 
  False -> CoT . return $ Fail
  _     -> CoT . return . Yield (i + i + 1) $ ex2

ex3 :: CoroutineT Int Int Identity String
ex3 = CoT . return . Await $ \i -> 
  CoT . return . Await $ \j -> 
    CoT . return . Yield (i+j) $ 
      CoT . return . Done $ "All done"


ex1'  = run_ ex1 [1..3]
ex2'  = run_ ex2 [1..3]
ex2'' = run_ ex2 [5..9]
ex3'  = run_ ex3 [1..3]


ex11 = flip run_ [1..4] $ ex1 ~> ex1
ex12 = flip run_ [1..4] $ ex1 ~> ex2
ex21 = flip run_ [1..4] $ ex2 ~> ex1

trans2 :: CoroutineT String Int Identity ()
trans2 = forever $ do
  a <- await  
  yield . length $ a


trans1 :: CoroutineT Int Int Identity ()
trans1 = do 
  a <- await
  let a' = a + a in case a' > 12 of 
    True -> stop
    _    -> yield a' >> trans1


rs = flip run_ ["hello", "world", "more", "strings", "sh", "loonnnggg"] $ trans2 ~> trans1

ex5 :: CoroutineT Int Int (StateT Int IO) ()
ex5 = forever $ do
  a <- await
  lift . modify $ (+a)
  s <- lift get
  lift . lift . print $ s
  yield s


ex6' = flip runStateT 0 . run_ ex5 $ [1..5]

ex7 = source [100..108] ~> ex5

ex7' = flip runStateT 0 . run_ ex7 $ []

ex8 = run $ source [1..5] ~> m ~> m' where 
  m = forever $ do 
    a <- await
    yield $ a + a
    return 404
  m' = forever $ do 
    a <- await
    yield $ a * a 
    return 200


t :: CoroutineT Int Int Identity String
t = forever $ do 
  a <- await
  yield $ a + 10 
  return "done"

t2 :: CoroutineT Int Int Identity Int
t2 = forever $ do 
  a <- await
  yield $ a * a 
  return 404

-- | note these could only be combined if their `Done a` value was discarded
tt2 :: CoroutineT Int Int Identity ()
tt2 = source [1..5] ~> t ~> t2


-- | manually build a coroutineT over Product Monad
simp :: CoroutineT Int Int (P.Product (State Int) (State String)) ()
simp = forever $ do 
  x <- await
  lift $ P.Product (modify (+x), modify (++ show x))
  yield x 


-- | observe: successfully "write" changes to state, but what if simple also depends on reads to state?
(si,ss) = unp . runT $ source [1..5] ~> simp


-- | problem: right now have no way to "get" current state of underlying product monad,
-- | this will help us test whether the state is "live"



-- | observation one: running a coroutine w/ product is a pretty horrible ordeal for user
-- | ie no generic way to run all w/ some predetermined initial cond

-- | runState si 0
-- | runState ss ""

-- | observation two: the underlying product, unless run, does not affect the top level computation

-- | compose two transformers with same underlying State monad but different param
type Stack = P.Product (P.Product Identity (State Int)) (State String)


ms1 :: CoroutineT Int Int (State Int) ()
ms1 = do 
  x <- await
  lift . modify $ (+x)
  x' <- lift get
  yield x'


ms2 :: CoroutineT Int Int (State String) ()
ms2 = do
  x <- await
  lift . modify $ (++"show")
  s <- lift get
  yield . length $ s


ms12 :: CoroutineT Int Int Stack ()
ms12 = source [1..4] ~>> ms1 ~>> ms2


(fst1, ms2') = unp . runT $ ms12

ms1' = snd . unp $ fst1


-- | both of these eval to undefined since ~>> is not defined
ms1'' = runState ms1' 0  :: ([Int], Int)
ms2'' = runState ms2' "" :: ([Int], String)




-- | though the spirit of separate effects is lost, since all the coroutines can change the same "global" state, 
-- | it is a lot easier to use

-- | DECISION: Roll with this even though it's modifying global var. Since it's very isolated, it's OK.

tr' = flip runState (1000, "manyLetters",True) . runT $ source [1..4] ~> eas2 ~> eas2 ~> eas3


eas1 :: CoroutineT Int Int (State (Int,String,Bool)) ()
eas1 = forever $ do
  x <- await
  n <- lift $ zoom _1 get
  yield $ x + n


eas2 :: CoroutineT Int Int (State (Int,String,Bool)) ()
eas2 = forever $ do
  x <- await
  s <- lift $ zoom _2 get
  yield $ x + (length s)


-- * note any coroutine can to modify "global" states at will here
eas3 :: CoroutineT Int Int (State (Int,String,Bool)) ()
eas3 = forever $ do
  x <-await
  b <- lift $ zoom _3 get
  case b of 
    True -> lift (zoom _2 $ put "true") >> yield x
    _    -> yield $ x - x




--------------------------------------------------------------------------------
--------------------------------- Utils ----------------------------------------
--------------------------------------------------------------------------------


encase :: Monad m => Step a b x (CoroutineT a b m x) -> CoroutineT a b m x
encase = CoT . return 


--------------------------------------------------------------------------------
------------------------------ Depricated --------------------------------------
--------------------------------------------------------------------------------


-- This is for prelim testing before `Source` was created, it should not be used

-- | Monoid constraint on return value ensure we always have a value though function may not be "Done"
run_ :: (Monoid x, Monad m) => CoroutineT a b m x -> [a] -> m ([b],x)
run_ (CoT m) as = m >>= \s -> case s of 
  Done x    -> return ([],x)
  Fail      -> return ([], mempty)
  Yield b r -> (\(bs,r) -> (b:bs,r)) `liftM` run_ r as  
  Await k   -> case as of 
    []     -> return ([],mempty)
    (x:xs) -> run_ (k x) xs


-- | run a coroutine capped off with a source
--runT' :: (Monoid x, Monad m) => CoroutineT a b m x -> m ([b],x)
--runT' c = resume c >>= \s -> case s of 
--  Done x    -> return ([],x)
--  Fail      -> return ([], mempty)
--  Yield b r -> (\(bs,r) -> (b:bs,r)) `liftM` runT' r
--  Await k   -> error "the stream is not properly capped"   
--  --  * note if `source` or any cap of stream were to have `Await` in it, then runT would be broken
--  --- * Problem: what is it awaiting... a machine w/ a Source should NEVER be awaiting!
--  --  * how do I ensure this case is never reached?


-- | first of all: this returning the same type x needs to go

--(~>) :: Monad m => CoroutineT a b m x -> CoroutineT b c m x -> CoroutineT a c m x
--c1 ~> c2 = CoT $ resume c2 >>= \s2 -> case s2 of 
--  Done x    -> return . Done $ x
--  Fail      -> return Fail
--  Yield c r -> return . Yield c $ c1 ~> r
--  Await k   -> resume c1 >>= \s1 -> case s1 of 
--    Fail      -> return Fail
--    Done x    -> return . Done $ x
--    Yield b r -> resume $ r ~> (k b)
--    Await h   -> return . Await $ \a -> h a ~> encase (Await $ \b -> k b)




-- THIS may or may not be feasible even if the correct types are constructed
  
-- | question: note in ~> above, the operations happen Inside the monad m, the only interaction w/ the monad
-- | is with "return"
(~>>) :: (Monad m, Monad m') => CoroutineT a b m x -> CoroutineT b c m' y -> CoroutineT a c (P.Product m m') ()
c1 ~>> c2 = undefined

-- * note something like: CoT $ resume c3 >>= \_ -> ...
-- * does not work since this takes us inside m, but we need to be inside Product m m'


--------------------------------------------------------------------------------
--------------------- Intermission with Monad.Product --------------------------
--------------------------------------------------------------------------------



a :: P.Product Maybe Maybe Int
a = P.Product (Just 12, Just 3)


b :: P.Product (State Int) (State String) ()
b = P.Product (s1,s2)  

s1 :: State Int ()
s1 = modify (+12) >> return ()

s2 :: State String ()
s2 = modify (++" cons string") >> return ()


s3 :: State Int Int
s3 = modify (+100) >> get >>= \x -> return x

s4 :: State String Int
s4 = modify (++"basara") >> get >>= \x -> return . length $ x


c = P.Product (s3,s4)


{-
  instance (Monad g, Monad h) => Monad (Product g h) where
    return a = Product (return a, return a)
    Product (g, h) >>= k = Product (g >>= fst . runProduct . k, h >>= snd . runProduct . k)
    Product (ga, ha) >> Product (gb, hb) = Product (ga >> gb, ha >> hb)
-}


-- | observation: it's unwiedly (don't even know how) to access underlying states
-- | IN fact, according to SO, it's not possible to do this
s5 :: P.Product (State Int) (State String) ()
s5 = do
  P.Product (modify (+404), modify (++ "404"))   
  --let (a,b) = unp $ P.Product (get,get) :: (State Int Int, State String String)
  return ()


s7 :: P.Product (State Int) (State Int) Int
s7 = do 
  let (a,b) = unp $ P.Product (get,get) :: (State Int Int, State Int Int)  
  return 404




-- | Using a product of states, (instead of product of state monads), then accessing each using lens is much easier



easy :: State (Int, String) (Int,String)
easy = do 
  n <- zoom _1 get
  x <- zoom _2 get
  zoom _2 $ put "som ting wong"
  return (n + 404, x ++ " 404")















