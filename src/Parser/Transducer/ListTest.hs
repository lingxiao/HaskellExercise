-- * For test to run correctly, everything from MyListT have to be exported

import Prelude hiding (foldr, foldl)

import Control.Monad
import Control.Comonad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Identity
import Control.Applicative

import Data.Foldable
import Data.Monoid

import ListT
import Coroutine hiding (encase)
import Type    
import Transducer
import Variants
import Utils


-- * game plan tomorrow:

-- 1. polish logic for build
-- 2. make fmap work correctly
-- 3. Integrate coroutines back into parser, how would it work?
--       1. should parser itself be a monad transformer?  where does underlying computations get lifted?
--          1.a.  might have to look at parserc again






----------------- another autoT that compose with other Coroutines -----------------------------

type Buffer  = String
type Feature = String

-- * recreate autoT so that its monad stack is `ListT m` instead of `ProcessT []`, so it can be composed with any 
-- * underlying monad

nfst :: NFST Maybe String Feature 
nfst = toNFST df where
  df (I 0) (Just "run")  = [(Just "root-run-", I 2)]
  df (I 2) (Just "s")    = [(Just "noun-Plural", F 1), (Just "Verb-ThirdPerson", F 1)]
  df (I 2) (Just "ning") = [(Just "Verb-Progressive", F 1)]
  df _     _             = []


buffer :: (Monad f, Monad m) => CoroutineT Char () (f Buffer) Bool (ListT (StateT Buffer m)) ()
buffer = do 
  c  <- request ()
  lift . lift . modify $ (++ [c])
  cs <- lift . lift $ get
  b  <- respond . return $ cs
  case b of 
    False -> buffer
    _     -> (lift . lift $ put []) >> buffer


auto :: Monad m => NFST f a b -> Bool -> CoroutineT (f a) Bool (f b) () (ListT m) ()
auto m t = do 
  a <- request t 
  case step m a of 
    []  -> auto m t 
    bs  -> do 
      (b,_,m') <- lift . liftList $ bs
      respond b
      auto m' True 


-- * IS this even the best solution to begin with???
-- * the logic here is troublesom since incomplete results are also yielded... need some stopping citerion
-- * at which point the result is yielded
build :: Monoid (f a) => [f a] -> Machine (f a) [f a]
build fs = do 
  bs <- await
  let bs' = (<>) <$> fs <*> [bs] <||> fs <||> [bs]
  yield bs'
  build bs'


-- * composite testing

runComp :: Session (ListT (StateT [a] Identity)) b -> [[b]]
runComp = runIdentity . flip evalStateT [] . runListT . runT

buaut :: Monad m => CoroutineT Char () (Maybe String) () (ListT (StateT Buffer m)) ()
buaut = buffer ~> auto nfst False


-- * eventually, might need a circular toplogy... hmm.. 
b' :: [[Maybe String]]
b'  = runComp $ source "run rest .. " ~> buaut
b'' = runComp $ source "running rest .. " ~> buaut ~> build []


-- unit testing 

bu :: [([Maybe Buffer], [()], ())]
bu = runIdentity . flip evalStateT [] . runListT . runT_ buffer "run" $ replicate 5 False ++ [True]


aut :: [[Maybe Feature]]
aut = fmap fst3 $ runIdentity . runListT . runT_ (auto nfst False) [Just "r",Just "ru",Just "run"] $ replicate 10 ()


-- * note this monoid v constraint make this a nightmare to use, GET RID of it
bui :: ([[Maybe Feature]], [()], ())
bui = run_ (build []) [Just "root-run-", Just "Verb-Pl"] $ replicate 10 ()


--------------------------------------- recreate autoT ---------------------------------------


autot :: Monad m => NFST f a b -> MachineT (f a) (f b) (ListT m) ()
autot m = do 
  a <- await 
  case step m a of 
    [] -> autot m
    bs -> do 
      (b,_,m') <- lift . liftList $ bs
      yield b 
      autot m'


-- * note Just <$> source [1..3] does not work here, since fmap maps g onto `return` value only
aa :: [[Maybe String]]
aa = runIdentity . runListT . runT $ source (Just <$> ["allow","boot"]) ~> autot nfst 

-- * each list correspond to a thread out outcomes
aa' = [[Just "_1",Just "_1"],[Just "_1",Just "_0"],[Just "_1",Just "_3"],[Just "_0"]]



-------------------------------- simple nondeterministic stuff -------------------------------- 

t1 :: [Bool]    -- * note how length t2 = 12, so all possible combinations are tried
t1 = runList $ liftList [1..4] >>= \x -> liftList [1..3] >>= \y -> return $ x + y > 3


-- * equaivalent to the list comprehension construction, except t2 above can have side effects
t1' = [ x + y > 3 | x <- [1..4], y <- [1..3] ]  :: [Bool]


-- | model some nondeterministic computations

-- * Some simple ListTs

t00 :: ListT (State Int) Int
t00 = ListT . return $ 1 `ConsT` (return (2 `ConsT` (return NilT)))   -- * running w/ si of 0 yield: ([1,2],0)


t01 :: ListT Identity Int
t01 = liftList [1..4]


-- * analgous to `[]`
t02 :: [Int]
t02 = runList $ liftList [\x -> x + 1, \x -> x - 1] <*> t01


------------------------------  contrast StateT [] vs ListT (State) ------------------------------ 


-- * here each item in the list have their own state

alt :: StateT String [] Int 
alt = do 
  x <- get
  do 
    i <- lift [1..3]
    modify $ (++" world")
    return $ i + length x

-- * [(1,0),(2,0),(3,0)]
alt' = flip runStateT "hello" $ alt :: [(Int,String)]

-- * here all items in the list share the same state, and for all m threads
-- * the underlying state is incred by some number n, not m * n, since each thread does not know of the other one's existence

t03'  = (lift . modify $ (+100)) >> lift get >>= \x -> return (\y -> y + x) :: ListT (State Int) (Int -> Int)

t03'' = (lift . modify $ (+1))   >> lift get >>= \x -> return (\y -> y - x) :: ListT (State Int) (Int -> Int) 

-- * ([201,202,-200,-199],201)
t03 :: ([Int], Int)
t03 = flip runState 100 . runListT $ t03' <> t03'' <*> t00



---------------------------------------------- other ---------------------------------------------- 

-- * Try to : visualize the flow of computation.
-- one way to see it that when `liftList ls >>= \x -> ...`, a copy of the function is running foreach value in list `ls`,
-- so when `return` is called, it's happening for each value, and the monad operations is doing the plumbing of consing the 
-- results into a `listTM`, `runListT` converts it back to a list
-- any function g is `fmapped` onto each value in the list, so all possibilities are explored

-- but where is the `ConsT` logic?

-- * now we add side effects, in this case state
-- * ([True,True,True,True,True,True,True,True,True,True,True,True],56)
t2 :: ([Bool], Int)
t2 = flip runState 2 . runListT $ do 
  let xs = liftList [1..4]
  let ys = liftList [1..3]
  x <- xs 
  y <- ys
  z <- lift get
  lift . modify $ \z -> z + x + y
  return $ x + y + z > 3             -- * recall `return` builds a listT of one item, how does listT "know" to ConsT them together?


-- * [(3,4),(4,3),(4,4)]
t3 :: [(Int,Int)]      
t3 = flip evalState 6 . runListT $ do 
  let as = liftList [1..4]
  x <- as
  y <- as
  z <- lift get
  guard $ x + y > z    
  return (x,y)


---------------------------------------------- runListT -----------------------------------------------


a :: [Int]
a = runList . liftList $ [1..4]


-- | joinT 
l123 :: ListT Identity (ListT Identity Int)
l123 = ListT . return $ liftLTM [l1,l2,l3] where 
  l1 = liftList [1..4]
  l2 = liftList [11]
  l3 = liftList [6..9]

l123' :: [Int]
l123' = runList . joinT $ l123









{-
-- | again not useful at all due to comonad restriction

-- | only other way to get listTM m a out of m (..) is to run m 
instance (Monad m, Comonad m) => Monoid (ListTM m a) where
  mempty          = NilT
  lt `mappend` ls = case lt of 
    NilT      -> ls
    ConsT a m -> case extract m of 
      NilT -> ConsT a $ return ls
      lt'  -> ConsT a $ return $ lt' `mappend` ls


-- | note this is not very usable due to comonad restriction
-- | how else would ListTM get out of its context? 
instance (Monad m, Comonad m) => Foldable (ListTM m) where
  foldr g a NilT         = a
  foldr g a (ConsT a' m) = foldr g (g a' a) (extract m)

-}





-- | game plan: 
--      1. In english, write out what ListTM is suppose to do
--      2. implement data ListTM and instances: functor, applicative, monad, traversable
--      3. implement ListTM functions analogous to list: foldr, scanl (?) 
--      4. implement primitives?


--      5. test on toy nondeterministic problems
--      6. test on usecase adapter ~> buffer ~> transducer ~> builder


-- * ListTM: 
--     provides a nondeterministic computational context for underlying monad
--     when running a computation with multiple results, there is some boilerplate logic that
--     involves putting the results together


-- | game plan:

--    1. test out different nonD examples, understand what they're suppose to do before you compile them
--    2. test it out on an example isomporhic to MyPipes problem from 2 days ago
--    3. go back to finish mypipes stuf


































