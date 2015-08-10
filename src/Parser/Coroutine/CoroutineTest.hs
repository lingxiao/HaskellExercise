{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleContexts #-}  
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}


-- read during free time: http://hackage.haskell.org/packages/archive/kan-extensions/3.6.2/doc/html/Data-Functor-Yoneda-Reduction.html

---------------------------------------------------------------------------------------------------------------------------
------------------------------------------------ Bidirectional Coroutines -------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------

--import Control.Monad.ListT

import Prelude hiding ((.),id)

import Control.Lens
import Control.Monad
import Control.Monad.List
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
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
import Coroutine


--------------------------------------------------------------------------------
---------------------------------- Test ----------------------------------------
--------------------------------------------------------------------------------


-- | goal by end of night: pretty package MyPipes and use it with Parser

-- | write the combinator `repeatedly`


-- | figure out a way to "cap" a coroutine so it takes in infinite list of parameters, so
-- | I don't have to manually pass it in


-- | think about if the current construction of client/server makes sense, because it seems a bit arbitrary


-------------------------------------------- refine autoT design --------------------------------------------

-- | here we make autoT' parameterize over some monad m, so that it can work w/ any other effectful coroutineT
-- | provided its monad is `ListT m`
autoT' :: Monad m => (String -> [Feature]) -> [Feature] -> CoroutineT Buffer [Feature] Feature () (ListT m) ()
autoT' g bs = do
  cs <- request bs
  case g cs of 
    []  -> autoT' g [] 
    bs' -> undefined



-------------------- usecase : parser ~> buffer ~> transduer with nondetermnisim --------------------

-- * some problems making this less than sublime:
--    1. cannot take products of monads, only monad products, thus preventing true modularity
--    2. too many nested [] constructs
--    3. passing parameters up and down kind of ties the coroutines together

-- * Generic solution: again, some sort of product monad construct that's usable

nfst :: String -> [Feature]
nfst "run" = ["root-run", "root-other"]
nfst "s"   = ["verb-third-person", "noun-plural"]
nfst _     = []


type StateThread = StateT Buffer []

-- | job: build the buffer and flush it based on downstream indicator
bu :: CoroutineT Char () Buffer Bool StateThread ()
bu = do 
  c  <- request ()
  lift . modify $ (++[c])
  cs <- lift get
  b  <- respond cs
  case b of 
    False -> bu
    _     -> (lift . put $ []) >> bu


-- | biggest problem here is that its monadic effect is not isolated, though that is precicely why it works

-- | could request and respond with Feature
-- | job: this should come from the transducer package, runs the transducer and output results
autoT :: (String -> [Feature]) -> Bool -> CoroutineT Buffer Bool Feature () StateThread ()
autoT g b = do 
  cs <- request b
  case g cs of 
    [] -> autoT g False
    bs -> do 
      b <- lift . lift $ bs   -- * this line requires a priori knowlege of how the monad is stacked
      respond b
      autoT g True


-- | job: build some larger data structure from uptream outputs
-- | for now explicity recurse with [feature] which changes states so underlying monad don't get crazy
build :: [Feature] -> Machine Feature [Feature]
build fs = do 
  bs <- await
  let bs' = (<>) <$> fs <*> [bs] <||> fs <||> [bs]
  yield bs'
  build bs'


--pip2 :: CoroutineT () () [Feature] () StateThread ()
pip2 = padap "runs# rest..." (=='#') ~> bu ~> autoT nfst False ~> build ["sent-one-", "sent-two-"]

pip2' = join . flip evalStateT [] . runT $ pip2   



--build' = run $ source ["verb-third-person"] ~> build ["root-run--", "root-other--"]
--auto = flip runStateT [] $ runT_ (autoT nfst False) ["run", "s"] $ replicate 10 ()
--bu' = flip runStateT [] $ runT_ bu "hello world" $ replicate 4 False ++ [True]



-------------------- usecase : parser ~> buffer ~> transduer with product states --------------------


-- | note if entire stream share local state,then there's no point in passing completed Feature up and down 
type Feature = String
type Buffer   = String
type Local    = State (Buffer,Feature)


-- | in reality, this should just be a source??
padap :: Monad m => String -> (Char -> Bool) -> MachineT () Char m ()
padap [] _     = stop
padap (x:xs) p = yield x >> if p x then stop else padap xs p 


buff :: CoroutineT Char () Buffer Bool Local ()
buff = do 
  c  <- request ()
  lift . zoom _1 . modify $ (++[c])
  cs <- lift . zoom _1 $ get
  b  <- respond cs
  case b of 
    False -> buff
    _     -> (lift . zoom _1 . put $ []) >> buff


pip1 = flip runState ([],[]) . runT $ padap "hello# rest..." (=='#') ~> buff ~> trans dfst False



-------------------- usecase : raw version of parser ~> buffer ~> transduer ------------------------


dfst :: String -> String
dfst "hello" = "world"
dfst "harro" = "word"
dfst "#"     = "EOL"
dfst _       = []


-- | note this needs to go inside the parser somehow
parserAdapter :: Monad m => String -> CoroutineT () () Char Bool m ()
parserAdapter []     = stop
parserAdapter (x:xs) = do
  b <- respond x
  case b of 
    False -> stop
    _     -> parserAdapter xs


buffer :: CoroutineT Char Bool String Bool (State String) ()
buffer = do 
  c  <- request True
  lift . modify $ (++ [c])
  cs <- lift get
  b  <- respond cs
  case b of 
    False -> buffer                         -- * case buffer does not need to be flushed
    True  -> (lift . put $ []) >> buffer    -- * case buffer does need to be flushed


trans :: Monad m => (String -> String) -> Bool -> CoroutineT String Bool String () m ()
trans g b = do 
  cs <- request b
  case (g cs) of 
    []  -> trans g False                   -- * case transducer cannot map as to bs, thus set match == False
    bs  -> respond bs >> trans g True      -- * case transducer map as to bs, yield bs and set match == True


-- | test pipeline

pipe :: Session (State String) String
pipe = (parserAdapter "hello rest...") ~> buffer ~> (trans dfst False)


pip :: ([String], String)
pip = flip runState [] . runT $ pipe    -- * (["world"],"rest...")


-- | unit tests
--par = runIdentity $ runT_ (parserAdapter "hello world") [(),(),(),(),()] [False,False,False,True]
--buf = flip runState [] . runT_ buffer "hello sha" $ replicate 5 False ++ [True] ++ replicate 3 False ++ [True]
--tra = runIdentity $ runT_ (trans dfst False) ["h","he","hel", "hell", "hello"] $ replicate 100 ()


  
  
--pAdapter :: String -> (Char -> Bool) -> CoroutineT () () Char Feature Local ()
--pAdapter [] _     = stop
--pAdapter (x:xs) p = do
--  fs <- respond x
--  case p x of 
--    True -> (lift . modify $ \(_,bs) -> (fs,bs)) >> stop
--    _    -> pAdapter xs p

--pa = flip runState ([],[]) $ runT_ (pAdapter "hello# rest" (=='#')) (replicate 10 ()) ["","","","","", "world"]


------------------------------ test full sessions composed by (~>) -------------------------------

-- | proxy using request/respond
prox :: Coroutine Int Bool Int String 
prox = do 
  x   <- request True
  str <- respond x
  _   <- respond $ length str + x + 100
  prox

-- * run proxy by itself
p' = run_ prox [1..4] ["a","bb","ccc","dddd"]

-- * run proxy as part of an enclosed pipeline
spc :: [Int]
spc = run $ source [1..4] ~> prox ~> client ["a","bb","ccc","dddd"]

-- | proxy using await/yield
mach :: Machine Int Int 
mach = forever $ do 
  x <- await
  yield $ x + 100

mach2 :: Monad m => MachineT Int Bool m ()
mach2 = forever $ do 
  x <- await
  if x > 102 then yield True else yield False

smc  = run $ source [1..4] ~> mach ~> mach2


-- | use monad transformer Feature with coroutineT

proxt :: CoroutineT Int Bool Int String (State Int) ()
proxt = do
  i <- request True
  s <- respond i
  lift . modify $ (+i)
  j <- lift get
  _ <- respond $ length s + j + 100
  proxt

sptc = flip runState 0 . runT $ source [1..4] ~> proxt ~> client ["a","bb","ccc","dddd"]


-- | use monad transformer Feature with machineT 

macht :: MachineT Int Int (State Int) ()
macht = forever $ do
  x <- await
  lift . modify $ (+x)
  y <- lift get
  yield y


smt = flip runState 0 . runT $ source [1..3] ~> macht 

smtc = flip runState 0 . runT $ source [1..5] ~> macht ~> mach2


-- | some fail cases
f1 = run $ stop ~> prox ~> client ["a","bb","ccc","dddd"]
f2 = run $ source [1..4] ~> stop ~> client ["a","bb","ccc","dddd"]
f3 = run $ source [1..4] ~> prox ~> stop



--------------------------------- test source and client ---------------------------------

s = run_ (source [1..4]) [(),(),()] [(),(),()]
c = run_ (client [1..4]) [(),(),()] [(),(),()]


-- | test primitives

e1 :: CoroutineT Int Bool Int String Identity ([Int],String)
e1 = do
  int <- request True
  str <- respond int
  return (pure int,str)

e1' = run_ e1 [1..4] ["hello", "second"]

e2 :: CoroutineT Int Bool Int String Identity ()
e2 = do
  int <- request True
  str <- respond int
  case str of 
    "hello" -> e2
    _       -> stop

e2' = run_ e2 [1..4] ["hello", "second"]


e3 :: CoroutineT Int String Int () Identity ()
e3 = do 
  x <- request "hello"
  _ <- respond $ x + 10
  e3

e3' = run_ e3 [1..3] [(),(),()]


-- | test composition operator (~>)
e23 :: CoroutineT Int Bool Int () Identity ()
e23 = e2 ~> e3


e23' = run_ e23 [1..3] [(),(),()]



--------------------------------- test CoroutineT type ---------------------------------

p1 :: CoroutineT () () Char Int Identity [Int]
p1 = encase . Respond 'a' $ \i -> return [i,i]


p2 :: CoroutineT Char Int () () Identity [Char]
p2 = encase . Request 404 $ \c -> return [c,c]

c0 :: CoroutineT Int Int Int Int Identity [Int]
c0 = encase . Request 404 $ \a -> 
      encase . Respond a $ \b' -> 
        return [b']


c0' :: ([Int],[Int],[Int])
c0' = run_ c0 [404] [1..4]

c1 :: CoroutineT Int Int Int Int Identity ()
c1 = do 
  a  <- request 404
  b' <- respond a
  c1


c1' = run_ c1 [404] [1..4]



































