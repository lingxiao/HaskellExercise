-----------------------------------------------------------------------------
-- | Module : ParserT.hs
-- | The primitive Parser type.
-----------------------------------------------------------------------------   

{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleContexts #-}  
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}


module ParserT where 

import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Applicative
import Control.Comonad
import Data.Monoid


import qualified Data.Machine as M
import Coroutine 
import Utils

-- * this Parser0 must satisfy the same behavior for all standard interfaces as Parser0.hs
-- * challenge for later: can you just make a Parser0 out of coroutines?? 


-- PROBLEMS encountered

-- how do you lift another computation into this one?
-- how do you implement applicative?



------------------------- Prelim Design 3: ParserT with early escape  -----------------------------



{-

  --- *  design one: embedd coroutine inside Parser Parser a (Coroutine a Bool b ()) b
  --- *  design two: embedd some adatper monad inside parser, so parser can inter-op with arbitrary monad process
  --- *  with combinator (=>>)

  --- *  Here Interop is like a monad of sort

  ideally: 

  2 new abstractions (maybe typeclass), `interop` and `<=` to compose two monads together

  `<=` is needed because ... bidirectional flow of information, but could this be rolled into coroutines?
  reason against rolling into coroutines: it doesn't return a coroutine, but a Parser


  Can't seem to find higher level abstraction here: 
  GENERAL:  a notion of embedding one function inside another, with multiple inputs, and one output


  sourceParse :: Parser a (InterOp) b     
  sourceParse = do 
    c <- getC
    b <- lift . interop . respond $ c                  <-- push char into buffer
    case b of 
      False -> sourceParse
      True  -> return . lift . interop $ request ()    <-- get result from underlying computation


  lexer :: NFST a b -> Coroutine a Bool b () ()
  lexer fst = buffer ~> autoT fst ~> sink 


  parseRaw :: Parser a b 
  parseRaw = sourceParse <= lexer fst


  (<=) :: Parser a b -> Coroutine a a' b b' v -> Parser a b 

  Note how signature of (<=) is very specific, it looks like it could generalized to a typeclass of 
  some sort


  better yet:

  `sourceParse ~> buffer ~> autoT fst ~> sink ~> mirror`

  where `mirror` puts the output back in the sourceParse

  some impdances:  coroutines should never return, just await and yield

  parser does return, other stuff depends on it. therefore they cannot be the same type?

  so need some higher order function that helps these two inter-op, which is why

  `parser <= lexer` might be needed still, where `<=` embeds lexer inside parser somehow


  Now think at a higher level of abstraction for all arbitrary functions, think about the vocab
  needed to describe this combinator -> it looks like a subroutine abstraction

  func_1 yields repeatedly
  func_1 requests termination 

  func_2 awaits repeatedly
  func_2 responds to termintation -> but what does it output?

  func_1 is super-routine, func_2 is subroutine, which could be made of coroutines
  

  super-routines could compose as coroutines??

  which could be nested inside another subroutine


  might need a way to convert a coroutine pipe into someting that "sort of returns" a value


  Critique the subroutine implementation

  what's the advantage of this complex abstraction over just calling another function?

  similar: in both cases, the function is recursing many times, and then finishing with a value

  difference: have to pass whole stream into buffer, which may either be unavailable, or even if it is,
 

-}

-- step zero: rank list of tasks from most important, to find some fixed points

-- 1. mock super/subroutine pair, run simple tests
-- 2. subroutine is actually a coroutine pipe?



-- step one: mock this subroutine abstraction, and characterize its relationship to coroutines

-- * There appear to be a pair of things here, the super routine and subroutine. 

-- * one possiblity is that they're both monad tranformers, and could also be embedded elsewhere


-- * use case: to yield value to subroutine, lift enter a >> rest of computations
-- * use case: to end subroutine and get value, lift exit >>= \result -> return result


-- * Transform any function into a subroutine 

-- * lexer is a subroutine with coroutine somehow "embedded" into it, 

-- * choice 1: monad transformer, lexer :: SubroutineT a (CoroutineT a a' b b') b
-- * choice 2: some higher order function, what what is it doing? embedd :: CoroutineT a a' b b' m v -> Subroutine a b


-- * Transform any function into a super routine


-- step depricated: Interop seems like another layer of abstraction over simple transformer over coroutine
-- see if parserT (coroutine) can be implemented, using `PT` defined below


-- * think about composition

{-

"embed"
(<=) :: superRoutine -> subRoutine -> superRoutine


identity criteria: 
super <= identity = super
identity <= sub   = sub

so what is identity?


associativity:

(super <= sub) <= sub' = super <= (sub <= sub')

This means w/e is on the left have be able to act as a super routine no matter what..



so there has to be symetry of how things interop


what is the difference between this and         



-}

-- * Write down the types. 










-------------------------- Prelim Design 2: non-deterministic ParserT  -------------------------------


-- * now what is the advantage of this over P { unP :: m ([a] -> (b,[a])) } ?
-- * aside from Fail state, which could be modeled by empty list in NonD case anyways?????

--data Stream a b r
-- = Fail 
-- | Yield (b,[a])     -- * r is not returned here, which should I use in later computations?
-- | Await (a -> r)    -- * r is returned here, after it receives a

newtype PT a m b = Pt { unp :: m ([a] -> m (b,[a])) }

type Parser a b = forall m. Monad m => PT a m b


runPt :: Monad m => PT a m b -> [a] -> m (b,[a])
runPt p as = unp p >>= \k -> k as  

runP :: PT a Identity b -> [a] -> (b,[a])
runP p = runIdentity . runPt p

instance Monad m => Functor (PT a m) where
  fmap g (Pt m) = Pt $ liftM (liftM (\(b,as) -> (g b, as)) . ) m 


instance Monad m => Applicative (PT a m) where
  pure b    = encase $ \as -> return (b,as)
  p1 <*> p2 = encase $ \as -> do
    (g,as') <- runPt p1 as          
    do 
      (b,as'') <- runPt p2 as'      
      return (g b, as'')            


instance Monad m => Monad (PT a m) where       
  return  = pure
  p >>= g = encase $ \as -> do 
    (b,as') <- runPt p as
    runPt (g b) as'


instance MonadTrans (PT a) where
  lift = Pt . liftM (\v -> \as -> return (v,as)) 


encase :: Monad m => ([a] -> m (b,[a])) -> PT a m b
encase = Pt . return

------------------------------------ Transform Coroutines ------------------------------------


-- * transformer over machines

instance Monoid Char where
  mempty = 'z'
  a `mappend` b = a

-- * implementation of s4 and buffer is below

-- * unit test s4
s4' = thd3 . runIdentity $ runT_ (runPt s4 "hello world") (replicate 10 ()) (replicate 4 [] ++ ["FeatureMatrix"])
-- * unit test buffer
b' = flip evalState [] . runT_ (buffer []) "hello#..." $ replicate 10 ()


-- * Now compose these two actions together

-- * superroutine parser call subroutine coroutine
-- * this will be generalized somehow

-- * do you somehow build a new coroutine out of this??
--call :: Monad m => PT b (CoroutineT a a' b b' m) x -> CoroutineT b b' c c' m u 
--call = undefined


-- * intermission with monad tranformer

-- * want to recreate a function that successfully does:

-- * t m ()
t :: WriterT String (State Int) ()
t = do
  tell "hello"
  lift . modify $ (+ length "hello")

-- * s m ()
sm :: ReaderT Int (State Int) ()
sm = do 
  n <- ask
  lift . modify $ (+n)

-- * t m () -> s m () -> t (s m) ()
call :: (MonadTrans t, MonadTrans s, Monad (t (s m)), Monad (s m), Monad m, MFunctor t) => t m a -> s m b -> t (s m) b
call t s = hoist lift t >> lift s


-- * note Reader is not entirely embedded inside Writer, 
-- * so composition function of form :: `WriterT m -> WriterT m -> WriterT m` can work on this stack
tsm :: WriterT String (ReaderT Int (State Int)) ()
tsm = t `call` sm


-- * note the underlying effects are visible in signature and need to be run layer at a time
tsm' = flip runState 200 . flip runReaderT 200 . runWriterT $ tsm



-- * increment forward a bit by swapping out the types with parser

s4 :: Monad m => PT Char (CoroutineT () () Char String m) String
s4 = do 
  c <- getC
  b <- lift . respond $ c 
  case b of 
    [] -> s4
    _  -> return b


-- * a buffer to use with s3

buffer :: String -> CoroutineT Char String () () (State String) ()
buffer bs = do 
  c <- request bs
  if bs /= [] then stop else case c of 
    '#' -> lift get >>= \bs' -> buffer $ bs' ++ "-EOL"
    _   -> (lift . modify $ (++ [c])) >> buffer []
















---------------------------- * intermission with S.O. Answer * -------------------------------


-- * with generic signature

co :: Monad m => m a -> m a -> m a
co = undefined


tma :: (Monad m, MFunctor t) => t m a
tma = undefined


ma :: Monad m => m a
ma = undefined


-- * tma has underlying monad ma, and co ma :: m a -> m a
-- * thus fmap (co ma) t (m a) = t (co ma ma) = t (m a), where fmap is hoist
comb :: (Monad m, MFunctor t) => t m a
comb = hoist (co ma) tma


com :: (Monad m, MFunctor t) => t m a -> t m a
com = hoist (co ma) 


-- * with state as underlying monad

co' :: State String Int -> State String b -> State String b
co' m1 m2 = m2 


tma' :: WriterT String (State String) String
tma' = do 
  x <- lift get
  tell $ x <> x
  return x


ma' :: State String Int
ma' = modify (\a -> a <> a) >> return 200


com' :: WriterT String (State String) String
com' = hoist (co' ma') tma'


rcom = flip runState "harro" $ runWriterT com'


-- **************** try again with coroutine as underlying monad ***************************


-- * test only signatures

cor :: Monad m => CoroutineT b b' c c' m v 
cor = undefined


tco :: (Monad m, MFunctor t) => t (CoroutineT a a' b b' m) b 
tco = undefined


tcoco :: (Monad m, MFunctor t) => t (CoroutineT a a' c c' m) b
tcoco = hoist (\co' -> co' `o` cor) tco


-- * trivial stack
tco0 :: Monad m => StateT Int (CoroutineT () () Int Bool m) Int 
tco0 = lift stop >> return 404


-- * upstream coroutine stop first 
tco1 :: Monad m => StateT Int (CoroutineT () () Int Bool m) Int
tco1 = do 
  x <- get
  case x > 5 of 
    True -> lift stop >> return x 
    _    -> (lift . respond $ x) >> modify (\x -> x + 1) >> tco1


-- * upstream coroutine stop first, alternate implementation
tco2 :: Monad m => StateT Int (CoroutineT () () Int Bool m) Int
tco2 = do 
  x <- get
  b <- lift . respond $ x
  case x > 5 of 
    True -> lift stop >> return x
    _    -> modify (\x -> x + 1) >> tco2


-- * upstream coroutine stop second
tco3 :: Monad m => StateT Int (CoroutineT () () Int Bool m) Int
tco3 = do 
  x <- get
  b <- lift . respond $ x
  case b of
    True -> return x 
    _    -> modify (\x -> x + 1) >> tco3


-- * a pure coroutine
cor1 :: Monad m => Bool -> CoroutineT Int Bool Int () m v
cor1 b = do 
  x <- request b 
  case x >= 10 of                      
    True -> respond x >> request True >> stop
    _    -> respond x >> cor1 False


-- * coroutineT over state monad
cor2 :: CoroutineT Int Bool Int () (State Bool) v
cor2 = do 
  b <- lift get
  x <- request b
  case x >= 10 of 
    True -> respond x >> request True >> stop
    _    -> respond x >> (lift . put $ False) >> cor2



-- * test trival `state (coroutine) ~~> coroutine`
tcoc0' :: [Int]
tcoc0' = runIdentity . runTs . flip runStateT 0 $ hoist (flip o (cor1 False)) tco0

-- * test state (coroutine) ~~> coroutine
tcoc1 :: Monad m => StateT Int (CoroutineT () () Int () m) Int
tcoc1 = hoist (\co1 -> co1 `o` cor1 False) tco3


tco1' :: [Int]
tco1' = runIdentity . runTs . flip runStateT 0 $ tcoc1   -- * [404,200]


-- * test state (coroutine) ~~> coroutine (state)
tcoc2 :: StateT Int (CoroutineT () () Int () (StateT Bool Identity)) Int
tcoc2 = hoist (flip o cor2) tco3

tcoc2' = runIdentity . flip evalStateT False . runTs . flip runStateT 0 $ tcoc2



-- * some problems:
-- * 1. the whole thing is yielding,there is no return value from runStateT
-- * 2. if transformer `return`, the coroutine also return, so it's put in `Done` state
-- * 3. structural issue: the transformer stack is inconvinient to run since its so deep


-- * address problem 1

-- * CENTRAL CONFLICT: mixing two ways of programming, one that yields and awaits and finish computation,
-- * and one that does have a notion of Done

-- * Proposed Resolution: the coroutine does not `Done v` but `Exit`, but what's the difference between this and Fail?
-- * a different way to make the coroutine?


sco :: Monad m => StateT Int (CoroutineT () () () () m) Int 
sco = (lift . return $ "404") >> return 404


wco :: Monad m => WriterT String (CoroutineT () () () () m) String 
wco = (lift . return $ 404) >> tell "told" >> return "returned"


scoo :: Monad m => StateT Int (CoroutineT () () Int () m) Int
scoo = (lift $ respond 1 >> respond 2 >> respond 3) >> return 404


-- * note `runTs` is implemented wrong such that any monad tranforming it does not properly work
sco' = runIdentity . runTs . flip runStateT 0 $ sco   -- []

-- * alternate implementation of `runTs` where the the value is returned
-- * this works although `fail` is now not working
-- * additionally, it doesn't make sense for coroutine to `return` a value
sco'' = runIdentity . runTs' . flip runStateT 0 $ sco  -- ([],(404,0))


scoo' = runIdentity . runTs' . flip runStateT 0 $ scoo  -- ([1..3],(404,0))


-- * proof of concept that applicative interface does work
scooap = runTs' . flip runStateT 0 $ (+) <$> sco <*> sco


-- * check same behavior with WriterT (CoroutineT m)
-- ([],("returned","told"))
wco' :: ([()], (String, String))
wco' = runIdentity . runTs' . runWriterT $ wco



-- ***************************** test feature of interaction between stateT and coroutineT ****************************


-- * hack to satisfy dumb constraint in my coroutine package
instance Monoid Int where
  mempty  = 0
  mappend = (+)


-- * same problem with `fail` and `done` states again

-- * this one needs to be composed with a CoroutineT m stack
mock_1 :: Monad m => StateT Int (CoroutineT () () Int Bool m) Int
mock_1 = do 
  x <- get
  b <- lift . respond $ x
  case b of 
    False -> modify (+100) >> mock_1
    _     -> do 
      lift stop 
      x' <- get
      return x'


-- * this one can be run as a standalone State (Coroutine m) stack
mock_1' :: Monad m => StateT Int (CoroutineT () () Int () m) Int
mock_1' = do 
  x <- get 
  lift . respond $ x
  case x > 500 of 
    False -> modify (+100) >> mock_1'
    _     -> do
      lift stop
      x' <- get
      return x'




-- * minimum viable stack demonstrating the problem

-- * mixing other coroutine primitve like `respond`  is fine
mock_ok :: Monad m => StateT Int (CoroutineT () () Int () m) ()
mock_ok = modify (+100) >> lift (respond 200) >> return ()

-- * mixing `stop` crashes whole computation, `stop` is like `Nothing` in maybe monad
mock_nok :: Monad m => StateT Int (CoroutineT () () Int () m) ()
mock_nok = modify (+100) >> lift stop >> return ()

-- * the underlying coroutine exits by returning a value, thus the whole computation does not crash
-- * however, this is contrary to the spirit of coroutine, which never returns 

-- * in addition, using runTs does not work still, need runTs', this is because the "return" value is discarded by runTs
-- * in runTs', the v in return (bs,v) is state's return value
mock_ok2 :: Monad m => StateT Int (CoroutineT () () Int () m) String
mock_ok2 = modify (+100) >> lift (respond 404) >> lift (return 200) >> return "return state"




-- * with `exit`, an existentially quantified `CoroutineT` type, the overlaying
-- * computation may continue after underlying coroutine stops, however, 
-- * we are again in typeclass hell as type of `exit` need to be explicitly defined

-- * also it doesn't actually exit, since we can keep on going if we want


-- * at this time it might make sense to... retool coroutine completely
-- * and FIX typeclass constraints in 
-- *    1. composition
-- *    2. run
-- *    3. try to get rid of as many synonyms as possible




mock_ok3 :: Monad m => StateT String (CoroutineT String String String String m) String
mock_ok3
  =  modify (++"hello") 
  >> (lift . respond $ "co1") 
--  >> lift (exit :: Monad m => CoroutineT String String String String m String) 
  >> (lift . respond $ "co2")
  >> return "all done"

  

-- * use `exit` primitive to mock a pipe of coroutines
-- * might be incompatibilies with return type v

se1 :: Monad m => StateT String (CoroutineT () () Int Bool m) String
se1 = do 
  x <- get
  b <- lift . respond $ length x
  case b of 
    False -> modify (++"a") >> se1
    True  -> get >>= \x' -> return x'



-- * does v need a `Monoid` constraint?
--e1 :: (Monad m, Monoid v) => Bool -> CoroutineT Int Bool Int () m v
--e1 b = do 
--  i <- request b
--  case i > 5 of 
--    True -> (e1 True) >> (exit :: (Monad m, Monoid v) => CoroutineT Int Bool Int () m v)
--    _    -> respond i >> e1 False

e2 :: (Monad m, Monoid v) => Bool -> CoroutineT Int Bool Int () m v
e2 b = exit



se12 :: Monad m => StateT String (CoroutineT () () Int () m) String
se12 = hoist (\e1 -> e1 `o` e2 True) se1












-- * try stack with `Maybe`, note how underlying failiure crashes whole computation 
mebe  = modify (+100) >> lift (Just 3) >> return () :: StateT Int Maybe ()
mebe' = modify (+100) >> lift Nothing  >> return () :: StateT Int Maybe ()



mock_2 :: Monad m => Bool -> CoroutineT Int Bool Int () m v
mock_2 b = do
  x <- request b
  case x > 500 of 
    False -> respond x >> mock_2 False
    _     -> respond x >> mock_2 True 

mock_12 :: Monad m => StateT Int (CoroutineT () () Int () m) Int
mock_12 = hoist (\m -> m `o` mock_2 False) mock_1



-- * in both examples below, the state is (0,0) why? 
-- * Because: use of `lift stop` wipe out state for some reason

-- * with a monoid declaration of Int hack, we can get the asnwer here
-- * however, 
-- * ([1,101,201,301,401,501,601],(0,0))
mock' = runTs' . flip runStateT 1 $ mock_12


-- * ([1,101,201,301,401,501],(0,0))
mock'' = runTs' . flip runStateT 1 $ mock_1'





-- * Problems:
-- * 1.  `return []` vs `return ([],v)`
-- * 2. why isn't state returning the updated underlying state and all?


-- * Solve problems:

-- * 1. figure out why stop wipe out returned values
-- * 2. figure out how to have a better run and (~>) function


-- * gameplan for Monday

-- * either fix coroutine so it's more general/usable
-- * or find new model of computation such that it does what I want
-- * maybe generalized coroutines a little bit?? how??


-- ***************************** End test feature of interaction between stateT and coroutineT *************************



-- * address problem 1 by seeing if it exists in machines
-- * conclusion: it appears to exist in machines as well

smo :: (Monad (M.MachineT m (M.Is ())), Monad m) => StateT String (M.MachineT m (M.Is ())) Bool
smo = return True


-- * observe things are returne like they should
-- * however, this can't be run since `No instance for (Monad (M.MachineT Identity (M.Is ())))`
smo' :: (Monad m, Monad (M.MachineT m (M.Is ()))) => m [(Bool, String)]
smo' = M.runT . flip runStateT "" $ smo


-- * manually construction with yield


-- * last return refers to `Plan`, this whole thing is incorrect, which is why instance `Num(Int,Int)` is needed
smo1 :: Monad m => StateT Int (M.MachineT m (M.Is Int)) Int
smo1 = StateT $ \s -> M.construct $ M.yield 404 >> M.yield 200 >> return 1000   



smo11 :: Monad m => StateT s m String
smo11 = StateT $ \s -> return ("return",s)

smo11' = runIdentity . flip runStateT 0 $ smo11


smo12 = StateT $ \s -> return (M.construct $ M.yield 404, s)


-- * note smo1 is not returning 
smo1' ::[(Int,Int)]
smo1' = M.run . flip runStateT 0 $ smo1   -- * [(404,404),(200,200)]


smo2 :: (Monad (M.MachineT m k), Monad m) => StateT Int (M.MachineT m k) Int
smo2 = do 
  x <- get
  lift . M.construct . M.yield $ 404
  return x


--smo2' = M.runT . flip runStateT 0 $ smo2 



-- * this here is needed, which looks like a really bad flaw
instance Num (Int,Int) where
  (x1,x2) + (y1,y2) = (x1+y1, x2+y2)
  (x1,x2) * (y1,y2) = (x1 * y1, x2 * y2)
  
  abs (x1,x2)    = (abs x1, abs x2)
  signum (x1,x2) = (signum x1, signum x2)
  negate (x1,x2) = (negate x1, negate x2)
  fromInteger i  = (fromIntegral i, fromIntegral i)

-- * Solution, have some value constructor `Exit` that pops out of the coroutine pipeline when called, this
-- * way no value need to be returned and no restrictions over the type of v in `Done v` is needed




-- ****************************** stuff above this line is working *****************************************


co1 :: Monad m => CoroutineT () () Int Bool m v 
co1 = do
  b <- respond 404
  case b of 
    True -> stop
    _    -> co1


co2 :: Bool -> CoroutineT Int Bool Int () (State Int) v
co2 b = do
  c <- request b
  lift . modify $ (+c)
  s <- lift get
  case s > 1000 of 
    True -> respond s >> co2 True
    _    -> respond s >> co2 False


-- * ([404,808,1212,1616],1616)
co12 =  flip runState 0 . runTs $ co1 `o` (co2 False)  


-- * with coroutine as underlying monad

rca :: StateT String (CoroutineT () () String Bool Identity) ()
rca = do
  x <- get
  b <- lift . respond $ x
  case b of 
    True  -> modify (<>x) >> rca
    False -> modify (++ "-EOL") >> get >>= \x -> return ()


ca :: Bool -> CoroutineT String Bool () () Identity u
ca b = do
  x <- request b 
  case length x > 20 of 
    True -> ca False
    _    -> ca True


--rcc :: StateT String (CoroutineT () () () () Identity) String
--rcc = hoist (\m -> m `chain` (ca True)) rca


--rc = runIdentity . runTT . flip runStateT "harro" $ rcc


-- * using rca and ca, but alternate SO answer

lca :: Monad m => StateT String (CoroutineT () () () () m) () 
lca = do 
  lift . respond $ ()
  modify (\x -> x <> x)
  x <- get 
  case length x > 20 of 
    True -> return ()
    _    -> lca


ca' :: Monad m => CoroutineT () () () () m u
ca' = forever $ request ()


lift' :: (MonadTrans t, Monad m, Monad (t m)) => (m a -> m b) -> t m a -> t m b
lift' f tma = tma >>= \ma -> lift . f . return $ ma 


lift'' :: (MonadTrans t, Monad m, Monad (t m)) => (m a -> m b -> m c) -> t m a -> m b -> t m c
lift'' g tma mb = tma >>= \ma -> lift . (flip g mb) . return $ ma


nest :: (Monad (t (CoroutineT a a' b b' m)), Monad m, MonadTrans t) =>
     t (CoroutineT a a' b b' m) () -> CoroutineT b b' b b' m u -> t (CoroutineT a a' b b' m) ()
nest tma ma = lift' (\ma' -> ma' ~> ma) tma



r1 :: Monad m => StateT a (CoroutineT a a' b b' m) a
r1 = StateT $ \s -> stop


r2 :: Monad m => CoroutineT b b' c c' m u
r2 = stop



impose :: (Monad m, Monad t) 
  => t (CoroutineT a a' b b' m v) 
  -> CoroutineT b b' c c' m u 
  -> t (CoroutineT a a' c c' m ())
impose sm m = sm >>= return . (flip (~>) m) 




-- * break down the soln and build it back up


part :: (Monad m, MonadTrans t) => CoroutineT a a' b b' m u -> t (CoroutineT a a' c c' m) ()
part = lift . (flip (~>) r2) . return 



























r4 :: State Int Int
r4 = modify (+200) >> get >>= \x -> return $ x + 100


r5 :: State Int Int
r5 = modify (+404) >> get >>= \x -> return $ x + 1000


r6  = liftM2 (,) r4 r5
r6' = runState r6 0




--rcca :: Monad m => StateT String (CoroutineT () () () () m) ()
--rcca = nest lca ca'


--rcca' = run . flip evalStateT "hello" $ rcca



--combine :: (Monad m, MonadTrans t, Monad (t m))
--  => (CoroutineT a a' b b' m u -> CoroutineT b b' c c' m u -> CoroutineT a a' c c' m ())
--  -> t (CoroutineT a a' b b' m) u
--  -> CoroutineT b b' c c' m u
--  -> t (CoroutineT a a' c c' m) u
--combine :: (MonadTrans t, Monad m, Monad (t (CoroutineT a a' b b' m)), Monad (t (CoroutineT a a' c c' m))) 
--  => (CoroutineT a a' b b' m v -> CoroutineT b b' c c' m v -> CoroutineT a a' c c' m v) 
--  -> t (CoroutineT a a' b b' m) v 
--  -> CoroutineT b b' c c' m v 
--  -> t (CoroutineT a a' c c' m) v
--combine g tma ma' = let f = flip g ma' in tma >>= lift . f . return




-- * some problems discovered

-- | hoist :: (Monad m, MFunctor t) => (forall a. m a -> n a) -> t m b -> t n b
-- * note forall a., so a cannot be a specif type? like ()? 
-- * solution: fix coroutine so the last param cannot be () -> plan to machines kind of abstraction
-- * figure out why this is the case (if it is) and how to get around it 



-- * with machines as underlying monad

rmb :: (Monad (M.MachineT m (M.Is Int)), Monad m) => StateT String (M.MachineT m (M.Is Int)) String
rmb = do 
  modify (\x -> x <> x)
  x <- get
  lift . M.construct $ do 
    M.yield x
    M.stop 
  return x


mb :: Monad m => M.ProcessT m a a
mb = M.repeatedly $ do 
 x <- M.await
 M.yield x 


rmbb :: (Monad (M.MachineT m (M.Is Int)), Monad m) => StateT String (M.MachineT m (M.Is Int)) String
rmbb = hoist (\m -> (M.~>) m mb) rmb


-- * error: No instance for (Monad (M.MachineT Identity (M.Is Int)))
-- * because `MachineT` is not a monad, `PlanT` is
rma :: Monad (M.MachineT Identity (M.Is Int)) => Identity [(String, String)]
rma = M.runT . flip runStateT "hello" $ rmbb




-- * build flat pipeline of machines using explicit `Is` in signature

m1 :: Monad m => M.MachineT m (M.Is Int) Int
m1 = M.repeatedly $ do 
  x <- M.await 
  M.yield $ x + x 

m2 :: Monad m => M.ProcessT m Int Int 
m2 = M.repeatedly $ do 
  x <- M.await 
  M.yield $ x * x 

m3 :: Monad m => M.MachineT m (M.Is Int) Int
m3 = (M.~>) (M.source [1..3]) $ (M.~>) m1 m2

m3' = M.run m3



-- * next course of action:
-- * fix coroutine so the types work with `hoist`



------------------------------------ using transformer feature ------------------------------------


stack :: PT Char (State Char) Bool
stack = do 
  c <- getC
  x <- lift get
  if [c,x] == "hi" then return True else return False


s = flip runState 'h' $ runPt stack "ii"



-- * test applicative
pa :: Parser a [a]
pa = (\a b -> a:b:[]) <$> getC <*> getC


-- * basic examples

satC :: (a -> Bool) -> Parser a a
satC g = do 
  c <- getC
  if g c then return c else fail "EOI"


getC :: Parser a a
getC = encase $ \(a:as) -> return (a,as)

r = runIdentity . runPt getC $ "hello"




-- **** The idea below is garbabe, in current iteration, but could it be refined? **** --


{-

  -- * there's two ways in which this parser output value b, via some `Escape` channel, or 
  -- * `Yield` the value through some internal logic
  data Stream a b r  
    = Escape b      -- * right now, these two are redudant            
    | Output b      -- * right now, these two are redudant 
    | Wait (a -> r)


  data ParserT a m b = Pat { un :: m ( Stream a b (ParserT a m b)) }


  runPat :: Monad m => ParserT a m b -> [a] -> m (b,[a])
  runPat p as = un p >>= \s -> case s of 
    Escape b -> return (b,as)
    Output b -> return (b,as)
    Wait k   -> case as of 
      []      -> fail "[]"
      (x:xs)  -> runPat (k x) xs


-}


{-

  newtype ParserT a m b = P { runP :: m ([a] -> [(b,[a])] ) }


  instance Monad m => Functor (ParserT a m) where
    fmap g (P m) = P $ m >>= \k -> return $ fmap (\(b,as) -> (g b, as)) . k


  instance MonadTrans (ParserT a) where
    lift = P . liftM (\v -> \as -> [(v,as)]) 



  -- * note the prescence of a monad is making this kind of difficult
  -- * how do you run a function then take it out of the monad m?
  -- * It's doable if you go "futher into the context"

  -- * (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  --instance Monad m => Applicative (ParserT a m) where
  --  pure b = encase $ \as -> [(b,as)]
  --  p1@(P m) <*> p2@(P n) = P $ m >>= \as -> runP p1 as >>= \bs -> do 
  --    (g,as') <- bs
  --    runP p2 as' >>= \bs' -> do 
  --      (a,as'') <- bs'
  --      return (g a, as'')


  --instance Applicative (Parser a) where
  --  pure a     = P $ \as -> [(a,as)]
  --  p1 <*> p2  = P $ \as -> do
  --    (g, as')  <- runP p1 as
  --    (x, as'') <- runP p2 as'
  --    return ( g x, as'' )


  f :: Monad m => ParserT a ((->) [a]) b -> [a] -> m [(b, [a])]
  f p1 = runP p1 >>= \k -> \as -> return $ k as 


  --do   -- * build anonymous function insdie P and m
  --(g, as') <- k as 
  --return (g,as')


  --(g,as')   <- ...   -- * tbd
  --(a,as'')  <- ...   -- * tbd
  --return (g a, as'')





  p :: ParserT Char Identity Char 
  p = encase $ \as -> case as of 
    []     -> []
    (x:xs) -> [(x,xs)]

  p' :: ParserT Char Identity Bool
  p' = (\a -> if a == 'a' then True else False) <$> p


  encase :: Monad m => ([a] -> [(b,[a])]) -> ParserT a m b
  encase = P . return
-}

-- * not sure what this function's role is
--execParserT :: Monad m => ParserT a m b -> [a] -> m ([([a],[a])])
--execParserT p as = unp p >>= \k -> case as of 
--  []     -> return []
--  (x:xs) -> (\bs -> (\(as,_) -> (as ++[x], xs)) <$> bs ) `liftM` execParserT p xs
--execParser p = runIdentity . execParserT p 


-------------------------- Prelim Design 1: deterministic Parser0  -------------------------------



{-

  -- * this just looks like coroutine suspension functor lite

  -- * NOTE: if it is really the case that coroutine's suspension functor is sufficent, then maybe
  -- * could build Parser0 out of that


  -- * question.. original Parser0 was stateless, why is this returning a new Parser0, is it necessary?
  data Stream0 a b r
    = Fail
    | Yield (b,r)
    | Await (a -> r)


  data ParserT0 a m b = P0 { unP :: m (Stream0 a b (ParserT0 a m b)) }

  type Parser0 a b = forall m. Monad m => Parser0T0 a m b

  instance Monad m => Functor (ParserT0 a m) where
    fmap g p = P0 $ unP p >>= \s -> case s of 
      Fail        -> return Fail
      Yield (b,r) -> return . Yield $ (g b, g <$> r)
      Await k     -> return . Await $ (g <$>) . k 


  -- * if this doesn't work exaclty like Parser0, this design doesn't work
  instance Monad m => Applicative (ParserT0 a m) where
    pure      = return 
    p1 <*> p2 = P0 $ unP p1 >>= \s -> case s of 
      Fail        -> return Fail
      Yield (g,r) -> undefined


  instance Monad m => Monad (ParserT0 a m) where
    return b = encase $ Yield (b, encase Fail)     -- * note rest of Stream0 is not yielded, this have to change
    p >>= g  = P0 $ unP p >>= \s -> case s of 
      Fail        -> return Fail
      Yield (b,r) -> unP $ g b    --- * <-- WHAT HAPPENS TO r???? Information is lost so this is making no sense
      Await k     -> return . Await $ (>>=g) . k

  -- * note how this is restraining what the Parser0 could do, in fact [b] is [a]

  -- * in this thing here there's no "hook" for some function :: [a] -> b
  runParser0T0 :: Monad m => ParserT0 a m b -> [a] -> m ([b],[a])
  runParser0T0 p as = unP p >>= \s -> case s of 
    Fail        -> return ([],as)
    Yield (b,r) -> (\(bs,_) -> (bs++[b],as)) `liftM` runParser0T0 r as
    Await k     -> case as of 
      []     -> return ([],as)
      (x:xs) -> flip runParser0T0 xs $ k x

  runParser0 :: Parser0 a b -> [a] -> ([b],[a])
  runParser0 p = runIdentity . runParser0T0 p


  -- * trivial Parser0 that lets one char through 
  p :: Parser0 Char Char
  p = encase $ Await $ \c -> encase $ Yield (c, encase Fail)


  p' = runParser0 p "hello world"   -- * ("h", "ello world")


  encase :: Monad m => Stream0 a b (ParserT0 a m b) -> ParserT0 a m b 
  encase = P0 . return



-}


































