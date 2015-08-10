{-# LANGUAGE NoMonomorphismRestriction #-} 


---------------------------------------------------------------------------------------------------
-- | Computatation constructs to run the Weighted Finite State Transducer 
---------------------------------------------------------------------------------------------------


module RunTransducer (


) where 


import Control.Applicative
import Control.Monad
import Control.Proxy
import Control.Proxy.Trans.State
import Control.Proxy.Trans.Writer

import Data.Monoid
import Data.Maybe
import Data.Tuple.Curry
import Data.List


import Test.QuickCheck
import Test.QuickCheck.All 
import Test.QuickCheck.Modifiers

import UtilsProxies
import RoseTree 
import TransducerPrimitives
import Transducer
import Utils

------------------------------------------------
------------------- Types ---------------------
------------------------------------------------


-- | Output emitted at each state and prob of arriving at this state
type Emission b    = (Output b, Prob)
-- | Output emitted at each state and prob of arriving at this state, and the transducer at this state
type EmitState a b = (Output b, Prob, WeightedTransducer a b)


-- | used by runTransducer to step through a list of inputs
-- | each Record state maps to a single present state
-- | Note this implies that there could be several tuples in the record w/ the same past state
type Record a b = [(Emission b, EmitState a b)]

-- | used by consumers downstream of runTransducer to record execution of runTransducer
-- | each Record state maps to a list of present show states
type Response b = [(Emission b, [Emission b])]



------------------------------------------------
--------------- Run Transducer -----------------
------------------------------------------------

{-

          Upstream | Downstream
            +------------+
            |            |
        ()  <==          <== ()
            |  runTrans  |
 Symbol a  ==>           ==> [(s0,[s1])]
            |            |
            +------------+

-}
runTransducer :: (Ord b) => (Monad m, Proxy p, Eq a, Eq b) => WeightedTransducer a b -> () -> Pipe (StateP (Record a b) p) (Input a) (Response b) m r
runTransducer t () = forever $ do
  input <- request ()
  past  <- get
  let present = iter t input past
  put present
  respond . toResponse $ present
    where 
      iter t0 input []   = (\pres@(b,p,t) -> ((b,p), pres)) <$> step t0 input 1.0
      iter _  input past = join [ (\pres -> ((b,p),pres)) <$> step t input p | (_,(b,p,t)) <- past ]
      toResponse = groupVals . fmap (\(past,(b,p,t)) -> (past,(b,p)) ) 


-- | simplify type  a bit
runTrans :: (Monad m, Proxy p) => WeightedTransducer a b -> () -> Pipe (StateP [EmitState a b] p) (Input a) ([Emission b]) m r
runTrans t () = forever $ do
  input <- request ()
  past  <- get
  let present = iter t input past
  put present
  respond . toResponse $ present
    where
      iter t0 input [] = step t0 input 1.0
      iter _  input ps = join [ step t input p | (_,p,t) <- ps ]
      toResponse = fmap (\(b,p,_) -> (b,p)) 


--------------------------------------------------------
--------------- Process Results of Run -----------------

-- these are generic and should be moved else where

--------------------------------------------------------
{-

  trace execution of (possibly) nondeterministic 
  function and build tree

           Upstream | Downstream
             +------------+
             |            |
       ()    <==           <== ()
             |    record  |
  [(k,[v])] ==>           ==> C
             |            |
             +------------+
-}

record :: (Proxy p, Monad m) => () -> StateP s p () s b' b1 m b
record () = forever $ do 
  xs <- request ()
  put xs



{-

  trace execution of (possibly) nondeterministic 
  function and build tree

           Upstream | Downstream
             +------------+
             |            |
       ()    <==           <== ()
             |    trace   |
  [(k,[v])] ==>           ==> C
             |            |
             +------------+
-}
--trace :: (Proxy p, Monad m) => () -> Consumer (StateP (RoseTree x) p) [x] m r
trace () = forever $ do
  xs  <- request ()
  t   <- get
  let t' = append t xs
  put t'


append :: Eq a => RoseTree a -> [(a,[a])] -> RoseTree a
append t xs  = foldr (\(k,vs) t -> fromJust $ grow k vs t) t xs


------------------------------------------
------------------ Test ------------------
------------------------------------------


-- | run a session
runStateProxy st session = runProxy $ evalStateK st $ session


-- | a session where running transducer outputs results to console
printRun inputs = fromListS inputs >-> runTransducer t0 >-> toConsole
-- | run printRun
test1 = runStateProxy [] $ printRun $ Sym <$> [A,A,C]


ls = [(1,[4..7])] :: [(Int,[Int])]


g :: Proxy p => () -> p C () () C IO ()
g = fromListS ls >-> toConsole


--h = fromListS ls >-> trace

--sesh3 inputs = fromListS inputs >-> runTransducer t0 >-> record



--sesh2 ins = fromListS ins >-> runTransducer t0 >-> record
--test2 = runSesh 0 $ sesh2 $ Sym <$> [A,A]



-- note here there are to stateK from record and from runTransducer, so it gets really unwiedly
--  REALLY SHOULD USE MACHINES FROM HERE ON OUT!!!!!
--session2 ins = fromListS ins >-> runTransducer t0 >-> record



--sesh2 inputs = fromListS inputs >-> trace
--test2 inputs = runSesh Bud $ sesh2 $ inputs : []
--test2 = runSesh Bud $ sesh2 $ Sym <$> [A]
--sesh2 t inputs = fromListS inputs >-> runTransducer t >-> trace



-- Some Samples --
table0 :: TransitionTable Alpha Alpha
table0 = (2, 4, Eps, Eps, 1) : toProbTable [(0,1,A,B,1),(1,2,A,B,1),(1,3,A,C,1),(2,4,B,B,1),(3,5,C,C,1)]
t0 :: WeightedTransducer Alpha Alpha
t0     = toTransducer [4,5] table0   
























{-

eval :: (Eq a, Eq b) => WeightedTransducer a b -> [Input a] -> RoseTree (Output b, Prob)
eval wfst = runAll (stepFST wfst) Bud
  where
    -- | this computation should escape early if bad input read
    stepFST wfst Bud  a = foldr rappend Bud $ pure <$> step wfst a 1.0
    stepFST wfst tree a = foldr (\(k,vs) t -> case grow k vs $ t of 
        Just nt -> nt 
        Nothing -> t
      ) tree $ [ (p, step t a 1.0) | p@(_,_,t) <- ends tree ]

-}
--runTransducer :: (Monad m, Proxy p) => WeightedTransducer a b -> () -> Pipe (StateP (Record a b) p) (Input a) (Response a b) m r
--runTransducer t () = forever $ do
--  input <- request ()
--  Record  <- get
--  let pres   = join [ step t input p | (b,p,t) <- Record ]
--  put pres 
--  respond pres




incr4 :: (Proxy p, Monad m) => () -> Consumer (StateP (RoseTree x) p) x m r
incr4 () = forever $ do
  x <- request ()
  t <- get
  put $ t `rappend` pure x

{-
  only the last x received is ouput as result of computation

     Upstream | Downstream
       +------------+
       |            |
  ()  <==           <== ()
       |   record   |
  x   ==>           ==> C
       |            |
       +------------+

-}
--recordResult :: (Proxy p) => () -> Consumer (StateP Int p) Int IO r
--recordResult = forever $ do 
--  xs <- request ()
--  ls <- get
--  lift$ print (xs, ls)
--  put $ xs ++ ls


--run seed = runProxy $ runStateK seed

--test3 = runProxy $ runStateK 100 $ sesh3 [1..4]





