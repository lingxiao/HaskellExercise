---------------------------------------------------------------------------------------------------
-- | Weighted Finite State Transducer Combinators
---------------------------------------------------------------------------------------------------



-- * NOTE: Consider the kind of combinators that should be here
-- * NOTE: there are too many ways to run the transducer and it's confusing


module TransducerFunctions (

  -- * Operations
    union
  , catenate
  , kleenePlus
  , kleeneStar
  , (.~>.)     

  -- * Run Transducer
  , runTrans
  , evalTrans
  , execTrans
  , logTrans


) where

import Control.Monad
import Control.Monad.State
import Control.Applicative

import Data.Machine
import Data.Monoid
import Data.Maybe
import qualified Data.List as L 

import Utils
import Transducer
import TransducerTypes
import MachinesUtils
import RoseTree


--------------------------------------------
---------------- Operations ----------------
--------------------------------------------


-- | Symbolically: L1 .+. L2 = { x | x <- L1 or x <- L2 }
union :: (Eq a, Eq b, Ord a, Ord b) => WeightedTransducer a b -> WeightedTransducer a b -> WeightedTransducer a b
union = (.+.)

 
-- | Symoblically: L1 .*. L2 = { xy | x <- L1, y <- L2 }
catenate :: (Eq a, Eq b, Ord a, Ord b) => WeightedTransducer a b -> WeightedTransducer a b -> WeightedTransducer a b
catenate = (.*.)


-- | Iteration
-- | Closure of L is the set of strings formed by taking any number of strings (possibly none) from L, 
-- | possibly with repetitions and concatenating all of them
-- | initial state eps trans to self
-- | terminal states eps trans to initial
kleenePlus :: (Ord a, Ord b) => WeightedTransducer a b -> WeightedTransducer a b
kleenePlus t
  | t == zero = one 
  | otherwise = fromJust $ t .~. (arcs, finalSts t)
  where arcs = let i = initSt t in epsArc i i : ( epsArc <$> (finalSts t) <*> [i] ) 


-- | K* = (K-plus) .+. One
-- | Symbolically: {"ab", "c"}* = {ε, "ab", "c", "abab", "abc", "cab", "cc", "ababab", "ababc", "abcab", "abcc", "cabab", "cabc", "ccab", "ccc", ...}.
-- | Phi * = {Eps}
kleeneStar :: (Ord a, Ord b) => WeightedTransducer a b -> WeightedTransducer a b 
kleeneStar t
  | t == zero = one
  | otherwise = one .+. kleenePlus t


-- | signature is wrong, need to send to deterministic weighted transducer
determinization :: (Ord a, Ord b) => WeightedTransducer a b -> WeightedTransducer a b
determinization = undefined

-- | minizmization with weights based on paper: https://wiki.inf.ed.ac.uk/twiki/pub/CSTR/ListenSemester1_2010_11/mohri-wfst_asr.pdf
minimization :: (Ord a, Ord b) => WeightedTransducer a b -> WeightedTransducer a b
minimization = undefined


---------------------------------------
----------------- Run -----------------
---------------------------------------


----- Types -----


-- | result of stepping the transducer one step 
type StepT  a b = (Tstate, Output b, Prob, WeightedTransducer a b)

-- | reduced result of stepping the transducer one step where the transducer itself is not shown
type Record b = (Tstate, Output b, Prob)

-- | Maps a past state to a list of current states
type History b = [(Record b, [Record b])]


toRecord :: StepT a b -> Record b
toRecord (s,b,w,_) = (s,b,w)


----- Run ------


-- | problem -> the full powers of machines is not being utilized here

-- | problem -> transducer is not really a monad and run exec and eval is confusing nomenclature

stepInputs :: ProcessT (State [StepT a b]) (Input a) (History b)
stepInputs = repeatedly $ do 
  a   <- await
  p0  <- get
  let p1  = [ (p, step t a w) | p@(_,_,w,t) <- p0 ]           -- [ (past, [History] ) ]
  put . join $ snd <$> p1                                     -- [ History ]
  yield [ (toRecord p, toRecord <$> ps) | (p,ps) <- p1]       -- [ (past', [History'])]
     

-- | outputs all possible paths of execution in list
runTrans :: WeightedTransducer a b -> [Input a] -> [History b]
runTrans t0 ins = flip evalState [(0,Eps,1.0,t0)] . runT $ source ins ~> stepInputs


-- | output all terminal states
evalTrans :: Ord b => WeightedTransducer a b -> [Input a] -> [Record b]
evalTrans t0 = ends . logTrans t0


-- | output boolean value denoting whether a string of input is accepted by the transducer 
execTrans :: Ord b => WeightedTransducer a b -> [Input a] -> Bool
execTrans t0 ins = [ f | (f,_,_) <- evalTrans t0 ins ] `overlap` finalSts t0
  where overlap a b = length (a `L.intersect` b) > 0


-- | Output all possible paths of execution in tree
-- | note traceExec does not have to be implement using machines in the current way its used
-- | since the execution is not actually traced in "real time" but rather after the fact
logTrans :: Ord b => WeightedTransducer a b -> [Input a] -> RoseTree (Record b)
logTrans t0 = traceExec . runTrans t0 



------------------------------------
----------- Some Samples -----------
------------------------------------

table0 = (2,4,Eps,Eps,1) : toProbTable [(0,1,A,BB,1),(1,2,A,BB,1),(1,3,A,CC,1),(2,4,B,BB,1),(3,5,C,CC,1)] :: TransitionTable Alpha Beta
t0     = toTransducer [4,5] table0   :: WeightedTransducer Alpha Beta
ins    = Sym <$> [A,A,B]             :: [Symbol Alpha]














