{-# LANGUAGE NoMonomorphismRestriction #-} 


---------------------------------------------------------------------------------------------------
-- | Computatation constructs to run the Weighted Finite State Transducer 


-- | Problem --> machines exports mealey which is conceptually equivalent to transducer. 

-- | so the transducer type in my package should be at same level of abtraction as type Machines?
---------------------------------------------------------------------------------------------------


module RunTransducer (

    runTrans
  , evalTrans
  , execTrans
  , logTrans

) where 



import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Applicative
import Data.Monoid
import Data.List
import Data.Machine

import TransducerTypes
import Transducer
import MachinesUtils
import RoseTree


{-

what this module need to accomplish: different ways of running the transducer

a function that runs the transducer and accepts or rejects an input sequence

a function that runs the transducer and logs all execution paths

a function taht runs the transducer and logs outcome only



Should index of state be abstracted?

there's a sea of way in which the outcome of computation is reHistoryed and it's not suable

-}



---------------------------------------
---------------- Types ----------------
---------------------------------------


-- | result of stepping the transducer one step 
type StepT  a b = (Tstate, Output b, Prob, WeightedTransducer a b)

-- | reduced result of stepping the transducer one step where the transducer itself is not shown
type Record b = (Tstate, Output b, Prob)

-- | Maps a past state to a list of current states
type History b = [(Record b, [Record b])]


--type Result b = [()]

toRecord :: StepT a b -> Record b
toRecord (s,b,w,_) = (s,b,w)



---------------------------------------
----------------- Run -----------------
---------------------------------------


-- | problem -> transducer is not really a monad and run exec and eval is confusing nomenclature
-- | problem -> actually don't need machines to implement this. it's an overkill

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
	where overlap a b = length (a `intersect` b) > 0



-- | Output all possible paths of execution in tree
-- | note traceExec does not have to be implement using machines in the current way its used
-- | since the execution is not actually traced in "real time" but rather after the fact
logTrans :: (Ord b) => WeightedTransducer a b -> [Input a] -> RoseTree (Record b)
logTrans t0 = traceExec . runTrans t0 




------------------------------------
----------- Some Samples -----------
------------------------------------

table0 = (2,4,Eps,Eps,1) : toProbTable [(0,1,A,BB,1),(1,2,A,BB,1),(1,3,A,CC,1),(2,4,B,BB,1),(3,5,C,CC,1)] :: TransitionTable Alpha Beta
t0     = toTransducer [4,5] table0   :: WeightedTransducer Alpha Beta
ins    = Sym <$> [A,A,B]             :: [Symbol Alpha]


