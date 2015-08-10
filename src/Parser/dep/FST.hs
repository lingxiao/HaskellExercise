---------------------------------------------------------------------------------------------------
-- | Finite State Transducer
---------------------------------------------------------------------------------------------------


module FST (
  
  -- * Types
  FiniteTransducer,

  -- * Type class

  -- * Operations
  toFST,
)where


import Control.Monad

import Data.Maybe
import qualified Data.Set as S

import Semiring

---------------
-- Data Type --
---------------

-- | A list of (state,state,inputAlphabet,outputAlphabet)
type TransitionTable a b s p = [(s,s,a,b)]

-- | Modified lookup table used by the `trans` function in the FST
type TransitionDict a b s p  = [((s,a),(s,b))]


-- | Note the transition function takes in current state and transition alphabet and
-- | outputs a tuple of next state, output alphabet, and transition probability
data FiniteTransducer a b s = FST {
  inAlpha  :: [a],
  outAlpha :: [b],
  states   :: [s],
  initial  :: [s],
  final    :: [s],
  trans    :: s -> a -> (s,b)
} 


----------------------
-- Type Constructor --
----------------------

{-
toFST is just toWFST w/o the probability in trans tuple, which is spefied in dict in collect
 
toWFSA is just to toWFST w/o the output alphabet, which is specified in bs in collect

toFSA is just toWFSA w/o the prob in trans tuple, which is specifid in dict in collect

transf implementation is same for all

-}




-- | Building a wfst from a list of start states, end states, and table of transition
toFST :: (Ord s, Ord a, Ord b) => [s] -> [s] -> TransitionTable a b s -> FiniteTransducer a b s 
toFST si sf table = FST as bs ss si sf (transf dict)
  where 
    (ss,as,bs,dict)  = collect table
    transf table s a = fromJust . lookup (s,a) $ table


-- | @Function: Convert Transition table into tuple of states, input and output alphabets, and modified
-- | transition table so it maybe used by the transition function
collect :: (Ord s, Ord a, Ord b) => TransitionTable a b s -> ([s],[a],[b], TransitionDict a b s)
collect table = undefined

--------------------------
-- Typeclass Operations --
--------------------------

----------------
-- Operations --
----------------


-----------
-- Tests --
-----------

data InAlpha  = A | B | C | D deriving (Eq,Ord,Show)
data OutAlpha = AA | BB | CC | DD | Ep  deriving (Eq,Ord,Show)
type State    = Int


table :: TransitionTable InAlpha OutAlpha State 
table = [(0,1,A,BB),(1,3,C,Ep),(3,4,D,DD),(3,5,C,CC),(4,6,A,Ep),(5,6,D,BB)]

fst :: FiniteTransducer InAlpha OutAlpha State 
fst = toWFST [0] [6] table


-----------
-- Utils --
-----------

-- | too lazy to think about it
rmvDup :: (Ord t) => [t] -> [t]
rmvDup = S.toList . S.fromList




































