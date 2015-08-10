---------------------------------------------------------------------------------------------------
-- | Finite State Automaton
-- | Tutorials: http://kar.kent.ac.uk/22057/2/Regular_Expressions_and_Automata_using_Haskell.pdf
-- | Cool Impl: https://github.com/leonidas/codeblog/blob/master/2011/2011-12-18-haskell-nfa.md
---------------------------------------------------------------------------------------------------

{-

SOURCES
main lecture: http://www.gavo.t.u-tokyo.ac.jp/~novakj/wfst-algorithms.pdf
A more detailed treatment: http://www.cs.nyu.edu/~mohri/pub/fla.pdf
simple example of composition: http://stackoverflow.com/questions/2649474/how-to-perform-fst-finite-state-transducer-composition

-}

module FSA (
  FiniteAcceptor,
  toFSA,
  compose,
  determinize,
  removeEp,
  weightPush,
  minimize
)where


import Control.Monad

import Data.Maybe
import qualified Data.Set as S


---------------
-- Data Type --
---------------


-- Finite State Accceptor --

-- | Note trans representation as a function is subject to change
data FiniteAcceptor a s = FSA {
  alphabet :: [a],
  states   :: [s],
  start    :: s,
  final    :: [s],
  trans    :: s -> a -> s
} 

type TransitionTable a s = [(s,s,a)]

----------------------
-- Type Constructor --
----------------------


toFSA :: (Ord s, Ord a) => s -> [s] -> TransitionTable a s -> FiniteAcceptor a s
toFSA si sf table = FSA (rmvDup alphabet) (rmvDup states) si sf (trans table')
  where 
    trans table s a = fromJust . lookup (s,a) $ table
    (states,alphabet,table') = foldr (\(s1,s2,a) (ss,as,ds) -> (s1:s2:ss,a:as,((s1,a),s2):ds) ) ([],[],[]) table


----------------
-- Operations --
----------------



compose = undefined
determinize = undefined
removeEp = undefined 
weightPush = undefined  
minimize = undefined



-----------
-- Tests --
-----------

data Alphabet = A | B | C | D | Ep deriving (Eq,Ord,Show)
type States   = Int


fstable = [(0,0,A),(0,1,B),(0,2,B), (1,3,Ep)]
fsa1 = toFSA 0 [3] [(0,0,A),(0,1,B),(0,2,B), (1,3,Ep)]



--g = S.fold (\(si,sf,a) (alphas,states) -> 
--  let x = S.insert si states in  
--  let y = S.insert sf states in
--  S.insert a alphas ) (S.empty,S.empty) 


-- | Finite State Transducer

-- | Weighted Finite State Accceptor

-- | Weighted Finite State Transducer

-----------
-- Utils --
-----------

-- | too lazy to think about it
rmvDup :: (Ord t) => [t] -> [t]
rmvDup = S.toList . S.fromList




































