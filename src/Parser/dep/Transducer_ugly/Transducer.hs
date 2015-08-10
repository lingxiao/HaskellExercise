{-# LANGUAGE FlexibleInstances #-} {-# LANGUAGE TemplateHaskell #-}
---------------------------------------------------------------------------------------------------
-- | Weighted Finite State Transducer 

--   A Probed ﬁnite-state transducer T = (A,B, Q, I, F, E, λ, ρ) over a semiring
--        K is speciﬁed by a ﬁnite _inputss alphabet A, a ﬁnite outputss alphabet B, a ﬁnite
--        set of states Q, a set of initial states I ⊆ Q, a set of ﬁnal states F ⊆ Q, a ﬁnite
--        set of transitions E ⊆ Q×(A∪{ε})×(B∪{ε})×K×Q, an initial state Prob
--        assignment λ : I → K, and a ﬁnal state Prob assignment

--   Sources:

--        main: http://tagh.de/tom/wp-content/uploads/fsm_unweigtedautomata.pdf
--        http://www.cs.cornell.edu/courses/   cs786/2004sp/Lectures/l02-axioms.pdf
--        http://acl.ldc.upenn.edu/P/P02/P02-1001.pdf
--        http://www.cs.nyu.edu/~mohri/pub/hwa.pdf
--        http://www.cs.nyu.edu/~mohri/pub/fla.pdf
--        http://www.cs.nyu.edu/~mohri/pub/fst.pdf
--        http://www.cis.upenn.edu/~pereira/papers/tcs.pdf
--        https://courses.engr.illinois.edu/cs373/Lectures/lec06.pdf
--        http://www.iaeng.org/publication/WCECS2010/WCECS2010_pp141-143.pdf
--        https://wiki.inf.ed.ac.uk/twiki/pub/CSTR/ListenSemester1_2010_11/mohri-wfst_asr.pdf
--        http://www.gavo.t.u-tokyo.ac.jp/~novakj/wfst-algorithms.pdf

-- on lenses: http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html

-- a turing machine in haskell: http://en.literateprograms.org/Turing_machine_simulator_(Haskell)

-- a transducer reg exp impl:   http://www.cis.uni-muenchen.de/~schmid/papers/SFST-PL.pdf
---------------------------------------------------------------------------------------------------


module Transducer (

    -- * Data type 
    WeightedTransducer

    -- * Typeclass Functions
  , zero
  , (.+.)
  , one
  , (.*.) 

    -- * Functions
  , toTransducer
  , step

  , initSt
  , finalSts

  , (.~.)    -- * extend 
  , (<~>)    -- * merge 
  , (.~>.)   -- * relational composition

) where 


import Language.Haskell.TH
import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Function
import Data.Monoid
import Data.Maybe
import Data.Tuple.Curry
import qualified Data.List as L


import Test.QuickCheck
import Test.QuickCheck.All 
import Test.QuickCheck.Modifiers

import Utils
import Semiring
import RoseTree 
import TransducerTypes
import TransducerGraph



----------------------------------------------------------------------------
----------- Nondeterministic Weight Finite State Transducer Type -----------
----------------------------------------------------------------------------

data WeightedTransducer a b = WFST {
  _initial :: InitSt,
  _final   :: [FinalSt],
  _graph   :: TransitionGraph a b,

  -- | current State of transducer -> note even though the transducer is nondeterministic
  -- | the transducer could only be in one state at a time
  _current  :: Tstate,

  -- | Used by step function only 
  nextStates :: TransitionFunc a b
}

$( makeLenses ''WeightedTransducer )


-----------------------------
--------- Constructor ------- 
-----------------------------

-- | Note by default initial state is 0
toTransducer :: (Ord a, Ord b) => [FinalSt] -> TransitionTable a b -> WeightedTransducer a b
toTransducer sf table = let g = toGraph table in WFST 0 sf g 0 (toTransf g) 

-- | distinguished transducer
emptyTransducer :: (Ord a, Ord b) => WeightedTransducer a b
emptyTransducer = WFST 0 [] [] 0 $ toTransf [] 


---------------------------------------------------
--------------- Execute Transducer ----------------
---------------------------------------------------


-- | @ Step the transducer forward on state, return a list of records of: output, prob of transition and transducer at new state
-- [(Output b, Prob, Tstate, WeightedTransducer a b)]                  

-- | Param:  Transducer at s0
-- | Param:  A symbol from Input Alphabet
-- | Param:  Probability of being at current state
-- | Output: Transducer at new state si for all i, output alphabet, and prob of arriving at si
step ::  WeightedTransducer a b -> Input a -> Prob -> [(Tstate, Output b, Prob, WeightedTransducer a b)]
step t a p = (\(s1,b,q) -> let t' = set current s1 t in (t'^.current, b, p*q, t')) <$> nextStates t (t^.current, a)


---------------------------------------------------
----------- Typeclass Implementations -------------
---------------------------------------------------

instance (Show a, Show b, Eq a, Eq b) => Show (WeightedTransducer a b) where
  show t = let g = t^.graph in case g of
    [] -> "Empty"
    --_  -> unlines . (fmap show) $ g 

    -- | use this for viewing current state
    _  -> show $ "state " ++ (show $ t^.current)


-- | Temp implementation -> is too strict:
-- | need to test of equivalence of graphs irrespective of form or index labels
instance (Eq a, Eq b) => Eq (WeightedTransducer a b) where
  t1 == t2 = equate _initial t1 t2 &&
             equate _final   t1 t2 &&
             equate _graph   t1 t2


-- | Zero:  Phi or empty transducer
-- | (.+.)
-- | initial state = initial t1
-- | final state   = final state t1, final state t2
-- | links         = eps from initial t1 to initial t2
instance (Eq a, Eq b, Ord a, Ord b) => Monoid (WeightedTransducer a b) where
  mempty = emptyTransducer
  mappend t1 t2
    | t1 == mempty = t2 
    | t2 == mempty = t1
    | otherwise    = fromJust $ (t1, arcs12, t1^.final) <~> (t2, [], t2^.final)
        where arcs12 = epsArc (t1^.initial) (t2^.initial) : []


-- | One:  {Eps} or empty string -> represented by A single node labele zero with transition to self
-- | (.*.)
-- | initial state = initial t1
-- | final state   = final state t2
-- | links         = eps from initial t1 to initial t2 + eps from final t1 to initial t2 
instance (Eq a, Eq b, Ord a, Ord b) => Semiring (WeightedTransducer a b) where
  zero  = mempty
  (.+.) = mappend
  
  one   = toTransducer [0] [(0,0,Eps,Eps,1)]
  t1 .*. t2 
    | t1 == zero || t2 == zero = zero
    | t1 == one  = t2
    | t2 == one  = t1
    | otherwise  = fromJust $ (t1, arcs12, []) <~> (t2, [], t2^.final)
        where arcs12 = epsArc (t1^.initial) (t2^.initial) : ( epsArc <$> (t1^.final) <*> [t2^.initial] )



--------------------------------------
----------- Exposed Getters ----------
--------------------------------------

-- | Export a final state function so value constructor is not exported
finalSts :: WeightedTransducer a b -> [FinalSt]
finalSts = _final

-- | see above
initSt :: WeightedTransducer a b -> InitSt
initSt = _initial


---------------------------------------------
----------- Transducer operations -----------
---------------------------------------------

infixr 7 .~.
infixr 7 <~>
infixr 7 .~>.


-- | Since there are a ton of implementation details associated with state indicies, these functions
-- | takes care of all index labeling rules under transducer combinations and extensions. 
-- | For functions describing the algebra of combining transducers, see typeclass definitions above
-- | and TransducerFunctions.hs

-- | extend transducer with new arcs and new states
(.~.) :: (Ord a, Ord b) => WeightedTransducer a b -> (TransitionTable a b , [FinalSt]) -> Maybe (WeightedTransducer a b)
t .~. ([], _) = Just t
t .~. (g, fs) = do 
  g' <- mergeGraph (toGraph g) $ t^.graph
  return $ toTransducer fs $ fromGraph g'


-- | merge two transducers together into one
-- | param 1: (transducer 1, arc from 1 to 2, final states of 1)  
-- | param 2: (transducer 2, arc from 2 to 1, final states of 2)
(<~>) :: (Ord a, Ord b) => (WeightedTransducer a b, TransitionTable a b, [FinalSt]) -> (WeightedTransducer a b, TransitionTable a b, [FinalSt]) -> Maybe (WeightedTransducer a b)
(t1,g12,fs1) <~> (t2,g21,fs2) = t1 .~. (gr, fs1 ++ fs2')
    where
      gr   = (fromGraph $ t2'^.graph) ++ g12' ++ g21'          -- new graph to be merged with t1's graph
      t2'  = incrSts n t2                                      -- increment all t2 states
      fs2' = (+n) <$> fs2                                      -- increment t2 final states
      g12' = (\(s1,s2,a,b,p) -> (s1,s2 + n,a,b,p) ) <$> g12    -- increment t2 states
      g21' = (\(s1,s2,a,b,p) -> (s1 + n,s2,a,b,p) ) <$> g21    -- increment t2 states
      n    = 1 + (maximum . _final) t1                         -- amount t2's states to be incremented by


-- | Relational composition where the output of one transducer is the input of another
-- | Doesn't really belong here but that means transducerCombinator.hs would have to import TransducerGraph, which is leaky abstraction
(.~>.) :: (Eq a, Eq b, Eq c, Ord a, Ord b, Ord c) => WeightedTransducer a b -> WeightedTransducer b c -> WeightedTransducer a c
t1 .~>. t2 = toTransducer fs g
  where 
    fs = rmvDup $ min <$> (t1^.final) <*> (t2^.final)
    g  = fromGraph . relCompose (t1^.graph) $ (t2^.graph)


----------------------------------------------
----------- Non-Exported Functions -----------
----------------------------------------------


incrSts :: Int -> WeightedTransducer a b -> WeightedTransducer a b 
incrSts n = let g = fmap (+n) in over graph ((incrS1 n) . (incrS2 n)) . (over initial (+n)) . (over final g) 

















