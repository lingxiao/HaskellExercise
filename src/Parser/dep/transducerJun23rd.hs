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
--        http://www.cs.cornell.edu/courses/cs786/2004sp/Lectures/l02-axioms.pdf
--        http://acl.ldc.upenn.edu/P/P02/P02-1001.pdf
--        http://www.cs.nyu.edu/~mohri/pub/hwa.pdf
--        http://www.cs.nyu.edu/~mohri/pub/fla.pdf
--        http://www.cs.nyu.edu/~mohri/pub/fst.pdf
--        http://www.cis.upenn.edu/~pereira/papers/tcs.pdf
--        https://courses.engr.illinois.edu/cs373/Lectures/lec06.pdf
--        http://www.iaeng.org/publication/WCECS2010/WCECS2010_pp141-143.pdf
--        https://wiki.inf.ed.ac.uk/twiki/pub/CSTR/ListenSemester1_2010_11/mohri-wfst_asr.pdf
--        http://www.gavo.t.u-tokyo.ac.jp/~novakj/wfst-algorithms.pdf

-- read this: http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html
---------------------------------------------------------------------------------------------------


module Transducer (

  -- * Data type 
  WeightedTransducer,

  -- * Typeclass Implementations
  mempty, mappend,
  zero, (.+.),
  one, (.*.),  

  -- * Functions
  toTransducer,
  exec,
  eval,

  initSt,
  finalSts,

  extend,
  merge,
  relationalComposition


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

import Semiring
import TransducerPrimitives
import TransducerGraph
import RoseTree 
import Utils

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


-----------------------------------------------
--------------- Run Transducer ----------------
-----------------------------------------------

type History a b = (Output b, Prob, WeightedTransducer a b)

-- | step the transducer forward on state, record action
step :: 
  WeightedTransducer a b ->      -- Transducer at s0
  Input a ->                     -- A symbol from Input Alphabet
  Prob    ->                     -- Probability of being at current state
  [History a b]                  -- Transducer at new state si for all i, output alphabet, and prob of arriving at si
step t a p = (\(s1,b,q) -> (b, p*q, set current s1 t)) <$> nextStates t (t^.current, a)


-- | Run and forget history
-- | The final probability is multiplied for all steps
exec :: WeightedTransducer a b -> [Input a] -> [(Output b, Prob)]
exec wfst (a:as) = runAll stepFST (step wfst a 1.0) as
  where stepFST past a = join [ step t a p | (_,p,t) <- past ]


-- | Run and remember history 
-- | note the transition probabilites does not accumlate down the tree
-- | Thus to calculate the prob of a particular leaf, traverse the tree down to leaf and multiply all probs

-- | consider alternate data structure since self-rolled rose tree is a hack
eval :: (Eq a, Eq b) => WeightedTransducer a b -> [Input a] -> RoseTree (Output b, Prob)
eval wfst = runAll (stepFST wfst) Bud
  where
    stepFST wfst Bud  a = foldr rappend Bud $ pure <$> step wfst a 1.0
    stepFST wfst tree a = foldr (\(k,vs) t -> case grow k vs $ t of 
        Just nt -> nt 
        Nothing -> t
      ) tree $ [ (p, step t a 1.0) | p@(_,_,t) <- ends tree ]


-- | Generic run function for arbitrary functor that store result of step function
runAll :: Functor f => (f (History a b) -> Input a -> f (History a b)) -> f (History a b) -> [Input a] -> f (Output b, Prob)
runAll stepFST start inputs = (\(b,p,_) -> (b,p)) <$> foldl stepFST start inputs 

---------------------------------------------
-------- Typeclass Implementations ----------
---------------------------------------------

-- | Temp implementation shows graph only
instance (Show a, Show b, Eq a, Eq b) => Show (WeightedTransducer a b) where
  show t = let g = t^.graph in case g of
    [] -> "Empty"
    _  -> unlines . (fmap show) $ g 
    -- | use this for viewing current
    --_  -> show $ "state " ++ (show $ t^.current)


-- | Temp implementation -> is WRONG
instance (Eq a, Eq b) => Eq (WeightedTransducer a b) where
  t1 == t2 = equate _initial t1 t2 &&
             equate _final   t1 t2 &&
             equate _graph   t1 t2


-- | mempty: Phi or empty set -> represented by empty transducer
-- | mappend
-- | initial state = initial t1
-- | final state   = final state t1 + final state t2
-- | links         = eps from initial t1 to initial t2
instance (Eq a, Eq b, Ord a, Ord b) => Monoid (WeightedTransducer a b) where
  mempty = WFST 0 [] [] 0 $ toTransf [] 
  mappend t1 t2
    | t1 == mempty = t2
    | t2 == mempty = t1
    | otherwise    = mergeT t1 t2 arcs12 [] (finalSts t1) (finalSts t2)
        where arcs12 = epsArc (initSt t1) (initSt t2) : []


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
    | otherwise  = mergeT t1 t2 arcs12 [] [] $ finalSts t2
        where arcs12 = epsArc (initSt t1) (initSt t2) : ( epsArc <$> (finalSts t1) <*> [initSt t2] )


-- | Helper
mergeT :: (Ord a, Ord b) => WeightedTransducer a b -> WeightedTransducer a b -> TransitionTable a b -> TransitionTable a b -> [FinalSt] -> [FinalSt] -> WeightedTransducer a b
mergeT t1 t2 g12 g21 f1 = fromJust . merge t1 t2 g12 g21 f1


------------------------------
----------- Getters ----------
------------------------------

-- | Export a final state function so value constructor is not exported
finalSts :: WeightedTransducer a b -> [FinalSt]
finalSts = _final

-- | see above
initSt :: WeightedTransducer a b -> InitSt
initSt = _initial


---------------------------------------------
----------- Transducer operations -----------
---------------------------------------------

-- | extend w/ new internal nodes and/or arcs
-- | extend w/ new final nodes and specify all final nodes of graph2
-- | extend w/ new initial node iff transducers smallest state is already larger than 0
extend :: (Ord a, Ord b) => WeightedTransducer a b -> TransitionTable a b -> [FinalSt] -> Maybe (WeightedTransducer a b)
extend t [] fs = Just t
extend t g fs  = do
  g' <- mergeGraph (toGraph g) $ t^.graph
  return $ toTransducer fs $ fromGraph g'


-- | Problem: Sea of params
merge :: (Ord a, Ord b) => 
  WeightedTransducer a b ->     -- Transducer 1
  WeightedTransducer a b ->     -- Transducer 2
  TransitionTable a b ->        -- arcs from transducer 1 to transducer 2
  TransitionTable a b ->        -- arcs from transducer 2 to transducer 1
  [FinalSt] ->                  -- final states of new transducer from transducer 1, if any
  [FinalSt] ->                  -- final states of new transducer from transducer 2, if any
  Maybe (WeightedTransducer a b)
merge t1 t2 g12 g21 fs1 fs2 = do
  g' <- mergeGraph (t1^.graph) (t2'^.graph ++ g12' ++ g21')
  return $ toTransducer (fs1 ++ fs2') $ fromGraph g'

  extend t1 ng $ fs1 ++ fs2'
  where
    -- merge t1 graph with t2 graph and new arcs
    ng   = mergeGraph (t1^.graph) (t2'^.graph ++ g12' ++ g21')
    t2'  = incrSts n t2                                      -- increment all t2 states
    fs2' = (+n) <$> fs2                                      -- increment t2 final states
    g12' = (\(s1,s2,a,b,p) -> (s1,s2 + n,a,b,p) ) <$> g12    -- increment t2 states
    g21' = (\(s1,s2,a,b,p) -> (s1 + n,s2,a,b,p) ) <$> g21    -- increment t2 states
    n    = 1 + (maximum . _final) t1                         -- amount t2's states to be incremented by





-- | Check to see if final states are always found in this manner -> seems to be hiding a bug
-- | Doesn't really belong here but that means transducerCombinator.hs would have to import TransducerGraph -> leaky abstraction
relationalComposition :: (Eq b, Eq c, Eq a, Ord a, Ord b, Ord c) => WeightedTransducer a b -> WeightedTransducer b c -> WeightedTransducer a c
relationalComposition t1 t2 = toTransducer fs g
  where 
    fs = rmvDup $ min <$> (t1^.final) <*> (t2^.final)
    g  = fromGraph . relCompose (t1^.graph) $ (t2^.graph)


----------------------------------------------
----------- Non-Exported Functions -----------
----------------------------------------------


incrSts :: Int -> WeightedTransducer a b -> WeightedTransducer a b 
incrSts n = let g = fmap (+n) in over graph ((incrS1 n) . (incrS2 n)) . (over initial (+n)) . (over final g) 






