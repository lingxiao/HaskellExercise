{-# LANGUAGE FlexibleInstances #-} {-# LANGUAGE TemplateHaskell #-}
---------------------------------------------------------------------------------------------------
-- | Probed Finite State Transducer 

--   A Probed ﬁnite-state transducer T = (A,B, Q, I, F, E, λ, ρ) over a semiring
--        K is speciﬁed by a ﬁnite _inputs alphabet A, a ﬁnite outputs alphabet B, a ﬁnite
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

--  TODOS:
--        consider finding better abstracted routines to "insert" arcs and states
--        None of the cases handle Eps transitions

-- read this: http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html
---------------------------------------------------------------------------------------------------


module WFST (

  -- * Data type -> export val constructor for now
  WeightedTransducer,

  -- * Typeclass interface
  zero,one,
  (.+.), (.*.),

  -- * Functions
  toTransducer,
  union,
  catenate,
  kleeneStar,
  (.**.)

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

import Semiring
import TransducerPrimitives
import TransducerGraph
import TransducerUtils


----------------------------------------------------------------------------
----------- Nondeterministic Weight Finite State Transducer Type -----------
----------------------------------------------------------------------------

data WeightedTransducer a b = WFST {
  _inputs  :: [Input a],
  _outputs :: [Output b],
  _initial :: [InitSt],
  _final   :: [FinalSt],
  _states  :: [Tstate],
  _trans   :: TransitionFunc a b,

  _graph   :: TransitionGraph a b
} 

$( makeLenses ''WeightedTransducer )


---------------------------------------------
-------- Typeclass Implementations ----------
---------------------------------------------

-- | Temp implementation shows graph only
instance (Show a, Show b, Eq a, Eq b) => Show (WeightedTransducer a b) where
  show t = let g = t^.graph in case g of
    [] -> "Empty"
    _  -> unlines . (fmap show) $ g


-- | Temp implementation -> is WRONG
instance (Eq a, Eq b) => Eq (WeightedTransducer a b) where
  t1 == t2 = equate (length . _initial) t1 t2 &&
             equate (length . _final)   t1 t2 &&
             equate _states             t1 t2 &&
             equate _inputs             t1 t2 &&
             equate _outputs            t1 t2 &&
             _graph t1 `graphIsomorphic` _graph t2



-- think about how this could be refactored into file that does not have access to internal rep of transducer ---

-- combinators for transduer the adt object should be here
-- combinators for transducer the math object should be in another file, w/ no acces to valeu constructor

-- | Zero: Phi or empty set -> represented by empty WFST
-- | One:  {Eps} or empty string -> represented by A single node labele zero with no transitions 
-- | (.+.) 
-- | initial state = initial t1
-- | final state   = final state t1 + final state t2
-- | transitions   = eps from initial t1 to initial t2 + trans t1 + trans t2
-- | (.*.)
-- | initial state = initial t1
-- | final state   = final state t2
-- | transitions   = eps from initial t1 to initial t2 + eps from final t1 to initial t2 + trans t1 + trans t2
instance (Eq a, Eq b, Ord a, Ord b) => Semiring (WeightedTransducer a b) where
  zero       = toTransducer [] [] []
  one        = toTransducer [[0]] [[0]] []

  t1 .+. t2 = t1 `o` t2
    where o = semiOp (t1^.final) $ t1^.initial

  t1 .*. t2
    | t1 == zero || t2 == zero = zero
    | t1 == one                = t2
    | t2 == one                = t1
    | otherwise  = t1 `o` t2
    where o  = semiOp [] $ (t1^.initial) ++ (t1^.final)




-- | Semiring Helper
semiOp :: (Eq a, Eq b, Ord a, Ord b) => [FinalSt] -> [Tstate] -> WeightedTransducer a b -> WeightedTransducer a b -> WeightedTransducer a b 
semiOp t1f ends t1 t2 = set initial (t1^.initial <||> t2^.initial) . over graph (fromJust . mergeGraph g' ) $ t2'
  where 
      g'   = (\i f -> toArc (i,f,Eps,Eps,1)) <$> ends <*> t2'^.initial
      t2'  = (over final (t1f ++ ))                     .
             over inputs (L.union $ t1^.inputs)         . 
             (over outputs (L.union $ t1^.outputs))     . 
             (over states (t1^.states ++ ))             . 
             (over graph (t1^.graph ++ ))               .
             overStates ( +(t1^.states.to length ))     $ t2


------------------------------------------
----------- Exported Functions -----------
------------------------------------------

-- | constructor
toTransducer :: (Ord a, Ord b) => [Tstate] -> [Tstate] -> TransitionTable a b -> WeightedTransducer a b
toTransducer si sf table = WFST (rmvDup as) (rmvDup bs) si sf (rmvDup $ si ++ ss ++ sf) (toTransf g) g
  where (ss,as,bs,g) = mconcat [([s1,s2],[a],[b],[toArc t]) | t@(s1,s2,a,b,p) <- table]


-- | Symbolically: L1 .+. L2 = { x | x <- L1 or x <- L2 }
union :: (Eq a, Eq b, Ord a, Ord b) => WeightedTransducer a b -> WeightedTransducer a b -> WeightedTransducer a b
union = (.+.)

 
-- | Symoblically: L1 .*. L2 = { xy | x <- L1, y <- L2 }
catenate :: (Eq a, Eq b, Ord a, Ord b) => WeightedTransducer a b -> WeightedTransducer a b -> WeightedTransducer a b
catenate = (.*.)



------- things below sould not be explicity interacting w/ bits w/i transducer, they are in differen file---------

-- | relational composition
-- | Symbolically : L1 .**. L2 = {(x,z) | (x,y) <- L1, (y2,z) <- L2, y == y2 }
-- | note non-reachable states are not trimed
infixr 7 .**.
(.**.) :: (Eq b, Ord a, Ord c) => WeightedTransducer a b -> WeightedTransducer b c -> WeightedTransducer a c
(.**.) t1 t2 = toTransducer' . relCompose (t1^.graph) $ t2^.graph



{-

tasks: merge initial -> final graph with exisitng graph
everything else remains same



-}



-- to rewrite -> cut these into orhtonogal functions and think about the kind of combinators needed 
-- to implement these

-- | Iteration
-- | Closure of L is the set of strings formed by taking any number of strings (possibly none) from L, 
-- | possibly with repetitions and concatenating all of them

-- | initial state eps trans to self
-- | terminal states eps trans to initial
kleenePlus :: (Ord a, Ord b) => WeightedTransducer a b -> WeightedTransducer a b
kleenePlus t = over graph (fromJust . mergeGraph g) t
  where 
    i = head $ t^.initial
    g = toArc (i,i,Eps,Eps,1) : ( (\f -> toArc (f,i,Eps,Eps,1)) <$> t^.final )


{-
  tasks: add new initial graph that connect with initial of original

  states incr
  initial same


-}

-- | Consider having a merge graph function
-- | does not work on zero and one

-- | K* = (K-plus) .+. One
-- | Symbolically: {"ab", "c"}* = 
-- | {ε, "ab", "c", "abab", "abc", "cab", "cc", "ababab", "ababc", "abcab", "abcc", "cabab", "cabc", "ccab", "ccc", ...}.
-- | Phi * = {Eps}
kleeneStar :: (Ord a, Ord b) => WeightedTransducer a b -> WeightedTransducer a b 
kleeneStar t = kleenePlus $ set initial (t^.initial) . over graph (fromJust . mergeGraph g') $ t'
  where 
    g' = (\i -> toArc (head $ t^.initial, i, Eps,Eps,1)) <$> t'^.initial
    t' = over states (t^.initial ++) . overStates (+1) $ t 





---------------------------------------------
----------- Non-Exported Functions ----------
---------------------------------------------


-- | HIDING BUG : Need foolproof way to find initial and final 
-- | use w/i this module only: construct new automata from transition graph
toTransducer' :: (Ord a, Ord b) => TransitionGraph a b -> WeightedTransducer a b 
toTransducer' g = WFST (rmvDup as) (rmvDup bs) si sf (rmvDup $ si ++ ss ++ sf) (toTransf g) g
  where 
    (si, sf)   = ([getInit g], getFinal g)
    (as,bs,ss) = mconcat [ ([a], [b], [s1,s2]) | (s1,s2,a,b,_) <- fromGraph g ]


-- | Map a function over all states of the transducer
overStates :: (Int -> Int) -> WeightedTransducer a b -> WeightedTransducer a b 
overStates f = over initial f' . (over final f') . (over states f') . (over graph $ overGraphStates f)
  where f' = fmap . fmap $ f


-- | insert arcs into transducer graph if possible
-- | If successful, add new symbols and states into transducer type as necessary
-- | Otherwise output Nothing
extendTransducer :: TransitionGraph a b -> WeightedTransducer a b -> Maybe (WeightedTransducer a b)
extendTransducer = undefined


------------------
-- Sanity Check --
------------------


data Alpha = A | B | C | D  deriving (Eq,Ord,Show)
data Beta  = AA | BB | CC | DD   deriving (Eq,Ord,Show)


table = toTable [(0,1,A,A,1),(1,2,B,B,1),(2,1,C,C,1)]  :: TransitionTable Alpha Alpha
t     = toTransducer  [[0]] [[2]] table                :: WeightedTransducer Alpha Alpha



te = toTransducer [[0]] [[1]] [([0],[1],Eps,Eps,1)]  :: WeightedTransducer Alpha Alpha












