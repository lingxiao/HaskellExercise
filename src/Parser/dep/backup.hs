{-# LANGUAGE FlexibleInstances #-} {-# LANGUAGE TemplateHaskell #-}
---------------------------------------------------------------------------------------------------
-- | Weighted Finite State Transducer 

--   A weighted ﬁnite-state transducer T = (A,B, Q, I, F, E, λ, ρ) over a semiring
--        K is speciﬁed by a ﬁnite _inputs alphabet A, a ﬁnite outputs alphabet B, a ﬁnite
--        set of states Q, a set of initial states I ⊆ Q, a set of ﬁnal states F ⊆ Q, a ﬁnite
--        set of transitions E ⊆ Q×(A∪{ε})×(B∪{ε})×K×Q, an initial state weight
--        assignment λ : I → K, and a ﬁnal state weight assignment

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
  
  -- * Types
  WeightedTransducer,
  Symbol(..),

  -- * Type class functions
  (.+.),
  (.*.),
  zero,
  one,

  -- * Operations
  toWFST,
  mapStates,
  union,
  concatenation,
) where


import Language.Haskell.TH
import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Monoid
import Data.Maybe
import Data.Tuple.Curry
import qualified Data.List as L
import qualified Data.Set as S
import Semiring



-- play w/ lense --



data Point = P { _x :: Double, _y :: Double } deriving (Show)
$( makeLenses ''Point )


setX, setY :: Double -> Point -> Point
setX  = set x
setY  = set y

---------------------
-- Primitive Types --
---------------------

-- | Note this is exported for now
-- | Wraps _inputs/outputs alphabet and add Epsilon transition
data Symbol a = Sym a | Eps deriving (Eq, Ord, Show) 

-- | State lable is represented as list of ints
-- | upon construction, each state label should be list of one int
-- | after n compositions, each label is list of n ints
type Tstate   = [Int] 
type InitSt   = Tstate 
type FinalSt  = Tstate 

-- | The weight of edge is a double 0.0 <= w <= 1.0
type Weight  = Double


-- | (state,state,_inputsAlphabet,outputsAlphabet,Probability)
type TransitionCell a b = (Int, Int, Symbol a, Symbol b, Weight)

-- | Defined by user
type TransitionTable a b = [TransitionCell a b]


-- | ((initial state, _inputs alphabet),(desitination state, outputs alphabet, trans probs))
type TransitionArc a b = ((Tstate, Symbol a),(Tstate, Symbol b, Weight))

-- | An Internal representation used by TransitionFunction as a lookup table
type TransitionGraph a b = [TransitionArc a b]

type TransitionFunc a b = Tstate -> Symbol a -> (Tstate, Symbol b, Weight)


------------------------------
-- Primitive Type Interface --
------------------------------

-- | Transition State | --

-- | Constructor
toTstate :: [Int] -> [Tstate] 
toTstate = toLists

-- | Transition Arc | --

-- | Constructor
toArc :: TransitionCell a b -> TransitionArc a b
toArc (s1,s2,a,b,p) = (([s1],a),([s2],b,p))

-- | Accesor functions -> consider promoting TransitionArc to actualy data type
fstSt :: TransitionArc a b -> Tstate
fstSt = fst . fst 

sndSt :: TransitionArc a b -> Tstate
sndSt (_,(s2,_,_)) = s2

input :: TransitionArc a b -> Symbol a
input  = snd . fst

output :: TransitionArc a b -> Symbol b
output (_,(_,b,_)) = b

weight :: TransitionArc a b -> Weight
weight (_,(_,_,w)) = w


-- | Note this only incrs index for state if s is an int only,
-- | What if s is tuple? is this a valid case?
mapArcIdx :: (Int -> Int) -> TransitionArc a b -> TransitionArc a b 
mapArcIdx g ((s1,a),(s2,b,p)) = ((fmap g s1, a),(fmap g s2, b, p))

-- | useful?
mapArcProb :: (Weight -> Weight) -> TransitionArc a b -> TransitionArc a b
mapArcProb g (t,(s2,b,p)) = (t, (s2, b, g p))


-- | Note this function assumes b1 == b2
composeArc :: (Eq a, Eq b, Eq c) => TransitionArc a b -> TransitionArc b c -> TransitionArc a c
composeArc ((s1,a),(s2,b1,p)) ((t1,b2),(t2,c,q)) = ((s1 ++ t1, a), (s2 ++t2, c, p * q))


-- | arc1 is connected to arc2 if initial state of arc 1 is final state of arc 2
arcConnected :: TransitionArc a b -> TransitionArc a b -> Bool
arcConnected a1 a2 = sndSt a2 == fstSt a1


-- | Graph | --

-- | Constructor
toGraph :: TransitionTable a b -> TransitionGraph a b 
toGraph = foldr ((:) <$> toArc) []

-- | Check if an arc is connected to any arc in a list of arcs 
isConnected :: TransitionArc a b -> TransitionGraph a b -> Bool
isConnected a = (elem True) . fmap (arcConnected a) 
-- isConnected = any arcConnected

-- | Two dicts are isomorphic if structure, trans prob, and input:output pairs
-- | are the the same, though indices may be different
isomorphic :: (Eq a, Eq b) => TransitionGraph a b -> TransitionGraph a b -> Bool
isomorphic = (==)


-- | Transition Function | --

-- | Constructor
toTransf :: Eq a => TransitionGraph a b -> TransitionFunc a b 
toTransf dict s a = fromJust . lookup (s,a) $ dict


-----------------------------------------
-- Weight Finite State Transducer Type --
-----------------------------------------


data WeightedTransducer a b = WFST {
  _inputs :: [Symbol a],
  outputs :: [Symbol b],
  initial :: [InitSt],
  final   :: [FinalSt],
  states  :: [Tstate],
  trans   :: TransitionFunc a b,

  graph   :: TransitionGraph a b
} 


-- | Type Constructor | --

-- | Public API: construct Autoamata given initial state, final state, and transition table
toWFST :: (Ord a, Ord b) => [Int] -> [Int] -> TransitionTable a b -> WeightedTransducer a b 
toWFST si sf table = WFST (rmvDup as) (rmvDup bs) (toTstate si) (toTstate sf) (toTstate . rmvDup $ si ++ ss ++ sf) (toTransf dict) dict
  where (ss,as,bs,dict) = mconcat [([s1,s2],[a],[b],[toArc t]) | t@(s1,s2,a,b,p) <- table]
  --where (ss,as,bs,dict) = foldMap (\(s1,s2,a,b,p) -> ([s1,s2],[a],[b],[toArc t]))


-- | HIDING BUG : Need foolproof way to find initial and final 
-- | use w/i this module only: construct new automata from transition dict
toWFST' :: (Ord a, Ord b) => TransitionGraph a b -> WeightedTransducer a b 
toWFST' dict = WFST (rmvDup as) (rmvDup bs) si sf (rmvDup $ si ++ ss ++ sf) (toTransf dict) dict
  where 
    (si,sf)      = ([fstSt $ dict !! 0], [sndSt . last $ dict])
    (as, bs, ss) = foldr (\arc (ins, outs, sts ) -> (input arc : ins, output arc : outs, fstSt arc : sndSt arc : sts)) ([],[],[]) dict


-- | Type Interface | --


-- | Map g onto states of transducer, build new transducer
mapStates :: (Eq a) => (Int -> Int) -> WeightedTransducer a b -> WeightedTransducer a b
mapStates g t = WFST (_inputs t) (outputs t) start fin sts (toTransf dict) dict
  where (start, fin, sts, dict) = mapStates' g t


-- | Note this is *not* exported
-- | Map g onto states of transducer, build modified states and dict in a tuple
mapStates' :: (Int -> Int) -> WeightedTransducer a b -> ([InitSt],[FinalSt],[Tstate], TransitionGraph a b)
mapStates' g t = let mapg = (fmap . fmap) g in (mapg . initial $ t, mapg . final $ t, mapg . states $ t, fmap (mapArcIdx g) . graph $ t )

-- | NOTE: Hiding a BUG if there's more than one initial state
-- | create a new initial state label for transducer
newInit :: WeightedTransducer a b -> InitSt
newInit = join . (fmap . fmap) (const 0) . initial

-- | Note Using Maxium is wrong and is easlily defeated
-- | create a new final state label for transducer
newFinal :: WeightedTransducer a b -> FinalSt
newFinal = fmap (+1) . maximum . final

------------------------------
-- Typeclass Implementation --
------------------------------


-- | Temp implementation shows graph only
instance (Show a, Show b) => Show (WeightedTransducer a b) where
  show = unlines . (fmap show) . graph


-- | Note: consider structure a "zip" operation over all fields of WFST 
-- | use case: (==) <$> t1 <*> t2  should compare equality of all comprable fields
-- | in general: g <$> t1 <*> t2 where g is binary funciton, should do g (_inputs t1) (_inputs t2)

-- | Note as long as the _inputs and outputs alphabet and graph of two WFST are the same, then 
-- | the two WFST are equal. the indicies and need not be the same
-- | Thus the inital and final states does not need to be qual
-- | dict1 == dict2 if structure is the same, the indicies need not be same

-- | A zip interface may simplify this
instance (Eq a, Eq b) => Eq (WeightedTransducer a b) where
  -- | Temp implementation check for size of states only -> this is wrong
  t1 == t2 =  (length . initial) t1 == (length . initial) t2 && 
              (length . final)   t1 == (length . final)   t2 && 
              (length . states)  t1 == (length . states)  t2 &&
              _inputs t1 == _inputs t2 &&
              outputs t1 == outputs t2 &&
              graph t1 `isomorphic` graph t2

{-

on f g x y = g (f x) (f y)
equating = on (==)
equating (length . initial) x y
equating (length . final)
-}


-- | Zero: Phi or empty set -> represented by empty WFST
-- | One:  {Eps} or empty string -> represented by A single node labele zero with no transitions 
-- | (.+.) 
-- | initial state = initial t1
-- | final state = final state t1 + final state t2
-- | transitions = eps from initial t1 to initial t2 + trans t1 + trans t2
-- | (.*.)
-- | initial state = initial t1
-- | final state = final state t2
-- | transitions = eps from initial t1 to initial t2 + eps from final t1 to initial t2 + trans t1 + trans t2


-- | Note: how do i get rid of the explcit definition of .*. zero below?
instance (Eq a, Eq b, Ord a, Ord b) => Semiring (WeightedTransducer a b) where
  zero       = toWFST [] [] []
  one        = toWFST [0] [0] []

  t1 .+. t2  = uncurryN WFST $ t1 `o` t2
    where o  = semiOp (final t1) (initial t1)

  t1 .*. t2
    | t1 == zero || t2 == zero = zero
    | t1 == one                = t2
    | t2 == one                = t1
    | otherwise  = uncurryN WFST $ t1 `o` t2
    where o  = semiOp [] (initial t1 ++ final t1)

-- | Helper | --

-- | Note if t1 does not have initial state in the case of "zero", t2's init state is used
semiOp :: (Eq a, Eq b, Ord a, Ord b) => 
  [FinalSt] -> [Tstate] -> WeightedTransducer a b -> WeightedTransducer a b -> 
  ([Symbol a],[Symbol b],[InitSt],[FinalSt],[Tstate],TransitionFunc a b,TransitionGraph a b)
semiOp finals1 connectedSts t1 t2 = (as, bs, initSt, fins, sts, toTransf graph'', graph'')
  where
    initSt  = initial t1 <||> initial t2
    (as,bs) = (_inputs t1 `L.union` _inputs t2, outputs t1 `L.union` outputs t2)
    graph'' = ((\i t1s -> ((t1s,Eps),(i,Eps,1.0))) <$> init2 <*> connectedSts) ++ graph'
    (fins, sts, graph')         = (finals1, states t1, graph t1) `mappend` (fin2, sts2, graph2)
    (init2, fin2, sts2, graph2) = mapStates' ( + (length . states $ t1)) t2


-- | Binary Relations | --

infixr 7 .**.
-- | relational composition
-- | Symbolically : L1 .**. L2 = {(x,z) | (x,y) <- L1, (y2,z) <- L2, y == y2 }
-- | note non-reachable states are not trimed
(.**.) :: (Eq b, Ord a, Ord c) => WeightedTransducer a b -> WeightedTransducer b c -> WeightedTransducer a c
(.**.) t1 t2 = toWFST' $ [ composeArc a1 a2 | a1 <- graph t1, a2 <- graph t2, output a1 == input a2]

-- | Distinguished element one under relational composition


-----------
-- Tests --
-----------

data Alpha = A | B | C | D  deriving (Eq,Ord,Show)
data Beta  = AA | BB | CC | DD   deriving (Eq,Ord,Show)

table1 = toTable [(0,1,A,A,1),(0,2,B,B,1),(2,3,C,C,1),(3,2,B,B,1)] :: TransitionTable Alpha Alpha
t1     = toWFST [0] [1,3] table1                                   :: WeightedTransducer Alpha Alpha

table2 = toTable [(0,1,A,A,1),(1,2,B,B,1),(2,1,C,C,1)] :: TransitionTable Alpha Alpha
t2     = toWFST [0] [2] table2                         :: WeightedTransducer Alpha Alpha

table5 = toTable [(0,1,A,A,1),(1,0,A,A,1)] :: TransitionTable Alpha Alpha
t5     = toWFST [0] [1] table5             :: WeightedTransducer Alpha Alpha



-- | Check Monoid laws | --

-- | identity and commutatity of (M,.+.zero) 
-- | passed test
tazr = t2 .+. zero :: WeightedTransducer Alpha Alpha
tazl = zero .+. t2 :: WeightedTransducer Alpha Alpha

-- | identity and commutatity of (M,.*., one)
-- | Passed test since two graphs are isomorphic to t2 and each other
tmor = t2 .*. one :: WeightedTransducer Alpha Alpha
tmol = one .*. t2 :: WeightedTransducer Alpha Alpha


-- | Check Semiring laws | --

-- | zero annilate .*.
-- | passed test by definition
tzml = zero .*. t1  :: WeightedTransducer Alpha Alpha
tzmr = t1 .*. zero  :: WeightedTransducer Alpha Alpha


-- | Distributiveness -> does not pass right now
ta = t2 .*. (t5 .+. t5)           :: WeightedTransducer Alpha Alpha
tb = (t2 .*. t5) .+. (t2 .*. t5)  :: WeightedTransducer Alpha Alpha


-- | Check corresponds to graphical representation | --

-- | graphical example found at http://tagh.de/tom/wp-content/uploads/fsm_unweigtedautomata.pdf slide 27
tu = t1 .+. t2    :: WeightedTransducer Alpha Alpha
te = t1 .*. t2    :: WeightedTransducer Alpha Alpha


-- | check homorphism of .+. and one
tor = t2 .+. one  :: WeightedTransducer Alpha Alpha
tol = one .+. t2  :: WeightedTransducer Alpha Alpha 


-- | Composition | --

table3 = toTable [(0,1,A,B,0.1),(1,1,C,A,0.3),(0,2,B,A,0.2),(1,3,A,A,0.4),(2,3,B,B,0.5)]   :: TransitionTable Alpha Alpha
t3     = toWFST [0] [3] table3                                                             :: WeightedTransducer Alpha Alpha

table4 = toTable[(0,1,B,C,0.3),(1,2,A,B,0.4),(2,2,A,B,0.6)]  :: TransitionTable Alpha Alpha
t4     = toWFST [0] [2] table4                               :: WeightedTransducer Alpha Alpha

-- | graphical example found at: https://wiki.inf.ed.ac.uk/twiki/pub/CSTR/ListenSemester1_2010_11/mohri-wfst_asr.pdf page 5
tc     = t3 `compose` t4  :: WeightedTransducer Alpha Alpha
















-----------------
-- Combinators --
-----------------

-- | Choice 
-- | Symbolically: L1 .+. L2 = { x | x <- L1 or x <- L2 }
union :: (Eq a, Eq b, Ord a, Ord b) => WeightedTransducer a b -> WeightedTransducer a b -> WeightedTransducer a b
union = (.+.)

 
-- | Sequencing
-- | Symoblically: L1 .*. L2 = { xy | x <- L1, y <- L2 }
concatenation :: (Eq a, Eq b, Ord a, Ord b) => WeightedTransducer a b -> WeightedTransducer a b -> WeightedTransducer a b
concatenation = (.*.)



-- Other combinators --


-- | NOTE: THERE SHOULD BE NO EXPLICIT WFST VALUE CONSTRUCTION BELOW THIS POINT
-- | EVERYTHING BELOW NEED TO BE MOVED TO A DIFFERENT FILE

-- CODE SMELL --
-- Everything below should be built from atomic operations (what are they?)
-- looks too impertive
-- they shouldnt even be in this file and therefore should not be able to 
-- construct new WFSTs using the value constructor


-- | Iteration
-- | Closure of L is the set of strings formed by taking any number of strings (possibly none) from L, 
-- | possibly with repetitions and concatenating all of them

-- | initial state eps trans to self
-- | terminal states eps trans to initial
kleenPlus :: (Eq a, Eq b) => WeightedTransducer a b -> WeightedTransducer a b
kleenPlus t = WFST (_inputs t) (outputs t) (initial t) (final t) (states t) (toTransf dict') dict'
  where 
    ninit = newInit t
    dict' = ((ninit,Eps),(ninit,Eps,1)) : ((\s -> ((s, Eps),(ninit, Eps, 1))) <$> (final t)) ++ graph t


-- | Decompose to empty language {Eps} transition to kleenPlus FST



-- | K* = (K-plus) .+. One
-- | Symbolically: {"ab", "c"}* = 
-- | {ε, "ab", "c", "abab", "abc", "cab", "cc", "ababab", "ababc", "abcab", "abcc", "cabab", "cabc", "ccab", "ccc", ...}.

-- | Phi * = {Eps}

-- | new init state signifiy Eps or empty language
-- | original initial state eps trans to self
-- | terminal states eps trans to original initial
-- | Add new initial state that eps trans to original initial, indexed largest final st + 1
kleeneStar :: (Eq a, Eq b) => WeightedTransducer a b -> WeightedTransducer a b
kleeneStar t = WFST (_inputs t) (outputs t) [ninit] (final t) (ninit : states t) (toTransf dict') dict'
  where
    dict'      = ((ninit,Eps),(ninit,Eps,1)) : ((oinit,Eps),(oinit,Eps,1)) : final_init  ++ graph t
    final_init = (\s -> ((s, Eps),(oinit, Eps, 1)) ) <$> (final t)
    (oinit, ninit) = (join . initial $ t, newFinal t)



-- | NOTE: only works w/ WeightedTransducer b b pairs
-- | Need real way to find final state
-- | how many final state could result from composition?
-- | Assume outputs alphabet of t1 is _inputs alphabet of t2
-- | prelim implementation
--compose :: (Eq a, Eq b, Eq c) => WeightedTransducer a b -> WeightedTransducer b c -> WeightedTransducer a c
compose t1 t2 = WFST (_inputs t1) (outputs t2) [fstSt $ dict' !! 0] [sndSt $ last dict'] sts (toTransf dict') dict'
  where 
    sts   = rmvDup . foldr (\a ss -> fstSt a : sndSt a : ss ) [] $ dict'
    dict' = reverse . foldl trim [head arcs] $ tail arcs 
    arcs  = [ composeArc a1 a2 | a1@(_,(_,b1,_)) <- graph t1, a2@((_,b2),_) <- graph t2, b1 == b2]
    trim graph arc = if arc `isConnected` graph then arc : graph else graph



-----------
-- Utils --
-----------

rmvDup :: (Ord t) => [t] -> [t]
rmvDup = S.toList . S.fromList


toLists :: [a] -> [[a]]
toLists = map (:[])

infixr 7 <||>
(<||>) :: [a] -> [a] -> [a]
(<||>) l@(x:xs) _ = l
(<||>) [] l2      = l2


-- | Should these be exported? 

toCell :: (Int, Int, a, b, Double ) -> TransitionCell a b
toCell (s1,s2,a,b,p) = (s1,s2, Sym a, Sym b, p) 

toTable :: [(Int, Int, a, b, Double)] -> TransitionTable a b 
toTable = fmap toCell















