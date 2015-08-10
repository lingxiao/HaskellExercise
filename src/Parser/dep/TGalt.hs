{-# LANGUAGE FlexibleInstances #-} {-# LANGUAGE TemplateHaskell #-}

---------------------------------------------------------------------------------------------------
-- A Graph for Weighted Finite Transducer Only 
---------------------------------------------------------------------------------------------------


module TransducerGraph (

	-- * Arc -> note the type constructor is also not exported
	toArc,

	-- * Graph
	TransitionGraph,
	toGraph,
	fromGraph,
	incrS1,
	incrS2,
	prune,
	mergeGraph,
	relCompose,

	-- * Transition Function
	TransitionFunc,
	toTransf

) where


import Language.Haskell.TH
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Lens
import Data.Maybe
import Data.List

import Utils
import TransducerPrimitives


--------------------------------------
----------- Transducer Arc -----------
--------------------------------------

-- | NOTE: nested datatype was a mistake. flattent it
data Key a = K { _s1 :: Tstate, _input :: Input a } deriving (Eq, Ord)
data Val b = V { _s2 :: Tstate, _output :: Output b, _prob :: Prob } deriving (Eq, Ord)

-- | For storage and manipulation
data TransitionArc a b = Arc { _k :: Key a, _v :: Val b} deriving (Eq, Ord)

-- | For transducer function
type TransitionTuple a b = (Key a, Val b)


$( makeLenses ''Key )
$( makeLenses ''Val )
$( makeLenses ''TransitionArc )



-- ******* TypeClass Implementation ******* 


instance (Show a, Show b, Eq a, Eq b) => Show (TransitionArc a b) where
	show arc = show (arc^.k.s1, arc^.v.s2, showSym (_input . _k), showSym (_output . _v), arc^.v.prob)
		where showSym field = let s = (unSym . field) arc in if s == Nothing then "Eps" else show . fromJust $ s


-- ******* Exported Function ******* 

-- | Constructor
toArc :: TransitionCell a b -> TransitionArc a b
toArc (s1,s2,a,b,p) = Arc (K s1 a) (V s2 b p)


-- ******* Non-Exported Functions ******* 

-- | Deconstructor
fromArc :: TransitionArc a b -> TransitionCell a b 
fromArc (Arc (K s1 a) (V s2 b p)) = (s1,s2,a,b,p)


toTuple :: TransitionArc a b -> ((Tstate, Symbol a),(Tstate, Symbol b,Prob))
toTuple (Arc (K s1 a) (V s2 b p)) = ((s1,a),(s2,b,p))


-- | Note this function assumes b1 == b2
composeArc :: TransitionArc a b -> TransitionArc b c -> TransitionArc a c 
composeArc a1 a2 = toArc ( min (a1^.k.s1) (a2^.k.s1), min (a1^.v.s2) (a2^.v.s2), (a1^.k.input), (a2^.v.output), (a1^.v.prob) * (a2^.v.prob))

-- | find arcs that emit from same source as this arc
siblingOf :: TransitionArc a b -> TransitionArc a b -> Bool
siblingOf a b = a^.k.s1 == b^.k.s1 && a^.v.s2 /= b^.v.s2

-- | given an arc, find all arcs that transition into the first state in arc
parentOf :: TransitionArc a b -> TransitionArc a b -> Bool
parentOf = flip childOf 

-- | given an arc, find all arcs that transition from the second state in arc
childOf :: TransitionArc a b -> TransitionArc a b -> Bool
childOf ch pa = pa^.v.s2 == ch^.k.s1



---------------------------------------
----------- Transducer Graph ----------
---------------------------------------

-- | Used for storage and manipulation
type TransitionGraph a b = [TransitionArc a b]


-- **********************************
-- ******* Exported Functions ******* 
-- **********************************

-- | Constructor
toGraph :: TransitionTable a b -> TransitionGraph a b 
toGraph = fmap toArc

-- | Deconstructor
fromGraph :: TransitionGraph a b -> TransitionTable a b 
fromGraph = fmap fromArc


incrS1 :: Int ->  TransitionGraph a b -> TransitionGraph a b
incrS1 n = fmap $ over k $ over s1 (+n)

incrS2 :: Int ->  TransitionGraph a b -> TransitionGraph a b
incrS2 n = fmap $ over v $ over s2 (+n)


-- | Merge two graphs together if they form cohesive graph
-- | Else output nothing
mergeGraph :: (Ord a, Ord b) => TransitionGraph a b -> TransitionGraph a b -> Maybe (TransitionGraph a b)
mergeGraph g1 g2 = let gs = split $ g1 ++ g2 in if length gs > 1 then Nothing else Just . rmvDup . join $ gs


-- | Prune graph of any unconnected arcs
-- | The graph with initial stateas 0 is the original graph. 
-- | Note there cannot be more than one graph with initial state of 0
prune :: (Ord a, Ord b) => TransitionGraph a b -> TransitionGraph a b
prune graph = join [ g | g <- split graph, zeroIn g ]
	where zeroIn g = length [ a | a <- g, a^.k.s1 == 0 ] > 0


-- | Binary Relations over set of arcs 
relCompose :: (Eq b, Eq c, Eq a, Ord a, Ord b, Ord c) => TransitionGraph a b -> TransitionGraph b c -> TransitionGraph a c 
relCompose g1 g2 = prune [ composeArc a1 a2 | a1 <- g1, a2 <- g2, a1^.v.output == a2^.k.input ]


-- **************************************
-- ******* Non-Exported Functions *******
-- **************************************


-- | find all arcs that transition into and out of states in this arc, and from same initial state
neighborArcs :: (Ord a, Ord b) => [TransitionArc a b] -> TransitionGraph a b -> [TransitionArc a b]
neighborArcs as g = foldr ((++) <$> flip neighbors g) [] as

-- | find all arcs that transition into and out of states in this arc, and from same initial state
neighbors :: (Ord a, Ord b) => TransitionArc a b -> TransitionGraph a b -> [TransitionArc a b]
neighbors arc g = rmvDup [ a | a <- g, arc `siblingOf` a || arc `parentOf` a ||  arc `childOf` a ]



-- **********************************************************
-- ******* Nonexported function used to Split a graph *******
-- **********************************************************


type Store a b ret = StateT (TransitionGraph a b) (StateT ([TransitionGraph a b]) Identity ) ret
type SubGraphs a b = [TransitionGraph a b]


execStore :: (Eq a, Eq b, Ord a, Ord b) => Store a b ret -> SubGraphs a b -> TransitionGraph a b -> SubGraphs a b
execStore m ps p = runIdentity . execStateT ( execStateT m p ) $ ps 


split :: (Eq a, Eq b, Ord a, Ord b) => TransitionGraph a b -> SubGraphs a b
split = execStore splitGraph []


-- | split a graph into subgraphs, each connected to self but not to another
-- | case 1 -> no subgraphs, seed subgraph stack with head of graph, remove head from graph. recurse
-- | case 2 -> have subgraph, remove all nodes in graph connected to subgraph and cons to subgraph. recurse
-- | case 2a -> no node from graph is connected to existing subgraph. pop node off of graph and seed new subgraph. recurse
-- | case 2b -> find list of node from graph. cons to subgraph and remove from graph. recurse.
splitGraph :: (Eq a, Eq b, Ord a, Ord b) => Store a b ()
splitGraph = do
	g  <- get
	gs <- lift $ get
	case gs of 
		[]      -> seed g
		(x:xs)  -> let connected = neighborArcs x g in case connected of 
			[]  -> seed g 
			_   -> connected `append` gs >> modify (rmvArcs connected) >> splitGraph
	where 
		seed []     = return ()
		seed (n:ns) = (lift . modify $ (:) [n]) >> put ns >> splitGraph
		rmvArcs     = flip (\\) 
		append as (x:xs) = lift . put $ (x ++ as) : xs 



-------------------------------------------
----------- Transition Function -----------
-------------------------------------------



-- | Looks up key in TransitionGraph and return Val
type TransitionFunc a b  = (Tstate, Symbol a) -> (Tstate, Symbol b, Prob)


-- | Used for lookup by Transition function
type TransitionDict  a b = [TransitionTuple a b]


-- ******* Exported Functions ******* 

-- | Constructor
toTransf :: Eq a => TransitionGraph a b -> TransitionFunc a b
toTransf g k = fromJust . (lookup k) . (fmap toTuple) $ g




-----------
-- Tests --
-----------

-- | Symbols | --
data Alpha = A | B | C | D  deriving (Eq,Ord,Show)
data Beta  = AA | BB | CC | DD   deriving (Eq,Ord,Show)


-- | graph tests | --

ga1 =  toArc (0,1,Sym A,Sym B,0.5)
ga2 =  toArc (1,2,Sym B,Sym C,0.5)
ga3 =  toArc (2,3,Sym B,Sym C,0.5)
g   =  [ga1,ga2, ga3] :: TransitionGraph Alpha Alpha


a1 = toArc (0,3,Eps,Eps,1)
a2 = toArc (3,4,Eps,Eps,1)
a3 = toArc (2,1,Eps,Eps,1)


-- | merge graph test | --

-- | Case0 extend w/ trivial arcs
g0 = mergeGraph [] g  												:: Maybe (TransitionGraph Alpha Alpha)

-- | case1 extend w/ arc from initial
g1 = mergeGraph [a1] g  											:: Maybe (TransitionGraph Alpha Alpha)

-- | case2 extend w/ arc from final
g2 = mergeGraph [a2] g  											:: Maybe (TransitionGraph Alpha Alpha)

-- | case 3 extend w/ arc from state somewhere in middle
g3 = mergeGraph [a3] g  											:: Maybe (TransitionGraph Alpha Alpha)

-- | case 4 extend w/ many arcs, all connected to graph and each other
g4 = mergeGraph [a2, toArc(3,2,Eps,Eps,1)] g  						:: Maybe (TransitionGraph Alpha Alpha)

-- | case 5 extend w/ many arcs, all connected to graph, but not to each other
g5 = mergeGraph [a1,a3] g  											:: Maybe (TransitionGraph Alpha Alpha)

-- | case 6 extend w/ many arcs, some connected to graph, but all to each other
g6 = mergeGraph [a1, toArc(3,5,Eps,Eps,1)] g  						:: Maybe (TransitionGraph Alpha Alpha)

-- | case 7 extend w/ many arcs, some connected to graph, some to each other, but have left overs
g7 = mergeGraph [a1,toArc(3,5,Eps,Eps,1),toArc(9,10,Eps,Eps,1)] g  :: Maybe (TransitionGraph Alpha Alpha)

-- | case 8 extend w/ many arcs, none connected to graph
g8 = mergeGraph [toArc(9,10,Eps,Eps,1), toArc(10,11,Eps,Eps,1)] g  :: Maybe (TransitionGraph Alpha Alpha)




{-


----- a failed attempt at finding final states -----

-- | find all children of given parent arc
childs :: TransitionArc a b -> TransitionGraph a b -> [TransitionArc a b]
childs pa g = [ a | a <- g, a `childOf` pa ] 


-- | list all final states of graph
finalSts :: (Ord a, Ord b) => TransitionGraph a b -> [FinalSt]
finalSts g = terminal1 g ++ terminal2 g


-- | a state is a non-cyclic terminal state if it has no childOfren
terminal1 :: (Ord a, Ord b) => TransitionGraph a b -> [FinalSt]
terminal1 g = [ a^.v.s2 | a <- g, childs a g == [] ]

-- | a state is a cyclic terminal state if.. 
terminal2 :: TransitionGraph a b -> [FinalSt]
terminal2 = undefined


-}




























