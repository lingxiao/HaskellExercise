{-# LANGUAGE FlexibleInstances #-} {-# LANGUAGE TemplateHaskell #-}

---------------------------------------------------------------------------------------------------
-- A Graph for Weighted Finite Transducer Only 
-- Note -> consider swapping this out w/ a construction from Data.Graph package
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
import TransducerTypes

import Test.QuickCheck
import Test.QuickCheck.All 
import Test.QuickCheck.Modifiers


--------------------------------------
----------- Transducer Arc -----------
--------------------------------------

-- | For storage and manipulation
data TransitionArc a b = Arc { _s1 :: Tstate, _s2 :: Tstate, _inputs :: Input a, _outputs :: Output b, _prob :: Prob }
	deriving (Eq, Ord)

$( makeLenses ''TransitionArc )


-- ******* TypeClass Implementation ******* 


instance (Show a, Show b, Eq a, Eq b) => Show (TransitionArc a b) where
	show (Arc s1 s2 a b p) = show (s1,s2, a, b, p)


-- ******* Exported Function ******* 

-- | Constructor
toArc :: TransitionCell a b -> TransitionArc a b
toArc (s1,s2,a,b,p) = Arc s1 s2 a b p


-- ******* Non-Exported Functions ******* 

-- | Deconstructor
fromArc :: TransitionArc a b -> TransitionCell a b 
fromArc (Arc s1 s2 a b p) = (s1,s2,a,b,p)


-- | Note this function assumes b1 == b2
composeArc :: TransitionArc a b -> TransitionArc b c -> TransitionArc a c 
composeArc (Arc s1 s2 a b p) (Arc t1 t2 b1 c q) = toArc (min s1 t1, min s2 t2, a, c, p*q)


-- | find arcs that emit from same source as this arc
siblingOf :: TransitionArc a b -> TransitionArc a b -> Bool
siblingOf a b = a^.s1 == b^.s1 && a^.s2 /= b^.s2

-- | given an arc, find all arcs that transition into the first state in arc
parentOf :: TransitionArc a b -> TransitionArc a b -> Bool
parentOf = flip childOf 

-- | given an arc, find all arcs that transition from the second state in arc
childOf :: TransitionArc a b -> TransitionArc a b -> Bool
childOf ch pa = pa^.s2 == ch^.s1



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
incrS1 n = fmap $ over s1 (+n)

incrS2 :: Int ->  TransitionGraph a b -> TransitionGraph a b
incrS2 n = fmap $ over s2 (+n)


-- | Merge two graphs together if they form cohesive graph
-- | Else outputs nothing
mergeGraph :: (Ord a, Ord b) => TransitionGraph a b -> TransitionGraph a b -> Maybe (TransitionGraph a b)
mergeGraph g1 g2 = let gs = split $ g1 ++ g2 in if length gs > 1 then Nothing else Just . rmvDup . join $ gs


-- | Prune graph of any unconnected arcs
-- | The graph with initial stateas 0 is the original graph. 
-- | Note there cannot be more than one graph with initial state of 0
prune :: (Ord a, Ord b) => TransitionGraph a b -> TransitionGraph a b
prune graph = join [ g | g <- split graph, zeroIn g ]
	where zeroIn g = length [ a | a <- g, a^.s1 == 0 ] > 0


-- | Binary Relations over set of arcs 
relCompose :: (Eq b, Eq c, Eq a, Ord a, Ord b, Ord c) => TransitionGraph a b -> TransitionGraph b c -> TransitionGraph a c 
relCompose g1 g2 = prune [ composeArc a1 a2 | a1 <- g1, a2 <- g2, a1^.outputs == a2^.inputs ]


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


type SubGraphs a b = [TransitionGraph a b]
type Store a b ret = StateT (TransitionGraph a b) (StateT ([TransitionGraph a b]) Identity ) ret


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
type TransitionFunc a b  = (Tstate, Input a) -> [(Tstate, Output b, Prob)]

-- | For transducer lookup function
-- | note each key could map to a list of outcomes since transducer is non-deterministic
type TransitionTuple a b = ((Tstate, Input a), [(Tstate, Output b, Prob)])


-- | Used for lookup by Transition function
type TransitionDict  a b = [TransitionTuple a b]



-- ******* Exported Functions ******* 

-- | Constructor -> consider using library functions instead of self-rolled hack
toTransf :: (Eq a, Eq b) => TransitionGraph a b -> TransitionFunc a b    
toTransf g k = fromMaybe [] $ lookup k . collapse $ g 



-- ******* Non-Exported Functions ******* 

collapse :: (Eq b, Eq a) => TransitionGraph a b -> TransitionDict a b 
collapse graph = foldr (\t@(k,v) dict -> 
	let jv = lookup k dict in if jv == Nothing then t : dict else insertV k v dict
	) [] $ fmap toTuple graph
		where insertV k v = fmap (\t@(kd, vd) -> if kd == k then (kd,vd ++ v) else t) 


toTuple :: TransitionArc a b -> TransitionTuple a b
toTuple (Arc s1 s2 a b p) = ((s1,a),[(s2,b,p)])

 



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




























