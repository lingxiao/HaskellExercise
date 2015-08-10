{- 

Created: March 10th 2013 
Based on lecture: https://alliance.seas.upenn.edu/~cis520/wiki/index.php?n=Lectures.DecisionTrees
adapted from https://github.com/chris-taylor/aima-haskell/blob/master/src/AI/Probability/Bayes.hs
good explanation of elimination algorithm: http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-825-techniques-in-artificial-intelligence-sma-5504-fall-2002/lecture-notes/Lecture16FinalPart1.pdf
also kind of helpful: http://www.cs.uiuc.edu/class/sp08/cs440/notes/varElimLec.pdf


Some soruces on how to structure haskell code using monads:
http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html


-}



module ML.Learning.BayesNet where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Control.Monad as Mo
import qualified Data.Map as M
import qualified Data.List as L

import Numeric.LinearAlgebra as NL
import Foreign.Storable

import Data.Map ( Map, (!) )
import Data.Traversable
import Data.Maybe


---------------
-- Bayes Net --
---------------

type Prob  = Double
type PaIdx = Int
type NumPa = Int

data Node e = Node { pa :: [e], ch :: [e], probs :: [ Vector Prob ] } deriving ( Show, Eq )
type NodeMap e = Map e ( Node e )

data BayesNet e = BayesNet { ns :: [e], nmap :: NodeMap e } deriving ( Show )



---------------------
-- Bayes Net Utils --
---------------------


-- | Creates bayes net from list of triples of form ( var, parents, conditional probs )
--   Right now this fn expects list to triples to be toplogically sorted so that all nodes
--   after ni are descendants of ni ( note they need not be children of ni )
--   The probs should be odered such that the first parent varies most slowly: P(C|A,B)

--   Given C = T
--   A | B | Prob
--   --+---+-----
--   T | T | 0.3
--   T | F | 0.8
--   F | T | 0.7
--   F | F | 0.1

bfromList :: Ord e => [ (e, [e], [e], [Prob]) ] -> BayesNet e
bfromList ns = BayesNet ( fst parts ) ( M.fromList $ snd parts )
	where 
		parts  = foldr build ([], []) $ toplogicalSort ns
		build (e,es,cs,ps) (ns,ms) = let n = Node es cs ( vectorize ps ) in ( e:ns, (e,n):ms)
		vectorize ps = fmap ( NL.fromList ) $ fmap (\p -> [ p, 1-p ]) ps



-- | Toplogically sort list so that all nodes after ni are descendants of ni
-- | stub for now
toplogicalSort :: [(e, [e], [e], [Prob])] -> [(e, [e], [e], [Prob])]
toplogicalSort = id



------------------
-- MonadT Stack --
------------------

type NHeap e ret = MaybeT ( StateT (NodeMap e) Identity ) ret

runheap :: NHeap e ret -> NodeMap e -> ( Maybe ret, NodeMap e )
runheap m nm = runIdentity $ runStateT ( runMaybeT m ) nm


-- | a monad morphism that allow error to propagate through the 
-- | NHeap monadT layers w/o explicit case checking 
hoistMaybe :: (Monad m) => Maybe a -> MaybeT m a
hoistMaybe ma = MaybeT (return ma)


--------------------------
-- Variable Elimination --
--------------------------


{-
	eliminationAsk: get nmap from bayes net and call inner fn `factorize` to
			        factorize return vector of probs, find appropriate value and output value

	factorize: given: toplogically sorted stack of elems, map of elem to nodes

		1. pop top elem from stack and:
			parent node 			 <- find and remove from nodeMap
			list of children keys    <- find from parent node
			list of children nodes   <- find and remove from nodeMap
			updated children probs   <- marginalize ( parent node ) ( children node )
			updated children nodes   <- makeNodes ( updated children probs )
			nodeMap 				 <- insert updated children nodes
			return nodeMap

		2. recurse step 1

		terminate: when stack empty, output marginalized reduced nodemap, one of node in map is elem queried

-}

eliminationAsk :: Ord e => BayesNet e -> (e, Bool) -> Maybe Prob
eliminationAsk bn (e,v) = mheap >>= \heap -> Just $ let pv = head . probs $ heap ! e in if v == True then pv @> 0 else pv @> 1 
	where
		mheap = mes >>= \es -> Just $ elimAsk es $ nmap bn
		mes   = let es = ns bn in let mi = L.elemIndex e es in mi >>= \i -> Just . fst $ splitAt i es 
		elimAsk []     heap = heap
		elimAsk (e:es) heap = let heap' = snd $ runheap ( elimNodeM e ) heap in elimAsk es heap'


elimNodeM :: Ord e => e -> NHeap e ()
elimNodeM e = do
	nodes <- lift $ get
	pa    <- hoistMaybe $ M.lookup e nodes
	modify ( M.delete e )
	let chkey = ch pa
	let chs   = fmap (\( k, n ) -> ( k, head $ eliminatePa e pa [n] ) ) $ getCh nodes chkey
	lift $ modify ( M.union $ M.fromList chs )
	return ()


getCh :: Ord e => NodeMap e -> [e] -> [(e, Node e)]
getCh nmap es = foldr (\c ns -> let n = M.lookup c nmap in case n of 
		Nothing -> ns
		Just _  -> ( c, fromJust n ) : ns
	) [] es


-- pure functions called by algorithm --

-- | Given the parent node name, the parent node and its children nodes
-- | marginalize its effect from children, output children nodes with marginalized probs
-- | Case: b -> P(B) -> [ P(A|B), P(C|B,D), P(E|B)P(F|B) ]        -> [P(A), P(C|D), P(E),P(F)]
-- | Case: b -> P(B) -> [] 				  						  -> []
eliminatePa :: Eq e => e -> Node e -> [ Node e ] -> [ Node e ]
eliminatePa e n ns = nnodes
	where
		nnodes =  zipWith (\p n -> Node ( fromJust $ filterPa e $ pa n ) ( ch n ) p ) ( fromJust . Mo.sequence $ nprobs ) ns
		nprobs =  let vpa = head . probs $ n in fmap (\( bs, idx, vs ) -> margProbs bs idx vpa vs ) params
		params =  fmap (\n -> let pas = pa n in ( combo . length $ pas, fromJust $ L.elemIndex e pas, probs n ) ) ns
		
		combo len = Mo.replicateM len [ True, False ]
		filterPa p ps = let midx = L.elemIndex p ps in midx >>= \idx -> let mps' = pluckL idx ps in mps' >>= \p -> return $ snd p


-- | Given a list of list of bools, parent index of boolean value w/i inner list of bool,
-- | parent prob vector and child prob vectors, return child probs vector list marginlized of parent prob
-- | Note the parent's prob vector must not be conditioned on its parent, so it must be a mx1 vector,
-- | where m is number of values the parent could 
margProbs :: [[Bool]] -> PaIdx -> Vector Double -> [ Vector Double ] -> Maybe [ Vector Double ]
margProbs bs idx vpa vs = Mo.sequence $ fmap (\( _, ms ) -> collapseWith addV ms ) grouped
	where
		(p, q)   = ( vpa @> 0, vpa @> 1 )
		grouped  = reverse . sortAndGroup $ zip ( snd probBool ) scaled
		scaled   = zipWith (\p v -> scaleV (*) p v ) ( fst probBool ) vs
		probBool = foldr   (\b (probs, bools) -> let (prob,bool) = reducePa b in (prob:probs, bool:bools) ) ([],[]) bs
		reducePa bs = let split = fromJust $ pluckL idx bs in ( if fst split == True then p else q, snd split )



----------------
-- List Utils --
----------------

-- | Pluck elements out of list at index, return element and rest of list in tuple
pluckL :: Int -> [a] -> Maybe ( a, [a] )
pluckL idx xs = case splitAt idx xs of 
	( _, [] )      -> Nothing
	( hs, (t:ts) ) -> Just ( t, hs ++ ts )
	

sortAndGroup :: Ord k => [ (k, a) ] -> [ (k, [a]) ]
sortAndGroup ts = M.toList $ M.fromListWith (++) [(k, [v]) | (k, v) <- ts]

------------------
-- Vector Utils --
------------------

-- | element wise addition
addV :: ( Storable a, Num a ) => Vector a -> Vector a -> Maybe ( Vector a )
addV v1 v2 = if dim v1 == dim v2 then Just $ zipVectorWith (+) v1 v2 else Nothing


-- | scale vector by constant
scaleV :: ( Storable a, Num a ) => ( a -> a -> a ) -> a -> Vector a -> Vector a 
scaleV g num v = mapVector (\e -> g num e ) v


-- | fold over list of vectors apply function g on each pairs of vector 
-- | correspond to sending a m x n matrix to a m x 1 vector
collapseWith :: ( Storable a, Num a ) => ( Vector a -> Vector a -> Maybe ( Vector a ) ) -> [Vector a] -> Maybe ( Vector a )
collapseWith g vs = foldr (\v1 v2 -> v2 >>= \w -> g v1 w ) ( return $ head vs ) ( tail vs )

----------------
-- Misc Utils --
----------------


-- | Given length of list and element within list, enumerate all possible combinations of True and False
combo :: Int -> Int -> [ Bool ]
combo n i = let combos = Mo.replicateM n [True,False] in fmap (\p -> p !! i ) combos



-----------
-- Tests --
-----------

nodes = nmap mit
nv = nodes ! 'v'
nt = nodes !'t'

end = factorize "vxstlba" ( Just $ nmap mit )


bs = [[True,True],[True,False],[False,True],[False,False]]
v = NL.fromList [0.4,0.6] :: Vector Double
m = map (NL.fromList )[[0.95,0.05], [0.94,0.06], [0.29,0.71], [0.1, 0.9]] :: [ Vector Double ]

bsmall = [True,False]
msmall = map (fromList )[[0.3,0.7],[0.5,0.5]] :: [ Vector Double ]

-- | helper
getProb net e = probs $ fromJust $ M.lookup e $ nmap net


-- | a linear causal graph

causal = bfromList [ 
	('a', [], "bz", [0.4, 0.3]), 
	('b', ['a'], "c", [0.3, 0.2]), 
	('c', ['b'], "d", [0.8, 0.7]), 
	('d', ['c'], "", [0.4, 0.9])]

cm = nmap causal

getProbC = getProb causal
pna = getProbC 'a'
pnb = getProbC 'b'
pnc = getProbC 'c'


-- | a more complicated graph 

mit = bfromList[
	('v', "",  "t",  [0.2]), 
	('x', "a", "",   [0.2,0.9] ),
	('s', "",  "bl", [0.4]),
	('t', "v", "",   [0.3,0.6] ), 
	('l', "s", "a",  [0.7, 0.2] ),
	('b', "s", "d",  [0.3, 0.6] ),
	('a', "tl","dx", [0.3,0.4,0.2,0.65] ),
	('d', "ab", "",  [0.9, 0.3, 0.8, 0.4]) ]

{-

mit = BayesNet {
	ns = "vtaxlsbd", 
	nmap = fromList [
		('a',Node {pa = "tl", ch = "xd", 
			probs = [fromList [0.3,0.7],fromList [0.4,0.6],fromList [0.2,0.8],fromList [0.65,0.35]]}
		),
		('b',Node {pa = "s", ch = "d",
			probs = [fromList [0.3,0.7],fromList [0.6,0.4]]}
		),
		('d',Node {pa = "ab", ch = "",
			probs = [fromList [0.9,0.1],fromList [0.3,0.7],fromList [0.8,0.2],fromList [0.4,0.6]]}
		),
		('l',Node {pa = "s", ch = "a",
			probs = [fromList [0.7,0.3],fromList [0.2,0.8]]}
		),
		('s',Node {pa = "", ch = "bl", 
			probs = [fromList [0.4,0.6]]}
		),
		('t',Node {pa = "v", ch = "", 
			probs = [fromList [0.3,0.7],fromList [0.6,0.4]]}
		),
		('v',Node {pa = "", ch = "t", 
			probs = [fromList [0.2,0.8]]}
		),
		('x',Node {pa = "a", ch = "", 
			probs = [fromList [0.2,0.8],fromList [0.9,1]]}
		)
	]
}

-}





---------------------------------------------------------------------------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------------------------------------------------------------------------



------------------------------------
-- Desugared version of elimNode --
------------------------------------


-- | elim node using the stateT layer only
elimNode :: Ord e => e -> NHeap e ()
elimNode e = do
		nodes <- lift $ get 
		let pa    = nodes ! e
		let chkey = ch pa
		modify ( M.delete e )
		let chs  = fmap (\( k, n ) -> ( k, head $ eliminatePa e pa [n] ) ) $ getCh nodes chkey
		lift $ modify ( M.union $ M.fromList chs )
		return ()


-- | desugared version of above
elimNode' :: Ord e => e -> NHeap e ()
elimNode' e = 
	lift $ get 
	>>= \nodes -> let pa = nodes ! e in let chkey = ch pa in 
	modify ( M.delete e )
	>>= \_     ->  let chs = fmap (\( k, n ) -> ( k, head $ eliminatePa e pa [n] ) ) $ getCh nodes chkey in 
	modify ( M.union $ M.fromList chs )
	>>= \_     -> return ()
	

-- | desugared version of monad using stateT and maybeT layer
-- | with line by line walkthrough
elimNodeM' :: Ord e => e -> NHeap e ()
elimNodeM' e = 
	-- | recreate NHeap by first lift maybe value into statemonadT context
	-- | then wrapping it in MaybeT monadT
	(MaybeT $ liftM Just get)
	-- | run NHeap to get nodes, find maybe node and then hoist maybe value into MaybeT constructor  
	>>= \nodes -> (MaybeT $ return $ M.lookup e nodes )
	-- | unwrap NHeap to get pa node
	-- | modify (...) is of type m(), make it Maybe m() and lift that into context
	-- | finally wrap in MaybeT to rebuild NHeap
	>>= \pa    -> (MaybeT $ liftM Just $ modify ( M.delete e ))
	-- | run above monadT and discard result, modify children
	>>= \_     -> let chs = getCh nodes $ ch pa in 
	let chs' = fmap (\(k, n) -> ( k, head $ eliminatePa e pa [n] ) ) chs in
	-- | create monad that has action of modifying map wrapped by StateT 
	modify ( M.union $ M.fromList chs' )
	-- | run monad and discard result, return unit
	>>= \_     -> return ()

-----------------------------
-- hand rolled State Monad --
-----------------------------

-- | for edcation purpose, roll own state type and make instance of monad
newtype ProbState s e = ProbState { run :: s -> ( e, s ) }

instance Monad ( ProbState s ) where
  return x = ProbState $ \probs -> ( x, probs )
  ( ProbState h ) >>= f = ProbState $ \s -> let ( e, s' ) = h s in
                                            let ( ProbState g ) = f e in g s'



takeN :: Ord e => [e] -> ProbState ( Map e ( Node e ) ) ( [ Maybe (Node e ) ] )
takeN es = do
	a <- takeEls es
	return a
	where 
		takeEls es = ProbState $ \nmap -> foldr (\e (ns, g ) -> (M.lookup e nmap : ns, M.delete e g ) ) ([],nmap) es 


insertN :: Ord e => [ ( e, Node e ) ] -> ProbState ( Map e ( Node e ) ) ()
insertN ns = do
	insertEls ns
	return ()
	where 
		insertEls xs = ProbState $ \nmap -> let nmap' = M.fromList xs in ( (), M.union nmap nmap')



-- | original algorithm impl with lots of explicity plumbing

eliminationAsk' :: Ord e => BayesNet e -> (e, Bool) -> Maybe Prob
eliminationAsk' bn (e,v) = mrmap >>= \rmap -> Just $ let pv = head . probs $ rmap ! e in if v == True then pv @> 0 else pv @> 1 
	where 
		mes   = let es = ns bn in let mi = L.elemIndex e es in mi >>= \i -> Just . fst $ splitAt i es 
		mrmap = mes >>= \es -> factorize es $ Just $ nmap bn


-- | Given ordersted stack of elems, map of elem to nodes, elem queried
-- | output maybe map marglinized of nodes in stack of elems
factorize :: Ord e => [e] -> Maybe (NodeMap e ) -> Maybe (NodeMap e)
factorize []     mheap = mheap
factorize (e:es) mheap = case insertCh of 
	Nothing 		 -> Nothing
	Just ( _, rmap ) -> factorize es $ Just rmap
	where
		takePa   = Mo.liftM ( run $ takeN [e] ) mheap
		(mpa, mchKey) = let mpa = Mo.join $ Mo.liftM head $ Mo.liftM fst takePa in ( mpa, Mo.liftM ch mpa )
		takeCh   = mchKey >>= \key -> Mo.liftM ( run $ takeN key ) $ Mo.liftM snd takePa 
		mchs     = Mo.liftM ( fmap fromJust ) $ Mo.liftM (filter (\x -> if x == Nothing then False else True ) ) $ Mo.liftM fst takeCh
		mchs'    = let marged = Mo.liftM3 eliminatePa ( return e ) mpa mchs in Mo.liftM2 zip mchKey marged
		insertCh = mchs' >>= \chs' -> Mo.liftM ( run $ insertN chs' ) $ Mo.liftM snd takeCh




























