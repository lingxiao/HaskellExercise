{- 

Created: March 10th 2013 
Based on lecture: https://alliance.seas.upenn.edu/~cis520/wiki/index.php?n=Lectures.DecisionTrees
adapted from https://github.com/chris-taylor/aima-haskell/blob/master/src/AI/Probability/Bayes.hs

-}



module ML.Learning.BayesNet where


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

type Prob = Double

data Node e = Node { pa :: [e], ch :: [e], probs :: [ Vector Prob ] } deriving ( Show )

data BayesNet e = BayesNet { ns :: [e], nmap :: Map e ( Node e ) } deriving ( Show )



---------------------
-- Bayes Net Utils --
---------------------


-- | Creates bayes net from list of triples of form ( var, parents, conditional probs )
--   Right now this fn expects list to triples to be toplogically sorted so that all nodes
--   after ni are descendants of ni ( note they need not be children of ni )
bfromList :: Ord e => [ (e, [e], [Prob]) ] -> BayesNet e
bfromList ns = BayesNet ( fst parts ) ( M.fromList $ snd parts )
	where 
		parts  = foldr build ([], []) $ toplogicalSort ns
		build (e,es,ps) (ns,ms) = let n = Node es ( vectorize ps ) in ( e:ns, (e,n):ms)
		vectorize ps = fmap ( NL.fromList ) $ fmap (\p -> [ p, 1-p ]) ps


-- | Toplogically sort list so that all nodes after ni are descendants of ni
--   stub for now
toplogicalSort :: [(e, [e], [Prob])] -> [(e, [e], [Prob])]
toplogicalSort = id



-- | Given node, find all children nodes
childOf :: e -> [ Node e ]
childOf  = undefined


--------------------------
-- Variable Elimination --
--------------------------

-- | Given a bayes net, and list of evidence, query prob x is true/false
--   Use: simpleQuery ( BayesNet [..] (M.fromList[..]) ) [(y,True),(z,False)] (x, True) 
condQuery :: Ord e => BayesNet e -> [(e,Bool)] -> (e,Bool) -> Maybe Prob
condQuery = undefined


-- | naive and partial implementation for linear causal net only
qsingle :: Ord e => BayesNet e -> (e, Bool) -> Maybe Double
qsingle net (e,value) = let dist = marginalize net e in dist >>= \(v:[]) -> Just $ v @> ( idxOf value )
	where 
		idxOf True  = 0
		idxOf False = 1
		marginalize net e = do
			els    <- gatherEls ( ns net ) e
			nodes  <- sequenceA $ fmap (\e -> M.lookup e $ nmap net ) els
			nprobs <- Just $ fmap probs nodes
			return $ foldl (\p q -> [ marg p q ] ) ( head nprobs ) ( tail nprobs )


gatherEls :: Eq a => [a] -> a -> Maybe [a]
gatherEls es e = let midx = L.elemIndex e es in midx >>= (\idx -> Just $ fst $ splitAt idx es ) >>= \es -> Just $ es ++ [e]


--   | Given vector of probs [ p(x=1)... p(x=n) ] and list of vectors ( isa matrix )
--   | of size mxn encoding p(y=yi|x=xj) for i = 1..m, j = 1..n, 
--   | marginalize out effects of x and return 1xm vector of p(y=i)
--   | used in foldr, thus the parameter types have to be consistent
marg :: [Vector Double] -> [ Vector Double ] ->  Vector Double
marg (v:[]) m = foldr (\v1 v2 -> zipVectorWith (+) v1 v2 ) ( head mv ) ( tail mv )
	where mv = zipWith (\p v -> mapVector ( *p ) v ) ( toList v ) m 




-- | Given the parent's vector to be marginalized out, and list of vectors conditional on it
-- | marginalize out the parent and return list of vectors of marginlized probs
-- | Case: P(A|B)(B)       = [P(A)]
-- | Case: P(A|B,C)(B)     = [P(A|C)]
-- | Case: P(A|B)(C|B)P(B) = [P(A),P(C)]
-- | Case: P(A) w.r.t. A   = [ Identity Vector ]
marginalize' :: NumPa -> PaIdx -> Vector Double -> [ [Vector Double] ] -> [ Vector Double ]
marginalize' pas idx v vs
	| length vs == 0 = [ mapVector (\x -> 0 * x + 1 ) v ]
	| otherwise      = fmap sumRow vs'
		where 
			sumRow v = foldr ( \v1 v2 -> zipVectorWith (+) v1 v2 ) ( head v ) ( tail v )
			vs'  = fmap ( \m -> zipWith (\p v -> mapVector( *p ) v ) ps m ) vs
			ps   = genProbs v pas idx

-- | generate a list of probs for a variable so that when it's zipped and * with list of vectors, 
-- | the variable is marginalized out of the list of vectors
-- | Ex: in a sequence [a1b1, a1b0, a0b1, a0b0], a is 0th element and b is 1st element
-- | [p, 1-p] -> 2 -> 0 -> [p, p, 1-p, 1-p] for a and, and [q, 1-q] -> 2 -> 1 -> [q,1-q,q,1-q] for b
genProbs :: Vector Double -> NumPa -> PaIdx -> [ Double ]
genProbs v n i = foldr (\xs l -> let a = xs !! i in let b = if a == 1 then p else q in b:l ) [] perm
	where
		perm  = Mo.replicateM n [1,0]
		p     = v @> 0
		q     = v @> 1


		

		

-- | Single query on arbitrarily shaped graphs
-- | Given a baye's net and an optimal ordering of nodes, and a node to be queried
-- | return the queried node's prob distribution
--single :: Ord => BayesNet e -> e -> [e] -> Vector Prob
single net e es = undefined


single' net e es ps = undefined

-- | given element, find all prob distributions characterizing element
findP e ps = undefined


--------------
-- Sampling --
--------------



-----------
-- Tests --
-----------

-- | helper
getProb net e = probs $ fromJust $ M.lookup e $ nmap net

{-

	Note the format: p(a = T ) = 0.4, p(b=T|a=T) = 0.3, p(b=T|a=F) = 0.2

	example = BayesNet {
		ns = "abcd", 
		nmap = fromList [
			('a',Node {pa = "",  probs = [fromList [0.4,0.6]]}),
			('b',Node {pa = "a", probs = [fromList [0.3,0.7],fromList [0.2,0.8]]}),
			('c',Node {pa = "b", probs = [fromList [0.8,0.2],fromList [0.7,0.3]]}),
			('d',Node {pa = "c", probs = [fromList [0.4,0.6],fromList [0.9,0.1]]})
		]
	}

-}


-- | a linear causal network

causal = bfromList [ ('a', [], "b", [0.4]), ('b', ['a'], "c", [0.3, 0.2]), ('c', ['b'], "d", [0.8, 0.7]), ('d', ['c'], "", [0.4, 0.9])]

getProbC = getProb causal
pna = getProbC 'a'
pnb = getProbC 'b'
pnc = getProbC 'c'


pc1 = qsingle causal ( 'c', True  )
pc0 = qsingle causal ( 'c', False )

-- | common cause 

commonCause   = bfromList [ ('x', "yz", [], [0.3] ), ( 'y', ['x'], "", [0.5, 0.2] ), ('z', ['x'],"", [0.2, 0.7]) ]

getProbCC = getProb commonCause

pnx = getProbCC 'x'  
pny = getProbCC 'y'  
pnz = getProbCC 'z' 

{-

BayesNet 
	{ns = "xyz", 
	nmap = fromList [
		('x',Node {pa = "",  probs =  [fromList [0.3,0.7]]}),
		('y',Node {pa = "x", probs =  [fromList [0.5,0.5],fromList [0.2,0.8]]}),
		('z',Node {pa = "x", probs =  [fromList [0.2,0.8],fromList [0.7,0.3]]})
	]
}

-}



-- | common effect

commonEffect  = bfromList [ ('y', [], "x", [0.1] ), ('z', [], "x", [0.4]), ('x', ['y', 'z'], "",[0.4, 0.3, 0.9, 0.7]) ]

getProbCE = getProb commonEffect

pnx' = getProbCE 'x'  
pny' = getProbCE 'y'  
pnz' = getProbCE 'z'  

{-

BayesNet 

{ns = "yzx", 
	nmap = fromList [
		('x', Node {pa = "yz", 
			probs = [fromList [0.5,0.5],fromList [0.3,0.7],fromList [0.7,0.3],fromList [0.9,0.1]]}
		),
		('y', Node {pa = "", 
			probs = [fromList [0.1,0.9]]}
		),
		('z', Node {pa = "", 
			probs = [fromList [0.4,0.6]]}
		)
	]
}

-}



-- | lecture graph

--mit = bfromList[
--	('v', "",   [0.2]), 
--	('t', "v",  [0.3,0.6] ), 
--	('a', "tl", [0.3,0.4,0.2,0.65] ),
--	('x', "a",  [0.2,0.9] ),
--	('l', "s",  [0.7, 0.2] ),
--	('s', "",   [0.4]),
--	('b', "s",  [0.3, 0.6] ),
--	('d', "ab", [0.9, 0.3, 0.8, 0.4]) ]











{-
	In expanded notation
	qsingle' net e = nprobs >>= \ps -> Just $ foldl (\p q -> [ marg p q ] ) ( head ps ) ( tail ps )
		where 
			nprobs  = nodes >>= \ns-> Just $ fmap probs ns 
			nodes   = Mo.join $ parents >>= \pa -> Just $ sequenceA $ fmap(\e -> M.lookup e $ nmap net ) pa 
			parents = gatherEls ( ns net ) e
-}







