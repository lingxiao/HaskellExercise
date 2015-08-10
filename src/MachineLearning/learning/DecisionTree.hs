{- 

Created: March 6th 2013 
Based on lecture: https://alliance.seas.upenn.edu/~cis520/wiki/index.php?n=Lectures.DecisionTrees

 ghc-pkg list

-}



module ML.Learning.DecisionTree where


import qualified Control.Monad as Mo
import qualified Data.Map as M
import qualified Data.List as L

import Numeric.LinearAlgebra as NL
import Foreign.Storable
import Data.Traversable
import Data.Maybe


-------------------
-- Decision Tree --
-------------------

-- | Decision Tree
--   f = feature index
--   v = value of best split
--   p = information gain of spliting on feature
data DTree f v p = Empty | Leaf f v p ( DTree f v p ) 
	deriving ( Show )


------------------------
-- Learning Algorithm --
------------------------

-- | IG(X) = H(Y) - H(Y|X)

-- | Maps label vector Y and sample matrix X to Decision Tree of arb depth
-- the index does not work
dtLearn :: Vector Double -> [Vector Double] -> DTree Int Double Double
dtLearn y [] = Empty
dtLearn y x  = Leaf idx v maxig $ dtLearn y x'
	where
		ig      = fromJust $ featureIg y x
		maxig   = maximum ig 
		idx     = fromJust $ L.elemIndex maxig ig

		plucked = fromJust $ pluckL idx x
		feature = fst plucked
		x'      = snd plucked
		v       = bestSplit feature y 


-- | Given vector, find best value to split on
--   Placeholder code for now since it might be prob domain specific
bestSplit :: (Storable a ) => Vector a -> Vector a -> a 
bestSplit v y = v @> 0


-- | collapse marix into a list of info gain IG(x) = H(Y) - H(Y|x)
featureIg :: Vector Double -> [ Vector Double ] -> Maybe [ Double ]
featureIg y xs = sequenceA $ fmap igx xs
	where 
		igx x = let mx = condEntropy y x in fmap (\x -> hy - x ) mx
		hy    = entropy y


-- | calculate H(Y|x) entropy of y conditional on x
condEntropy :: (Ord a, Storable a) => Vector a -> Vector Double -> Maybe Double
condEntropy y x = ps >>= (\p -> Just $ sum p )
	where
		ps = let g = zipWith (*) px in ey >>= (\x -> Just $ g x)
		px    = prob . summarize $ x
		ey = partition y x >>= (\vs -> Just $ fmap entropy vs )


-- | Find H(Y) Entropy of vector
--   Use: entropy [1,0,1,0] = -0.69
entropy :: Vector Double -> Double
entropy v = negate . sum $ fmap ( \p -> if p == 0 then 0 else p * log p ) $ prob . summarize $ v


-- | Partition y according to unique labels in x
--   Use: partition fromList[1,0,1,1] fromList[0,0,1,1] = [ fromList [0,1,1], fromList[0] ] 
partition :: ( Ord a, Storable b, Storable a ) => Vector a -> Vector b -> Maybe [Vector b]
partition x y = sequenceA $ fmap (\bs -> pruneV bs y ) (fmap toList $ encodeV x :: [[Int]])




-- | NOTE : everything below here should be pure w.r.t. to their lableled domain


-------------------------
-- Summary Statistics  --
-------------------------

type SummaryList a b c = [ (a, b, c) ]

label :: SummaryList a b c -> [ a ]
label = foldr (\( l, _, _ ) ls -> l : ls ) []

count :: SummaryList a b c -> [ b ]
count = foldr (\( _, c, _ ) cs -> c : cs ) []

prob :: SummaryList a b c -> [ c ]
prob = foldr (\( _, _, p ) ps -> p : ps ) []


-- | Takes a Vector [ v1, v2, ... vn ] to SummaryList [(a,b,c)] of label, frequency and probability
--   Use: summarize ( fromList [1,0,1,0] ) = [(1,2, 0.5),(0,2,0.5)]
summarize :: (Fractional a, Ord b, Storable b) => Vector b -> SummaryList b a a
summarize v = fmap (\(l,f) -> ( l, f, f/len ) ) freq
	where 
		freq = M.toList $ M.fromListWith (+) [ (c,1) | c <- xs ]
		len  = L.genericLength xs
		xs   = toList v


----------------------
-- List Operations  --
----------------------

-- | Pluck elements out of list at index, return element and rest of list in tuple
pluckL :: Int -> [a] -> Maybe ( a, [a] )
pluckL idx xs = case splitAt idx xs of 
	( _, [] )      -> Nothing
	( hs, (t:ts) ) -> Just ( t, hs ++ ts )
	

------------------------
-- Vector Operations  --
------------------------

-- | Map vector to a list of unique elems in vector
--   Use: uniqueElems (fromList[11,1,2,11]) = [1,2,11]
uniqueElems :: (Ord a, Storable a) => Vector a -> [a]
uniqueElems v = fmap (\xs -> head xs ) $ L.group . L.sort $ toList v


-- | Map a vector to a matrix of 0s and 1s where first column signifies location of first unique elem, etc
--   Use: encodeV ( fromList [11,13,13,11] ) = trans . fromLists $ [ [1,0,0,1], [0,1,1,0] ]
encodeV :: (Num b, Ord a, Storable b, Storable a) => Vector a -> [Vector b]
encodeV v = fmap (\e -> tobinary e v ) $ uniqueElems v
	where tobinary e = mapVector (\x -> if x == e then 1 else 0 )



-- | Drop elements from vector at 0s to return sub vector
--   Use: pruneV [1,0,1,0] ( fromList [11..14] ) = fromList [11, 13]
pruneV :: (Eq a, Num a, Storable b) => [a] -> Vector b -> Maybe (Vector b)
pruneV bs v 
	| length bs == dim v = let pruneMap = zip bs $ toList v in Just . fromList $ [ e | ( keep, e ) <- pruneMap, keep == 1 ]
	| otherwise 	     = Nothing



------------------------
-- Matrix Operations  --
------------------------

-- | Pairwise operation on matrices 
--   naive implementation of matlab fn of same name
bsxfun :: (Element b, Element a, Element c) => (a -> b -> c) -> Matrix a -> Matrix b -> Maybe ( Matrix c )
bsxfun g m1 m2
	| rows m1 == rows m2 && cols m1 == cols m2 = Just . fromLists $ fmap toList vs
	| otherwise 							   = Nothing
		where vs = zipWith (\v1 v2 -> zipVectorWith g v1 v2 ) ( toRows m1 ) ( toRows m2 )


-- | Plucks col vector from matrix at given column and return vector and plucked matrix
pluckCol :: Element a => Matrix a -> Int -> Maybe (Vector a, [Vector a])
pluckCol m idx = pluckL idx $ toColumns m


-- | Plucks row vector from matrix at given row, return vector and plucked matrix
pluckRow :: Element a => Matrix a -> Int -> Maybe (Vector a, [Vector a])
pluckRow m idx = pluckL idx $ toRows m

-------------
-- Testing --
-------------


x1 = fromList[11,12,12,11] :: Vector Double
x2 = fromList[12,11,12,11] :: Vector Double
x3 = fromList[11,12,12,12] :: Vector Double
y  = fromList [1,0,0,1] :: Vector Double
x  = [ x1, x2, x3 ]


v1 = fromList [0,0,1,1] :: Vector Double
v2 = fromList [25, 12, 12, 25] :: Vector Double

m1 = trans $ fromLists [ [1..4], [11..14] ] :: Matrix Double
m2 = trans $ fromLists [ [21..24], [31..34] ] :: Matrix Double


