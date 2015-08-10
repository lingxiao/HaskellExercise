
---------------------------------------------------------------------------------------------------
-- | Rose Tree
-- | Transcribed from Data.Tree

-- | non-transcribed part very hacky, incomplete assortment of combinators


-- | used but read: http://pubs.rgrossman.com/dl/proc-006.pdf
-- | Music: http://www.youtube.com/watch?v=YPiez4gp9nM&list=PL1BE01F57554D01DC

-- | Understand this when possible: http://www.haskell.org/haskellwiki/Zipper
-- | on derivitives: http://stackoverflow.com/questions/5919901/data-structure-differentiation-intuition-building
---------------------------------------------------------------------------------------------------


module RoseTree (

	  RoseTree(..)
	, flatten
	, graft
	, graftAll
	, ends
	, rappend


) where


import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.Foldable (Foldable(foldMap), toList)
import Data.Traversable (Traversable(traverse))
import Text.Show.Functions

import Test.QuickCheck
import Test.QuickCheck.All 
import Test.QuickCheck.Modifiers

import Utils


------------------------------------
------------- Data Type ------------
------------------------------------

-- | consider getting rid of Bud to simplify
-- | Note labled nodes make RoseTree non-associative
data RoseTree a = Bud | Node a [RoseTree a]
	deriving (Eq)

------------------------------------
----- Typeclass Implementations ----
------------------------------------

instance (Show a) => Show (RoseTree a) where
	show = unlines . draw

instance Functor RoseTree where 
	fmap g Bud 		   = Bud
	fmap g (Node a ts) = Node (g a) $ (fmap . fmap) g ts

-- | <*> Implementation:
-- | Apply home node function g onto home node value a in tree t
-- | Apply g onto entire list of trees in t `append`
-- | apply all functions gs to all values ts in t
-- | Data.Tree implementation: (Node g gs) <*> t@(Node a ts) = Node (g a) $ (fmap . fmap) g ts ++ fmap (<*>t) gs
instance Applicative RoseTree where
	pure a      = Node a []
	Bud <*> _   = Bud
	_   <*> Bud = Bud
	gs <*> as   = fromNodes . fmap (\g -> fmap g as) $ flatten gs

-- | (>>=) :: Monad m => m a -> (a -> m b) -> m b
instance Monad RoseTree where
	return a        = Node a []
	Bud       >>= _ = Bud
	Node a as >>= g = Node a' (as' ++ fmap (>>= g) as)
		where Node a' as' = g a


instance Foldable RoseTree where
	foldMap g Bud 		  = mempty
	foldMap g (Node a as) = g a `mappend` foldMap (foldMap g) as


-- | traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
-- | (Node <$> g a) :: Maybe ([RoseTree a] -> (RoseTree a))
-- | (traverse . traverse) g as :: Maybe [RoseTree a]
instance Traversable RoseTree where
	traverse g Bud 		   = pure Bud
	traverse g (Node a as) = Node <$> g a <*> (traverse . traverse) g as


------------------------------------
--------- Exported Functions -------
------------------------------------

-- | Flatten a tree into list
-- | The elements of a tree in pre-order
flatten :: RoseTree a -> [a]
flatten Bud = []
flatten t   = collectN t []
	where collectN (Node a ts) ls = a : foldr collectN ls ts


-- | consider rewriting this by implementing zipper for rosetree
ends :: Eq a => RoseTree a -> [a]
ends Bud = []
ends t   = collectLeaf t []
	where collectLeaf (Node a ts) ls = if ts == [] then a : foldr collectLeaf ls ts else foldr collectLeaf ls ts


graft :: Eq a => 
	a ->                  -- | value of a terminal node within tree
	[a] -> 		          -- | list of new values added to terminal node
	RoseTree a ->         -- | original rose tree
	Maybe (RoseTree a)    -- | new rose tree if provided terminal node is in tree
graft _ [] t              = Just t
graft a ns Bud 		      = Just $ Node a $ pure <$> ns
graft a' ns t@(Node a as) = if size nt > size t then Just nt else Nothing
	where 
		nt = if as == [] then cons a' ns t else Node a $ fmap (cons a' ns) as
		cons a' ns t@(Node a as) = if a == a' then Node a (as ++ ns') else Node a $ (cons a' ns) <$> as
		ns' = fmap pure ns



-- | note this is even more klugdy than rest of stuff
graftAll :: (Eq a, Ord a) => [(a,[a])] -> RoseTree a -> Maybe (RoseTree a)
graftAll ns t = foldM (\t (k,vs) -> graft k vs t) t $ [ (k, join vs)  | (k,vs) <- groupVals ns ]




-----------------------------------------
---------- Non-Exported Functions -------
-----------------------------------------


isTerminal :: Eq a => RoseTree a -> Bool
isTerminal Bud         = True
isTerminal (Node a as) = if as == [] then True else False


size :: RoseTree a -> Int
size = length . flatten


-- | Build a rose tree from list of leaves
-- | Note Bud trees are discarged
fromNodes :: [RoseTree a] -> RoseTree a
fromNodes [] 	  = Bud 
fromNodes (n:ns) = case n of 
	Bud 		-> fromNodes ns
	(Node a as) -> Node a $ as ++ ns


drawForest :: Show a => [RoseTree a] -> String
drawForest  = unlines . fmap (unlines . draw)

-- | Note Data.Tree exports drawVerticle which displays a better format
draw :: (Show a) => RoseTree a -> [String]
draw Bud 		  = [""]
draw (Node x ts0) = (show x) : drawSubTrees ts0
	where
	    drawSubTrees []  = []
	    drawSubTrees [t] =
	        "|" : shift "`- " "   " (draw t)
	    drawSubTrees (t:ts) =
	        "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts
	    shift first other = zipWith (++) (first : repeat other)



-- | consider getting rid of this function
-- | Note graft is not associative
rappend :: RoseTree a -> RoseTree a -> RoseTree a
rappend Bud t = t
rappend t Bud = t
rappend (Node a as) t2 = Node a $ as ++ [t2]

-------------------------------------
--------------- Tests ---------------
-------------------------------------

t1 = Node 1 []			    	:: RoseTree Int
t2 = Node 2 []			    	:: RoseTree Int
t3 = Node 3 [t1,t2]				:: RoseTree Int
t4 = Node 4 [t1,t3,t2]			:: RoseTree Int

t5 = Node "a" [Node "bend" [],Node "c" [Node "dend" [], Node "fend" []]] :: RoseTree String



f1 = Node (\x -> x * x) [] 	    :: RoseTree (Int -> Int)
f2 = Node (\x -> x * x * x) [] 	:: RoseTree (Int -> Int)
f3 = Node id [f1,f2] 			:: RoseTree (Int -> Int)







