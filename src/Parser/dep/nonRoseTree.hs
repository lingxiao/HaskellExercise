
---------------------------------------------------------------------------------------------------
-- | Rose Tree
---------------------------------------------------------------------------------------------------


module RoseTree (

	RoseTree(..),

) where


import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Maybe
import Data.Monoid
import qualified Data.Foldable as F

import Test.QuickCheck
import Test.QuickCheck.All 
import Test.QuickCheck.Modifiers


-- | Note this is not really a rose tree -> have to change name
data RoseTree a = Nil | Leaf a | Branch [RoseTree a]
	deriving (Eq,Show)


flatten :: RoseTree a -> RoseTree a 
flatten Nil 	    = Nil
flatten (Leaf a)    = Leaf a
flatten (Branch ts) = Branch $ fmap flatten ts



instance Monoid (RoseTree a) where
	mempty  = Nil
	mappend t 			Nil 	    = t
	mappend Nil 		t  			= t
	mappend a@(Leaf _) b@(Leaf _)   = Branch [a,b]
	mappend a@(Leaf _) (Branch bs)  = Branch $ a:bs
	mappend (Branch bs) a@(Leaf _)  = Branch $ bs ++ [a]
	mappend (Branch as) (Branch bs) = Branch $ as ++ bs


instance F.Foldable RoseTree where
	foldr f z Nil 		  = z
	foldr f z (Leaf a)	  = f a z
	foldr f z (Branch ts) = undefined




instance Functor RoseTree where
	fmap g t = case t of 
		Nil 	  -> Nil
		Leaf a    -> Leaf $ g a
		Branch bs -> Branch $ (fmap . fmap) g bs 


--(<*>) :: Applicative f => f (a -> b) -> f a -> f b

instance Applicative RoseTree where
	pure a            = Leaf a
	Nil   	 <*> _    = Nil
	_ 		 <*> Nil  = Nil
	(Leaf g) <*> t    = fmap g t
	t <*> (Leaf a)    = fmap (\g -> g a) t

	-- | Cross product of branches


--part (Branch (g:gs)) (Branch (a:as)) = Branch $ g <*> a



part (Branch (g:gs)) t2 = g <*> t2




--temp (Branch gs) (Branch as) = gs <*> as


instance Monad RoseTree where
	return a = Leaf a
	t >>= g  = undefined


--instance Foldable RoseTree where
--instance Traversable RoseTree where



-- Tests --

t1 = Leaf 1						    	:: RoseTree Int
t2 = Branch [t1, Branch [t1,t1],t1]		:: RoseTree Int
t3 = Branch [t1,t2]						:: RoseTree Int

f1 = Leaf (\x -> x * 10) 			  	:: RoseTree (Int -> Int)
f2 = Leaf (\x -> x + x) 			  	:: RoseTree (Int -> Int)
f3 = Branch [f1,Branch [f1,f2]] 	  	:: RoseTree (Int -> Int)
f4 = Branch [Branch [f1,f2],f1] 	  	:: RoseTree (Int -> Int)








