{- 

Created: March 25th 2013 
Based on lecture: https://alliance.seas.upenn.edu/~cis520/wiki/index.php?n=Lectures.DecisionTrees


main lecture: http://cs229.stanford.edu/section/cs229-hmm.pdf
ex w/ number: http://www.cs.sjsu.edu/~stamp/RUA/HMM.pdf
review of constrained optimzation: http://mat.gsia.cmu.edu/classes/QUANT/NOTES/chap4.pdf
http://www.cs.mcgill.ca/~dprecup/courses/ML/Lectures/ml-lecture20.pdf


-}



module ML.Learning.HMM where


import Data.Maybe
import Foreign.Storable
import Numeric.LinearAlgebra as NL
import Control.Applicative

import qualified Control.Monad as Mo
import qualified Data.List as L




---------------------
-- Data Structures --
---------------------


type ObsProb     = Double
type TransProb   = Double
type EmitProb    = Double

type TransMatrix = Matrix TransProb
type EmitMatrix  = Matrix EmitProb


--------------------------------
-- Brute force Implementation --
--------------------------------

-- | Find probabililty of a sequence of observations: P( o1, o2, ... oT ) by bruteforce

-- Enumerate all possiblities


------------
-- Tests --
------------


-- Light Example ---

-- | State Alphabet
data Lstates       = On | Off deriving ( Show )
-- | Obervation Alphabet 
data Lobservations = Light | Dark deriving ( Show )

-- Transition (A) and Observation (B) Matrices --
lt = fromColumns $ fmap fromList [[0,0],[0.6,0.3],[0.4,0.7]] :: Matrix TransProb
le = fromLists [[0.4,0.6],[0.1,0.9]] 						 :: Matrix EmitProb 


-- Example 1 states
lo1 = [ Light, Light, Dark ]

-- State space
enumStates1' = fromLists [[1.0,0.6,0.6],[1.0,0.6,0.4],[1.0,0.4,0.3],[1.0,0.6,0.7],[1.0,0.7,0.7],[1,0.7,0.3],[1,0.3,0.4],[1,0.3,0.6]] 			   :: Matrix TransProb
-- Emission probs mapping state space
enumEmit'    = fromColumns $ fmap fromList [[0.4,0.4,0.4,0.4,0.6,0.6,0.6,0.6],[0.4,0.4,0.6,0.6,0.6,0.6,0.4,0.4],[0.1,0.9,0.1,0.9,0.9,0.1,0.9,0.1]] :: Matrix EmitProb    


-- 1. find liklihood of sequence P(light, light, dark )
plo1 = ( foldCol (*) 1 $ trans enumStates1' ) <.> ( foldCol (*) 1 $ trans enumEmit' ) :: ObsProb

-- 2. find prob value of most likely state sequence, argMax P(s1,...sT | light, light, dark )
mlo1 = maximum . toList $ zipVectorWith (*) ( foldCol (*) 1 $ trans enumStates1' ) $ foldCol (*) 1 $ trans enumEmit' :: ObsProb



-- 3. find P(si|light,light,dark)


-- 4. find A, B transition and emission matrices given stet of obervations









-- Weather Example ---

-- | State Alphabet
data Wstates       = Sun | Cloud | Rain deriving ( Show )
-- | Obervation Alphabet 
data Wobservations = Water | Juice | Soup

wt  = fromColumns $ fmap fromList [[0,0,0,0],[0.33,0.8,0.2,0.1],[0.33,0.1,0.6,0.2],[0.33,0.1,0.2,0.7]] :: Matrix TransProb
we  = fromColumns $ fmap fromList [[0.5,0.25,0.25],[0.4,0.25,0.35],[0.6,0.25,0.15]] 			       :: Matrix EmitProb


wo1 = [ Water, Soup ]
wo2 = [ Water, Soup, Juice ]




---------------------------------
-- Dynamic Programming Methods --
---------------------------------



------------------
-- Matrix Utils --
------------------

-- | @Param:  function
-- | @Param:  m x n matrix
-- | @Output: n x 1 vector
foldCol :: (Element a, Storable b) => (a -> b -> b) -> b -> Matrix a -> Vector b
foldCol g i m = fromList $ fmap (\v -> foldVector g i v ) $ toColumns m


-- | @Param:  function over pairs of matrix elements
-- | @Param:  m x 1 vector
-- | @Param:  n x 1 vector
-- | @Output: m x n matrix, represented by list of length n
outerOp :: ( Element a, Storable a ) => ( a -> a -> a ) -> Vector a -> Vector a -> [Vector a]
outerOp g v1 v2 = fmap NL.fromList $ fmap (\e1 -> fmap (\e2 -> g e2 e1 ) $ NL.toList v1) $ NL.toList v2



-- | @Param:  m x 1 vector
-- | @Param:  n x 1 vector
-- | @Output: m x n matrix
outerProd :: ( Element a, Storable a, Num a ) => Vector a -> Vector a -> [Vector a]
outerProd = outerOp (*)




































