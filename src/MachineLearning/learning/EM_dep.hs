{- 

Created: March 28th 2013 

Based on lectures:
http://cs229.stanford.edu/notes/cs229-notes7b.pdf
https://alliance.seas.upenn.edu/~cis520/wiki/index.php?n=Lectures.EM
http://cs229.stanford.edu/notes/cs229-notes8.pdf


Documention used: 
statistics package:
http://hackage.haskell.org/packages/archive/statistics/0.10.2.0/doc/html/Statistics-Distribution-Normal.html
matrix library:
http://hackage.haskell.org/packages/archive/hmatrix/0.14.1.0/doc/html/Data-Packed-Matrix.html
sample:
http://hackage.haskell.org/packages/archive/StatisticalMethods/0.0.0.1/doc/html/src/Statistics-EM-TwoGaussian.html

-}


module ML.Learning.EM where

import Statistics.Distribution
import Statistics.Distribution.Normal
import Numeric.LinearAlgebra as NL
import Foreign.Storable
import Data.Maybe

import qualified Control.Monad as Mo
import qualified Data.Map as M
import qualified Data.List as L



-----------
-- Types --
-----------

-- | Represent matrix as list of vectors so we can use list functions
type VMatrix a = [Vector a]
-- | value of data
type Data  = Double
-- | mean
type Mu    = Double
-- | standard deviation
type Sigma = Double
-- | mixing proportion
type Phi   = Double
-- | probability
type Probs = Double



------------------
-- EM algorithm --
------------------

{-
	Index Conventions:
	k := number of Gaussians
	j := jth Gaussian

	m := number of samples
	i := ith sample

	d := number of dimension of data
-}

-- | Expectation step | --

-- | Given a m x x d matrix of sample size m and sample dimension d, and a 
-- | and an k x d of means and standard deviation, where k is num of clusters
-- | and a k x 1 of phis, mixing proportion of k distributions
-- | return a d-length list of prob matrix, each one of dimension m x k  
genEmat :: VMatrix Data -> [[Mu]] -> VMatrix Sigma -> [Phi] -> [VMatrix Probs]
genEmat xs ms ss ps = zipWith (\x d -> toProbs d ps x ) xs $ zipWith (\m s -> genDistr m s ) ms ss 


-- | P(zj|xi) = P(xi|zj)P(zj) / P(xi), where P(xi) = SUM P(xi|zj) P(zj) 
-- | And P(xi|zj) = N(xi|uj, sigmaj), where N is a Gaussian of mean uj and stdDev sigmaj
-- | And P(zj) = Phij, mixing proportion of Guassian j
-- | naive implementation with lots of list flipping
-- | consider merits of native Matrix implementation?
toProbs :: [NormalDistribution] -> [Phi] -> Vector Data -> VMatrix Probs
toProbs ds ps x = NL.toColumns $ NL.fromLists probs
	where
		px_pz = fmap (\x -> zipWith (*) ps $ fmap (\d -> density d x ) ds ) $ toList x
		probs = fmap (\v -> let px = foldr (+) 0 v in fmap (\e -> e / px ) v ) px_pz


-- | given k x 1 vector of means and std deviation, return length-k list of normal distributions 
-- | @Output: k x 1 list of NormalDistribution
-- | @Param:  k x 1 vector of means
-- | @Param:  k x 1 vector of stdDevs
genDistr :: [Mu] -> Vector Sigma -> [NormalDistribution]
genDistr m s = zipWith (\a b -> normalDistr a b ) m $ toList s



-- | Maximization step | --


-- | @Output: generate a tuple of vector of means, and list of phis
-- | @Param:  m size of data
-- | @Param:  k number of gaussians 
-- | @Param:  m x 1 x d matrix of data
-- | @Param:  m x k x d matrix of probs, where d is length of list
genPhiMu :: Double -> Double -> VMatrix Data -> [VMatrix Probs] -> ([[Mu]], [Phi])
genPhiMu m k xs probs = ( ms, foldr (\l ls -> zipWith avg l ls ) p ps  )
	where
		( ms, p:ps )  = foldr (\(m, p) (ms, ps) -> (m:ms, p:ps) ) ([],[]) $ zipWith (\x prob -> gen m x prob ) xs probs 
		avg a b       = (a+b)/k
		gen m x pzx   = foldr (
			\v (a,b) ->
				let sumed  = foldVector (+) 0 v in 
				let sumedx = foldVector (+) 0 $ zipVectorWith (*) v x in
				( sumedx/sumed : a, sumed/m : b )			
			) ([], []) pzx


-- | @Output: 
-- | @Param:  
-- | @Param:  
-- | @Param:  
--x_m = head $ zipWith (\x m -> fmap (\mu -> mapVector (\e -> e - mu ) x ) m ) xs ms 
--genSigma :: [Vector Data] -> [[Mu]] -> [VMatrix Probs] -> [VMatrix Sigma]
genSigma :: Matrix Data -> Matrix Mu -> Matrix Probs -> Matrix Sigma
genSigma xs ms probs = undefined
	


-----------
-- Tests --
-----------


mix :: [Double]
mix = [0.5,0.5]


x = [NL.fromList[ 4.12 ]] :: VMatrix Double



-- | toy 1-D data set
x1 :: VMatrix Double 
x1 = [NL.fromList[ -0.39, 3.72, 0.48, 1.01, 4.12 ]]

ex1 :: [VMatrix Probs]
ex1 = genEmat x1 m1d s1d mix

------------------
-- 1-D data set --
------------------

-- | init 1D clusters in R2 with 
-- | cluster1 ~ N(1,10)
-- | cluster2 ~ N(0,11)
-- | mixing proportion: [0.5,0.5]

m1d :: [[Mu]]
m1d = [[4,1]]

s1d :: VMatrix Sigma
s1d = [fromList[1,0.5]]

dist1d :: [[NormalDistribution]]
dist1d = zipWith (\m s -> genDistr m s ) m1d s1d 


-- | Note each vector in the list represent value of xi at one dimension
-- | m x k x d : 20 x 2 x 1
x1d :: VMatrix Double 
x1d = [NL.fromList[ -0.39, 0.12, 0.94, 1.67, 1.76, 2.44, 3.72, 4.28, 4.92, 5.53, 0.06, 0.48, 1.01, 1.68, 1.80, 3.25, 4.12, 4.60, 5.28, 6.22 ]]

e1d :: [VMatrix Double]
e1d = genEmat x1d m1d s1d mix

mp1d :: ([[Mu]], [Phi])
mp1d = genPhiMu 20 2 x1d e1d


------------------
-- 2-D data set --
------------------

x2d :: VMatrix Double 
x2d = [
	NL.fromList[ 0.12, 0.94, 1.67, 1.76, 4.28, 4.92, 5.53, 0.06, 0.48, 1.01, 1.68, 1.80, 3.25, 4.60, 5.28, 6.22 ],
	NL.fromList[ 0.12, 0.94, 1.67, 1.76, 4.28, 4.92, 5.53, 0.06, 0.48, 1.01, 1.68, 1.80, 3.25, 4.60, 5.28, 6.22 ]]

-- | init 2 clusters in R2 with 
-- | cluster1 ~ N((1,10), (1,1) )
-- | cluster2 ~ N((0,11), (0.5,0.5) )
-- | mixing proportion: [0.5,0.5]

m2d :: [[Mu]]
m2d = [[1,0], [10,11]]

s2d :: VMatrix Sigma
s2d = [fromList[1,0.5], fromList[1, 0.5]]

dist2d :: [[NormalDistribution]]
dist2d = zipWith (\m s -> genDistr m s ) m2d s2d 


e2d :: [VMatrix Double]
e2d = genEmat x2d m2d s2d mix

{-

a 2 x 2 x 16 Ematrix with k = 2, d = 2, m = 16
Emat :: [ VMatrix Probs ]
Emat = [
	[
		fromList [0.13543198589,0.1991509780345,0.1596923770076,0.1620288797635,9.198630904121386e-4,9.185624900122854e-5,6.979482992577385e-6,0.12823564721281017,0.17424625637948724,0.19946116689304105,0.15829645385544638,0.14484577638074134,1.586982591783371e-2,3.059509650568865e-4,2.0993751465808657e-5,2.415267234749841e-7],
		fromList [0.387616615125,6.81430104458e-2,1.50614850307e-3,8.12310818083e-4,4.894969512623367e-17,3.762687687923536e-22,1.0931451255389192e-27,0.39608021179365605,0.2516443410981171,5.186357668282056e-2,1.4106022569413848e-3,6.119019301137718e-4,2.6695566147628514e-10,1.6655880323799287e-19,2.4324252177522695e-25,9.921853231965093e-35]
	],

	[	
		fromList [1.268184474664e-22,2.9842318105e-19,1.707150789784e-16,3.51320876121e-16,1.5673636908959892e-8,4.966969706699559e-7,9.142874604773688e-6,6.997629333825254e-23,4.166187042066049e-21,5.62365075264895e-19,1.8553544837056728e-16,4.999189374248589e-16,2.5474689794218418e-11,9.286809222776447e-8,2.898591253178643e-6,1.5746840645376075e-4],
		fromList [6.05637317989e-104,4.97727638854e-89,9.79932636303e-77,2.73368117287e-75,2.3813443628639805e-40,3.1067263014747675e-33,4.0921028313909586e-27,4.416273456881153e-105,2.97729594394214e-97,8.234640028362594e-88,1.4229448103497672e-76,1.2121047949078819e-74,2.699513024588587e-53,1.0556163502452736e-36,1.5207888243366865e-29,5.6890792705194585e-21]
	]
]

-}





------------------
-- 3-D data set --
------------------

x3d :: VMatrix Double 
x3d = [
	NL.fromList[ 0.12, 0.94, 1.67, 1.76, 4.28, 4.92, 5.53, 0.06, 0.48, 1.01, 1.68, 1.80, 3.25, 4.60, 5.28, 6.22 ],
	NL.fromList[ 0.12, 0.94, 1.67, 1.76, 4.28, 4.92, 5.53, 0.06, 0.48, 1.01, 1.68, 1.80, 3.25, 4.60, 5.28, 6.22 ],
	NL.fromList[ 0.12, 0.94, 1.67, 1.76, 4.28, 4.92, 5.53, 0.06, 0.48, 1.01, 1.68, 1.80, 3.25, 4.60, 5.28, 6.22 ]]








-----------
-- Utils --
-----------


-- | given dimension of matrix, generate matrix of random numbers 
randMat :: Int -> Int -> VMatrix Double
randMat = undefined


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


------------------
-- Matrix Utils --
------------------


innerProd :: Matrix a -> Matrix a -> a
innerProd = undefined


-- | @Param:  m x 1 vector
-- | @Param:  n x 1 vector
-- | @Output: m x n matrix
outerProd :: Vector a -> Vector a -> Matrix a 
outerProd v1 v2 = undefined









-- manually calculate density
{-

normal :: Mu -> Sigma -> Double -> Probs
normal m s x = coeff * ( exp power )
	where
		coeff = 1 / ( s * ( sqrt $ 2 * pi ) )
		power = -1 * ( (( x - m )^ 2 )/( 2 * (s ^ 2)) )

-- | given k-length list of distributions, k-length list of phis, and mx1 vector of x, 
-- | return m x k matrix of P(zj|xi) -> this is wrong, it doesnt do that
toProbs' :: [NormalDistribution] -> [Phi] -> Vector Double -> VMatrix Double
toProbs' ds ps vs = zipWith (\d p -> toProb p d vs ) ds ps
	where toProb p d us = scaleV (*) p $ mapVector (\e -> density d e ) us


-}







----------------------------------
-------- Depricated --------------
----------------------------------
{-



-- | @Param:  m x 1 vector of data, where m is number of samples
-- | @Param:  k x 1 vector of means, where k is number of gaussians
-- | @Param:  k x k matrix of stdDevs
-- | @Param:  list of length k of phis, or mixing proprotion of guassians
-- | @Output: m x k matrix of P(zj|xi) for i in {1..m}, j in {1..k}
genEmat :: Vector Data -> Vector Mu -> Matrix Sigma -> [Phi] -> Matrix Probs
genEmat xs ms ss ps = undefined


-- | P(zj|xi) = P(xi|zj)P(zj) / P(xi), where P(xi) = SUM P(xi|zj) P(zj) 
-- | And P(xi|zj) = N(xi|uj, sigmaj), where N is a Gaussian of mean uj and stdDev sigmaj
-- | And P(zj) = Phij, mixing proportion of Guassian j
-- | naive implementation with lots of list flipping
-- | consider merits of native Matrix implementation?


-- | @Param:  list of length k of guassian functions
-- | @Param:  list of length k of mixing proportions
-- | @Param:  m x 1 vector of data
-- | @Output: m x k matrix of P(zj|xi)
toProbs :: [NormalDistribution] -> [Phi] -> Vector Data -> Matrix Probs
toProbs ds ps x = undefined


-- | @Param:  k x 1 matrix of means
-- | @Param:  k x 1 matrix of stdDevs
-- | @Output: list of length k of guassian functions
genDistr :: Vector Mu -> Vector Sigma -> [NormalDistribution]
genDistr = undefined

-- | Maximization step | --


-- | @Output: generate a tuple of vector of means, and list of phis
-- | @Param:  m size of data
-- | @Param:  k number of gaussians 
-- | @Param:  m x 1 x d matrix of data
-- | @Param:  m x k x d matrix of probs, where d is length of list
genPhiMu :: Double -> Double -> Matrix Data -> [Matrix Probs] -> ([[Mu]], [Phi])
genPhiMu m k xs probs = undefined



-- | @Output: 
-- | @Param:  m x 1 vector of data
-- | @Param:  k x 1 vector of distribution means
-- | @Param:  m x k matrix of probs, P(zj|xi)
genSigma :: Vector Data -> Vector Mu -> [Vector Probs] -> [Vector Double]
genSigma xs ms probs = undefined

--covmats  = zipWith (\xv mv -> outerOp (-) xv mv ) xs ms 
	

--x_m = head $ zipWith (\x m -> fmap (\mu -> mapVector (\e -> e - mu ) x ) m ) xs ms 


-- 1 Dim Data --
x :: [Vector Double]
x = [NL.fromList[ -0.39, 0.12, 0.94, 1.67, 1.76, 3.25, 4.12, 4.60, 5.28, 6.22 ]]

mu :: [Vector Mu]
mu = [NL.fromList[0.5,4]]

probs :: [[Vector Probs]]
probs = [[ NL.fromList [1.0e-2,2.0e-2,3.0e-2,4.0e-2,5.0e-2,6.0e-2,7.0e-2,8.0e-2,9.0e-2,0.1], NL.fromList [0.99,0.98,0.97,0.96,0.95,0.94,0.9299999999999999,0.92,0.91,0.9]]]




-- 2 Dim Data --

x2 :: [Vector Double]
x2 = [NL.fromList[ -0.39, 0.12, 0.94, 1.67, 1.76, 3.25, 4.12, 4.60, 5.28, 6.22 ], NL.fromList[ -0.39, 0.12, 0.94, 1.67, 1.76, 3.25, 4.12, 4.60, 5.28, 6.22 ]]

mu2 :: [Vector Mu]
mu2 = [NL.fromList[0.5,4],NL.fromList[0.5,4]]


probs2 :: [[Vector Probs]]
probs2 =[[ NL.fromList [1.0e-2,2.0e-2,3.0e-2,4.0e-2,5.0e-2,6.0e-2,7.0e-2,8.0e-2,9.0e-2,0.1], NL.fromList [0.99,0.98,0.97,0.96,0.95,0.94,0.9299999999999999,0.92,0.91,0.9]], [ NL.fromList [1.0e-2,2.0e-2,3.0e-2,4.0e-2,5.0e-2,6.0e-2,7.0e-2,8.0e-2,9.0e-2,0.1], NL.fromList [0.99,0.98,0.97,0.96,0.95,0.94,0.9299999999999999,0.92,0.91,0.9]]]
-}























