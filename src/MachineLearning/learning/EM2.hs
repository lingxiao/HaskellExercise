{- 

Created: April 9th 2013 

Based on lectures:
http://cs229.stanford.edu/notes/cs229-notes7b.pdf
https://alliance.seas.upenn.edu/~cis520/wiki/index.php?n=Lectures.EM
http://cs229.stanford.edu/notes/cs229-notes8.pdf
http://ai.stanford.edu/~chuongdo/papers/em_tutorial.pdf

Documention used: 
statistics package:
http://hackage.haskell.org/packages/archive/statistics/0.10.2.0/doc/html/Statistics-Distribution-Normal.html
matrix library:
http://hackage.haskell.org/packages/archive/hmatrix/0.14.1.0/doc/html/Data-Packed-Matrix.html
sample:
http://hackage.haskell.org/packages/archive/StatisticalMethods/0.0.0.1/doc/html/src/Statistics-EM-TwoGaussian.html


Problems encountered:
1. Don't understand fundamental statistical concepts enough to chunk the info 
2. Don't understand problem domain enough to find appropriate data structure
3. Poor abstraction leads to mixing of domain logic w/ implementation logic, producing complex code
4. Didn't try to write expressions for steps of simplest possible example and know then make sure
   it gets the correct answer, then try to scale up

-}


module ML.Learning.EM where

import Statistics.Distribution
import Statistics.Distribution.Normal
import Numeric.LinearAlgebra as NL
import Foreign.Storable
import Data.Maybe
import Control.Applicative

import qualified Control.Monad as Mo
import qualified Data.Map as M
import qualified Data.List as L



----------------
-- Data Types --
----------------

-- | value of data
type Data  = Double
-- | mean
type Mu    = Double
-- | standard deviation
type Sigma = Double
-- | mixing proportion
type Phi   = Double
-- | probability
type Prob  = Double


------------------
-- EM algorithm --
------------------

{-
	Index Conventions:
	k := number of Gaussians
	j := jth Gaussian

	m := number of samples
	i := ith sample

	d := number of dimensions of data


	The posterior probability matrix encoding P(zj|xi) is of dimension: m x d x k

	Note: all alogorithms work for data in Rn for one gaussians only
	To calculate for all gaussians, just map functions onto list of all gaussians


	Problem: Algorithm does not change the seed labels for any number iterations

-}

-- | Algorithm Main | --

-- | Note: Consider refactoring using a fixed-point combinator to find fix point of EM function
runEM :: [[Data]] -> ( [[Mu]], [[Sigma]], [[Phi]] ) -> Int -> ([[Mu]], [[Sigma]], [[Phi]])
runEM x ret@(m, s, p) iter
	| iter == 0 = ret
	| otherwise = runEM x ( mstep x $ estep x m s p ) $ iter - 1


-- | Arbitrary dimensions | --

estep :: [[Data]] -> [[Mu]] -> [[Sigma]] -> [[Phi]] -> [[[Prob]]]
estep x m s p = getZipList $ estep1D <$> ZipList x <*> ZipList m <*> ZipList s <*> ZipList p

mstep :: [[Data]] -> [[[Prob]]] -> ([[Mu]], [[Sigma]], [[Phi]])
mstep x labels = foldr (\(m, s, p) (ms, ss, ps) -> (m:ms, s:ss, p:ps) ) ([], [], []) $ zipWith (\a b -> mstep1D a b ) x labels



-- | 1 Dimensional Data Only | --

-- | expectation step for 1D data for arbitrarily many gaussians
estep1D :: [Data]-> [Mu] -> [Sigma] -> [Phi] -> [[Prob]]
estep1D x m s p = labels
	where 
		normal  = zipWith (\m s -> normalDistr m s ) m s            			            :: [ NormalDistribution ]
		px_z    = fmap (\x -> fmap (\n -> density n x ) normal ) x   			            :: [[Prob]]
		px_z_zs = fmap (\px_z -> zipWith (*) px_z p ) px_z			   			            :: [[Prob]]
		pxis    = fmap (\px_n_pz -> 
			let a = foldr (+) 0 px_n_pz in if a == 0 then a + 0.0001 else a ) px_z_zs       :: [Prob]
		labels  = zipWith (\top pxi -> 
			fmap (\pxz_z -> 
				let l = pxz_z / pxi in if l == 0 then l + 0.0001 else l ) top ) px_z_zs pxis :: [[Prob]]


mstep1D :: [Data] -> [[Prob]] -> ( [Mu], [Sigma], [Phi] ) 
mstep1D x labels = ( means, sigmas, phis )
	where
		pzs       =  foldr (\a b -> zipWith (+) a b) ( head labels ) $ tail labels        :: [Double]
		label_xs  =  foldr (\a b -> zipWith (+) a b) ( head label_xss ) $ tail label_xss  :: [Prob]
		label_xss =  zipWith (\label x -> fmap (\l -> x * l) label) labels x    		  :: [[Prob]]
		means     =  zipWith (\t b -> t/b) label_xs pzs  							      :: [Mu]
		
		phis      =  let g a = a / ( fromIntegral . length $ xs ) in fmap g pzs 		  :: [Phi]
		
		xs_m      = let normalize means e = fmap (\m -> e - m ) means in fmap ( normalize means ) x :: [[Data]]
		covm      = let zipmult = (zipWith . zipWith) (*) in zipmult labels $ zipmult xs_m xs_m     :: [[Sigma]]
		sigmas    = foldr (\a b -> zipWith (+) a b ) ( head covm ) $ tail covm						:: [Sigma]



-----------
-- Tests --
-----------

-- Init Params for x in 2-D -- 


xs2    = [[1,2,1.5,12,9,10],[1,2,1.5,12,9,10]]     :: [[Data]]
u20    = [[1,10],[1,10]]                           :: [[Mu]]

sig2   = [[1,1], [1,1]]                         :: [[Sigma]]
pz2    = [[0.5,0.5],[0.5,0.5]]                  :: [[Phi]]



stub = runEM xs2 (u20, sig2, pz2)   ::  Int -> ([[Mu]], [[Sigma]], [[Phi]])
(u2d, sig2d, phi2d) = stub 100		:: ([[Mu]], [[Sigma]], [[Phi]])
list = fmap stub [1,2,5,10,100]		:: [ ([[Mu]], [[Sigma]], [[Phi]])]



-- Init Params for x in 1D -- 

-- Observation -> Where the initial mu_js are set really matters! 
-- If no mu_j is in same order of magnititude as data, algorithm will 'think' data comes from 
-- no guassians_j for all j in {1..k}

xs'  = [1..100] 		  :: [Data]
u0'  = [1,90]            :: [Mu]

xs   = [1,2,1.5,12,9,10] :: [Data]
u0   = [1,6]             :: [Mu]

xs''   = [1,2,1.5,12,9,10, 100, 80, 120, 97]  :: [Data]
u0''   = [1,90]			  :: [Mu]


sig  = [1,1]              :: [Sigma]
phi  = [0.5,0.5]          :: [Phi]


-- E step --
-- N(uj, sigma_j) 
normal1   = zipWith (\m s -> normalDistr m s ) u0 sig            			         :: [ NormalDistribution ]
-- P(xi|zj)
px_z1     = fmap (\x -> fmap (\n -> density n x ) normal1 ) xs   			         :: [[Prob]]
-- P(xi|zj)P(zj)
px_z_zs1  = fmap (\px_z -> zipWith (*) px_z phi ) px_z1			   			         :: [[Prob]]
-- P(xi) = Sum ( j=1, j=k) P(xi|zj)P(zj)
pxis1     = fmap (\px_n_pz -> foldr (+) 0 px_n_pz ) px_z_zs1         			     :: [Prob]
-- P(zj|xi) = P(xi|zj)P(zj) / P(xi)
labels1   = zipWith (\top pxi -> fmap (\pxz_z -> pxz_z / pxi ) top) px_z_zs1 pxis1'  :: [[Prob]]
	where pxis1' = fmap (\e -> if e == 0 then e + 0.00001 else e ) pxis1

-- M step --

-- P(zj) = Sum (i=1,i=m) P(zj|xsi) P(xi)
pzs1     =  foldr (\a b -> zipWith (+) a b) ( head labels1 ) $ tail labels1       :: [Double]
-- P(zj|xi)xi
label_xs =  foldr (\a b -> zipWith (+) a b) ( head label_xss ) $ tail label_xss	  :: [Prob]
	where label_xss = zipWith (\label x -> fmap (\l -> x * l) label) labels1 xs   

-- ML mean --
-- uj = (Sum (i = 1, i = m ) P(zj|xi) p(xi) )/ P(zj)
u1   =   zipWith (\t b -> t/b) label_xs pzs1  					      :: [Mu]

-- ML phi --
-- phi_j = ( Sum (i=1, i=m) P(zj|xi) ) / m
phi1 = let g a = a / ( fromIntegral . length $ xs ) in fmap g pzs1    :: [Phi]

-- ML sigma --
-- Sigma_j
sig1 =  foldr (\a b -> zipWith (+) a b ) ( head covm ) $ tail covm     :: [Sigma]
	where 
		covm = let zipmult = (zipWith . zipWith) (*) in zipmult labels1 $ zipmult xs_m xs_m    
		xs_m = let normalize means e = fmap (\m -> e - m ) means in fmap ( normalize u1 ) xs 



