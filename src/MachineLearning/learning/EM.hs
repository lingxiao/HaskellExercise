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

expMaxAlgo :: Int -> Matrix Data -> [Matrix Prob] -> [Matrix Prob]
expMaxAlgo r x label = runAlgo r 1 ( rows x ) x seed label 
	where
		seed = let ms = fromLists [[1]] in (ms,ms,ms)

		runAlgo r incr m x param@(phis, mus, sigs) label 
			| even incr && incr == r = label
			| odd incr  = runAlgo r ( incr + 1 ) m x ( maxStep ( fromIntegral m ) x label ) label
			| otherwise = runAlgo r ( incr + 1 ) m x param $ expStep m x mus sigs phis


-- | Expectation step | --

-- | Calculate posterior probabiliy P(zj|xi) for all data points m in all dimension d for all gaussians
-- | @Param:  m, row dim of label matrix
-- | @Param:  m x d matrix of data
-- | @Param:  d x k matrix of means
-- | @Param:  d x k matrix of stdevs
-- | @Param:  d x k matrix of phis
-- | @Output: list of length-k of m x d of P(zj|xi) labels
expStep :: Int -> Matrix Data -> Matrix Mu -> Matrix Sigma -> Matrix Phi -> [Matrix Prob]
expStep m x mus sigs phis = zipWith (\pzx phi -> genPzx m px pzx phi ) pxzs ( toColumns phis )
	where
		normals = genNormal mus sigs
		pxzs    = fmap ( genPxz x ) normals
		px      = genPx m pxzs phis


-- | Calculate posterior probabiliy P(zj|xi) for all data points m in all dimension d for jth gaussian
-- | Recall:  P(zi|xj) = P(xi|zj)P(zj)/P(xi)
-- | @Param:  m, row dim of label matrix
-- | @Param:  m x d matrix of labels probs, P(xi|zj) for one gaussian
-- | @Param:  m x d matrix of P(xi) 
-- | @Param:  d x 1 vector of phis, or P(zj) for one gaussian
-- | @Output: m x d of P(zj|xi) for jth gaussian
genPzx :: Int -> Matrix Prob -> Matrix Prob -> Vector Phi -> Matrix Prob
genPzx m px pxz phi = ( genPxz_z m pxz phi ) / px 


-- | Calculate probabiliy P(xi|zj) for all data points m in all dimension d for jth gaussian
-- | @Param:  m x d matrix of data
-- | @Param:  d x 1 list of gaussians
-- | @Output: m x d matrix of labels probs, P(xi|zj) for jth gaussian
genPxz :: Matrix Data -> [NormalDistribution] -> Matrix Prob
genPxz x ns = fromRows $ fmap fromList $ fmap (\v -> zipWith (\x n -> density n x) v ns ) $ fmap toList $ toRows xmat 


-- | Calculate pobability P(xi) 
-- | @Param:   list of length-k of m x d P(xi|zj)
-- | @Param:   d x k matrix of Phis, or P(zj)
-- | @Output:  m x d prob of P(xi)
genPx :: Int -> [Matrix Prob] -> Matrix Phi -> Matrix Prob
genPx m pxzs phis = let pxs' = zipWith (\pxz phi -> genPxz_z m pxz phi ) pxzs $ toColumns phis in foldr (+) ( head pxs' ) ( tail pxs' )


-- | Calculate pobability P(xi|zj)P(zj) for one gaussian j
-- | @Param:  m, row dim of label matrix
-- | @Param:  m x d matrix of labels probs, P(xi|zj) for jth gaussian
-- | @Param:  d x 1 vector of phis, or mixing proportions, or P(zj)
-- | @Output: m x d matrix of P(xi|zj)P(zj)
genPxz_z :: Int -> Matrix Prob -> Vector Phi -> Matrix Prob
genPxz_z m pxz phi = pxz * ( repmat ( fromRows [phi] ) m 1 ) 


-- | Create Gussian functions given mean and stdev for all k gaussians in all dimension d
-- | @Param:  k x d matrix of means
-- | @Param:  k x d matrix of variances
-- | @Output: k x d matrix of normal functions, represented as k-length list of d-length lists
genNormal :: Matrix Mu -> Matrix Sigma -> [[NormalDistribution]]
genNormal mus sigs = zipWith (\ms ss -> zipWith (\m s -> normalDistr m s ) ms ss ) mus' sigs'
	where [ mus', sigs' ] = fmap (\m -> fmap toList $ toColumns m ) [ mus, sigs ]



-- | Maximization step | --

-- | Maximiztion step of algorithm
-- | @Param:  m number of data points
-- | @Param:  m x d data matrix
-- | @Param:  m x d label matrix
-- | @Output: triple of d x k matrices, so that each vector characterize one gaussian in all dimensions
maxStep :: Double -> Matrix Data -> [Matrix Prob] -> (Matrix Phi, Matrix Mu, Matrix Sigma)
maxStep m x labels = ( fromColumns phis, fromColumns mus, fromColumns sigs )
	where
		pzs     	  = fmap genSumPhi labels 
		phis    	  = fmap (\pz -> mapVector (\p -> p / m ) pz ) pzs
		( mus, sigs ) = foldr (\(m, s) (ms, ss) -> (m:ms, s:ss) ) ([],[]) muSig
		muSig         = zipWith (\label pz -> let mu = genMean xmat label pz in ( mu, genVar xmat mu label pz ) ) labels pzs



-- | Find the mixing proportion of one gaussian distribution 
-- | @Param:  m x d label matrix
-- | @Output: d x 1 vector of sumed phis, need to divide by m to get phis
genSumPhi :: Matrix Prob -> Vector Double
genSumPhi label = foldCol (+) 0 label


-- | Find the mean of a gaussian mixture
-- | @Param:  m x d sample matrix 
-- | @Param:  m x d label matrix P(zj|xi) for one j in {1..k}
-- | @Param:  m x 1 vector of labels P(zj) for one cluster j
-- | @Output: d x 1 mean vector
genMean :: Matrix Data -> Matrix Prob -> Vector Prob -> Vector Mu
genMean x label pz = foldCol (+) 0 (x * label) / pz


-- | Find variance vector of a gaussian mixture
-- | @Param:  m x d sample matrix
-- | @Param:  d x 1 mean vector of gaussian
-- | @Param:  m x d label matrix
-- | @Param:  m x 1 vector of labels P(zj) for one j for all xi in X
-- | @Output: d x 1 variance vector
genVar :: Matrix Data -> Vector Mu -> Matrix Prob -> Vector Prob -> Vector Sigma
genVar x mu label pz = ( takeDiag $ genCovmat x mu label ) / pz


-- | Find covariance matrix of a gaussian mixture
-- | @Param:  m x d sample matrix
-- | @Param:  1 x d mean vector of gaussian
-- | @Param:  m x d label matrix
-- | @Output: d x d covariance matrix
genCovmat :: Matrix Data -> Vector Mu -> Matrix Prob -> Matrix Sigma
genCovmat x mu label = let x'' = (sqrt label) * x' in ( trans x'' <> x'' )
	where x'  = x - repmat ( fromRows [mu] ) ( rows x ) 1


-----------
-- Tests --
-----------


-- For convinience --
zeromat = fromColumns [zeros, zeros] :: Matrix Double
zeros = fromList [0,0,0,0,0,0,0] :: Vector Double


-- Given Data X --

-- | data point x is in R2, centered around three guassians
-- | Dimension: m x d :: 7 x 2 matrix
xmat = fromRows $ fmap fromList [[0.5,5],[1,3],[1.3,3],[3,9],[1.5,12],[11,9],[8,12]] :: Matrix Data 


-- Initialize priors @ Iter 0--

-- | Prior matrices of labels P(zj|xi) 
-- | Dim: m x d x k :: 5 x 2 x 3 matrix, or a list of length 2 of 5 x 3 matrices
labeli = fromList [1/3,1/3,1/3,1/3,1/3,1/3,1/3] :: Vector Double

label0 = fmap fromColumns [ [ labeli, labeli ],[ labeli, labeli ],[ labeli, labeli ] ] :: [ Matrix Prob ]

labeli1 = fromList [1/2,1/2,1/2,1/2,1/2,1/2,1/2] :: Vector Double
labeli2 = fromList [1/4,1/4,1/4,1/4,1/4,1/4,1/4] :: Vector Double

label0' = fmap fromColumns [[labeli1, labeli1], [labeli2, labeli2], [labeli2, labeli2]] :: [Matrix Prob]


-- Run Max @ Iter 1 -- 
m = rows xmat :: Int

iter1@(phis1,mus1,sigs1) = maxStep ( fromIntegral m ) xmat label0' :: (Matrix Phi, Matrix Mu, Matrix Sigma)


-- Run Exp @ Iter 1 --
label1 = expStep m xmat mus1 sigs1 phis1 :: [Matrix Prob]

-- run subparts for sanity check
normals = genNormal mus1 sigs1 :: [[NormalDistribution]]
pxzs    = fmap ( genPxz xmat ) normals :: [Matrix Prob]
px     = genPx ( fromIntegral m ) pxzs phis1 :: Matrix Prob


-- Run Max @ Iter 2 --
iter2@( phis2, mus2, sigs2 ) = maxStep ( fromIntegral m ) xmat label1 :: (Matrix Phi, Matrix Mu, Matrix Sigma)

-- Run Exp @ Iter2 --
label2 = expStep m xmat mus2 sigs2 phis2 :: [Matrix Prob]



trial1 = expMaxAlgo 200 xmat label0' :: [Matrix Prob]




------------------------
-- Statistics Utils --
----------------------

-- | @Param:  m x 1 vector
-- | @Param:  mean of vector
-- | @Param:  length of vector
-- | @Output: covariance of vector
covarv :: Vector Double -> Double -> Double -> Double
covarv v m l = let l' = sqrt (l-1) in let v' = mapVector (\x -> (x - m) / l') v in v' <.> v'


meanv :: (Fractional t, Storable t) => Vector t -> t
meanv v = foldVector (+) 0 v / ( fromIntegral $ dim v )



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


-- | Pairwise operation on matrices 
--   naive implementation of matlab fn of same name
bsxfun :: (Element b, Element a, Element c) => (a -> b -> c) -> Matrix a -> Matrix b -> Maybe ( Matrix c )
bsxfun g m1 m2
	| rows m1 == rows m2 && cols m1 == cols m2 = Just . fromLists $ fmap toList vs
	| otherwise 							   = Nothing
		where vs = zipWith (\v1 v2 -> zipVectorWith g v1 v2 ) ( toRows m1 ) ( toRows m2 )



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




-- | Find the mixing proportion of one gaussian distribution 
-- | @Param:  m number of data points
-- | @Param:  m x d label matrix
---- | @Output: ( d x 1 vector of phis, d x 1 vector of sumed )
--genPhi' :: Double -> Matrix Prob -> ( Vector Phi, Vector Double )
--genPhi' m label = let sumed = foldCol (+) 0 label in ( mapVector (\e -> e / m ) sumed, sumed )


---- | marginalize out xi to get vectors of P(zj)
--pz1 = foldCol (+) 0 label1 :: Vector Double 
--pz2 = foldCol (+) 0 label2 :: Vector Double

--phis = fmap ( genPhi' 5 ) labels :: [( Vector Phi, Vector Double )]

--mean1 = genMean xmat label1 pz1 :: Vector Mu
--mean2 = genMean xmat label2 pz2 :: Vector Mu

--var1 = genVar xmat mean1 label1 pz1 :: Vector Sigma
--var2 = genVar xmat mean2 label2 pz2 :: Vector Sigma
-- Unrelated to above, For Isolated testing --

-- | matrix of phis, or mixing proportion
-- | Dim: k x d :: 3 x 2
--phi = fromRows $ fmap fromList [[0.5,0.3,0.2],[0.4,0.2,0.4]] :: Matrix Phi

---- | Matrix of distribution means 
---- | Dim: k x d, 3 x 2
--means = fromColumns $ fmap fromList [ [1,4], [2,10], [9,10] ] :: Matrix Double

---- | matrices of covariances
---- | Dim:  d x d x k : 2 x 2 x 3, or list of length 2 of 2 x2 matrices
--covmat1  = fromColumns $ fmap fromList [[0.5,0.3],[0.3,0.6]] :: Matrix Double
--covmat2  = fromColumns $ fmap fromList [[0.1,0.4],[0.4,0.9]] :: Matrix Double
--covmat3  = fromColumns $ fmap fromList [[1,4],[4,9]] :: Matrix Double
--covmats = [ covmat1, covmat2, covmat3 ] :: [Matrix Double]
    
--runMany :: Int -> Int -> Matrix Data -> (Matrix Phi, Matrix Mu, Matrix Sigma) -> [Matrix Prob] -> [Matrix Prob]



{-

	On April 9th, remade algorith for 1-D for abitrary k using lists intead of vectors
	Found the 'right' workflow to crank through algorithm



-- Algorithm in function form --


runEM :: [[Data]] -> [[Sigma]] -> [[Phi]] -> [[Mu]] -> Int -> [[Mu]]
runEM x s p m iter = run iter x s m p
	where run iter x s p m
		| iter == 0     = m
		| otherwise     = runEM x s p ( mstep x $ estep x m s p ) $ iter - 1



hmm' = [1,2,3,100]   :: [Data]
hmm =  [1..20]    :: [Data]

labels1' =  estep1D hmm u0 sig phi     :: [[Prob]]    
mu1'     =  mstep1D hmm labels1       :: [Mu]

labels2' =  estep1D hmm mu1' sig phi   :: [[Prob]]    
mu2'     =  mstep1D hmm labels2'      :: [Mu]

labels3' =  estep1D hmm mu2' sig phi   :: [[Prob]]    
mu3'     =  mstep1D hmm labels3'      :: [Mu]


labels4' =  estep1D hmm mu3' sig phi   :: [[Prob]]    
mu4'     =  mstep1D hmm labels4'      :: [Mu]


-- compare w/ runEM

must = runEM [hmm] [sig] [phi] [u0]        :: Int -> [[Mu]]
list = Mo.liftM (Mo.join . must) [1,2,3,4,100] :: [[Mu]]



-- | Scale e-step up to data of arbitrary dimensions
estep :: [[Data]] -> [[Mu]] -> [[Sigma]] -> [[Phi]] -> [[[Prob]]]
estep x m s p = getZipList $ estep1D <$> ZipList x <*> ZipList m <*> ZipList s <*> ZipList p

-- | Scale m-step up to data of arbitrary dimensions
mstep :: [[Data]] -> [[[Prob]]] -> [[Mu]]
mstep x labels = zipWith (\a b -> mstep1D a b ) x labels


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
				let l = pxz_z / pxi in if l == 0 then l + 0.001 else l ) top ) px_z_zs pxis :: [[Prob]]


-- | maximazation step for 1D data for arbitrarily many gaussians
mstep1D :: [Data] -> [[Prob]] -> [Mu] 
mstep1D x labels = means
	where
		pzs       =  foldr (\a b -> zipWith (+) a b) ( head labels ) $ tail labels        :: [Double]
		label_xs  =  foldr (\a b -> zipWith (+) a b) ( head label_xss ) $ tail label_xss  :: [Prob]
		label_xss =  zipWith (\label x -> fmap (\l -> x * l) label) labels x    		  :: [[Prob]]
		means     =  zipWith (\t b -> t/b) label_xs pzs  							      :: [Mu]


-----------
-- Tests --
-----------


-- Init Params for 2-D -- 

xs2    = [[1..100],[100..200]] 				    :: [[Data]]
xs2'   = [[1,2,1.5,12,9,10],[1,2,1.5,12,9,10]]  :: [[Data]]
sig2   = [[1,1], [1,1]]                         :: [[Sigma]]
pz2    = [[0.5,0.5],[0.5,0.5]]                  :: [[Phi]]
u20    = [[3,6],[3,6]]                          :: [[Mu]]



-- EM for 100 steps in 2D data --
muf2d = runEM xs2 sig2 pz2 u20 100 :: [[Mu]]


-- Init Params in 1D -- 

xs  = [1..100] 		 :: [Data]

xs'phi   = [1,2,1.5,12,9,10] :: [Data]
sig  = [1,1]             :: [Sigma]
phi   = [0.5,0.5]        :: [Phi]
u0   = [3,6]             :: [Mu]


-- E step --
normal1   = zipWith (\m s -> normalDistr m s ) u0 sig            			         :: [ NormalDistribution ]
-- P(xi|zj)
px_z1     = fmap (\x -> fmap (\n -> density n x ) normal1 ) xs   			         :: [[Prob]]
-- P(xi|zj)P(zj)
px_z_zs1  = fmap (\px_z -> zipWith (*) px_z phi ) px_z1			   			         :: [[Prob]]
-- P(xi) = Sum ( j=1, j=k) P(xi|zj)P(zj)
pxis1     = fmap (\px_n_pz -> foldr (+) 0 px_n_pz ) px_z_zs1         			     :: [Prob]
-- P(zj|xi)
labels1   = zipWith (\tophp pxi -> fmap (\pxz_z -> pxz_z / pxi ) top) px_z_zs1 pxis1'  :: [[Prob]]
	where pxis1' = fmap (\e -> if e == 0 then e + 0.00001 else e ) pxis1

-- M step --

-- P(zj) = Sum (i=1,i=m) P(zj|xi) P(xi)
pzs1     =  foldr (\a b -> zipWith (+) a b) ( head labels1 ) $ tail labels1       :: [Double]
-- P(zj|xi)xi
label_xs =  foldr (\a b -> zipWith (+) a b) ( head label_xss ) $ tail label_xss	  :: [Prob]
	where label_xss = zipWith (\label x -> fmap (\l -> x * l) label) labels1 xs   

-- ML mean -
u1   =   zipWith (\t b -> t/b) label_xs pzs1  					      :: [Mu]

-- ML phi --
phi1 = let g a = a / ( fromIntegral . length $ xs ) in fmap g pzs1    :: [Phi]

-- ML sigma --

sig1 =  foldr (\a b -> zipWith (+) a b ) ( head covm ) $ tail covm  :: [Sigma]
	where 
		covm = let zipmult = (zipWith . zipWith) (*) in zipmult px_z1 $ zipmult xs_m xs_m    
		xs_m = let normalize means e = fmap (\m -> e - m ) means in fmap ( normalize u1 ) xs 



-- Try EM manually for two steps in 1D, actually converges very quickly --  
labels1 =  estep1D xs u0 sig phi   :: [[Prob]]    
mu1     =  mstep1D xs labels1      :: [Mu]

labels2 = estep1D xs mu1 sig phi   :: [[Prob]]
mu2     = mstep1D xs labels2       :: [Mu]


-- EM for 100 steps in 1D data yields somewhat different answer as 2 steps --
muf1d  = Mo.join $ runEM [xs] [sig] [phi] [u0] 100 :: [Mu]

-}












