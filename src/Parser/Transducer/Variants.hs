{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
---------------------------------------------------------------------------------------------------
-- | Deterministic Finite State Transducer 
-- | Nondeterministic Finite State Acceptor
-- | Deterministic Finite State Acceptor 

-- | Note there are no functions that convert between NFST, FST, NFSA and FSA
---------------------------------------------------------------------------------------------------

module Variants (

	  FST 
	, NFSA
	, FSA

	, toFST
	, toNFSA
	, toFSA

	, runNFST
	, runNFST'
	, runFST
	, runNFSA
	, runFSA

	, autoT     -- * exported for now

) where

import Control.Monad
import Control.Monad.Trans

import Data.Machine
import Data.Monoid
import Data.Foldable

import Type
import Utils
import Transducer


------------------------------
---------- Data Type ---------
------------------------------

type FST f a b = NFST f a b

type NFSA f a  = NFST f a ()

type FSA f a   = NFSA f a


-----------------------------
--------- Constructor ------- 
-----------------------------

toFST :: (Foldable f, Functor f, Monoid (f a), Monoid (f b)) => FSTtable f a b -> FST f a b 
toFST t = toNFST $ \s -> toList . t s


toNFSA :: (Foldable f, Functor f, Monoid (f a), Monoid (f ())) => NFSAtable f a -> NFSA f a
toNFSA t = toNFST $ \s -> toTuple . t s


toFSA :: (Foldable f, Functor f, Monoid (f a), Monoid (f ())) => FSAtable f a -> FSA f a
toFSA t = toNFST $ \s -> toTuple . toList . t s 



----------------------------
------------ Run ----------- 
----------------------------

-- | Lifts a Transducer to a ProcessT
-- | Note this is similar to auto function for Automata class in the Data.Machine.Process package
-- | But its instance cannot be implemented due to type signature difference
autoT :: NFST f a b -> ProcessT [] (f a) (f b)
autoT = construct . plan where
    plan m = do 
        a <- await
        case step m a of
            []  -> plan m  			-- * note how this keeps on running until success
            xs  -> do
                (b,_,m') <- lift xs
                yield b
                plan m'



runNFST' :: Foldable f => NFST f a b -> [f a] -> [[f b]]
runNFST' t as = runT $ source as ~> (autoT t)


runNFST :: (Functor f, Monoid (f a), Monoid (f b)) => NFSTtable f a b -> [(f a)] -> [[f b]]
runNFST t as = runT $ source as ~> (autoT . toNFST $ t)



runFST :: (Foldable f, Functor f, Monoid (f a), Monoid (f b)) => FSTtable f a b -> [f a] -> [f b]
runFST t as = head . runT $ source as ~> (autoT . toFST $ t)



runNFSA :: (Foldable f, Functor f, Monoid (f a), Monoid (f ())) => NFSAtable f a -> [f a] -> [Bool]
runNFSA = undefined


runFSA :: (Foldable f, Functor f, Monoid (f a), Monoid (f ())) => FSAtable f a -> [f a] -> Bool
runFSA = undefined



-------------------------------
------------- Utils ----------- 
-------------------------------


toTuple :: Monoid (f a) => [Fstate] -> [(f a, Fstate)]
toTuple = fmap (\s -> (mempty, s))


------------------------------
------------- test ----------- 
------------------------------

t1 :: NFSAtable Maybe String 
t1 (I 0) (Just "a") = [F 1]
t1 (I 0) Nothing    = [I 0]
t1 (F 1) (Just "b") = [I 0, F 1]
t1 _ _ 				= []


t2 :: FSAtable Maybe String
t2 (I 0) Nothing    = Just . F $ 1
t2 (I 0) (Just "a") = Just . I $ 0
t2 _ _ 				= Nothing



m2f :: NFSTtable Maybe String String
m2f (I 0) (Just "_1") = [(Just "0_1 ", (F 1))]
m2f (I 0) (Just "_0") = [(Just "0_0 ", (I 0))]
m2f (F 1) (Just "_0") = [(Just "1_0 ", (I 0))]
m2f (F 1) (Just "_1") = [(Just "1_1 ", (F 1))]
m2f _ _ 		      = []
















