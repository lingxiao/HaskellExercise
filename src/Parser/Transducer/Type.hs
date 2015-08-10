{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}

---------------------------------------------------------------------------------------------------
-- Weighted Finite Transducer Primitive Types
---------------------------------------------------------------------------------------------------

module Type (

	  Symbol (..)		
	, Fstate (..)
	, NFSTtable
	, FSTtable
	, NFSAtable
	, FSAtable

	, showSym				-- * consider depricating

) where

import Control.Monad
import Data.Maybe
import Data.Monoid

------------------------------
---------- Data Type ---------
------------------------------

-- | Canonical functor for symbol is type Maybe
-- | Any symbol a maps to Just a, Eps transition maps to Nothing
type Symbol a   = Maybe a

-- | A state is either an intermediate state I Int or a final state F Int
data Fstate = I Int | F Int 
	deriving (Eq, Ord)

-- | A nondeterministic transition table maps a state and input symbol to a list of output symbols and states
-- | Note the user should specify the wildcard case for this function that outputs []
-- | (Functor f, Monoid (f a), Monoid (f b)) => 
type NFSTtable f a b = Fstate -> f a -> [(f b,Fstate)]



-- | A deterministic transition table maps state and input symbol to either output tuple, or nothing
-- | Note the user should specify the wildcard case for this function that outputs Nothing
-- | (Functor f, Monoid (f a), Monoid (f b)) => 
type FSTtable f a b = Fstate -> f a -> Maybe (f b, Fstate)

type NFSAtable f a  = Fstate -> f a -> [Fstate]

type FSAtable f a   = Fstate -> f a -> Maybe Fstate

------------------------------
---------- Typeclass ---------
------------------------------

instance Show Fstate where
	show (I s) = show s
	show (F s) = show s


--------------------------------
------------- Utils ------------
--------------------------------

showSym :: Show a => Symbol a -> String
showSym (Just a) = show a
showSym Nothing  = "eps"























