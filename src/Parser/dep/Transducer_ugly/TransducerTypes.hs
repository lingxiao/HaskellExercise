{-# LANGUAGE MultiParamTypeClasses #-}

---------------------------------------------------------------------------------------------------
-- Weighted Finite Transducer Primitive Types
-- A work in progress: lots of functions don't work, need be better thought out
-- suggestion: only interact at graph level, not arc and graph level

-- Symbol is concrete
-- Graph and Arc are abstract
---------------------------------------------------------------------------------------------------

module TransducerTypes (

	-- * Primitives

	Symbol (..),
	Input,
	Output,
	Tstate,
	InitSt,
	FinalSt,
	Prob,

	-- * User Specified format

	TransitionCell,
	TransitionTable,
	toProbTable,
	toTable,
	epsArc,

	-- * global test symbols -> to be deleted
	Alpha(..), Beta (..)

) where

import Control.Monad
import Data.Maybe

----------------------------------------------------
----------------- Transducer Class -----------------
----------------------------------------------------


{-
class Automata a b where
	runA :: 
	evalA ::
	execA ::

-}

-------------------------------------------
----------------- Symbols -----------------
-------------------------------------------

-- | Symbol a = Sym a + EpsilonTransition
data Symbol a = Sym a | Eps deriving (Eq, Ord)

-- | Symbol synonyms 
type Input  a = Symbol a
type Output b = Symbol b


instance (Show a) => Show (Symbol a) where
	show s = case s of 
		Eps   -> "Epsilon"
		Sym a -> show a


---------------------------------------------------
----------- Probability and State Label -----------
---------------------------------------------------

-- | The Prob of edge is a double 0.0 <= w <= 1.0
type Prob  = Double

-- | State lable is represented as list of ints
-- | upon construction, each state label should be list of one int
-- | after n compositions, each label is list of n ints
type Tstate   = Int
type InitSt   = Tstate 
type FinalSt  = Tstate 


----------------------------------------------------
------------ User specified WFST format ------------ 
----------------------------------------------------


type TransitionCell  a b = (Int, Int, Symbol a, Symbol b, Prob)
type TransitionTable a b = [TransitionCell a b]


toProbTable :: [(Int,Int,a,b,Prob)] -> TransitionTable a b
toProbTable = fmap (\(s1,s2,a,b,p) -> (s1,s2,Sym a, Sym b,p))


toTable :: [(Int, Int,a,b)] -> TransitionTable a b 
toTable = fmap (\(s1,s2,a,b) -> (s1,s2,Sym a, Sym b,1.0))

-- | distinguished arc
epsArc :: Tstate -> Tstate -> TransitionCell a b
epsArc s1 s2 = (s1,s2, Eps, Eps, 1)





-- | used for tests

data Alpha = A | B | C | D  deriving (Eq,Ord,Show)
data Beta  = AA | BB | CC | DD   deriving (Eq,Ord,Show)










