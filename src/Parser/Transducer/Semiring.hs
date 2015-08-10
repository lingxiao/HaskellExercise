{-# LANGUAGE TemplateHaskell #-}

---------------------------------------------------------------------------------------------------
-- | A Class for Semiring 

--  A system (K, .+., .*.,zero,one) is a semiring if: 
--		(K, .+.,zero) is a commutative monoid with identity element zero; 
--		(K, .*.,one) is a monoid with identity element one; .*. distributes over .+.; 
--		zero is an annihilator for .*.: for all a ∈ K, a .*. zero = zero .*. a = zero.

--  Source: http://hackage.haskell.org/packages/archive/weighted-regexp/0.1.0.0/doc/html/Data-Semiring.html
--  Another application of semiring: http://www.cs.nyu.edu/~mohri/pub/jalc.pdf
---------------------------------------------------------------------------------------------------

module Semiring where


infixr 6 .+.
infixr 7 .*.


class Semiring a where
	zero, one    :: a
	(.+.), (.*.) :: a -> a -> a



-- Semiring instances

instance Semiring Int where
	zero  = 0
	one   = 1
	(.+.) = (+)
	(.*.) = (*)


instance Semiring Bool where
	zero  = False
	one   = True
	(.+.) = (||)
	(.*.) = (&&)








