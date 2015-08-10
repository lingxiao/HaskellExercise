{-# LANGUAGE ImpredicativeTypes #-} {-# LANGUAGE NoMonomorphismRestriction #-}

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Play with bad solution part iii
-- | Creator: Xiao Ling
-- | Created: November 17th
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module AppTests where

import Control.Monad
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import Data.List 
import Data.Maybe
import Data.Monoid
import Text.Show.Functions

import ParserT
import ParserCombinator
import ParserUtils

import Core2
import JType2
import SigParser2

{-----------------------------------------------------------------------------
	Core Tests
------------------------------------------------------------------------------}

a,pr,pa,pa3,pa4,pa4,lst1,lst2,tu1 :: Alpha
a   = Po . Atm (Left 'a') (J 0) $ [] 				-- * a 
pr  = Pr . Atm (Right . Dn $ "Char") (J 0) $ [] 	-- * Char
pa  = Pa f [] [a, pr] 								-- * f a Char
pa2 = Pa m [] [pa] 				    				-- * m (f a Char)
pa3 = Pa m [] [pa2] 								-- * m (m (f a Char))
pa4 = Pa f [] [pa3]
lst1 = Pa l [] [a]
lst2 = Pa l [] [pr]
tu1  = Pa t [] [a,lst2]

f,m,l,t :: Atom
f = Atm (Left 'f') (J 1) []
m = Atm (Left 'm') (J 2) []
l = Atm (Right . Dn $ "List") (J 1) []
t = Atm (Right . Dn $ "Tuple") (J 1) []

as = unAlpha <$> [a,pr,pa,pa3,pa4,pa4,lst1,lst2,tu1]
