
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleContexts #-}  
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | Module : Mock Application that create typeclass in javascript Test
-- | Creator: Xiao Ling
-- | Created: October 26th
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------


import Control.Monad
import Control.Monad.Reader
import Control.Applicative

import Data.Monoid
import Data.Maybe
import Data.List

import Control.Monad.State
import Text.Show.Functions
import Core
import Strategy


-- * an object with j value and j function
o :: JType [KV]
o = O [KV ("TYPE", S "Maybe"), KV ("v", N 12), KV ("f", F f)]
	where f (N n) = N $ n + 100
