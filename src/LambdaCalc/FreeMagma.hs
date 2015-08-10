{-# Language RankNTypes, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, DeriveFunctor, ExistentialQuantification, GADTs #-}

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- * Author : Xiao Ling
-- * Date   : May 18th 2014
-- * File   : Learn free
-- * Source : https://drive.google.com/file/d/0B51SFgxqMDS-NDBOX0ZDdW52dEE/edit
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

import Data.Monoid
import Control.Applicative
import Control.Monad
import Data.List
import Data.Either

{-----------------------------------------------------------------------------
 I. Typeclass
 ------------------------------------------------------------------------------}

class Magma m where
  o :: m -> m -> m

instance Magma Int where
  o = (+)

instance Magma Integer where
  o = (+)

{-----------------------------------------------------------------------------
 II. Typeclass Associated Combinator
------------------------------------------------------------------------------}
  
accum :: Magma m => m -> [m] -> m
accum = foldr o 

{-----------------------------------------------------------------------------
 III. Free Magma datatype
------------------------------------------------------------------------------}

data FreeMagma a = Var a | Tree (FreeMagma a) (FreeMagma a)
	deriving (Show, Functor)

instance Magma (FreeMagma a) where
	o = Tree

-- * Interpreter and example use
interpFreeMagma :: Magma b => (a -> b) -> FreeMagma a -> b
interpFreeMagma f (Var a)    = f a
interpFreeMagma f (Tree a b) = interpFreeMagma f a `o` interpFreeMagma f b

cencode :: String -> Int
cencode "a" = 1
cencode "b" = 2
cencode []  = 0

-- * free magma example
magX  = Var "a" `Tree` Var "b" 				 :: FreeMagma String
tmagX = interpFreeMagma cencode magX	     :: Int


{-----------------------------------------------------------------------------
 IV. Free Monoid datatype
------------------------------------------------------------------------------}

instance Monoid Integer where
	mappend  = (+)
	mempty   =  0

instance Monoid Int where
	mappend  = (+)
	mempty   =  0

-- * note this doesn't satisfy TreeM MEmpty a == a
data FreeMonoid a = VarM a | MEmpty | (FreeMonoid a) `TreeM` (FreeMonoid a)
	deriving (Functor,Show)

instance Monoid (FreeMonoid a) where
	mappend = TreeM
	mempty  = MEmpty

interpFreeMon1 :: Monoid b => (a -> b) -> FreeMonoid a -> b
interpFreeMon1 f m = case m of
	VarM a      -> f a
	MEmpty      -> mempty
	TreeM a1 a2 -> interpFreeMon1 f a1 <> interpFreeMon1 f a2

fm1  = TreeM (VarM "a") MEmpty   	:: FreeMonoid [Char]
fm2  = TreeM MEmpty (VarM "a")      :: FreeMonoid [Char]     -- * note VarM "a" /= "a"
fm3  = TreeM (VarM "a") (VarM "b")  :: FreeMonoid [Char]
fmse = interpFreeMon1 cencode <$> [fm1,fm2,fm3]

-- * formulation with correct free mononid, the list
interpFreeMon :: Monoid b => (a -> b) -> ([a] -> b)
interpFreeMon f []     = mempty
interpFreeMon f (a:as) = f a <> interpFreeMon f as

fm1'  = (++) "a" mempty
fm2'  = (++) mempty "a"
fm3'  = (++) "a" "b"
--fmse' = interpFreeMon cencode <$> [fm1',fm2',fm3']

















































  
