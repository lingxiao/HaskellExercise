{-# LANGUAGE TypeSynonymInstances #-} {-# LANGUAGE FlexibleInstances #-} {-# LANGUAGE GeneralizedNewtypeDeriving #-}

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Types shared between different parts of application
-- | Creator: Xiao Ling
-- | Created: November 21st
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Core (

	-- * Primitive Types

	  Error
	, FnName
	, DataName
	, Signature
	, ClassName
	, ClassPred
	, I (..)
	, J (..)
	, Kind

	-- * Parsing 

	, Sym   (..)
	, Alpha (..)
	, Beta
	, SigTree
	, EqMatrix

	-- * JS Typeclass

	, ClassEnv
	, AdtEnv

	-- * Primitive Functions

	, k0, k1
	, ppTree
	, uAlpha

	-- * Project Utils
	, listToEither
	, maybeToEither

) where 

import Data.Monoid
import Control.Monad
import Control.Applicative
import Text.Show.Functions
import qualified Data.Set as S

-- * TODO: impedance between Haskell Prims and JS Prims
-- * TODO: come up with more sensible names
-- * TODO: combe up with some unified way to do errors, right now its' all over the place

{-----------------------------------------------------------------------------
   I. Primitives Types
------------------------------------------------------------------------------}

type Error     = String					
type FnName    = String 
type DataName  = String 			  -- * Name of js data type
type Signature = String 			  -- * unparsed function signature
type ClassName = String 			  

type ClassPred = [(ClassName, Kind)]  -- * List of existing classes and their kinds

newtype I    = I Int deriving (Eq, Ord, Num, Enum, Show)    -- * location of a particular `Alpha` within the `SigTree`
newtype J    = J Int deriving (Eq, Ord, Num, Enum, Show)    -- * location of a `Sym` within an `Alpha`. Ie, in "f (m a)", a is 0 and f is 2
newtype Kind = K Int deriving (Show, Eq, Ord)   	        -- * * or * -> * or * -> * -> * or ..

{-----------------------------------------------------------------------------
   II. Parsing
------------------------------------------------------------------------------}

-- * Sym represents a polymorphic type indexed within a larger nested form at level Y
-- * It carries some context where data type represented by `Char` maybe drawn from some set typeclasses of some `Kind`s
data Sym = Sy Char J ClassPred deriving (Show, Eq)

-- * NOTE: list and tuple are just sguared adts!!
-- * they should prob be put into wildcards!!!!
-- * this means SigParser is going to change again!! :(

data Alpha    
	= Po Sym    				     -- * polymorphic type, maybe unparametrized typeclass  					   "a" or "Monoid m => m"
	| Pr DataName 			         -- * pre-defined set of primitive type          						    					   "Bool"
	| Lst Alpha 			         -- * list				   							    					   "[a]" 
	| Tup Alpha Alpha 			     -- * tuple				   							    					   "(a,b)"
	| Pa Sym ClassPred Alpha         -- * paramed type where the whole nested form maybe drawn from some typeclass "(Monoid (t (m a)), Monad m) => t (m a)"
	| Ad DataName ClassPred [Alpha]  -- * algebraic DataName of arbitrary kind. if the adt is parameterized, the whole DataName maybe drawn from some typeclass
	deriving (Show, Eq) 		


-- * Annotate each paramter with its location. Additionally, parameters that are functions are not parsed.  
type Beta = (I, Either Signature Alpha)

-- * SigAlpha is either some unparsed string, or fully parsed `Alpha`
-- * It is an unbalanced tree where the structure encodes proper form of function currying up to last element of list
type SigTree = [Beta]

-- * Jth DataName in Ith parameter has the same type as Jth' type in Ith' parameter 
type EqMatrix = [(,) (I,J) (I,J)]  	

{-----------------------------------------------------------------------------
   III. JS Typeclass
------------------------------------------------------------------------------}

type ClassEnv a = [(ClassName, a)]


type AdtEnv a = [a]

{-----------------------------------------------------------------------------
	IV. Functions over Data Types
------------------------------------------------------------------------------}

-- * Only nonparamterized and singly parameterized types are supported
k0, k1 :: Kind 
k0 = K 0
k1 = K 1

-- * `uAlpha . pAlpha` is identity of a single `Singature` alphabet
uAlpha :: Alpha -> Signature
uAlpha a = let unSym (Sy c _ _) = c in case a of 
	Pr p     -> p 
	Po c     -> pure . unSym $ c
	Lst a    -> "[" ++ uAlpha a ++ "]"
	Tup a b  -> "(" ++ uAlpha a ++ "," ++ uAlpha b ++ ")"
	Pa c _ s -> case s of 
		Pa _ _ _ -> pure (unSym c) ++ " (" ++ uAlpha s ++ ")"
		_        -> pure (unSym c) ++ " " ++ uAlpha s 
	Ad n _ as -> init $ n ++ " " ++ foldr (\a as -> uAlpha a ++ " " ++ as) "" as


ppTree :: SigTree -> IO ()
ppTree = mapM_ print

{-----------------------------------------------------------------------------
	Project Utils
------------------------------------------------------------------------------}

listToEither :: a -> [b] -> Either a b 
listToEither a m = case m of 
	[]   -> Left a
	x:xs -> Right x

maybeToEither :: a -> Maybe b -> Either a b 
maybeToEither a m = case m of 
	Nothing -> Left a 
	Just b  -> Right b

