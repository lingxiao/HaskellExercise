{-# LANGUAGE TypeSynonymInstances #-} {-# LANGUAGE FlexibleInstances #-} {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-} {-# LANGUAGE KindSignatures #-} {-# LANGUAGE GADTs #-}

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Core attempt 2
-- | Creator: Xiao Ling
-- | Created: December 3rd
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- * Think about how to export less datatypes

module Core2 (

	-- * Primitive Types 

	  Error
	, Signature
	, FnName (..)
	, DataName (..)
	, ClassName (..)
	, Kind (..)
	, I (..)
	, J (..)

	-- * Parser and Analyer

	, Atom (..)
	, Alpha (..)
	, SigTree
	, EqMatrix
	, ClassPred

	-- * Js Typeclass

	, ClassEnv
	, AdtEnv

	-- * Functions over Alpha and SigTree

	, unAlpha
	, ppTree

	-- * Project Utils

	, listToEither
	, maybeToEither

) where 

import Data.Monoid
import Control.Monad
import Control.Applicative
import Text.Show.Functions
import qualified Data.Set as S


{-----------------------------------------------------------------------------
   I. Primitives Types
------------------------------------------------------------------------------}

type Error     = String					
type Signature = String 			  

newtype FnName    = Fn String deriving (Eq,Show)
newtype DataName  = Dn String deriving (Eq,Show) 			
newtype ClassName = Cn String deriving (Eq,Show) 			  

-- * The Atom's location within a some Alpha is denoted by `J`, while the alpha's
-- * location within a signature is denoted by `I`. 

-- * `J` denotes an `Atom`'s' verticle location within some composite `Alpha`
-- * `N` denotes an `Atom`'s horiztonal location within some composite `Alpha`
-- * `I` denotes an `Alpha`'s horiztional location within some `SigTree`
-- * Exmaple : `f (m a) -> m a (t b)`, b's (N,J,I) is (1,0,1)  
newtype N    = N Int deriving (Eq, Ord, Num, Enum, Show)
newtype J    = J Int deriving (Eq, Ord, Num, Enum, Show)    
newtype I    = I Int deriving (Eq, Ord, Num, Enum, Show)    

-- * challege, get rid of the need for index to begin with
-- * or cast all eq operations as matrix operations
--data Idx = N Int | I Int | J Int  						-- * should write some prim functions over this that denote notionof equality??
	--deriving (Eq, Ord, Num, Enum, Show)

-- * you might need an intermideate form that carries functions??

newtype Kind = K Int deriving (Show, Eq, Ord)   	       

 -- * List of existing classes and their kinds
type ClassPred = [(ClassName, Kind)] 


data Comp a = Monoid a => C { runC :: Int -> a }

data Dat a = D a

data Dat2 a where 
	D2 :: a -> Dat2 a

data Comp2 a where 
	C2 :: Monoid a => (Int -> a) -> Comp2 a



runC2 :: Comp2 a -> Int -> a
runC2 (C2 g) = g


--newtype JInt  = JI Int
--newtype JBool = JB Bool

-- * how do you model objects?
-- * it's a list of forall a. JType a, so maybe you can't?
-- * So you do not want the added expresivitly of ADTs 
data JType a where 
	JI :: Int    -> JType Int
	JB :: Bool   -> JType Bool
	JS :: String -> JType String



{-----------------------------------------------------------------------------
   II. Parsing 
------------------------------------------------------------------------------}

-- * RIGHT NOW: you want to think about how to express this such that you can use common ops such 
-- * as union over alphas
-- * and in the process do equality/consistency check

-- * Judgements:
-- * typeclass validity
-- * adt validity
-- * kind valididty
-- * kind consistency
-- * consider `kind = Star | Arr Kind Kind`

-- * also consider one represetation of ClassEnv with several "views"

-- * you could annote each Alpha with Boolean flag denoting +/- validity
-- * this way when two alphas are unioned, you know which one is correct
-- * can you cast this as a monoid?

-- * EqMatrix describe dependancy between args

-- * should you express some adt lang using JType primitives?
-- * you really need to develop core in tandem with jtype, since they service each other
-- * in general, think about how to stratefy things more
-- * you should fill the rest of this doc with rules checking equalities


data Ty :: * -> * where
	TyInt :: Ty Int
	TyBoo :: Ty Bool
	TyStf :: Int -> Ty Int
	TyStg :: Int -> Int -> Ty Bool

data Ty2 = TyInt2 Int | TyBoo2 Bool deriving (Show)

data Ty3 where 
	TyInt3 :: Int -> Ty3
	TyBoo3 :: Bool -> Ty3

data MaybeA :: * -> * where
	NothingA :: MaybeA a
	NothingI :: MaybeA Int
	JustA    :: a -> MaybeA a


data Ty4 a = TyBoo4 Bool

data Ty5 a where
	TyBoo5 :: Bool -> Ty5 Bool


-- * maybeunion: consistenciy
-- * maybelift : tc/adt scope

-- * The "meta" info records "a"s kind, typeclass "a" is drawn from, and the coordinates 
-- * N and J of atom
type Meta   = (Kind, [ClassName], N, J)

-- * An Atom "a" carries the original signature denoting "a" and some "meta" information
-- * The "a" is either a `Char` denoting some polymorphic type, or some `DataName` 
-- * should polymorphic type be its own Atom all together?
-- * should you even have Meta inside Atom?
-- * You know for some consructos of Atom, Kind and J are fixed
-- * in fact, J = f Kind, so there's some type level programming that coul be done here
-- * Or construct Alpha and/or atom such that Meta is more constrained

-- * Ie: data Atom = Typed DataName Meta | Forall Char Meta

data Atom   = Atm (Either Char DataName) Meta 				
	deriving (Eq,Show)

-- * `Alpha` is either a pure type of kind = 0, or composite type of kind > 0
-- * Each composite type `t` also carries `Meta` information for the whole `t`
data Alpha  = An Atom | Co Atom Meta [Alpha] 
	deriving (Eq,Show)

-- * `Beta` carries additional information used to build some execution `Strategy`
-- * Since `Strategy` can operate over parameters that are functions, they are not parsed
type Beta     = (I, Either Signature Alpha)
type SigTree  = [Beta]

-- * An equality sparse matrix encodes type-checking `Move`s that compose to form a `Strategy`. 
-- * Where (N,J) Atom in Ith Alpha must have the same type as (N',J') Atom in Ith' Alpha
type EqMatrix = [(,) (N,I,J) (N,I,J)]  	

{-----------------------------------------------------------------------------
   III. JS Typeclass
------------------------------------------------------------------------------}

type ClassEnv a = [(ClassName, a)]

type AdtEnv a = [a]

{-----------------------------------------------------------------------------
	IV. Primitive functions over Data Types
------------------------------------------------------------------------------}

-- * does it make sense to isolate all value constructors?

-- * Alpha constructors
an :: Char -> [ClassName] -> N -> Alpha
an x cs n = An $ Atm (Left x) (K 0, cs, n, J 0)


unAlpha :: Alpha -> Signature
unAlpha a = case a of 
	An x      -> uAtom x
	Co x _ as -> case sugar a of 
		Just s  -> s 
		Nothing -> if as == [] then uAtom x else case head as of 
			Co _ _ _  -> uAtom x ++ " (" ++ unAs as ++ ")" 
			_ 		  -> uAtom x ++ " " ++ unAs as
	where 
		unAs as = init $ foldr (\a as -> unAlpha a ++ " " ++ as) "" as
		uAtom (Atm (Left c) _      ) = pure c
		uAtom (Atm (Right (Dn n)) _) = n
		sugar (Co (Atm c _ ) _ as  ) = case c of
			Right (Dn "List")  -> Just $ "[" ++ unAlpha (head as) ++ "]"
			Right (Dn "Tuple") -> Just $ "(" ++ unAlpha (head as) ++ ", " ++ unAlpha (as !! 1) ++ ")"
			_ 				   -> Nothing


ppTree :: SigTree -> IO ()
ppTree = mapM_ print

{-----------------------------------------------------------------------------
	VI. Project Utils
------------------------------------------------------------------------------}

listToEither :: a -> [b] -> Either a b 
listToEither a m = case m of 
	[]   -> Left a
	x:xs -> Right x

maybeToEither :: a -> Maybe b -> Either a b 
maybeToEither a m = case m of 
	Nothing -> Left a 
	Just b  -> Right b

