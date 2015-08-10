{-# LANGUAGE RankNTypes #-} {-# LANGUAGE FlexibleContexts #-} {-# LANGUAGE MultiParamTypeClasses #-} {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-} {-# LANGUAGE FlexibleInstances #-}

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Pure Simply Typed lambda calculus 
-- | Creator: Xiao Ling
-- | Created: January 8th, 2014
-- | Source:  Types and Programming Langauges, Pierce
-- | 		  HOAS for less pain in the ASS: http://lambda-the-ultimate.org/node/3627
-- | 		  https://en.wikipedia.org/wiki/Generalized_algebraic_data_type#Higher-order_abstract_syntax
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module STLC (

	  Term (..)
	, Type (..)
	, Base (..)

	, check
	, eval

) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Applicative 
import Data.Monoid
import Data.List

import Core
import ULC


{-
	Universe

		term  := x | λx ∈ T . t | tt
		value := λx. t
		type  := base | type -> type

	Evaluation: call by value

		------------------------------		substitution
		 (λx.t ∈ T)v  -> [x +-> v]t 	    

			 t1 -> t1'
		-------------------- 				application 1
		 t1 t2  -> t1' t2

			 t2 -> t2' 
		--------------------				application 2
		  v t2  -> v t2'


	Typing

		       (x : T) ∈ Γ
		----------------------------- 	     Type Var 		 (axiom)
		        Γ |- x : T


		    Γ, x : T |- t : T'
		------------------------------ 		Type Abstraction (lengthen)
		Γ |- (λx : T . t) : T -> T'


		   Γ |- t : T -> T', t' : T
		------------------------------		Type Application (shorten)
			    Γ |- tt' : T'

-}

infixr 4 <-:
infixr 9 :->

{-----------------------------------------------------------------------------
	I. DataType 
------------------------------------------------------------------------------}

-- * Unviverse of Terms * --

data Term  						  -- * External Representation of terms
	= Var Id  					  -- * x
	| Lam Id Type Term 		 	  -- * λx:T . E
	| App Term Term 			  -- * t t'
	deriving (Eq)

data TermIn  					  -- * Internal representation of terms using De-Brujin's index, without type annotation
	= VarIn Idx Info			  -- * i
	| LamIn Type TermIn Info 	  -- * λT. E
	| AppIn TermIn TermIn Info    -- * t t'
	deriving (Eq)

-- * Unviverse of Types * --

data Type = Base | Type :-> Type  	-- * Universe of all simply typed λ-calc types
	deriving (Eq)

type Base = () 					    -- * Base type has the unit type only

-- * Other * --

type Env = [Type] 				    -- * Env is a list of types, where index of type maps to debrujin's index of the variable

{-----------------------------------------------------------------------------
	II. Type class implementation and Prim Function over DataTypes
------------------------------------------------------------------------------}

instance Show Type where 
	show Base       = "o"
	show (t :-> t') = show t ++ " -> " ++ show t'

instance Show Term where 
	show t = case t of 
		Var x      -> x
		Lam x ty t -> "(λ" ++ x ++ ":" ++ show ty ++ ". " ++ show t ++ ")" 
		App t t'   -> "("  ++ show t ++ " " ++ show t' ++ ")"

instance Show TermIn where 
	show t = case t of 
		VarIn i _        -> show i
		LamIn ty t' _    -> "(λ" ++ show ty ++ ". " ++ show t' ++  ")"
		AppIn t' t'' _   -> "("  ++ show t' ++ " "  ++ show t'' ++ ")"

-- * note without extensions, this is basically the same as untyped 
-- * is it possible to have some base lambda skeleton of a formal system
-- * with some side-effect-full traversable implemented over the skeleton
-- * and encode/deocde are just that?

instance DeBrujinPair Term TermIn where
	label ti = case ti of 
		Var x 	   -> mrk x $ flip VarIn (Rec x "")
		Lam x ty t -> local (x:) $ (\t' -> LamIn ty t' $ Rec x "") <$> label t
		App t t'   -> (\a b -> AppIn a b nofo) <$> label t <*> label t'
	decode ti = case ti of 
		VarIn _ (Rec x _)    -> Var x 
		LamIn ty t (Rec x _) -> Lam x ty $ decode t 
		AppIn t t' _ 	     -> App (decode t) (decode t') 

{-----------------------------------------------------------------------------
	IV. Type Checker
------------------------------------------------------------------------------}

-- * exported version of check
check :: Term -> Either Err Type
check = check' . encode

check' :: TermIn -> Either Err Type
check' = runIdentity . runEitherT . flip runReaderT [] . checker 

checker :: TermIn -> TypeChkr Type
checker ti = ask >>= \en -> case ti of 
	VarIn i _    -> if length en <= i then lift . left $ "Error: Unbound variable" else return $ en !! i
	LamIn ty t _ -> local (ty:) $ ((:->) ty) <$> checker t
	AppIn t t' _ -> do 
		ty  <- checker t
		ty' <- checker t'
		either (lift . left) return $ ty <-: ty'

-- * Shorten Type tree by function application
-- * TODO: compelete this by converting AST to list, then use stripPrefix. See STLCplus
(<-:) :: Type -> Type -> Either Err Type
ty <-: ty' = undefined


{-----------------------------------------------------------------------------
	V. Interpreter
------------------------------------------------------------------------------}

-- * note if the STLC syntax is to be extended, then you cannot use ULC interpretor
-- * unless you decopomose them to primitive expressions, after type checking!

-- * Type check and then evaluate pure STLC expression and outputs untyped results
-- * TODO: apply `recCon` to final result so output is typed
eval :: Term -> Either Err TermUn
eval t = let ti = encode t in case check' ti of 
	Left e  -> Left e 
	Right _ -> Right . evalFull . erase $ ti

-- * erase all type information after `ti` type checks
erase :: TermIn -> TermUn
erase = go . decode where 
	go ti = case ti of 
		Var x     -> VarUn x
		Lam x _ t -> LamUn x $ go t
		App t t'  -> AppUn (go t) (go t')

-- * Given some environment of types `en`, reconstruct a STLC expression from untyped `tu`
reCon :: Env -> TermUn -> TermIn
reCon en tu = undefined

  
{-----------------------------------------------------------------------------
	VI. Derived Types and Values
------------------------------------------------------------------------------}

              
-- * Boolean * --

-- * Note this assignements is not unique
-- * Also note it is restricted to taking in arguments of base type only
boolT :: Type 
boolT = Base :-> Base :-> Base

-- * Nte without type variables or type schemas, 
-- * these functions only work for the Base type of (), but not on unit which has () -> ()
tru, fls :: Term
tru = Lam "x" Base $ Lam "y" Base (Var "x")
fls = Lam "x" Base $ Lam "y" Base (Var "y")

-- * Unit * -- 

unitT :: Type
unitT = Base

unit :: Term 
unit = Lam "x" Base $ Var "x"


-- * The semicolon, sequences two terms
-- * seqt t1 t2 is t1;t2 := (λx : (). t2)t1
seqe :: Term -> Term -> Term
seqe t t' = Lam "x" Base t' `App` t


{-----------------------------------------------------------------------------
	VII. Tests
------------------------------------------------------------------------------}

-- * External Representation * -- 
 
[x,y,z,f] = Var <$> ["x","y","z", "f"]

sI   = Lam "x" Base $ x										  		  			 -- * λx. x 		       	 :: o -> o 
sT   = Lam "x" Base $ Lam "y" Base $ x   							 			 -- * λx. λy. x       		 :: o -> o -> o
sK   = Lam "x" tx $ Lam "y" ty $ Lam "z" tz $ App (App x z) (App y z) where      -- * λx. λy. λz. (xz)(yz)   :: (o -> o -> o) -> (o -> o) -> o -> o
	tx = Base :-> Base :-> Base 
	ty = Base :-> Base 
	tz = Base 

-- * this function should not type check!!
sfxy = Lam "f" tf $ App (Lam "x" tx $ App f x) (Lam "y" ty $ App f y) where      -- * λf. (λx. f x) (λy. f y) :: (o -> o -> o -> o) -> o
	tf = Base :-> Base :-> Base :-> Base
	ty = Base :-> Base
	tx = Base 

sfxy'  = Lam "x" tx $ Lam "y" ty $ App (App y x) x where 
	tx = Base
	ty = Base :-> Base

-- * Internal Representation * -- 

[tI,tT,tK,tfxy,tfxy'] = encode <$> [sI,sT,sK, sfxy,sfxy'] 			

-- * Evaluation * -- 

c0  = Lam "s" ts $ Lam "z" tz (Var "z")          					    -- * λs.λz. z
c1  = Lam "s" ts $ Lam "z" tz (App (Var "s") $ Var "z")  				    -- * λs.λz. sz
scc = Lam "n" tn $ Lam "s" ts $ Lam "z" tz $ App s nsz where                                -- * λn.λs.λ.z s(n s z)
	nsz     = App (App (Var "n") (Var "s")) (Var "z") 
	[n,s,z] = Var <$> ["n","s","z"]

-- * Typed plus
plus = Lam "m" tn $ Lam "n" tn $ Lam "s" ts $ Lam "z" tz $ msnsz where   		    -- * λm. λn. λs. λz. ms(nsz)
	msnsz = App (App (Var "m") (Var "s")) nsz
	nsz   = App (App (Var "n") (Var "s")) (Var "z")

-- * Typed add
add m n = eval $ App (App plus m) n 		

c2 = eval $ App scc c1
c1' = add c0 c1


-- * some types * -- 

tn = (Base :-> Base) :-> Base :-> Base
ts = Base :-> Base
tz = Base

{-

	There are two obvious ways to implement that substitution: as a literal substitution
	(you go through the e and replace every instance of x with v until you get to something that shadows x, 
	then evaluate the resulting expression), or with an environment (you record the binding x=v somewhere, 
	then evaluate e, and every time you encounter a variable you look it up in your ‘environment’ [table of bindings]).

	Yes, that expression will type-check so long as y has the appropriate type 
	(i.e. it's in your context and has a type that matches T→T)


	Inversion of the typing relation
		1. If Γ ⊢ x:R, then x:R ∈ Γ.
		2. If Γ ⊢ λx:T1.t2 :R, then R = T1 → R2 for some R2 with Γ,x:T1 ⊢t2 :R2.
		3. If Γ ⊢ t1 t2 :R, then there is some typeT11 such that Γ ⊢t1 :T11→Rand Γ ⊢t2:T11.
		4. If Γ ⊢ true  :R, then R=Bool.
		5. If Γ ⊢ false :R, then R=Bool.
		6. If Γ ⊢ if t1 thent 2 else t3 : R, then Γ ⊢t1 :Bool and Γ ⊢t2,t3 :R


-}




