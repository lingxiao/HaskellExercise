{-# LANGUAGE RankNTypes #-} {-# LANGUAGE FlexibleContexts #-} {-# LANGUAGE MultiParamTypeClasses #-} {-# LANGUAGE FunctionalDependencies #-}

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Pure un-typed lambda calculus using de-brujin's index
-- | Creator: Xiao Ling
-- | Created: January 4th, 2014
-- | Source:  Types and Programming Langauges, Pierce
-- | 		  http://ttic.uchicago.edu/~pl/classes/CMSC336-Winter08/lectures/lec4.pdf
-- | HoTT  :  https://github.com/HoTT/HoTT-Agda
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module ULC (

        TermUn (..)

	, evalFull
	, evalbyVal
	
) where

import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import qualified Control.Monad.State as S
import Control.Applicative 
import Data.List
import Core

-- * temp imports
import Control.Monad.Trans
import Data.Monoid

{-
	Universe

		term  := x | λx. t | tt
		value := λx. t

	Operational Semantics: Call by Value

		------------------------		substitution
		 (λx.t)t'  -> [x +-> t']t 	    

			 t1 -> t1'
		------------------ 				application 1
		 t1 t2  -> t1' t2

			 t2 -> t2' 
		------------------				application 2
		  v t2  -> v t2'

-}

infixr 4 +->

{-----------------------------------------------------------------------------
	I. DataType 
------------------------------------------------------------------------------}

-- * Unviverse * -- 

data TermUn				     		-- * External representation of terms
	= VarUn Id			     		-- * x
	| LamUn Id TermUn 		 		-- * λx. t
	| AppUn TermUn TermUn 	        -- * tt
	deriving (Eq)

data TermIn  				        -- * Interal representation of terms
	= VarI Idx  Info 		        -- * variable represented by de brujin's index
	| LamI TermIn Info 		        -- * λ. t
	| AppI TermIn TermIn Info     	-- * tt
	deriving (Eq)

-- * Computations * --

-- * Substitute some free variable annotated by Idx by `t`
data Subst t  = Idx `Subst` t

{-----------------------------------------------------------------------------
	II. Prelude Typeclass implementation
------------------------------------------------------------------------------}

instance Show TermUn where 
	show t = case t of 
		VarUn x    -> x
		LamUn x t  -> "(λ" ++ x ++". " ++ show t ++ ")"
		AppUn t t' -> "(" ++ show t ++ " " ++ show t' ++ ")"

instance Show TermIn where 
	show t = case t of 
		VarI i _       -> show i
		LamI t' _      -> "(λ" ++ ". " ++ show t' ++ ")"
		AppI t' t'' _  -> "(" ++ show t' ++ " " ++ show t'' ++ ")"

{-----------------------------------------------------------------------------
	III. De-Brujin's index
------------------------------------------------------------------------------}

instance DeBrujinPair TermUn TermIn where
	label ti = case ti of 
		VarUn x    -> mrk x $ flip VarI (Rec x "")
		LamUn x t  -> local (x:) $ flip LamI (Rec x "") <$> label t
		AppUn t t' -> (\a b -> AppI a b nofo) <$> label t <*> label t'
	decode ti = case ti of 
		VarI _ (Rec x _)  -> VarUn x
		LamI t (Rec x _)  -> LamUn x $ decode t
		AppI t t' _ 	  -> AppUn (decode t) (decode t')

{-----------------------------------------------------------------------------
	IV. Substitution and Beta Reduction
------------------------------------------------------------------------------}

-- * Bind all free occurances of variable 0 in term `t` with term `t'`
betaRedux :: TermIn -> TermIn -> TermIn
betaRedux t t' = decr $ subst t $ 0 +-> incr t'

-- * Substitute all free occurances of variable `j` in λ-expression `ti` by `e`
subst :: TermIn -> Subst TermIn -> TermIn
subst ti s@(Subst j e) = case ti of 
	VarI i _    -> if i == j then e else ti
	LamI t x    -> LamI (subst t $ succ j +-> incr e) x
	AppI t t' x -> AppI (subst t s) (subst t' s) x


-- * Shift all indices within `ti` that is greater than or equal to `c` by `d` units
shift :: Idx -> Idx -> TermIn -> TermIn
shift d c ti = case ti of 
	VarI i x    -> if i < c then ti else VarI (i + d) x
	LamI t x    -> LamI (shift d (c + 1) t) x  
	AppI t t' x -> AppI (shift d c t) (shift d c t') x


-- * distinguished shift operation: when λ-binder is pruned during beta reduction, index drop by 1
decr :: TermIn -> TermIn
decr = shift (-1) 0

-- * distinguished shift operation: when substitution goes under a λ-binder, index increase by 1
incr :: TermIn -> TermIn
incr = shift 1 0

(+->) :: Idx -> t -> Subst t
i +-> t = Subst i t

{-----------------------------------------------------------------------------
	V. Evaluation
------------------------------------------------------------------------------}

-- * Full evaluation
evalFull :: TermUn -> TermUn
evalFull = evalBy pFull

-- * Call by value 
evalbyVal :: TermUn -> TermUn
evalbyVal = evalBy pVal

-- * Run `Interp` computation, throw away logging
evalBy :: (TermIn -> Bool) -> TermUn -> TermUn
evalBy p = decode . fst <$> runIdentity . flip runStateT [] . eval p . encode 

-- * Interpreter evaluates `ti` by `step`ing it forward if `ti` is not yet a value as checked by predicate `p`. 
-- * Evaluation is complete when `ti` is at fixed point. Intermediate results are `trace`d
eval :: (TermIn -> Bool) -> TermIn -> Interpreter TermIn
eval p ti = trace ti >> step ti >>= \ti' -> if ti' == ti then return ti' else trace ti' >> eval p ti' 
	where 
		step ti = case ti of 
			VarI _ _     -> return ti
			LamI t x     -> flip LamI x `liftM` step t
			AppI t t' x  -> case p t of 
				False -> (\v -> AppI v t' x) `liftM` step t
				True  -> case p t' of 
					False -> (\v' -> AppI t v' x) `liftM` step t'
					True  -> case t of 
						LamI e _ -> return $ betaRedux e t' 
						_	 -> return ti 
						
-- * Full evaluation is-value predicate
pFull :: TermIn -> Bool
pFull (AppI _ _ _) = False 
pFull _		       = True

-- * Call by Value is-value predicate
pVal :: TermIn -> Bool
pVal (LamI _ _) = True
pVal _          = False


