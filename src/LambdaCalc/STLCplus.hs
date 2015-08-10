
{-# LANGUAGE RankNTypes, GADTs, DataKinds, TypeOperators, KindSignatures, PolyKinds, TypeFamilies, UndecidableInstances, FunctionalDependencies, FlexibleInstances  #-} 

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Simply Typed lambda calculus with Term and Type Extensions
-- | Creator: Xiao Ling 
-- | Created: January 16th, 2014
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module STCLplus (

	  Term (..)
	, prodTy
	, sumTy

	, (>>>)
	, pfst
	, psnd 

	, eval

) where  			

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Applicative 
import Data.List

import Core
import ULC

infixl 4 >>>
infixr 4 <-:
infixr :->
infixr :::

-- TODO: type inference using unificatio

{-----------------------------------------------------------------------------
	I. DataType 
------------------------------------------------------------------------------}

-- * Universe of Terms * -- 

data Term :: Type -> * where 
	Var :: Id -> Term a
	Lam :: Id -> Type -> Term b -> Term (a :-> b)
	App :: Term (a :-> b) -> Term a -> Term b 

	Let :: Id -> Term a -> Term b -> Term b 
	Tup :: Term a -> Term b -> Term (a :*: b)
	Lft :: Term a -> Term (a :+: b)
	Rgt :: Term b -> Term (a :+: b)

	Tru :: Term Boolean
	Fls :: Term Boolean
	Uni :: Term Unit

data TermIn :: Type -> * where 
	VarI :: Idx -> Info -> TermIn a 
	LamI :: Type -> TermIn b -> Info -> TermIn (a :-> b)
	AppI :: TermIn (a :-> b) -> TermIn a -> TermIn b

	LetI :: TermIn a -> TermIn b -> Info -> TermIn b   
	TupI :: TermIn a -> TermIn b -> TermIn (a :*: b)

	TruI :: TermIn Boolean
	FlsI :: TermIn Boolean
	UniI :: TermIn Unit

-- * Universe of Types * --

data Type 
 	= Type :-> Type
 	| Type :*: Type
 	| Type :+: Type
 	| Boolean
 	| Unit
 	| Void

-- * Derived Terms and Types * -- 

-- * A herterogenous list paramaterized over kind: [Type]
data Env :: [Type] -> * where 
	Nil   :: Env '[]  								        
	(:::) :: Term t -> Env ts -> Env (t ': ts)

-- * Constructs a 'Type from a 'List of 'Type, where 'List and 'Type are kinds
-- * Note the empty Product construted from the empty 'List is just () 
type family TyProd (ts :: [Type]) :: Type 				
type instance TyProd '[]       = Unit
type instance TyProd (t ': ts) = t :*: TyProd ts

-- * N-ary product type constructor 
prodTy :: Env ts -> Term (TyProd ts)
prodTy Nil 	      = Uni 
prodTy (x ::: xs) = x `Tup` prodTy xs


data Tag :: [Type] -> Type -> * where
   First :: Tag (t ': ts) t
   Next  :: Tag ts s -> Tag (t ': ts) s

type family TySum (ts :: [Type]) :: Type
type instance TySum '[]       = Void
type instance TySum (t ': ts) = t :+: TySum ts

 -- * N-ary sum type constructor
sumTy :: Tag ts t -> Term t -> Term (TySum ts)
sumTy First    x = Lft x
sumTy (Next t) x = Rgt (sumTy t x)

{-
-- * prod constructor for homogenous list, note it does not type check
since Tup x prod xs is an inf type
and tup and Uni are not the same a

	prod' :: [Term a | b | c | .. ] -> Term (a * b * c * ... ) 
	prod' []     = Uni
	prod' (x:xs) = x `Tup` prod' xs

	where [Term a|b|c|...] is Env ts 
	where Env ts is 

	data Env ts = Nil | Term t ::: Env (t:ts) 

	and (a * b * c * ...) is TyProd ts where

	TyProd :: [Type] -> [Type]
	TyProd []   = ()
	TyProd x:xs = x * TyProd xs

-}

{-----------------------------------------------------------------------------
	II. TypeClass and Primitive Functions over Types
------------------------------------------------------------------------------}

instance DeBrujinPair (Term a) (TermIn a) where
	label ti  = case ti of 
		Var x      -> mrk x $ flip VarI (Rec x "") 
		Lam x ty t -> local (x:) $ (\t' -> LamI ty t' $ Rec x "") <$> label t
		App t t'   -> (\a b -> AppI a b) <$> label t <*> label t'
		
		Let x t t' -> local (x:) $ (\v v' -> LetI v v' $ Rec x "") <$> label t <*> label t'
		
		Tup t t'   -> TupI <$> label t <*> label t'
		Tru        -> return TruI
		Fls        -> return FlsI
		Uni        -> return UniI

	decode ti = case ti of 
		VarI _ (Rec x _)    -> Var x 
		LamI ty t (Rec x _) -> Lam x ty $ decode t
		AppI t t' 			-> App (decode t) (decode t')
		LetI t t' (Rec x _) -> Let x (decode t) (decode t')
		TupI t t'           -> Tup (decode t) (decode t')
		TruI 				-> Tru
		FlsI 				-> Fls
		UniI 				-> Uni

instance Show (Term a) where 
	show t = case t of 
		Var x       -> x
		Lam x ty t  -> "(λ" ++ x ++ ":" ++ show ty ++ ". " ++ show t ++ ")" 
		App t t'    -> "("  ++ show t ++ " " ++ show t' ++ ")"
		Let x t t'  -> "let " ++ x ++ " = " ++ show t ++ " in " ++ show t'
		Tup t t'    -> "(" ++ show t ++ ", " ++ show t' ++ ")"
		Lft t       -> show t 
		Rgt t       -> show t 
		Tru  		-> "true"
		Fls  		-> "false"
		Uni 		-> "()"

instance Show (TermIn a) where 
	show t = case t of 
		VarI i _      -> show i
		LamI ty t' _  -> "(λ" ++ show ty ++ ". " ++ show t' ++  ")"
		AppI t' t''   -> "("  ++ show t' ++ " "  ++ show t'' ++ ")"
		LetI t t' _   -> "let 0 = " ++ show t ++ " in " ++ show t'
		TupI t t'     -> "(" ++ show t ++ ", " ++ show t'
		TruI     	  -> "true"
		FlsI     	  -> "false"
		UniI		  -> "()"

instance Eq Type where 
	t1 == t2 = case (t1,t2) of 
		(_ :-> _, _ :-> _)   -> tyLst t1 == tyLst t2
		(Unit,Unit)          -> True
		(Boolean, Boolean)   -> True
		(t :*: t', s :*: s') -> t == s && t' == s'
		_ 		     		 -> False

instance Show Type where 
	show t = case t of 
		a :-> b -> show a ++ " -> " ++ show b 
		Unit  	-> "()"
		Void    -> "Void"
		Boolean -> "Bool"
		a :*: b -> "(" ++ show a ++ ", " ++ show b ++ ")"

-- * Note: id /= lstTy . tyLst since tyLst destroy any "functions as parameters"
tyLst :: Type -> [Type]
tyLst ty = case ty of 
	t :-> t' -> tyLst t ++ tyLst t'
	t        -> [t]

lstTy :: [Type] -> Maybe Type
lstTy [] = Nothing
lstTy ts = Just $ foldr (:->) (head ts) (tail ts)

-- * Deconstruct 'ty` when Function of type `ty` is bound to varible of `ty'`
-- * Pnemonic: <-: is the inverse of constructor :->
(<-:) :: Type -> Type -> Maybe Type
ty <-: ty' = join $ lstTy <$> stripPrefix (tyLst ty') (tyLst ty)

{-----------------------------------------------------------------------------
	III. Derived Syntax
------------------------------------------------------------------------------}

-- * Sequence operation `;`
(>>>) :: Term Unit -> Term a -> Term a
t1 >>> t2 = Lam "x" Unit t2 `App` t1

-- * First Tuple Projection
pfst :: Term (a :*: b) -> Term a
pfst (Tup a _ ) = a

-- * Second Tuple Projection
psnd :: Term (a :*: b) -> Term b 
psnd (Tup _ b)  = b

-- * N-ary Product Type
                  
-- * N-ary Sum Type

{-----------------------------------------------------------------------------
	IV. Type Checker
------------------------------------------------------------------------------}

typeof :: Term a -> Either Err Type
typeof = typeof' . encode 

typeof' :: TermIn a -> Either Err Type
typeof' = runIdentity . runEitherT . flip runReaderT [] . checker

checker :: TermIn a -> TypeChkr Type
checker ti = ask >>= \en -> case ti of 
	VarI i (Rec x _) -> if length en <= i then lift (left $ "Error: unbound variable " ++ x) else return $ en !! i
	LamI ty t _      -> local (ty:) $ ((:->) ty) <$> checker t
	AppI t t'  	     -> do 
		ty  <- checker t
		ty' <- checker t'
		case ty <-: ty' of 
			Just r  -> return r
			Nothing -> lift . left $ "Error: cannot apply value of type " ++ show ty' ++ " to function of type " ++ show ty

	LetI t t' x      -> checker t >>= \ty -> checker $ LamI ty t' x `AppI` t   -- * let x = t1 in t2 := (λx:T1. t2) t1
	TupI t t'        -> do 
		ty  <- checker t 
		ty' <- checker t' 
		return $ ty :*: ty'
	UniI 			 -> return Unit
 	_        		 -> return Boolean 


{-----------------------------------------------------------------------------
  V. Interpreter
------------------------------------------------------------------------------}

-- * Note, you should split typeof' from eval, so there's a "compile" step and then a "run" step
eval :: Term a -> Either Err TermUn
eval t = do
	ti <- return $ encode t 
	typeof' ti
	return $ evalFull . erase $ ti

-- * Erase type information and church encode all value extensions
erase :: TermIn a -> TermUn 
erase = go . decode where 
	go :: Term a -> TermUn
	go ti = case ti of  		
		Var x      -> VarUn x 
		Lam x _ t  -> LamUn x $ go t
		App t t'   -> go t `AppUn` go t'
		Let x t t' -> (LamUn x $ go t') `AppUn` (go t)
		Tup t t'   -> LamUn "f" $ LamUn "a" $ LamUn "b" $ (VarUn "f" `AppUn` go t) `AppUn` go t'
		Tru        -> LamUn "x" $ LamUn "y" $ VarUn "x"
		Fls        -> LamUn "x" $ LamUn "y" $ VarUn "y"
		Uni        -> LamUn "x" $ VarUn "x"

{-----------------------------------------------------------------------------
	X. Tests
------------------------------------------------------------------------------}


[x,y,z,f] = Var <$> ["x","y","z", "f"]

-- * basic syntax

sI   = Lam "x" Unit $ x								 -- * λx. x 		       	 :: o -> o 
sT   = Lam "x" Unit $ Lam "y" Unit $ x   					 -- * λx. λy. x       		 :: o -> o -> o
sK   = Lam "x" tx $ Lam "y" ty $ Lam "z" tz $ App (App x z) (App y z) where      -- * λx. λy. λz. (xz)(yz)   :: (o -> o -> o) -> (o -> o) -> o -> o
	tx = Unit :-> Unit :-> Unit 
	ty = Unit :-> Unit 
	tz = Unit 

-- * church numerals 

tn = (Unit :-> Unit) :-> Unit :-> Unit  	
ts = Unit :-> Unit
tz = Unit

scc  = Lam "n" tn $ Lam "s" ts $ Lam "z" tz $ s `App` nsz where                  -- * λn.λs.λ.z. s(n s z)  
	nsz     = (n `App` s) `App` z
	[n,s,z] = Var <$> ["n","s","z"]

plus = Lam "m" tn $ Lam "n" tn $ Lam "s" ts $ Lam "z" tz $ msnsz where   	-- * λm. λn. λs. λz. ms(nsz)
	msnsz = App (App (Var "m") (Var "s")) nsz
	nsz   = App (App (Var "n") (Var "s")) (Var "z")

add m n = App (App plus m) n

c0  = Lam "s" ts $ Lam "z" tz (Var "z")   					-- * λs.λz. z
c1  = scc `App` c0 								-- * λs.λz. sz
c2  = scc `App` c1
c3  = add c1 c2 								-- * though not fully evaluated, the result hand-evaluates gives the right answer

-- * extended syntax

sseq1 = Uni >>> sI
slet1 = Let "x" sI sT
slet2 = Let "x" c2 (App scc $ Var "x") 		-- * let x = (λs.λz. z) in (λnsz. s(n s z))x.  
stup1 = Tup sI sT 
stup2 = Tup c1 scc
stup3 = Tup sseq1 slet2

-- * derived types

p1 = prodTy $ c1 ::: c2 ::: c3 ::: Nil

-- * Test function application of varying argument types

sIb = Lam "x" Boolean $ x 						-- * id function defined over Bools
f1  = App sI Tru 								-- * does not type check since sI take in 
f2  = App sIb Tru 								-- * does type check since sIb is defined over Bools
f3  = ((sK `App` sT) `App` sI) `App` Uni 		-- * application of k combinator to correctly typed values







