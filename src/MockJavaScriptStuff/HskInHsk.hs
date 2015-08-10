{-# LANGUAGE RankNTypes #-}
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Type Inference and Typechecker 
-- | Creator: Xiao Ling
-- | Created: December 12th
-- | Source:  http://web.cecs.pdx.edu/~mpj/thih/TypingHaskellInHaskell.html
-- | 		  http://pragprog.com/magazines/2013-06/unification
-- | 		  http://propella.blogspot.com/2009/04/prolog-in-haskell.html
-- |   		  http://www.andres-loeh.de/LambdaPi/LambdaPi.pdf
-- |  		  DataKinds and type-level literals.
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module HskInHsk where

import Data.Monoid
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Identity
import Control.Applicative hiding (Const)
import Text.Show.Functions
import Data.Maybe
import Data.List

infixr 4 ->>
infixr 4 +->
infixr 4 <.>
infixr 4 <+>
infixr 5 <:>
infixr 4 <<- 

{-----------------------------------------------------------------------------
	I. Universe
------------------------------------------------------------------------------}

type Id = String 

data Kind = Star | KArr Kind Kind 
	deriving (Eq,Show)

data Type 
	= TVar Tyvar 		-- * polymorphic type
	| TCon Tycon 		-- * algebraic data type 
	| TAp Type Type     -- * lambda abstraction over Type
	| TGen Int 		    -- * generic or quanitifed type variables, not sure why this is here yet
	deriving (Eq,Show)

data Tyvar = Tyvar Id Kind 
	deriving (Eq,Show)

data Tycon = Tycon Id Kind
	deriving (Eq, Show)

{-----------------------------------------------------------------------------
	Ia. Application Typeclasses
------------------------------------------------------------------------------}

-- * overload some function `kind` to get kind of a datatype
-- * in the future, make it `t -> Either Error Kind` so the function is total
class Kinded t where 
	kind :: t -> Kind 

-- * since subst can apply to any data type with type components, we overload functions with typeclass
-- * `apply replace every occurance of type variable with corresponding type, while `tv` collects type variables 
class Typed t where 
	apply :: Subst -> t -> t 
	tv    :: t -> [Tyvar]

-- * Similar to `Typed` above where inteading of `apply`ing a subst of TVar for TCon, 
-- * `inst` replace `TGen n` with nth element of `[Type]`
class Instantiate t where 
	inst :: [Type] -> t -> t 


{-----------------------------------------------------------------------------
	II. Prim Operations over Universe and Typeclass Instances
------------------------------------------------------------------------------}

-- * Primitive Haskell Types * -- 

[tUnit,tChar,tInt,tInteger,tFloat,tDouble,tString] 
	=   flip prim Star 
	<$> ["()", "Char", "Int", "Integer", "Float", "Double", "String"] :: [Type]

-- * Common Haskell Types * -- 

tList = prim "[]"  $ order 2 
tTup  = prim "(,)" $ order 3 
tArr  = prim "->"  $ order 3

list :: Type -> Type
list = TAp tList 

tup :: Type -> Type -> Type
tup a = TAp $ TAp tTup a 

(->>) :: Type -> Type -> Type
a ->> b = TAp (TAp tArr a) b

-- * Typeclass Imstances * -- 

instance Kinded Type where
	kind t = case t of 
		TGen _   -> error "this case should never be reached"
		TVar t'  -> kind t'
		TCon t'  -> kind t'
		TAp t' _ -> case kind t' of 
			KArr _ k -> k 				
			_ 		 -> error "poorly formed type should have been linted"

instance Kinded Tyvar where 
	kind (Tyvar _ k) = k

instance Kinded Tycon where
	kind (Tycon _ k) = k

instance Typed Type where 
	apply s t = case t of 
		TVar x -> case lookup x s of   		-- * replace type variable with some `Type`
			Just t'  -> t'
			Nothing  -> t 
		TAp l r -> TAp (apply s l) (apply s r)
		_ 		-> t
	tv t = case t of 
		TVar v   -> pure v
		TAp l r -> tv l `union` tv r
		_ 		 -> mempty

instance Typed a => Typed [a] where 
	apply s = fmap $ apply s
	tv      = nub . concat . fmap tv

{-----------------------------------------------------------------------------
	III. Substitutions 
------------------------------------------------------------------------------}

-- * DataType * -- 

type Subst = [(Tyvar, Type)]

-- * Primitive Subsitutions * -- 

-- * to ensure well-typed expressions, only kind-preserving subst are allowed
-- * Sets of substituions: 
memSubst :: Subst
memSubst = []

(+->) :: Tyvar -> Type -> Either String Subst 		
x +-> t 		
	| TVar x == t      = Right memSubst 			 -- * empty subst for variable itself
	| x `elem` tv t    = Left "occurs check failed"  -- * if x appears in the type, then fail??
	| kind x /= kind t = Left "Kinds mismatch"
	| otherwise		   = Right [(x,t)]

-- * composite subsititions * -- 

-- * run both substitions, output all possible results 
-- * satisfying : s1 <.> s2 = apply s1 . apply s2
(<.>) :: Subst -> Subst -> Subst
s1 <.> s2 = [ (x, apply s1 y) | (x,y) <- s2 ] <> s1

-- * Merge or parrallel application of two substitions. Succeed if both substition agree on value of the variable
-- * satisfying reflexivity, that is to say `s1 <+> s2 == s2 <+> s1`
(<+>) :: Subst -> Subst -> Either String Subst
s1 <+> s2 = if agree then Right (s1 <> s2) else Left "merge failed" where 
	agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v)) $ (fst <$> s1) `intersect` (fst <$> s2)

{-----------------------------------------------------------------------------
	IV. Most General Unifier and Pattern Matching
------------------------------------------------------------------------------}

-- * the minimum number of substitions such that two types are equal
-- * satisfying s = s' <.> mgu for all s
mgu :: Type -> Type -> Either String Subst
mgu t1 t2 = case (t1,t2) of 
	(TVar x, _)            -> x +-> t2
	(_ , TVar x)           -> mgu t2 t1
	(TCon t, TCon t')      -> Right memSubst 
	(TAp l r, TAp l' r') -> case mgu l l' of 
		Right s1 -> case mgu (apply s1 r) (apply s1 r') of 
			Right s2 -> Right $ s2 <.> s1 
			err 	 -> err
		err'     -> err' 
	_ 			 -> Left "Types do not unify"


-- * Pattern matching
-- * Given two types t1 and t2, find some substituion s s.t. apply s t1 == apply s t2
-- * this is used during pattern matching, why does one use <.> while the other use <+>?
match :: Type -> Type -> Either String Subst
match t1 t2 = case (t1,t2) of 
	(TVar x, _)  	       -> if kind x == kind t2 then x +-> t2 else Left "Kind mismatch"
	(TCon t, TCon t')      -> if t1 == t2 then Right memSubst else Left "type one must be variable"
	(TAp l r, TAp l' r') -> case match l l' of
		Right s1 -> case match r r'  of 
			Right s2 -> s1 <+> s2
			err      -> err
		err' 		 -> err'
	_ 			     -> Left "Types do not mach"

{-----------------------------------------------------------------------------
	V. MGU and Pattern Matching of (Qualified) Predicates 
------------------------------------------------------------------------------}

-- * DataType * -- 

-- * Assert "element of" relation: Type ∈ Class
data Pred   = IsIn Id Type 	
	deriving (Show,Eq)

-- * Qualified Types, adds contexts to some statement of type `t`
data Qual t = [Pred] :=> t 	
	deriving (Show,Eq)

-- * Element of Symbol: ∈
(<<-) :: Type -> Id -> Pred
(<<-) = flip IsIn

-- * convenient representation transformer in context of some computation 
tupQual :: Monad m => m (Qual t) -> m ([Pred],t)
tupQual m = m >>= \(ps :=> t) -> return (ps,t)

-- * Typeclass implementation * -- 

instance Kinded Pred where 
	kind (IsIn _ t) = kind t

instance Kinded t => Kinded (Qual t) where 
	kind (_ :=> t) = kind t

instance Typed Pred where
	apply s (IsIn i t) = i `IsIn` apply s t
	tv (IsIn i t) 	   = tv t

instance Typed t => Typed (Qual t) where
	apply s (ps :=> t) = apply s ps :=> apply s t
	tv (ps :=> t) 	   = tv ps `union` tv t


-- * mgu and match * -- 

mguPred, matchPred :: Pred -> Pred -> Either String Subst
mguPred   = lift_ mgu
matchPred = lift_ match

-- * Most general unifier and matching can only occur if two elems are drawn from the
-- * the same typeclass
lift_ :: (Type -> Type -> Either String Subst) -> Pred -> Pred -> Either String Subst
lift_ g (IsIn c t) (IsIn c' t') = if c == c' then g t t' else Left "Classes differ" 

-- * two predicates `p` and `q` `overlap` if some mgu exists  
overlap :: Pred -> Pred -> Bool
overlap p q = either (const False) (\xs -> if xs == [] then False else True) $ mguPred p q 


{-----------------------------------------------------------------------------
	VI. Classes
------------------------------------------------------------------------------}

-- * DataTypes * --

-- * A product type `(Id -> Maybe Class) x [Type]`
-- * Where the function `classes` maps `Id` to maybe `Class`, and 
-- * defaults is a list of ??? 
data ClassEnv = CE {
	  classes  :: Id -> Maybe Class
	, defaults :: [Type]		
} deriving (Show)

type Class = (,) [Id] [Inst] 		-- * a tuple of its superclass and instances

-- * Example Instances: 
-- * 	[t ∈ Eq, t ∈ Ord] :=> t ∈ Monoid
-- * 	[a ∈ Eq, a ∈ Ord, (Maybe a) ∈ Functor] :=> (Maybe a) ∈ Monad 
type Inst  = Qual Pred 				


-- * The computation of modifing a typeclass environment
-- * Note this includes both adding new classes and instantiating their instances
type EnvTransformer = ClassEnv -> Either String ClassEnv

-- * Read/Modify ClassEnv * -- 

-- * Initial ClassEnv value
initEnv :: ClassEnv
initEnv = CE (\i -> Nothing) [tInteger, tDouble]

-- * read super class identifiers
super :: ClassEnv -> Id -> [Id]
super ce i = join . maybeToList . fmap fst $ classes ce i

-- * read instances
insts :: ClassEnv -> Id -> [Inst]
insts ce i = join . maybeToList . fmap snd $ classes ce i

-- * modify class env 
modClassEnv :: ClassEnv -> Id -> Class -> ClassEnv 
modClassEnv ce i c  = ce { classes = \j -> if i == j then Just c else classes ce j }


-- * Prim EnvTransformers and Composition * -- 

-- * composition of env transformers
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
f <:> g = \ce -> f ce >>= \ce' -> g ce'

-- * define a new class with a list of superclasses

-- * Add some class `c` into ClassEnv
-- * `c` may have list of superclass `cs`, s.t. if  c' ∈ cs then c ⊆ c'
addClass :: Id -> [Id] -> EnvTransformer
addClass c cs ce 
	| defined (classes ce c)  			  = Left "Class already defined"
	| any (not . defined . classes ce) cs = Left "superclass not defined"
	| otherwise 						  = return . modClassEnv ce c $ (cs,[])

-- * Add some instance `ps :=> p` into the ClassEnv
addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) ce 
	| not . defined $ classes ce i = Left "No class defined for instance"
	| any (overlap p) qs  		   = Left "overlapping instance"
	| otherwise 				   = Right $ modClassEnv ce i c
	where 
		its = insts ce i
		c   = (super ce i, (ps :=> p):its)
		qs  = [ q | (_ :=> q) <- its ]

{-----------------------------------------------------------------------------
	VII. Entailment
------------------------------------------------------------------------------}

-- * Given predicate `p = t ∈ c`, find all super classes of `c` where if c' is a superclass
-- * then c ⊆ c', thus p' = t ∈ c'
bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn c t) = p : join [ bySuper ce (IsIn c' t) | c' <- super ce c ]


-- * Given predicate `p = t ∈ c`, find type `t` in instance store of class `c` and collect all predicates of `t`
-- * If `t` has no more predicates, output []. If `t` Does not exist, then output Nothing
byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn c t) = msum [ tryInst p i | i <- insts ce c ] 


-- * Cases:

-- * p' = Int ∈ Ord, p = [a ∈ Eq] => a ∈ Ord
-- * ----------------------------------------
-- * 			   a : Int
-- * ----------------------------------------
-- * 		Int ∈ Eq, Int ∈ Ord

-- *   p' = Int ∈ Ord, p = [] => a ∈ Ord
-- * ----------------------------------------
-- * 			   a : Int
-- * ----------------------------------------
-- * 			Empty Predicate

-- * p' = Int ∈ Ord, p = [Char ∈ Eq ] => Char ∈ Ord
-- * ----------------------------------------------
-- * 			   Nothing 
tryInst :: Pred -> Inst -> Maybe [Pred]
tryInst p (ps :=> p') = eitherToMaybe $ matchPred p' p >>= \sub -> return $ apply sub <$> ps 


-- * `entail` evaluate to true iff predicate `p` will hold when all `ps` are satisfied
entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (bySuper ce <$> ps) || 
	case byInst ce p of 
		Just qs -> if qs == [] then False else all (entail ce ps) qs
		Nothing -> False


{-----------------------------------------------------------------------------
	VIII. Type Scheme
------------------------------------------------------------------------------}

-- * Represent polymorphic types, ie:
-- | `(Functor f, Monad m, Eq a) => f a -> m a -> a` quantifies to 
-- | `∀ [* -> *, * -> *, *] (0 ∈ Functor, 1 ∈ Monad, 2 ∈ Eq) => f a -> m a -> a`
-- * where indices 0 and 1 map to position of their respective kinds in [Kind]
data Scheme = Forall [Kind] (Qual Type) 
	deriving (Eq,Show)

instance Typed Scheme where 
	apply s (Forall ks qt) = Forall ks (apply s qt)
	tv (Forall _ qt)       = tv qt

-- * Send type to scheme without quantification
freeScheme :: Type -> Scheme
freeScheme t = Forall [] $ [] :=> t

-- * Send qualified type to scheme 
-- * Scheme constructor quantifies a qualified type `qt` w.r.t. a list of type variables `vs`
quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall (kind <$> vs') (apply s qt) where 
	vs' = [ v | v <- tv qt, v `elem` vs ] 		
	s   = zip vs' $ TGen <$> [0..]

{-----------------------------------------------------------------------------
	IX. Assumptions
------------------------------------------------------------------------------}

-- * Maps variable name to type scheme
data Assump = Id :>: Scheme

instance Typed Assump where
	apply s (i :>: sc) = i :>: (apply s sc)
	tv (_ :>: sc)      = tv sc

-- * Find some scheme in list of assumptions
findSch :: Id -> [Assump] -> Either String Scheme
findSch i xs = case xs of 
	[] 			    -> Left $  "Unbound identifier " ++ i 
	(i' :>: sc):xs' -> if i == i' then Right sc else findSch i xs'


{-----------------------------------------------------------------------------
	X. Type-Inference Monad
------------------------------------------------------------------------------}

-- * Inference Computation Datatype * -- 

-- * Infer the type `t` of some expression `e` 
-- * question: how does [Assump] play into this? 
type TypeInferT m e t = ClassEnv -> [Assump] -> e -> InferT m ([Pred],t)
type TypeInfer e t    = forall m. Monad m => TypeInferT m e t

-- * Inference computation update current substitution, and counter to annotate new vars
-- * It may have some side-effect `m` and output value `a`, which usually is the inferred type
type InferT m a = StateT (Subst,Int) m a
type Infer a    = forall m. Monad m => InferT m a

inferT ::  Monad m => InferT m a -> m a 
inferT = flip evalStateT (memSubst,0)

infer :: InferT Identity a -> a
infer = runIdentity . inferT

-- * Primitive TIs * -- 

-- * create some new variable based on kind and current counter
newVar :: Kind -> Infer Type
newVar k = get >>= \(s,n) -> put (s,succ n) >>= \_ -> return . TVar $ Tyvar (enumId n) k
e
-- * find mgu of two types and unpdate current substitution
unify :: Type -> Type -> InferT (Either String) ()
unify t1 t2 = do 
	(s,n) <- get
	s'    <- lift $ mgu (apply s t1) $ apply s t2
	put (s' <.> s, n)

-- * Send some scheme to a qualified type, rename TGen with variables 
-- * Ex: 
-- * Raw QualType   : 				 (a ∈ Eq, f ∈ Functor)  	     => f a 					:: Qual Type
-- * quantified     :  ∀ *, * -> * . (TGen 0 ∈ Eq, TGen 1 ∈ Functor) => (TGen 0) (TGen 1) 		:: Scheme
-- * final QualType :  			     (v0 ∈ Eq, v1 ∈ Functor)  		 => v1 v0 					:: Qual Type
newInst :: Scheme -> Infer (Qual Type)
newInst (Forall ks qt) = mapM newVar ks >>= \vs -> return $ inst vs qt

-- * Instantiate TC instances * -- 

-- * replace every occurance of `TGen n` with nth variable from `ts`
instance Instantiate Type where 
	inst ts t = case t of 
		TGen n   -> ts !! n 
		TAp l r -> TAp (inst ts l) $ inst ts r
		_ 		 -> t 

instance Instantiate a => Instantiate [a] where 
	inst ts = fmap $ inst ts  

instance Instantiate t => Instantiate (Qual t) where 
	inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance  Instantiate Pred where
	inst ts (IsIn c t) = IsIn c $ inst ts t 

{-----------------------------------------------------------------------------
	XI. Type Inference on Literals 
------------------------------------------------------------------------------}

-- * Special syntax for prims
data Literal 
	= LitInt  Integer
	| LitChar Char
	| LitRat  Rational
	| LitStr  String

-- * Type assignment for prim types, annotate with predicate if l ∈ Class
inferLit :: Literal -> Infer ([Pred],Type)
inferLit l = case l of 
	LitChar _ -> return ([],tChar)
	LitInt _  -> newVar Star >>= \v -> return ([IsIn "Num" v], v)
	LitRat _  -> newVar Star >>= \v -> return ([IsIn "Fractional" v],v)
	LitStr _  -> return ([],tString)


{-----------------------------------------------------------------------------
	XII. Type Inference on Expressions
------------------------------------------------------------------------------}

-- * is this the language being typed? why is different than syntax of simply-typed lambda calc or system F?
data Expr 	
	= Var Id 								-- * Term variable annotate with id
	| Lit Literal 							-- * Primitives
	| Const Assump 							-- * ¿que?
	| Ap Expr Expr 							-- * Function application 
	| Let BindGroup Expr 					-- * Local Definintion, now what is BindingGroup?

-- * recall:
-- * Inference m e t = ClassEnv -> [Assump] -> e -> InferT m ([Pred],t)
inferExpr :: TypeInferT (Either String) Expr Type
inferExpr ce as e = case e of 
	
	Var i  			 -> do 
		sc <- lift $ findSch i as 		        	-- * find type scheme for variable i in as :: Assumptions
		tupQual $ newInst sc 				    	-- * create a new scheme and output in tuple form
	
	Lit l  			 -> inferLit l 	

	Const (i :>: sc) -> tupQual $ newInst sc        

	Let bg e   		 -> do 
		(ps,as') <- inferBindGrp ce as bg 			-- * note classEnv come into play here, maybe binding something to class?   
		(ps',t') <- inferExpr ce (as' <> as) e 		-- * why are assumptions combined? actually, you're still not sure what assumptions are
		return (ps <> ps', t')

	Ap l r 			 -> do 
		(ps,t)   <- inferExpr ce as l 	
		(ps',t') <- inferExpr ce as r 				-- * why does the assumption not propagate here?
		t''      <- newVar Star 		 			-- * declare a new variable of kind *
		unify (t' ->> t'') t 			 			-- * (r -> vn) `unify` l, but why is this step done?
		return (ps <> ps', t'') 		 			-- * output variable of kind *, but why?


{-----------------------------------------------------------------------------
	XIII. Type Inference on Patterns and Alternatives
------------------------------------------------------------------------------}

-- * Patterns * -- 

-- * Pattern matching --> still really iffy about what's going on
data Pat 
	= PVar Id 					-- * Match any value and bind result to variable i
	| PWildcard 				-- * Match all
	| PAs Id Pat 				-- * Binds variable it to any value matching pattern `pat`. PAs i p = i@p 
	| PLit Literal 				-- * Match only pattern denoted by some `Literal`
	| PNpk Id Integer 			-- * mach any positive integral value m >= k, and bins variablei to differentce m -k 
	| PCon Assump [Pat] 		-- * Match only data buit using constructor furnction a :: Assump with a sequenceo f args matching [Pat]


-- * note assumptions are not passed in here 
inferPat :: Pat -> Infer ([Pred], [Assump], Type)
inferPat p = case p of 
	PVar i    -> newVar Star >>= \v         -> return ([], [i :>: freeScheme v], v)
	PWildcard -> newVar Star >>= \v         -> return ([],[],v)
	PAs i p'  -> inferPat p' >>= \(ps,as,t) -> return (ps, (i :>: freeScheme t) : as,t)
	PLit l    -> inferLit l  >>= \(ps,t)    -> return (ps,[],t)
	PNpk i x  -> newVar Star >>= \v         -> return ([v <<- "Integral"], [i :>: freeScheme v], v)
	PCon a ps -> undefined

inferPats :: [Pat] -> Infer ([Pred],[Assump],[Type])
inferPats = undefined

-- * Alternatives * -- 

-- * Refer to syntax of form `case pred of { Pat -> Expr; Pat -> Expr; ... }
type Alt = ([Pat], Expr)

-- * recall: TypeInferT m e t = ClassEnv -> [Assump] -> e -> InferT m ([Pred],t)
inferAlt :: TypeInferT (Either String) Alt Type
inferAlt ce as (pat,e) = do  
	(ps,as',ts)   <- inferPats pat 							-- * infer a list of patterns
	(ps',t)       <- inferExpr ce (as' <> as) e  			-- * infer the expression, where assumptions built from pattern propagates
	return (ps <> ps', foldr (->>) t ts) 					-- * combine predicates, reconstruct original function ? -- * why is type of pattern put into the function signature?


inferAlts :: ClassEnv -> [Assump] -> [Alt] -> InferT (Either String) [Pred]
inferAlts = undefined


{-----------------------------------------------------------------------------
	XV. Binding Group
------------------------------------------------------------------------------}

type BindGroup = ([Expr], [Impl])
type Impl      = ()

inferBindGrp :: TypeInfer BindGroup [Assump]
inferBindGrp = undefined



{-----------------------------------------------------------------------------
	XX. Utils
------------------------------------------------------------------------------}

enumId :: Int -> Id
enumId = (++) "v" . show

-- * construct primitves of kind *
prim :: Id -> Kind -> Type
prim t = TCon . Tycon t

-- * construct a `Kind` of arbitary order
order :: Int -> Kind
order n = if n == 0 then Star else KArr Star . order $ pred n

defined :: Maybe a -> Bool
defined Nothing = False 
defined _ 		= True 

eitherToMaybe :: Either a b -> Maybe b 
eitherToMaybe (Right x) = return x
eitherToMaybe _ 		= Nothing

















