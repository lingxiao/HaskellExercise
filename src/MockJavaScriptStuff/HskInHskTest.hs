{-# LANGUAGE RankNTypes #-}
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Test Simple implementation of System F
-- | Creator: Xiao Ling
-- | Created: December 14th
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module HskInHskTest where


import Data.Monoid
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Applicative
import Text.Show.Functions
import Data.Maybe
import Data.List
import HskInHsk

{-----------------------------------------------------------------------------
	I. Test DataTypes
------------------------------------------------------------------------------}


-- * misc * -- 

sig1, sig2 :: Type
sig1  = TLam (TLam tArr tInt) $ TLam tList (TVar (Tyvar "a" Star))  	-- * "Int -> [a]"
sig2  = tInt ->> tInt

-- * type variables * -- 

va,vb,vc,vf,vm,vt :: Tyvar
va = Tyvar "a" Star
vb = Tyvar "b" Star
vc = Tyvar "c" Star
vf = Tyvar "f" $ order 1
vm = Tyvar "m" $ order 1
vt = Tyvar "t" $ order 2

-- * types  * -- 

a,b,c,f,m, t, tMaybe, tMaybeC, tEither, t1,t2 :: Type
a  = TVar va 
b  = TVar vb
c  = TVar vc 
f  = TVar vf 
m  = TVar vm
t  = TVar vt
fa = TLam f a
ma = TLam m a

-- * named data types * -- 

tMaybe  = TCon $ Tycon "Maybe" $ order 1
tMaybeC = TLam tMaybe tChar
tMaybeA = TLam tMaybe a
tEither = TCon . Tycon "Either" $ order 2

-- * function signatures * --

t1 = a ->> fa 
t2 = b ->> tMaybeC
t3 = b ->> ma
t4 = [IsIn "Eq" a] :=> (a ->> fa)  -- * not the right use case?

{-----------------------------------------------------------------------------
	II. Test Substitutions
------------------------------------------------------------------------------}

-- * atomic subst * -- 

sub1,sub2,sub3,sub4,sub5,sub6:: Either String Subst
sub1 = (Tyvar "a" Star) +-> a
sub2 = (Tyvar "a" Star) +-> fa
sub3 = (Tyvar "a" Star) +-> f
sub4 = (Tyvar "a" Star) +-> tChar
sub5 = (Tyvar "b" Star) +-> tInt
sub6 = (Tyvar "f" $ order 1) +-> tMaybe

-- * composed subst * -- 

sub7,sub8 :: Subst
sub7 = unRt sub4 <.> unRt sub5 		-- * non-consec subst
sub8 = unRt sub_ <.> unRt sub6 		-- * consecutive subst
	where sub_ = (Tyvar "Maybe" $ order 1) +-> (TCon $ Tycon "Maybe2" $ order 1)

me1,me2 :: Either String Subst
me1 = unRt sub4 <+> unRt sub5
me2 = unRt sub4 <+> unRt sub_ where 
	sub_ = (Tyvar "a" Star) +-> tInt


{-----------------------------------------------------------------------------
	IV. Test mgu and match
------------------------------------------------------------------------------}

-- * mgu * -- 

m1,m2,m3,m4,m5,m6,m7,m8,m9 :: Either String Subst
m1 = mgu a a
m2 = mgu a b
m3 = mgu a tChar
m4 = mgu tChar b
m5 = mgu f tChar
m6 = mgu tMaybe f
m7 = mgu fa fa
m8 = mgu fa tMaybeC
m9 = mgu t1 t2

-- * match * -- 

ma1,ma2,ma3,ma4, ma5, ma6 :: Either String Subst
ma1 = match a a 		 			-- * same type variable,no mgu											
ma2 = match a b 					-- * different type variable, a +-> b
ma3 = match tMaybe tEither			-- * both types, no pattern matching 
ma4 = match f tMaybe 				-- * f `match` Maybe, same kind
ma5 = match t1 t2  					-- * a ->> fa `match` b ->> tMaybeC, same arity and kinded args
ma6 = match t1 t3					-- * a ->> fa `match` b ->> ma, same arity and a +-> b, but fa +-/-> ma


{-----------------------------------------------------------------------------
	V. Test mgu and match in typeclass context
------------------------------------------------------------------------------}

-- * there is a possiblity that I might not understand unification enough
-- * to really get the most out of this, or ask educated questions0
-- * this is expecially true for match, where you're not sure what it's used for

-- * predicates and qualified predicates
pa = IsIn "Eq" a		   :: Pred   	 -- * a ∈ Eq
pb = IsIn "Eq" b 		   :: Pred   	 -- * b ∈ Eq 
pt = IsIn "Eq" (tup a b)   :: Pred 		 -- * (a,b) ∈ Eq
qt = [pa,pb] :=> pt        :: Qual Pred  -- * (a,b) ∈ Eq, a ∈ Eq, b ∈ Eq

pa' = IsIn "Ord" a							:: Pred 
pb' = IsIn "Ord" b							:: Pred
qt' = [pa',pb'] :=> (IsIn "Ord" $ tup a b)  :: Qual Pred

pint  = IsIn "Eq" tInt 						:: Pred
pchar = IsIn "Eq" tChar 					:: Pred

-- * test `Typed` typeclass
tvqt = tv qt 								:: [Tyvar]
apqt = apply [(Tyvar "a" Star,tInt)] qt     :: Qual Pred

-- * test mgu 
mp1,mp2,mp3,mp4 :: Either String Subst
mp1 = mguPred pa pa 	 	-- * a ∈ Eq, a ∈ Eq,   empty substitution
mp2 = mguPred pa pb 		-- * a ∈ Eq, b ∈ Eq,   a +-> b
mp3 = mguPred pa pint		-- * a ∈ Eq, int ∈ Eq, a +-> Int
mp4 = mguPred pa' pa 		-- * a ∈ Ord, a ∈ Eq,  drawn from different sets

-- * test match. 
mpp1 = matchPred pa pa  	-- * empty subst
mpp2 = matchPred pa pb 		-- * a `match` b
mpp3 = matchPred pa pint   	-- * a `match` int
mpp4 = matchPred pa pa'	 	-- * error: drawn from different sets

-- * test overlap

-- * why is it that 
ov1 = overlap pa pa 		-- * a ∈ Eq, a ∈ Eq, no subst |- no overlap
ov2 = overlap pa pb 		-- * a ∈ Eq, b ∈ Eq, a +-> b
ov3 = overlap pa pint 		-- * a ∈ Eq, Int ∈ Eq, a +-> b
ov4 = overlap pa pa' 		-- * a ∈ Eq, a' ∈ Ord, different classes |- no overlap
ov5 = overlap pint pchar    -- * Int <- Eq, Char <- Eq, different types |- no overlap

{-----------------------------------------------------------------------------
	VI. Test Classes 
------------------------------------------------------------------------------}

-- * instances * -- 

is1 = [] :=> (a <<- "Eq") 		       					   -- * Inst Eq a 
is2 = [a <<- "Eq"] :=> (a <<- "Ord")   					   -- * Eq a => Ord a
is3 = [a <<- "Eq", f <<- "Functor"] :=> (fa <<- "Monoid")  -- * (Eq a, Functor f) => Monoid (f a) 


-- * Throway Classes * -- 

c1,c2,c3 :: ClassEnv
c1 = CE cs [] where 
	cs = \i -> case i of 
		"Eq"          -> Just ([],[])
		"Ord"         -> Just (["Eq"],[])
		"Functor"     -> Just ([],[])
		"Applicative" -> Just (["Functor"],[])
		_ 		  	  -> Nothing

-- * use `addClass` function
c2 = unRt $ (addClass "Monad" ["Functor", "Applicative"]) c1

-- * add instances, it's throwing overlap inst error though it shouldnt occur
c3 = unRt $ eis c2 where
	eis  =  addInst [] (IsIn "Ord" tUnit) 
	    <:> addInst [] (IsIn "Ord" tChar)
	    <:> addInst [] (IsIn "Ord" tInt)
		<:> addInst [IsIn "Ord" a,IsIn "Ord" b] (IsIn "Ord" $ tup a b)


-- * Prelude Classes * -- 

preludeCs :: EnvTransformer
preludeCs = coreCs <:> numCs <:> pInsts

coreCs :: EnvTransformer
coreCs =  addClass "Eq"       []
      <:> addClass "Ord"      ["Eq"]
      <:> addClass "Show"     []
      <:> addClass "Read"     []
      <:> addClass "Bounded"  []
      <:> addClass "Enum"     []
      <:> addClass "Functor"  []
      <:> addClass "Monad"    []

numCs :: EnvTransformer
numCs  =  addClass "Num"        ["Eq", "Show"]
      <:> addClass "Real"       ["Num", "Ord"]
      <:> addClass "Fractional" ["Num"]
      <:> addClass "Integral"   ["Real", "Enum"]
      <:> addClass "RealFrac"   ["Real", "Fractional"]
      <:> addClass "Floating"   ["Fractional"]
      <:> addClass "RealFloat"  ["RealFrac", "Floating"]

-- * Type constructor instances
pInsts :: EnvTransformer
pInsts =  addInst []					      (tUnit <<- "Eq")
	  <:> addInst [tUnit <<- "Eq"]            (tUnit <<- "Ord")
	  <:> addInst []      			          (tChar <<- "Eq")
	  <:> addInst [tChar <<- "Eq"]            (tChar <<- "Ord")
	  <:> addInst []      			          (tInt  <<- "Eq")
	  <:> addInst [tInt <<- "Eq"]             (tInt <<- "Ord")
	  <:> addInst [a <<- "Ord", b <<- "Ord"]  ((tup a b) <<- "Ord")

-- * Type variable instances
pInstsV :: EnvTransformer
pInstsV =  addInst [] 		 (a <<- "Eq") 
	   <:> addInst [a <<- "Eq"] (a <<- "Ord")


-- * Prelude env Γ used for remainder of tests
gam :: ClassEnv
gam = unRt $ preludeCs initEnv

{-----------------------------------------------------------------------------
	VII. Test Predicate Entailment 
------------------------------------------------------------------------------}

-- * bySuper
bs1,bs2,bs3,bs4,bs5 :: [Pred]
bs1 = bySuper gam $ tUnit <<- "Eq" 	   -- * Γ |-  () ∈ Eq        ||- () ∈ Eq     
bs2 = bySuper gam $ tUnit <<- "Ord"    -- * Γ |-  () ∈ Eq        ||- () ∈ Ord    
bs3 = bySuper gam $ tUnit <<- "Monad"  -- * Γ |-  () ∈ Monad     ||- () ∈ Monad  
bs4 = bySuper gam $ a <<- "Ord"        -- * Γ |-  a  ∈ Eq        ||- a  ∈ Ord   
bs5 = bySuper gam $ (tup a b) <<- "Eq" -- * Γ |-  a ∈ Eq, b ∈ Eq ||- (a,b) ∈ Eq  


-- * tryInst, a helper function that required special attention
tr1,tr2,tr3,tr4 :: Maybe [Pred]
tr1 = tryInst (a    <<- "Ord")    $ []  			 :=> (b <<- "Ord") 	   -- * a ∈ Ord `match` b ∈ Ord, no subst, therefore Empty Pred       
tr2 = tryInst (tInt <<- "Ord")    $ [a <<- "Eq"]     :=> (a <<- "Ord")     -- * Int ∈ Ord `match` a ∈ Eq => a ∈ Ord, a : Int, Int ∈ Eq
tr3 = tryInst (tInt <<- "Ord")    $ [tChar <<- "Eq"] :=> (tChar <<- "Ord") -- * Int ∈ Ord `match` [Char ∈ Eq] => Char ∈ Ord, type mismatch. Nothing
tr4 = tryInst (tInt <<- "Monoid") $ [a <<- "Eq"]     :=> (a <<- "Ord")     -- * Int ∈ Monoid `Match` a ∈ Eq => a ∈ Ord, Class mismatch. Nothing


-- * byInst
bi1,bi2,bi3,bi4 :: Maybe [Pred]
bi1 = byInst gam $ b <<- "Eq"  	  -- * Nothing, predicate over b d.n.e. in Γ
bi2 = byInst gam $ a <<- "Ord"    -- * Nothing, predicate over a d.n.e. in Γ
bi3 = byInst gam $ tInt <<- "Eq"  -- * Γ |-  Int ∈ Eq   ||- Empty Predicate
bi4 = byInst gam $ tInt <<- "Ord" -- * Γ |-  Int ∈ Ord  ||- Int ∈ Eq


-- * entail
en1,en2,en3,en4,en5 :: Bool
en1 = entail gam [tInt <<- "Eq"]      (tInt <<- "Ord")      -- * False by super, True by Instance declaration
en2 = entail gam [tInt <<- "Ord"]     (tInt <<- "Eq")       -- * True by Super
en3 = entail gam [tInt <<- "Eq"]      (tInt <<- "Monoid")   -- * False by Super, false by Instance 
en4 = entail gam [tInt <<- "Monoid"]  (tInt <<- "Ord") 	    -- * False by Super, True by Instance?, reduce to `en5`
en5 = entail gam [tInt <<- "Monoid"]  (tInt <<- "Eq") 	    -- * False by Super, True by Instance??

{-----------------------------------------------------------------------------
	VIII. Test Type Scheme 
------------------------------------------------------------------------------}

-- * note the position of the kinds maps to position of predicate
-- * The major new thing here is the kind information is expressed in the constraint header
-- * note Qual Type is some function signature, and it's quantified w.r.t. to a list of type variables

sch1,sch2,sch3,sch5,sch6 :: Scheme
sch1 = quantify [va,vb] $ [a <<- "Monoid", a <<- "Eq"]  :=> (a ->> b)    -- | ∀ *, *        . (a ∈ Monoid, a ∈ Eq) => a -> b
sch2 = quantify [va,vf] $ [f <<- "Functor"]		        :=> TLam f a     -- | ∀ * -> *, *   . f ∈ Functor          => f a
sch3 = quantify [va,vf] $ [a <<- "Eq", f <<- "Functor"] :=> (fa ->> a)   -- | ∀ *, * -> *   . (f ∈ Functor, a ∈ Eq)=> f a -> a

-- * ∀ * -> *, * -> *, * . (0 ∈ Functor, 1 ∈ Monad, 2 ∈ Eq) => f a -> m a -> a
sch4 = quantify [vm,vf,va] $ [f <<- "Functor", m <<- "Monad", a <<- "Eq"] :=> (fa ->> ma ->> a)

-- | ∀ * -> *, * -> * -> * . (0 ∈ Functor, 1 ∈ Bifunctor) => t a (f a)
sch5 = quantify [vt,vf] $ [f <<- "Functor", t <<- "Bifunctor"] :=> (TLam (TLam t a) fa) 

-- | ∀ * -> *, * -> * -> * . (f ∈ Functor, t ∈ Bifunctor) => f (t a a)
sch6 = quantify [vf,vt] $ [f <<- "Functor", t <<- "Bifunctor"] :=> (TLam f $ TLam (TLam t a) a)


{-----------------------------------------------------------------------------
	IX. Test Assumptions
------------------------------------------------------------------------------}








{-----------------------------------------------------------------------------
	X. Test Type-Inference Monad
------------------------------------------------------------------------------}

-- * new var
nv1 :: Type
nv1 = infer $ newVar (order 2)

-- * mgu
un1,un2 :: Either String ()
un1 = inferT $ unify a tInt
un2 = inferT $ unify (a ->> fa) (tInt ->> tMaybeC)

-- * Instantiate Type, replace all TGen with type found in first parameter of `inst`
it1,it2 :: Type
it1 = inst [tInt,tChar] $ TGen 1
it2 = inst [tMaybe,tChar] $ TLam (TGen 0) (TGen 1)

qit1 :: Qual Type
qit1 = inst [f,tChar] $ [f <<- "Functor"] :=> (TLam (TGen 0) $ TGen 1)

-- * newInst computation
-- * swaps out `TGen n` with nth variable generaed by `newVar` computation
ni1 :: Qual Type
ni1 = infer $ newInst sch1



{-----------------------------------------------------------------------------
	XX. utils
------------------------------------------------------------------------------}

-- * unsafe partial extraction
unRt :: Show a => Either a b -> b
unRt (Right x) = x
unRt (Left e)  = error $ show e















