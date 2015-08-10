{-# LANGUAGE RankNTypes #-}
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Simply typed Lambda calculus
-- | Creator: Xiao Ling
-- | Created: December 22nd
-- | Source:  http://www.andres-loeh.de/LambdaPi/LambdaPi.pdf
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module SimplyTypedLambdaCalc where

import Data.Monoid
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Identity
import Control.Applicative
import Text.Show.Functions
import Data.Maybe
import Data.List

infixr 4 ->>
infixr 4 <<-


-- * maybe you should do an even simpler version than below, a simply typed λ-calc with explicit type annotation 


{-----------------------------------------------------------------------------
	I. Universe
------------------------------------------------------------------------------}

{-
	Syntax 
		Represent bound variables using de brujin's indices so that α-equaliy of terms reduce to syntax equality of terms
			ie, λx y z. x z (y z) becomes λ 3 1 (2 1)
		
		Represent free variables using absolute references, or names
		
		Represent values using ADTs
			Represent functions using haskell functions to take haskell's ability for function application
			To check function equality, they must be `Quote`d back into concrete representation

		Separate inferable (Term↑) and checkable (Term↓) terms at the syntax level

-}


data InfTerm 				   -- * Inferable terms, or terms without explicity type annotation. Its type is output by the type-checking algorithm
	= Bound Int 		       -- * bound variable and its de brujin's index
	| Free  Name  		       -- * free variable and its absolute reference
	| TAp   InfTerm ChkTerm    -- * application, where a function of inferred type is bound to a domain of checkable type, yielding a range of inferable terms
	| ChkTerm `IsIn` Type      -- * checkable Term ∈ Type
	deriving (Eq,Show)

data ChkTerm  			       -- * Checkable terms, or terms with explict type anotation. 
	= Inf  InfTerm 			   -- * embedded inferable term 
	| TLam ChkTerm  		   -- * function abstraction, note variables are not introduced due to use of de brujin's index
	deriving (Eq,Show)

data Name  						-- * Absolute reference of variables
	= Global String 			-- * Refer to some global variable name
	| Local  Int 				-- * detail: used when passing a binder to an algorithm, where bound variable is converted to free fraible temporarily
	| Quote  Int 				-- * alternate representation of terms in function that could be used for equality check
	deriving (Eq,Show)

data Type 
	= TFree Name 				-- * type identifiers
	| Arr Type Type   			-- * function arrows
	deriving (Eq,Show)

 
data Value 						-- * How is this used?
	= Lam (Value -> Value) 		-- * lambda abstraction, where function is represented using haskell functions
	| Neut Neutral 				-- * neutral term ?  what does it do?
	deriving (Eq,Show)
 
data Neutral 					-- * How is this used?
	= Free Name
	| NAp Neutal Value
	deriving (Eq,Show)


{-----------------------------------------------------------------------------
	II. Primitive Functions over Universe
------------------------------------------------------------------------------}

vfree :: Name -> Value
vfree = Neut . Free


{-----------------------------------------------------------------------------
	III. Evaluation
------------------------------------------------------------------------------}



{-----------------------------------------------------------------------------
	IV. Type Check
------------------------------------------------------------------------------}






{-

-- * literal translation, to be deleted * -- 

 
type Id = String 			-- * Variable annotation

data Tyvar = Tyvar Id 		-- * Variable

data Base = O | L 			-- * Base types := {type of proposition, type of individuals}

data Type = B Base | Type `TArr` Type 		

data Expr  				    -- * Terms
	= Var Tyvar 		    -- * variable
	| Expr `IsIn` Type      -- * type assignment
	| Ap Expr Expr 		    -- * function application
	| Lam Tyvar Expr        -- * function abstraction

data Vals  					-- * Terms evaluate to Values
	= Neut  				-- * some neutral term
	| VLam Tyvar Vals 		-- * function abstraction

data Neut 					-- * Question: what is this term used for?
	= NVar Tyvar 		    -- * variable
	| NAp Neut Vals  	    -- * function application


{-----------------------------------------------------------------------------
	II. Primitive Functions over Universe
------------------------------------------------------------------------------}

-- * Value constructor synonyms * - 

-- * `->`
(->>) :: Type -> Type -> Type
(->>) = TArr

-- * `∈`
(<<-) :: Expr -> Type -> Expr
(<<-) = IsIn

-}




{-

lets consider some very simple types
ill use standard haskell types where possible
ok so lets consider pairs:
data (a,b) = (a,b)
the pair type has one constructor:  (,) :: a -> b -> (a,b)
if we now look at what a case statement for pairs are, it would look something like this:
casePair :: (a,b) -> (a -> b -> c) -> c
casePair (x,y) f = f x y
casePair p (\x y -> z)   is more or less the same as writing something like   case p of { (x,y) -> z }
lingxiao: ok, so if we look at what the case expression is doing for pairs, casePair p :: (a -> b -> c) -> c   for fixed a and b, but for any c
now if i ask you, lingxiao, how can you do something with a pair?
well, you case analyze it/pattern match it to get out its parts
that is to say, in a very real sense, anything you do with a pairs is defined in terms of casePair
or could be, if casePair was the primitive thing here (pretend it is!)

lingxiao: ok, so now, if everything we do with pairs is essentially _via_ casePair, what's the point of the pair constructor (,) at all?
lingxiao: why not simply define this:
data Pair a b = forall c. (a -> b -> c) -> c
mkPair :: a -> b -> Pair a b
mkPair x y = \f -> f x y
you'll notice that mkPair x y is _identical_ to casePair (x,y)
so we can define the pair as the very action that it's case analyzer would have when applied to the pair!

mkPair x y == casePair (x,y) == \f -> f x y

lingxiao: ok, so now lets look at sums (Either)
data Either a b = Left a | Right b
caseEither :: Either a b -> (a -> c) -> (b -> c) -> c
caseEither (Left x) f g = f x
caseEither (Right y) f g = g y
again, you can sort of view it like this:   caseEither d (\x -> m) (\y -> n)   is like   case d of { Left x -> m ; Right y -> n }
lingxiao: but we can then play this same game: why not just define Either directly in terms of its case behavior:   data Sum a b = forall c. (a -> c) -> (b -> c) -> c
	left  :: a -> Sum a b ; left x = \f g -> f x
	right :: b -> Sum a b ; right y = \f g -> g y

so we can again define it in terms of its case behavior
so that   left x   ==   caseEither (Left x)   ==   \f g -> f x

so this is pretty interesting: we dont need constructors, etc. to "really" exist, we can just define the constructors to be the function that we would have gotten by case analysis
lingxiao: and in fact it's pretty easy to define this translation in a mechanical fashion
lingxiao: but what about types like Nat:   data Nat = Zero | Suc Nat
well, now we have an actually interesting choice to make

if we continue to use the "encode as case" we get this:
caseNat :: Nat -> c -> (Nat -> c) -> c
caseNat Zero z s = z
caseNat (Suc n) z s = s n
or by encoding:
data FunnyNat = forall c. c -> (FunnyNat -> c) -> c
lingxiao: we would get this for the new "constructors":
zero = \z s -> s
suc n = \z s -> s n
wait how'd you go from FunnyNat to definition of new "constructors"
right so
I decide to write all possible patterns for Max:
you do the same thing as you do for pairs and eithers: the new constructors should be just the thing you'd get out from doing case on it
caseNat Zero == \z s -> z
so the funny constructor zero should just be \z s -> z
just like for either   caseEither (Left x) = \f g -> f x
so the funny constructor should be   left x = \f g -> f x
lingxiao: you're just sort of defining the new constructor as old-constructor-then-case
lingxiao: ok so, thats one option, its called the scott encoding

for non-recursive types like (,) and Either, the scott encoding and the church encoding happen to be identical
they both act identical to case analysis functions
but for recursive types, scott encoding and church encodings differ
for recursive types, scott encodings are STILL case analysis
but church encodings are recursion principles (ie folds)
lingxiao: so lets define foldNat instead of caseNat
using unencoded nats
foldNat :: Nat -> c -> (c -> c) -> c
foldNat Zero z s = z
foldNat (Suc n) z s = s (foldNat n z s)
ok, well, let's play the same trick as before with caseNat, only with foldNat:
FunnyNat = forall c. c -> (c -> c) -> c
zero = \z s -> z
suc n = \z s -> s ???														-- * problem here: cannot map foldNat to church encoding since no named function exist to describe recursion
well before we would have written n in place of ??? but now we need to do a fold instead
but whats fold? well, the number itself is the fold, thats the point 		-- * Solution here: number itself is built using folds, where the deconstructor is the "constructor"
so ??? = (n z s)
suc n = \z s -> s (n z s)

thats basically how all church encodings work
now think about bools, right
im sure you've seen this:   true = \x y -> x
false = \x y -> y
but why that?
well what's case on bool?
caseBool True x y = x
caseBool False x y = y
caseBool, aka if-then-else
you can scott/church encode everything this way, depending on whether you want to use primitive recursion combinators or use structural recursion
scott encoding = constructors are just applying case
church encoding = constructors are just applying folds
i put the orders differently than most folds, tho, but
for lists its _obviously_ foldy:   nil = \z f -> z   ;   cons x xs = \z f -> f x (xs z f)

what we want, what we should know regardless of what implementation we have, is this:
casePair (makePair m n) (\x y -> z)   ==   z[m/x, n/y]
one way to implement that is directly: make `makePair` and `casePair` symbolic gadgets that just wrap things up
and then we define a beta reduction that does that substitution

-}



























