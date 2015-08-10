{-# LANGUAGE GADTs #-} {-# LANGUAGE FlexibleContexts #-} {-# LANGUAGE ImpredicativeTypes #-} 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Mock Application that create typeclass in javascript
-- | Creator: Xiao Ling
-- | Created: October 26th
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Strategy2 where 

import Control.Monad
import Control.Monad.Reader
import Control.Applicative

import Data.Dynamic
import Data.Monoid
import Data.Maybe
import Data.List

import Control.Monad.State
import Text.Show.Functions

import Coroutine



{-
	Mental Model

		A simple typeclass that's unaware of typeclass hierarchy

		-- * Types description at the Top level * --

		The "user" declares some `TypeclassDecl`, which the "program" converts to a `TypeClass`. 
		Each `Typeclass` runs in some context `InstEnv`, which is a collection of all `Instance` of the `Typeclass` 
		The user delcares an `Instance` of a typeclass, which is stored in the appropriate `InstEnv`

		-- * Types description at the Second level * --

		A `TypeclassDecl` is a mapping from some `ClassName` to a list of tuples, mapping `FuncName` to its `FuncSignature`
		A `Typeclass` is a mapping from `ClassName` to a list of `MemberFunc`. 
		Each `MemberFunc` is a mapping from `FuncName` to `Strat`. 
		A `Strat` is the logic of how to run some implementation of the member function inside an `InstEnv` 
		`Strat` is built by parsing the `FuncSignature` of a member function and mapping each character to a `Move`
		A `Move` is just a `Coroutine` that awaits and yields some `Term`, and runs in context of some `InstEnv`.
		A `Strat`, then, is just a coroutine pipeline

		An `Instance` is a mapping from `DataName` to a list of `FuncImpl`
		An `FuncImpl` is a mapping from `FuncName` to `BinFunc`, or some implemetation of the member function, satisfying the member's `FuncSignatue`

		-- * Type description at the Third level * --

		Each `MemberFunc` is a Binry function of signature :: `a -> b -> InstEnv -> c`, or some function that takes two parameters and 
		some environement, and emits some type `c`
		Since typeclass member functions are often nullary or unary, `NulaMember` and `UnaMember` synonyms are also provided

	Naming Convention

		fn : FuncName
		cn : ClassName
		dn : DataName
		i  : Instance
		g  : FuncImpl
		h  : Strat
		v  : some primitive type
		hs : [Strat]
		sm : StratMap

-}

{-----------------------------------------------------------------------------
   Types
------------------------------------------------------------------------------}

-- * Primitive synonyms

type SigAlpha    = [String]
type FuncSig     = String
type FuncName    = String
type DataName    = String
type ClassName   = String

-- * Class types

type Class       = (,) ClassName [MemberFunc]
type MemberFunc  = (,) FuncName Strat

-- * Instance types

type Instance    = (,) DataName [FuncImpl]
type FuncImpl    = (,) FuncName Dynamic			 -- * unwrapped : `(,) FuncName (BinFunc a b c)`

-- * Function representation where the types are expclity and need to wrapped in 
-- * Dynamic data type
-- * Advtange: 
-- *    you can pattern match on underlying function if you know the types ahead of time and run it
-- * Disadvantage: you cannot have a function over any type that does not impelemnt
-- *    `Typeable`, including some parametrized type `Functor f=> f a` 
type BinFunc a b c = a -> b -> c
type UnaFunc b c   = BinFunc () b  c 
type NulFunc c     = BinFunc () () c


data BinFunc2 = forall a b c. BF (a -> b -> c)

-- * Program types

type Move      = forall m. MonadReader InstEnv m => CoroutineT Term Term m ()
-- * current strategy is just a coroutine pipeline, but some moves accept parameters
type Strategy  = forall m. MonadReader InstEnv m => CoroutineT Term Dynamic m ()

-- * another strategy formulation that emits values
type Strategy2 = forall a m. (MonadReader InstEnv m, Typeable a) => CoroutineT Term a m ()

-- * to be depricated
type Strat   = Move

-- * note how R should be parameterized, this is making Term very unpredicatble
data Term     = S [DataName] | F Dynamic 

type InstEnv  = (,) ClassName [Instance]

type StratMap = String -> Maybe Move

-- * typeclass implementation

instance Show Term where
	show t = case t of 
		S n -> show n
		F d -> show d 


{-----------------------------------------------------------------------------
	Primitive Operations
------------------------------------------------------------------------------}

-- * A note on design: since the member function representation, the class and
-- * the instance representation are in flux. So there should be no expclity use of
-- * data constructors outside of this section

-- * Constructors * --

-- * member function instance constructor
mem :: (Typeable a, Typeable b, Typeable c) => FuncName -> BinFunc a b c -> FuncImpl
mem fn = (,) fn . toDyn

-- * class constructor
toClass :: ClassName -> [(FuncName, FuncSig)] -> Class
toClass cn gs = (,) cn $ (\(gn, s) -> (gn, fromSig s)) <$> gs where
	fromSig = undefined

-- * construct an `Instance` where the `FuncImpl a b c` are wrapped in `Dynamic` constructor
toInst :: ClassName -> DataName -> [(FuncName, Dynamic)] -> Instance
toInst _ dn = (,) dn

-- * empty Term
memTerm :: Term
memTerm = S mempty

-- * create an empty instance environemnt
env :: ClassName -> InstEnv
env = flip (,) mempty

-- * Accessors * --

putInst :: Instance -> InstEnv -> InstEnv
putInst i (cn,is) = (cn, is ++ [i])

askInst :: InstEnv -> DataName -> Either String Instance
askInst (cn,is) dn = listToEither msg [ i | i@(dn', _) <- is, dn' == dn ] 
	where msg = "Instance " ++ dn ++ " not found for class " ++ cn 

askFunc :: Instance -> FuncName -> Either String Dynamic
askFunc (n,gs) fn = listToEither msg [ f | (fn', f) <- gs, fn' == fn ] 
	where msg = "Function " ++ fn ++ " not found instance " ++ n


{-----------------------------------------------------------------------------
	Program
------------------------------------------------------------------------------}

-- * Moves * --

-- * None of the moves below are final or atomic
-- * develop some philosophy as to what parameters should be awaited and what should be
-- * passed in excplityly
-- * right now it's very ad-hoc

-- * there should be some domain-free moves that hides the next layer
-- * from the implementation of environment, and gives next layer
-- * a language to do elementary operations on the environment, 
-- * but is at a higher level of abstraction than ask, tell etc


-- * cap the head of of a pipeline to prevent leaky awaits 
capH :: Move
capH = source . pure $ memTerm


-- *  note how this has JOP built into the function, this is non-modular
typed :: JOP -> Move
typed x = do 
	t <- await
	case t of 
		S xs -> either err (\x -> yield . S $ xs ++ [x]) $ typeof x
		F _  -> err "Inappropriate Term Function"


-- * a prelim move, could be decomposed further
sameType :: Move
sameType = do 
	t <- await
	case t of 
		S xs -> if all (== head xs) xs then yield t else error "not all types equal"
		F _  -> err "not implemented"


-- * prelim find func, why are variables bound to it?
-- * this is a strategy when o1 and o2 are the same type
-- * obviously, use combinators to get rid of these case statements
fndFunc :: FuncName -> Move
fndFunc fn = do
	t <- await
	case t of 
		F _      -> err "Inappropriate Term Function"
		S []     -> err "No type specified"
		S (t1:_) -> do
			env <- lift ask
			case env `askInst` t1 of 
				Right i -> case i `askFunc` fn of 
					Right g -> yield . F $ g
					Left e  -> err e
				Left e  -> err e


-- * this would replace capT
eval :: (Typeable a, Typeable b) => a -> b -> Strategy2
eval x y = do 
	t <- await
	case t of 
		S _  -> err "No Function Emitted"
		F g -> case fromDynamic g of 
			Nothing -> error "No function found"
			Just g' -> yield $ g' x y


-- * note this is the only eval that works for fmap since some function f for any f would not implement Typeable
-- * the signautre of the whole pipeline seems to affect each element along the pipeline
-- * it might make more sense to have many evals? but then we're back at where we started, where we
-- * have to enumerate strategies up to catagories of function signatures
eval' :: (String -> String) -> JOP -> Strategy2
eval' x y = do
	t <- await
	case t of 
		S _  -> err "No Function Emitted"
		F g -> case fromDynamic g of 
			Nothing -> error "No function found"
			Just g' -> yield $ g' x y



-- * check outcome of eval satisfy closure
-- * note this and typed (?) could be broken down into `sat` primitve taking some predicate
-- * note how sat closure awaits something else
satClosure :: Monad m => JOP -> CoroutineT JOP JOP m ()
satClosure x = do
	y <- await
	let t1 = typeof x
	let t2 = typeof y
	case (t1,t2) of 
		(Right t1', Right t2') -> if t1' == t2' then yield y else err "does not satisfy closure"
		_ 					   -> err "no type info provided"


-- * reformulate signature of Strategy2 so it can accomadate this
io :: (String -> String) -> JOP -> CoroutineT Term JOP (ReaderT InstEnv IO) ()
io a b = do 
	t <- await
	case t of 
		F g -> do 
			let g' = fromJust . fromDynamic $ g :: (String -> String) -> JOP -> JOP
			lift . lift . print $ g' a b
		S _ -> err "IO cannot function"


-- * this one is depricated
-- * cap the tail of a pipeline to send Move to Strategy or throw error if output is not function
capT :: forall m. MonadReader InstEnv m => CoroutineT Term Dynamic m ()
capT = await >>= \t -> case t of 
	F g -> yield g
	_   -> err "No Function Emitted"


-- * Strategy * --

--runStrat env = head . flip runReader env . runT

{-----------------------------------------------------------------------------
	Utils
------------------------------------------------------------------------------}

listToEither :: a -> [b] -> Either a b 
listToEither a m = case m of 
	[]   -> Left a
	x:xs -> Right x


{-----------------------------------------------------------------------------
	Tempoary js object 
------------------------------------------------------------------------------}

-- * this should not be in this file
-- * object holds DataName and some `value :: String` only
type JOP = [(String,String)]


obj :: DataName -> String -> JOP
obj dn v = (,) "TYPE" dn : (,) "v" v : []


typeof :: JOP -> Either String DataName
typeof []    = Left "No TYPE information found"
typeof (x:_) = if fst x /= "TYPE" then typeof [] else Right $ snd x 

valof :: JOP -> String
valof xs = if length xs > 1 then snd $ xs !! 1 else []









