{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleContexts #-}  
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}



import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Applicative

import Data.Monoid
import Data.Maybe
import Data.List

import Control.Monad.State
import Control.Monad.Morph
import Control.Parallel.Strategies
import Text.Show.Functions



{-

	General idea: 

	some data structure maybe hold some computations
	you need to define some higher order function to traverse the structure and apply 
	arbitray strategy to each computation 

	This separates how the structure is created from the sequence in which it's evaluated
-}



f :: Int -> Int 
f x = x + x



-- * In this case, `return` happens imediately, while f x and f y are evaluated in parralell
strat1 :: Int -> Int -> Eval (Int,Int)
strat1 x y = do 
	a <- rpar $ f x
	b <- rpar $ f y
	return (a,b)

-- * with seq, `return` happens  after f y is evaluated
-- * since rseq waits for the evaluation of its argument before returning.
strat2 :: Int -> Int -> Eval(Int,Int)
strat2 x y = do 
	a <- rpar $ f x
	b <- rseq $ f y
	return (a,b)

-- * here `return` does not happend until both f x and f y are evaluated, since rseq is called on value of x
strat3 :: Int -> Int -> Eval(Int,Int)
strat3 x y = do
	a <- rpar $ f x
	b <- rseq $ f x
	rseq a
	return (a,b)


-- * some strategy constructors and comCompators 

-- * create a strategy to work with
stratPair :: Strategy (a,b)
stratPair (a,b) = do
	a' <- rpar a 
	b' <- rpar b 
	return (a',b')

-- * to evaluate p using stratPair, call `runEval . stratPair $ ps`, or the synonym `using = runEval . stratPair`
-- * recall ps = stratPair p :: Eval (Int,Int)

-- * note p is a tuple of thunks
p = (1 + 2, 4 + 4) :: (Int,Int)
psv  = runEval . stratPair $ p
psv' = p `using` stratPair

-- * we can decompose using down even further to parametrize strategies
-- * by factoring out `rpar` from `stratPair`

evalPair' :: Strategy a -> Strategy b -> Strategy (a,b)
evalPair' s1 s2 (a,b) = do 
	a' <- s1 a 
	b' <- s2 b 
	return (a',b')


-- * note here that `rpar :: a -> Eval a` is a strategy itself
stratPair' = evalPair' rpar rpar




------------------ mock typeclass.js --------------------------------

-- ** Primitive Javascript Types ** --

data JType 
        = JUndef
        | JBool Bool
        | JNum  Double
		| JStr  String
        | JArr  [JType]
        | JArgs [JType]
        | JObj  [(String, JType)]
        | JFunc JType						
       deriving (Eq, Ord, Show)







data Stuff = forall a. Stuff a | B Bool

fs = undefined :: Stuff -> Stuff 

sfs :: Stuff
sfs = Stuff fs

-- ** primitive JType operations ** --

-- * the dot operation in OOP
oo :: JType -> String -> Maybe JType
oo (JObj xs) n = listToMaybe [ snd x | x <- xs, fst x == n ]
oo _ 		 _ = Nothing


-- * ask what does a TypeClass do ?? 
-- * it takes the name of a function to be called and js types, and search through the record for its instance before applying to params
-- * String -> [Instances] -> JType -> JType -> (JType, TypeClass)

-- * but it also does this computation of adding intances
-- * Instance -> [Instances] -> ((), TypeClass)

-- * so it looks like there's two operations it needs to suport "Instance Adding" and "Instance Evaluating"
-- * better yet, build a new typeclass form original

-- * TypeClass -> Instance -> TypeClass

-- * so typeclass, with only evaluating operation is just some mutli-param state monad
-- * TypeClass = Tc { eval :: String -> JType -> JType -> (JType, TypeClass) }

-- * except the part where evaluating the typeclass is statelemss, so typeclass is really just some reader monad


-- * following the reader monad idea, how would you re-express typeclass Impl by what it does??
-- * challenge the notion that it has to be stored in a record format: what if it's like this..

{-


	rethink the whole thing, let's pick one example and trace compuations

	type TypeName      = JType
	type TypeClassName = String


	surface : jfmap (\x -> x + x ) Just 12 = ... 

	detail  : jfmap (JFunc \x -> x + x) (JObj [("TYPE", JStr "Maybe"), ("v", JNum 12)])


	fmap runs in context of a store of implementations

	it looks up "TYPE" of (Just 12) and finds implementation in the store, the applies that implementation to g and ja

	so..


	jfmap :: [Instances] -> JType -> JType -> Reader [Instances] JType

	generearlized fmap is Binm

	runBinf :: Binm [Instance] -> (JType -> JType) -> JType -> Reader [Instances] JType
	runBinm :: Binf [Instance] -> JType -> JType -> Reader [Instances] JType
	runId   :: Idi [Instance] -> JType -> Reader [Instances] JType | None

	-- ^-- suggestion: instead of running in a reader, use partial application instead


	-- * these types cover all possible function signature of all typeclass interfaces
	data Idf  = IdF (JType -> JType) 		   | NonId
	data BinF = BinM (JType -> JType -> JType) | BinF ((JType -> JType) -> JType -> JType)
	
	-- * an instance is just a mapping between `TYPE` of a js data and its typeclass name and interface implementations 
	type Instance = (TypeName, TypeClassName, Idi, BinF)		
	
	

	-- * note there's a also a mapping from name of typeclass to name of its functions, 
	-- * ie ("Monoid", ["mempty", "mappend"]), ("Functor", ["", "fmap"])
	-- * further more, the names of those functions also exist in some namespace

	-- * Goal: make this and the stypeclass store the only implicitly stateful part of the application
	-- * namespace : JObj [("TYPE", JStr "Namespace"), ("fmap", JFun .. ), ("mappend", JFun ...), ("mempty", JObj [..]))]
	

	start again from the outside

	-- * specific implementation of fmap

	insts :: [Instance] 
	scope :: JType

	-- * at specific time of application
	fmap g m = flip evalReader insts $ binf g m 
	mappend x y = flip evalReader insts $ binm x y

	-- * as stored in scope, what happens when insts change? how do we explicity describe that??
	-- * in js, insts is in fact a pointer, but we don't want that ..
	-- * so actually we don't want partial application
	fmap    = flip evalReader insts 		:: (JType -> JType) -> JType -> Jtype
	mappend = flip evalReader insts 		:: JType -> JType -> Jtype

	-- * so actually this is all you have:
	fmap    = flip evalReader				:: [Instance] -> (JType -> JType) -> JType -> JType
	mappend = flip evalReader		     	:: [Instance] -> JType -> JType -> JType

	
	-- * this isn't quite right, you actually want a diff logic for each, let's write some out and see what we have

	type TypeClassInterface = Reader [Instance] JType
	type Promise 			= a -> JType


	-- * note in this implementation both binf and binm accept fully evaluated ids only


	-- * flip fmap
	binf :: JType -> (JType -> JType) ->  TypeClassInterface
	binf m g = do 
		case m `oo` "TYPE" of 
			Nothing -> error ""
			Just t  -> 
				is <- ask
				case is `inst` t of 
					Nothing       -> error ""
					Just (_,_,k)  -> let r = k g m in if check t r then return r else error ""  		


	-- * what about (>>=) :: m a -> (a -> m b) -> m b
	-- * it translates to :: JType -> (JType -> JType) -> JType
	-- * but does not follow logic of binf, more like binm, in fact monad is monoid in category of endofunctors 

	-- * mappend, ap , eq
	binm :: String -> JType -> JType -> TypeClassInterface
	binm n x y = do
		case (x `oo` "TYPE", y `oo` "TYPE") of 
			(Nothing, _) -> error ""
			(_, Nothing) -> error ""
			(t1, t2    ) -> case (isPromise n t1, isPromise n t2) of 
				(True, True)   -> error ""
				(True, False ) -> binm n (idf y) y
				(False, False) -> binm n x (idf x)
				_  			   -> case t1 == t2 of 
					False -> error ""
					True  -> 
						is <- ask
						case is `inst` t of 
							Nothing      -> error ""
							Just (_,_,k) -> let r = k x y in if check t r then return r else error ""  		
	

	-- * mempty, pure, mzero, return 
	idf :: JType -> a -> TypeClassInterface
	idf v x = do 
		case x `oo` "TYPE" of 
			Nothing -> error ""
			Just t  -> 
				is <- ask
				case is `inst` t of 
					Nothing      -> error ""
					Just (_,k,_) -> let r = k v in if check t r then return r else error ""


	checkR :: String -> JType -> Bool
	checkR t r = case r `oo` "TYPE" of 
		Nothing -> error ""
		Just t1 -> t == t1 
	
	isPromise :: String -> String -> Bool
	isPromise t1 = (==) t1 ++ "Promise" 


	-- * now onto the promise type, promise end up being a js object right?

	mempty = \_   -> JObj [("TYPE", "MonoidPromise"),   ("mappend", JFunc binm JUndef]  :: Promise
	pure   = \a   -> JObj [("TYPE", "AppPromise")   ,   ("pure"   , JFunc binm a    )]  :: Promise
	

	-- * in fact, this is the logic of all binary and unary operations in typeclass, so scope look like this
	-- * actually each one does do something differnt, so maybe it is different
	scope = JObj [("TYPE", JStr "scope"), ("fmap", JFunc flip evalReader), ("mappend", JFunc flip evalReader), ("mempty", JFunc flip eval Reader), ... ]
	
	



	Binf :: (JType -> JType) -> JType -> Reader [Instance] JType
	Binm :: (JType -> JType -> JType) -> JType -> JType -> Reader [Instance] JType


-}







-- maybe type constructor
--jmaybe :: JType
--jmaybe = JObj [("TYPE", JStr "Maybe"), ("just", jjust), ("nothing"), jnothing]


-- a maybe value constructor
jjust :: JType -> JType
jjust x = JObj [("TYPE", JStr "Maybe"), ("v", x)]

jnothing :: JType
jnothing = jjust JUndef

ja :: JType
ja = jjust . JNum $ 100

g :: JType -> JType
g (JNum x) = JNum $ x + 100							 



{-


	-- * JObj [("TYPE" : JStr "Maybe"), ("fmap" : JFunc ..), ("ap" : JFunc ..)]
	type JsData   = JType      					

	-- * a persistent list that stores all typeclasses
	--type TypeClassStore = [TypeClass]			

	-- * In typeclass.js, the following should be expressed by the ADT provided by adt.js

	-- * the typeclass type that persists and is read by its interface
	-- * note how this is a stateful implementation, how do you get rid of it????
	-- * you're using a record as if it's an object and writing stateful code in what should be stateless stuff
	--data TypeClass      = Tc  { tcn  :: String, is  :: [TypeClassImpl]			       }


	evalBinf :: (JType -> JType) -> JType -> Reader [TypeClassImpl] ()
	evalBinf g x = do 
		is <- ask 
		case x `oo` "TYPE" of 
			Nothing       -> error "no type"
			--Just (JStr t) -> case is `inst2` t of 
				--Nothing -> error "typeclass not implemented for type " ++ t
		--		Just i  -> undefined
		--		--cof i $ g x 

	inst2 :: [TypeClassImpl] -> String -> Maybe TypeClassImpl
	inst2 ts n = listToMaybe [ t | t <- ts, name t == n ]

	-- * note this is not some boxed computation, just a regular store
	-- * type built by expression `instance TypeClass where`
	-- * Note in javascript this would keep a pointer to the raw js data, which is bad
	-- * it can't even be expressed in haskell, so don't express it
	data TypeClassImpl  = Imp { name :: String, raw :: JsData, idf :: Idi, cof :: Bini }

	-- * Specific implementations of identity and binary function 
	-- * binary function is composition in the case of monoidal typeclass and mapping in the case of functoral typeclass
	-- * Identity implementation is either some function in monoidal typeclass or None in functoral typeclass
	data Idi  = Idi { runId  :: JType -> JType 	      }   | NoIdi
	data Bini = Binm { runCom :: JType -> JType -> JType } | Binf { runCof :: (JType -> JType) -> JType -> JType }


	-- ** typeclass implementations ** --

	instance Show Idi where 
		show (Idi f) = "<id>"
		show NoIdi 	 = "<NoId>"

	instance Show Bini where 
		show (Binm f) = "<Monoidal binf>"
		show (Binf f) = "<Functoral binf>"

	instance Show TypeClass  	where show (Tc n is)     = n ++ ": " ++ show is
	instance Show TypeClassImpl where show (Imp n _ x y) = n ++ " (<data> , " ++ show x ++ ", " ++ show y ++ ")"


	-- ** primitive operations ** --

	-- * the dot operation in OOP
	oo :: JType -> String -> Maybe JType
	oo (JObj xs) n = listToMaybe [ snd x | x <- xs, fst x == n ]
	oo _ 		 _ = Nothing

	-- * typechecking is just reading the first property of the object or default to native implementation
	jtype :: JType -> JType
	jtype xs = case xs of 
		JObj (x:_) -> snd x
		JObj []    -> JStr "Object"
		JUndef     -> JStr "Undefined"
		JBool _    -> JStr "Boolean"
		JStr  _    -> JStr "String"
		JArgs _    -> JStr "Object"
		JFunc _    -> JStr "Function"


	inst :: TypeClass -> String -> Maybe TypeClassImpl
	inst tc n = listToMaybe [ x | x <- is tc, name x == n ]

	tc :: TypeClassStore -> String -> Maybe TypeClass
	tc ts n = listToMaybe [ x | x <- ts, tcn x == n ]


	-- ** Program Logic ** --

	-- * this function is built by somes sort of functoral higher class function that give arise to fmap
	-- * when `jfmap` is called, the function first *dynamically* reads the type of the object, then find corresponding instance in store
	-- * Note `jfmap` could throw type error
	jfmap :: TypeClass -> (JType -> JType) -> JType -> JType
	jfmap tc g m = let jt = m `oo` "TYPE" in case jt of 
		Nothing        -> error "no type specified"
		Just (JStr t)  -> case tc `inst` t of 
			Nothing -> error $ "type " ++ t ++ " does not implement typeclass " ++ tcn tc
			Just i  -> undefined


	-- * functor over SumInts
	-- * this function is defined by `instance functor Maybe where fmap = ...`
	mfmap :: (JType -> JType) -> JType -> JType
	mfmap g m = case m `oo` "v" of 
		Just JUndef -> jnothing
		Just x      -> jjust $ g x


	-- * TESTS 

	t1, t2 :: TypeClassImpl
	t1 = Imp "list"   (JObj []) (Idi $ \_ -> JArr []) (Binm $ \x y -> x `o` y) where o (JArr xs) (JArr ys) = JArr $ xs ++ ys
	t2 = Imp "sumInt" (JObj []) (Idi $ \_ -> JNum 0 ) (Binm $ \x y -> x `o` y) where o (JNum x) (JNum y)  = JNum $ x + y 
	t3 = Imp "Maybe"  (JObj []) NoIdi $ Binf $ \g m -> g m


	jmon, jfun :: TypeClass
	jmon = Tc "Monoid" [t1,t2]
	jfun = Tc "Functor" [t3]	

	store = [jmon, jfun] :: TypeClassStore
-}





-- * the typeclass type match name of typeclass with a list of its implementations

--data TypeClass a b c = Tc { tcname :: TcName, is :: [TypeClassImpl a b c] }






------------------ mock sugaredAdt.js --------------------------------



-- * conclusion, what you want to do next in JS is a dead end (function returning a reference)
-- * so focus on making typeclasses in JS instead

data TypeConst  = T JType
	deriving (Eq) 							--- * T JStr "Maybe"

data ValueConst = V JType | Prom    
	deriving (Eq) 							--- * V (JArr [JStr String])

type Adt        = (TypeConst, ValueConst)   --- * Note this means you add more value constructors to Adt even after the promise is evaluated, which is in fact what happens inside adt.js

type Scope a    = (String, [a])  					

-- * TypeClass Implementations

instance Show TypeConst where 
	show (T (JStr s)) = show $ "TypeConst " ++ s

instance Show ValueConst where
	show (V (JArr xs)) = "ValueConst " ++ show ( extractJT <$> xs )
	show Prom   	   = "Promise Value constructors"

instance Monoid ValueConst where 
	mempty        = Prom
	a `mappend` b = case (a,b) of 
		(Prom, Prom) -> Prom
		(Prom, x   ) -> x
		(x   , Prom) -> x
		(V (JArr xs), V (JArr ys)) -> V . JArr $ xs ++ ys

-- * Primitive Type Operations

extractJT :: JType -> String
extractJT (JStr s)  = s 
extractJT _ 		= error "not implemented"

pureT :: String -> TypeConst
pureT = T . JStr

pureV :: [String] -> ValueConst
pureV  = V . JArr . fmap JStr

scope :: String -> Scope a
scope = flip (,) []

scopeTo :: a -> Scope a -> Scope a
x `scopeTo` (n, xs) = (n, x:xs)


-- * placeholder for `adt` in adt.js
adt_ :: String -> Adt
adt_ = flip (,) Prom . pureT 

-- * placeholder for 'type' in adt.js
type_ :: Adt -> [String] -> Adt
type_ (t,v) s = (t, v <> pureV s)


promise :: String -> State (Maybe Adt) ()
promise s = get >>= \a -> if a /= Nothing then error "unfufilled promise" else put . Just . adt_ $ s

fullfill :: [String] -> State (Maybe Adt) ()
fullfill v = do
	a <- get
	case a of 
		Nothing -> return ()
		Just t  -> put . Just $ type_ t v


-- * declaring a data type creates a tuple of typeconstructor and promise to create value constructor
jdata :: String -> Maybe Adt
jdata = flip execState Nothing . promise 

-- * fullfil promise to create value constructor and put the datatype into scope 
jval :: Maybe Adt -> [String] -> State (Scope Adt) ()
jval ma xs = do 
	let mb = flip execState ma . fullfill $ xs
	case mb of 
		Nothing -> return ()
		Just b  -> modify (scopeTo b)


-- * test pair, note the computation only allows *one* data to be declared at a time

d1 = jdata "Either"
s1 = flip execState (scope "window") $ jval d1 ["Left", "Right"]

d2 = jdata "Maybe"
s2 = flip execState s1 $ jval d2 ["Maybe", "Just"]

























