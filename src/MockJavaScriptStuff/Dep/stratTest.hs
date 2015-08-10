{-# LANGUAGE GADTs #-} {-# LANGUAGE FlexibleContexts #-}  {-# LANGUAGE ImpredicativeTypes #-} {-# LANGUAGE NoMonomorphismRestriction #-}


module StratTest where 

import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Dynamic
import Data.Monoid
import Data.Maybe
import Data.List

import Control.Monad.State
import Text.Show.Functions

import Coroutine
import Strategy2

{-----------------------------------------------------------------------------
	Test primitives
------------------------------------------------------------------------------}

-- * Monoid * --

-- * class Monoid a where ...
monClass :: Class 
monClass = toClass "Monoid" [("mempty", "a"), ("mappend", "a -> a -> a")]

-- * instance Monoid (Sum Int) where ... 
monSumInt :: Instance
monSumInt = toInst "Monoid" "SumInt" $ [mem "mempty" g1, mem "mappend" g2] where 
	g1 :: () -> () -> Int
	g1 _ _ = 0
	g2 :: Int -> Int -> Int
	g2 = (+)


monListChar :: Instance
monListChar = toInst "Monoid" "ListChar" $ mem "mempty" g1 : mem "mappend" g2 : [] where 
	g1 :: () -> () -> JOP
	g1 _ _ = obj "ListChar" []
	g2 :: JOP -> JOP -> JOP
	g2 o1 o2 = obj "ListChar" $ valof o1 ++ valof o2


-- * Collection of all monoid instances
monEnv :: InstEnv
monEnv = putInst monListChar . putInst monSumInt . env $ "Monoid"

-- * Applicative * --
apClass :: Class
apClass = toClass "Applicative" $ ("pure", "a -> f a") : ("ap", "f (a -> b) -> f a -> f b") : []

-- * note even though applicatives is defined over functors, a parametrized type, this is not allowed with Data.Dynamic
apListChar :: Instance
apListChar = toInst "Applicative" "ListChar" $ mem "pure" g1 : mem "ap" g2 : [] where 
	g1 :: () -> Char -> JOP
	g1 _ = obj "ListChar" . pure
	g2 :: JOP -> JOP -> JOP
	g2 = error "current implemetnation does not allow function inside lists"


apEnv :: InstEnv
apEnv = apListChar `putInst` env "Applicative"

-- * Monad * -- 

monaClass :: Class
monaClass = toClass "Monad" [("eta", "a -> m a"), ("mu", "m (m a) -> m a")]

-- * note you can't have undeclared parametric variables here 
-- * this is a fatal shortcoming but such restrictions will not be present in js
monaListChar :: Instance
monaListChar = toInst "Monad" "ListChar" $ [mem "eta" eta, mem "mu" mu, mem "bind" bind] where 
	eta :: () -> Char -> JOP
	eta _  = obj "ListChar" . pure
	mu :: () -> JOP -> JOP
	mu _ m = error "currently cannot hold anything other than string in JOP"
	bind :: JOP -> (String -> JOP) -> JOP
	bind m g = case valof m of 
		"" -> m
		x  -> g x

monaEnv :: InstEnv
monaEnv = monaListChar `putInst` env "Monad"

-- * functor * --

funClass :: Class 
funClass = toClass "Functor" . pure $ ("fmap", "(a -> b) -> f a -> f b") 

-- * note due to constraints of Typeable *and* JOP implementation, mapping function has to be of concrete type and String only
funListChar :: Instance
funListChar = toInst "Functor" "ListChar" . pure $ mem "fmap" f where 
	f :: (String -> String) -> JOP -> JOP
	f g = obj "ListChar" . g . valof


funEnv :: InstEnv
funEnv = funListChar `putInst` env "Functor"


-- * make some js object representation * --
o1, o2 :: JOP
o1 = obj "ListChar" "hello" 
o2 = obj "ListChar" "world" 


{-----------------------------------------------------------------------------
	Test Move && Strategy
------------------------------------------------------------------------------}

testStrat :: InstEnv -> Move -> Term
testStrat env s = head . flip runReader env . runT $ s

-- * test typed
test1 = let (S n) = head $ runReader (runT $ capH ~> typed o1) monEnv in n

-- * test sameType
test2 = testStrat monEnv $ source [S ["A", "A"]] ~> sameType
test3 = testStrat monEnv $ source [S ["A", "B"]] ~> sameType

-- * test typed and sameType
test4 = testStrat monEnv $ capH ~> typed o1 ~> sameType

-- * test fndFunc
test5 = testStrat monEnv  $ source [S ["SumInt", "SumInt"]] ~> fndFunc "mappend"
test6 = testStrat monEnv  $ source [S ["SumInt", "SumInt"]] ~> fndFunc "mempty" 
test7 = testStrat monEnv  $ source [S ["nada"  , "SumInt"]] ~> fndFunc "mempty" 
test8 = testStrat monaEnv $ source [S ["SumInt", "SumInt"]] ~> fndFunc "mempty" 

-- * test on strategy
test9 = testStrat monEnv $ capH ~> typed o1 ~> typed o2 ~> sameType ~> fndFunc "mappend" 

-- * test capT
test10 = head . flip runReader monEnv . runT $ source [F . toDyn $ (1 ::Int)] ~> capT
test11 = flip runReader monEnv . runT $ source [S []] ~> capT

-- * prelim attemp at generating a function ready to bind
-- * for now don't worry about how to actually run the function once emitted
-- * the goal here is to recover some semblance of `mappend :: a -> a -> a`
-- * right now you have to bind it to seme function which then runs it
-- * another layer ontop of mappendPrelim, this gives you a mappend function, thought it only works
-- * on JOP since anything packed by `toDyn` needs to have all types excplicity defined, so no 
-- * parametrized types `Functor f => f a` or polymorphic test `a`

-- * Monad * --

-- * strategy for eta is same as pureStrat
etaStrat :: a -> JOP -> Strategy
etaStrat v x = capH ~> typed x ~> fndFunc "eta" ~> capT

eta :: Typeable a => InstEnv -> a -> JOP -> JOP
eta env v x = g () v where g = fromJust . fromDynamic . head . flip runReader env . runT . etaStrat v $ x

-- * here how do we check closure is satisfied? within the pipeline?
-- * is it possible to simulate a run?
bindStrat :: JOP -> (a -> JOP) -> Strategy
bindStrat x g = capH ~> typed x ~> capT


-- * Monoid * --

-- * some semblance of mappendStrat should go into the Class tuple
-- * and class tuple would go into some list read to be lifted into env?
-- * this is a strategy that would be automatically built
mappendStrat :: JOP -> JOP -> Strategy
mappendStrat x y = capH ~> typed x ~> typed y ~> sameType ~> fndFunc "mappend" ~> capT

-- * with mempty you have to find the appropriate instance in store by comparing to some known non-mempty or monoidal else output promise?
-- * think about it for a bit and bring 

-- * what's a use case for this? well when it's emitted, you're emitting a promise
memptyStrat :: a -> JOP -> Strategy
memptyStrat v x = capH ~> typed x ~> fndFunc "mempty" ~> capT


-- * this is a naked strategy ready to be lifted to environment and bound to variables
mappend_ :: InstEnv -> JOP -> JOP -> JOP
mappend_ env x y = g x y where g = fromJust . fromDynamic . head . flip runReader env . runT . mappendStrat x $ y

-- * This is the naked mempty, not it needs a non-mem element from some monoidal set to resolve the mempty instance
mempty_ :: InstEnv -> JOP -> JOP
mempty_ env x = g () () where g = fromJust . fromDynamic . head . flip runReader env . runT . memptyStrat () $ x


-- * Applicative * --

-- * manuall mock applicaitve, should be the same rules as monoid
apStrat :: JOP -> JOP -> Strategy
apStrat x y = capH ~> typed x ~> typed y ~> sameType ~> fndFunc "ap" ~> capT

-- * pure is slightly different in that it takes some a. note even though in this signature a is a parametric, dyanmic demains a named type
pureStrat :: a -> JOP -> Strategy 
pureStrat v x = capH ~> typed x ~> fndFunc "pure" ~> capT

pure_ :: Typeable a => InstEnv -> a -> JOP -> JOP
pure_ env v x = g () v where g = fromJust . fromDynamic . head . flip runReader env . runT . pureStrat v $ x


-- * reformulate monoid using strategy * -- 

-- * note here the params can be passed in normally
mappendStrat' :: JOP -> JOP -> Strategy2
mappendStrat' x y = capH ~> typed x ~> typed y ~> fndFunc "mappend" ~> eval x y

mappend_' :: InstEnv -> JOP -> JOP -> JOP
mappend_' env x y = head $ runReader (runT $ mappendStrat' x y) env

-- * mempty demands () be passed in for both params of eval 
-- * mem strat does not require some `v` though
memStrat' :: Typeable a => a ->  JOP -> Strategy2
memStrat' v x = capH ~> typed x ~> fndFunc "mempty" ~> eval () v

mempty_' :: InstEnv -> JOP -> JOP
mempty_' env x = head $ runReader (runT $ memStrat' () x) env 


-- * reforumluate monad and now add typecheck for outcome * -- 

-- * note Strategy signature needs work
-- * note how the type of x can be inspected but the type of g cannot be inspected until
-- * after evaluation. this makes it possible to defined `>>=` pipelines that have hidden type error 
-- * that will not be caught until run time
bindStrat' :: (Typeable a, Monad m) => JOP -> (a -> JOP) -> CoroutineT Term JOP (ReaderT InstEnv m) ()
bindStrat' x g = capH ~> typed x ~> fndFunc "bind" ~> eval x g ~> satClosure x

bind' :: Typeable a => InstEnv -> JOP -> (a -> JOP) -> JOP
bind' env x g = head $ runReader (runT $ bindStrat' x g) env

-- * function that satisfies closure as defined by "TYPE" tuple
g :: String -> JOP
g xs = if xs == [] then obj "ListChar" "a" else obj "ListChar" $ pure . head $ xs

-- * function that does not sat closure as defined by "TYPE" tuple
g' :: String -> JOP
g' xs = if xs == [] then obj "ListString" "z" else obj "ListString" $ pure . head $ xs

-- * the first one works great, the second one throws error as intended
og, og' :: JOP
og  = bind' monaEnv o1 g
og' = bind' monaEnv o1 g'


-- * reformulate applicative's pure using strateg2 * --

-- * note here there's a parameter mismatch since pure takes () as first param

-- * Note here we really have to make sure the function inside Dynamic is passed the 
-- * correct types, since Dynamic removes all type information
-- * in this case inappropriate params are passed to pureStrat', what's a way to ensure it 
-- * doesn't happen?
pureStrat' :: Typeable a => a -> JOP -> Strategy2
pureStrat' v x = capH ~> typed x ~> fndFunc "pure" ~> eval () v

pure_' :: Typeable a => InstEnv -> a -> JOP -> JOP
pure_' env v x = head $ runReader (runT $ pureStrat' v x) env

-- * formulate functor * -- 


fmapStrat' :: (String -> String) -> JOP -> Strategy2
fmapStrat' g x = capH ~> typed x ~> fndFunc "fmap" ~> eval' g x 

test :: JOP
test = head . flip runReader funEnv . runT $ fmapStrat' g'' o1

g'' :: String -> String
g'' = flip (++) " functor says hi"

-- * note signature of fmap_ can be some polymorphic type from set `Typeable`
--fmap_ :: (Typeable a, Typeable b) => InstEnv -> (a -> b) -> JOP -> JOP
--fmap_ env g x = head $ runReader (runT $ fmapStrat' g x) env 


{-
	what to do the next 30 minutes:
	
	workout details of strategy2, deprecate strategy1 and strat

	workout details of Term first, how loaded should it be?

	workout some principle way to pass information around <--> decompose moves into combinators

-}

{-----------------------------------------------------------------------------
	throwaway tests
------------------------------------------------------------------------------}

-- * test veritcally integrated strategy for mempty
vertmem :: FuncName -> a -> JOP -> Move
vertmem fn v d = do
	env <- lift ask
	case typeof d of 
		Right t -> case env `askInst` t of 
			Right i -> case i `askFunc` fn of 
				Right g -> yield . F $ g
				Left e  -> err e 
			Left e  -> err e
		Left e   -> err e


-- * run vertemem and send Term to Dynamic
vertmem' :: Dynamic
vertmem' = g where F g = head $ runReader (runT $ vertmem "mempty" 100 o1) monEnv 

-- * run the function and get back the mempty result 
vertmem'' :: JOP
vertmem'' = g () () where g = fromJust . fromDynamic $ vertmem'

-- * test strategy that also evaluates the value

r :: Strategy2
r = source [F $ toDyn g] ~> eval o1 o2 where
	g :: JOP -> JOP -> JOP
	g _ _ = obj "ListChar" "example r"


r' :: JOP
r' = head $ runReader (runT r) apEnv

-- * not going to be relevant but let's see if you can make one for some other type
-- * it does work in fact, but all the types have to declared again
rr :: Strategy2
rr = source [F . toDyn $ g] ~> eval (1 :: Int) (2 :: Int) where
	g :: Int -> Int -> Int
	g = (+)

rr' :: Int
rr' = head $ runReader (runT rr) apEnv

{-
	Today's goal:

	mock more use cases manually to uncover problems
	find more fundamental combinators for move?
	redesign store?
	make a parser that crawls through signature and send signature to Strat


-}




-- * test vertically integrated stategy for mappend

-- * vertically integrated strategy to evaluate binary function 
vert :: FuncName -> JOP -> JOP -> Move
vert fn d1 d2 = do 
	case (typeof d1, typeof d2) of 
		(Right t1, Right t2) -> case t1 == t2 of 
			False -> err $ t1 ++ " does not match " ++ t2
			_     -> do 
				env <- lift ask 
				case env `askInst` t1 of 
					Right i -> case i `askFunc` fn of 
						Right g -> yield . F $ g
						Left e  -> err e
					Left e  -> err e
		_ 					 -> err "No Type specified"


-- * monAdd = mappend
-- * this gives you the gist of what you want, excpet the implemetnatio of [Char] mappend is in terms of prmitive [Char], not JOP
monAdd :: InstEnv -> JOP -> JOP -> String
monAdd env d1 d2 = let (F g) = head $ runReader (runT $ vert "mappend" d1 d2) env in fromJust . fromDynamic $ g


-- * dep

-- * note this is just [a] where a is Char but remember you can't toDyn parameterized types
monStr :: Instance
monStr = toInst "Monoid" "String" $ mem "mempty" g1 : mem "mappend" g2 : [] where
	g1 :: () -> () -> String
	g1 _ _ = []
	g2 :: String -> String -> String
	g2 = (++)




{-----------------------------------------------------------------------------
	An Altnerate Binary Function datatype design
	
	* Function representation where types are existentially quantified
	* Advatanges: 
		you can have arbitrary underlying type, including parameterized types
	* Disadvantages: 
		can't run the underlying function unless you declare some typeclass on the underlying
		this veers close to the existential-quanitification anti-pattern
------------------------------------------------------------------------------}

-- * one can construct some polymorphic list using forall, thus destroying all information of underlying types
-- * you need to declare some run class for it so it can actually be run?
data Func where 
	Func :: (a -> b -> c) -> Func

-- * now let's see if you can run it
-- * how do you declare a runable typeclass
-- * intuition says you cant because you can't gaurentee which a and b you'll pass in right?
-- * morally you want this, which you can't since you can't guarentee what's 'wrapped' will be 
-- * some funtion Int -> Int -> Int
--runit :: Func -> Int
--runit (Func g) = g 1 2 

-- * note here with Func, you can have parameteric types
-- * except there's nothing you can do with underlying types right?
-- * so when the ttime comes to run it, you can't
ls'' :: [(FuncName, Func)]
ls'' = ("eta", eta'') : ("mu", mu'') : []

eta'' :: Func
eta'' = Func $ \_ x -> Just x

mu'' :: Func
mu'' = Func $ \_ m -> case m of 
	Nothing 	  -> Nothing
	Just Nothing  -> Nothing
	Just (Just x) -> pure x





