{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleContexts #-}  
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}


-- * {-# OPTIONS_GHC -ddump-simpl #-}


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | Module : Mock Application that create typeclass in javascript
-- | Creator: Xiao Ling
-- | Created: October 26th
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Strategy where 

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

-------------------------------
----------- Types -------------
-------------------------------

{-
	sources:
	http://www.aosabook.org/en/ghc.html
	http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.137.2063&rep=rep1&type=pdf
	http://stackoverflow.com/questions/7828072/how-does-haskell-printf-work
	http://okmij.org/ftp/Haskell/vararg-fn.lhs
	http://web.cecs.pdx.edu/~mpj/pubs/fundeps-esop2000.pdf
	http://www.haskell.org/wikiupload/d/dd/TMR-Issue8.pdf
	http://www.haskell.org/haskellwiki/Heterogenous_collections
	http://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/
	http://www.haskell.org/haskellwiki/Type_arithmetic

-}

-- * This part of the application is totally separate form the primitive js types defined above
-- * A `Plan` is the user defined signature put into list form, so that function signatures such as 
-- * `a -> a -> a`            becomes `["a", "a", "a"]`
-- * `a -> (a -> f a) -> f a` becomes `["a", "a -> f a", "f a"]`
-- * Each element in the list maps to a `Move`, and `Move`s sequence together to form a `Strategy`

-- * the signature expresses either some type A or some functon F
-- * note the original string is preserved
-- * Also ask if this is necessary
--data Sequence = A String | F String

--data Move = M String

---- * a strategy is a sequence of moves
--type Strategy m = CoroutineT Sequence Operation m ()

---- * an operation is a strategy waiting to be evaluated in context
--data Operation = J String

----------------------------------
---- Primitive Operations --------
----------------------------------

------------------------------------
---- play with simple types --------
------------------------------------


-- * So the trick here is to make it work with functions of arbitrary a, and arbitrary arity, so from some data type
-- * `Term a b` you can unpack a function of infinite arity and types
{-
	so if we could express some function of arb signature, how would we run it?

	since we can't see that far, let's just express the functions first

	train of thoughts:


	a typeclass can have operations of any arity, with a range of types
	for each typeclass signature you need to generate a strategy

	the generative function, therefore, need to emit a function of arbitrary arity and type

	so there needs to be some way to represent that

	choice 1: use signature of form `Jtype -> Jtype` where first argument is an array of arguments, and underlying type is hidden
		advantages:
			simulate js
		disadvantages:
			ties this bit of logic to the domain
			signature not expressive

	choice 2: use some typeclass foo shit form printf
		advantages:
			now this bit of logic work for all functions of any signature
		disadvantages:
			signature is still not expressive? 
	
	-- * annotate the type of some js data type
	type TYPE = String

	-- * annotate the signature of the member functions
	-- * all instances of typeclasses implemeting functions (in js) is assumed to *not* satify the signature
	-- * so at *runtime*, both inputs and outputs will be typechecked. 
	data VariadicF = ... 

	-- * A term is either TYPE or a variadic function
	data Term = T TYPE | F VariadicF

	-- * A move is a coroutine that awaits and yields terms
	data Move = MonadReader Env => CoroutineT Term Term m ()

	-- * envirnoment could hold any number of terms
	data Env  = E Term | En Term Env


	-- * you could also model the param and the result

	data Param a b = A a | b `Mul` Param a

	-- * and all functions are :
	
	VariadicF a b c = P { runV :: Param a b -> c }

	-- * evaluating the actual function would involve crawling through the param tree and 


	-- * alternatively, you could do:

-}

-- * mock print variadic functions

class FooType a where 
	bar :: IO () -> a

instance FooType (IO ()) where
	bar = id

-- * each arg has type IO
instance (Show x, FooType r) => FooType (x -> r) where
	bar s x = bar (s >> print x)

foo :: FooType a => a
foo = bar . return $ ()


-- note the polymorphic return type
class ManyStrings a where
  manyStrings :: String -> a

-- this is where the magic happens
-- we say that next is an instance of ManyStrings
-- so we get manyStrings :: String -> String -> next
-- and through the power of currying and recursion we get the varadic arguments
instance ManyStrings next => ManyStrings (String -> next) where
  manyStrings s1 s2 = manyStrings (s1 ++ " " ++ s2)

-- this is the base case for our typeclass recursion
instance ManyStrings String where
  manyStrings s = s



-- * first play with fundeps: http://web.cecs.pdx.edu/~mpj/pubs/fundeps-esop2000.pdf

-- * Add is some mutliparam type class where first two params uniqely determine c
-- * has one member function add that sends to function that sends b to c
class Add a b c | a b -> c where 
	add :: a -> b -> c

instance Add Int Int Int where
	add = (+)



-- * try one last time with functions with variable number variably typed args

-- * try again with example from : http://okmij.org/ftp/Haskell/vararg-fn.lhs

-- * r uniquely determines a
class BuildList a r | r -> a where 
	build :: [a] -> a -> r

instance BuildList a [a] where 
	build xs x = xs ++ [x]

instance BuildList a r => BuildList a (a -> r) where 
	build xs x x' = build (x:xs) x'

tolist :: BuildList a r => a -> r
tolist = build []


-- * use
-- toList True False True True :: [Bool]

-- * heterogenous collections
{-f
	Heterogeneous Collections:

	source: http://www.haskell.org/haskellwiki/Heterogenous_collections
	
	options: 

	1. algebraic datatypes that hide underlying value
		advantage:    simple. you know the space of underlying types
		disadvantage: absolutely no info on a specific underlying type.
					  number of types have to be fixed or clumsy to extend new types

	2. universal type Data.Dynamic 
		advantage    : work with all types
		disadvantage : there's no polymorphism of underlying type allow

	existential types:
		what it is: Essentially existential values pack up a value with operations on that value,
					and hide the actual value's types. Thus objects of differing types can be used,
					as long as they all provide a common interface.

		advantage: when you pair existential with a typeclass like: 
						`data Showable = forall a . Show a => MkShowable a`
					there is information on what you can do with the underlying type
					the only thing you could do here is to show it
-}

hlist :: [Dynamic]
hlist = [ toDyn "String", toDyn (1 :: Int), toDyn 'x', toDyn True]

-- * exitential types using GADT. 
-- * note the `forall a.` is not actually needed in this way of expression
data ShowMon where 
	ShowMon :: forall a. (Show a, Monoid a) => a -> ShowMon

-- * value constructor function
sh :: (Monoid a, Show a) => a -> ShowMon
sh = ShowMon

-- * create a heternogenous list
shs :: [ShowMon]
shs = [sh "hello", sh . Sum $ 1]

shs2 :: [ShowMon]
shs2 = [sh "world", sh . Sum $ 10]

-- * operations supported by the list, show
shs' :: IO ()
shs' = print $ (\(ShowMon a) -> show a) <$> shs

-- * operations suport by the list as seen by the signature, mappend
-- * However, this cannot be used since there's no guarentee the underlying element are matched up type-wise
--shs'' = zipWith mon shs shs2 where mon (ShowMon a) (ShowMon b) = ShowMon $ a <> b



-- * this is the alternative way of doing it, it hides all infromation about arity *and* type of params
-- * and you're back to hacking in dynamic typed environment which is no good.
-- * conclude that stuff you try to do with variadic function is morally similar to this and therefore even
-- * if you "get it", it'll be no better than this

data JT = T1 Int | T2 String | T3 Bool | T4 [JT] | T5 ()

data JF = JF { runJf :: JT -> JT }

addjt :: JT -> JT -> JT
addjt (T1 x) (T1 y) = T1 $ x +  y
addjt _      _      = error "uh oh" 


g :: JF
g = JF $ \(T4 xs) -> foldr (
	\x y -> case (x,y) of 
		(T1 _, T1 _) -> addjt x y
		_ 			 -> error "this whole thought proceess is pig-headed"
	) (T1 0) xs


gg = let (T1 x) = runJf g (T4 [T1 1, T1 2, T1 3]) in x





-- * initial Mockup with fixed numer of arguments and types of functions

-- * a term with only two possible functions
-- * a term is either a type or some very specific computation signature
data Term a b = T String | U (a -> b) | B (b -> b -> b) 


unU :: Term a b -> (a -> b)
unU t = case t of 
	U g -> g
	_   -> error ""

unB :: Term a b -> (b -> b -> b)
unB t = case t of 
	B g -> g 
	_   -> error ""


-- * A move is a single coroutine taking some `c` as input and emitting `Term a b` while reading some `Env a b`
type Move a b     = MonadReader (Env a b) m => CoroutineT (Term a b) (Term a b) m ()

-- * A strategy is just a sequence of moves composed together
type Strategy a b = Move a b


-- * Env Envs a term and two computations
type Env a b = (Term a b, Term a b, Term a b)


unFst :: Env a b -> Term a b 
unFst (_,g,_) = g 

unSnd :: Env a b -> Term a b 
unSnd (_,_,g) = g

name :: Env a b -> Term a b
name (n,_,_) = n

-- * lift a strategy into Env
liftTo :: Strategy a b -> Env a b -> Term a b
liftTo sg = head . runReader (runT sg) 


env :: Env a [a]
env = (T "List",  U $ \x -> [x], B $ \x y -> x ++ y)

env' :: Env a a
env' = (T "", U $ \x -> x, B $ \x _ -> x)


-- * note the parsed thing can be used as string
{-
	some function should fold over the plan and convert it to a
	stategy, or coroutine pipeline, this will ouput some
	operation ready to be impelmented

-}

-- * chkType is a move
chkType :: Move a b
chkType = do 
	s <- lift ask
	case name s of 
		T [] -> err "no Type"
		x    -> yield x

-- * find func is another move
fndFunc :: Move a b
fndFunc = await >> lift ask >>= \s -> yield . unSnd $ s

-- * function to autmoatcially create strategy
-- * this is directly pattern matching on list of string
fromSig :: [String] -> Strategy a b
fromSig xs = if xs == [] then stop else go xs where 
	go []     = pass
	go (x:xs) = case x of 
		"a"    -> chkType ~> go xs
		"(..)" -> fndFunc ~> go xs
		_      -> go xs

-- * create a strategy and lift into enviornment
opr' :: Term a [a]
opr' = fromSig ["a", "(..)"] `liftTo` env

-- * unbox the computation in operation and bind to parameters
oprt' :: [Int]
oprt' = unB opr' [1..3] [5]


-- * manually make a strategy 

-- * a strategy is a sequence of moves
strat :: Strategy a b
strat = chkType ~> fndFunc ~> pass

-- * op is a strategy waiting to be lifted into an environment and bound
-- * to variables
op :: Env a b -> Term a b
op = head . runReader (runT strat) 

-- * lift operation into reader context and bind variables
opr :: [Int]
opr = unB (op env) [1..3] [5]





-- * mock a signature move map, therein lies the problem since they have differnt signatures
-- * first option: is there another way to make this map?
-- * for example, as you crawl over the signature you could build a coroutine
-- * the signature of this would be:
-- * except b would be some functon, but b could also be of any arity which might cause a problem
--fromSig :: [String] -> CoroutineT a (b -> b -> b) (Reader (Env a b)) ()

-- * this is morally equivalent to the map thing, except the mapping and the composing and parsing is all put in one function
--fromSig []     = pass
--fromSig (x:xs) = case x of 
--	"a"    -> chkType ~> fromSig xs
--	"(..)" -> fndFunc ~> fromSig xs

-- * the emitted function is some language, try to draw some inspiration from Core

-- * make a toy move-strategy system with simple input and output types

-- * moves to do arithmetic operations
type IntMove x = CoroutineT Int Int (Reader x) ()

-- * a strategy to do arithmetic opertions . A series of moves compose to form a strategy
type IntStrat x = CoroutineT Int Int (Reader x) ()

s1 :: IntMove Int
s1 = lift ask >>= \s -> yield $ s + 1

s2 :: IntMove Int 
s2 = await >>= \x -> lift ask >>= \s -> yield $ s + x

s3 :: IntMove Int 
s3 = await >>= \x -> lift ask >>= \s -> yield $ s + x 


fromSig' :: [String] -> IntStrat Int
fromSig' xs = if xs == [] then stop else s1 ~> go xs where 
	go []     = pass
	go (x:xs) = case x of 
		"a"    -> s2 ~> go xs
		"(..)" -> s3 ~> go xs
		_      -> go xs

s12 :: IntStrat Int
s12 = fromSig' ["a", "a", "(..)", "a"]

ts12 :: [Int]
ts12 = runReader (runT s12) 100


-- * now how do you box a and b so that the signature is nice?


-------------------------------------
---- play with serious types --------
-------------------------------------

{-
	
	things that needs to happen:

	steps need to be able to go inside one list or some other foldable structure
	Envs potentially need to go inside one list
	the arity of the function need not be exposed

	so the types have to be carefully designed to accomadate inflow and outflow??

	what if it doesnt matter?

-}
















