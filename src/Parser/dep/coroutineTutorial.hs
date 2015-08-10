{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}  
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction #-} 

----------------------------------------------------------------------------------------------------------------------------------
-- | Coroutine tutorial
-- | Concepts:
-- | Monad Transform
-- | Monad Morphism
-- | http://themonadreader.files.wordpress.com/2011/10/issue19.pdf

----------------------------------------------------------------------------------------------------------------------------------


import Prelude hiding ((.),id)
import Control.Monad
import Control.Category
import Control.Applicative
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans.Class

import Data.Monoid
import Data.Maybe
import Data.Functor.Identity 
import Data.Functor.Compose


-- | stuff from: fpcomplete, monad.coroutine, machines
-- | source: https://www.fpcomplete.com/school/pick-of-the-week/coroutines-for-streaming/part-2-coroutines
-- | unrelated: http://www.cs.ox.ac.uk/people/daniel.james/functor/functor.pdf
-- | unrelated: https://www.fpcomplete.com/user/liyang/profunctors
--http://imgur.com/gallery/Pdj6C

--------------------------------- The Monad Reader Issue 19 Tutorial  ---------------------------------



--------------------- I. Simplest example: Trampoline, or Non-cooprative coroutine  ---------------------

-- What: Lifting a computation into Trampolinei context alternates the compuation from pause to go

-- | m is base monad
-- | r is result of function
-- | within base monad, Either Left (pause, Trampolinei m r) or Right (output r)
data Trampolinei m r = Trampi { bounce :: m (Either (Trampolinei m r) r) }


-- | implement functor for practice
-- | what I should learn: how to visualze layers of compuations
-- | g is applied to result r of compuation, if compuations is not yet suspended
-- | then pass application of g donwstream until suspension
instance Monad m => Functor (Trampolinei m) where
	fmap g (Trampi m) = Trampi $ liftM (apply g) m
		where 
			apply g (Right r) = Right $ g r 
			apply g (Left  s) = Left $ fmap g s


instance Monad m => Applicative (Trampolinei m) where
	pure = Trampi . return . Right
	(Trampi g) <*> (Trampi m) = Trampi $ liftM2 apply g m
		where 
			apply (Right a) (Right b) = Right $ a b
			apply (Right a) (Left b ) = Left $ pure a <*> b 
			apply (Left a ) (Right b) = Left $ a <*> pure b
			apply (Left a ) (Left b ) = Left $ a <*> b


-- | (>>=) :: m a -> (a -> m b) -> m b
instance Monad m => Monad (Trampolinei m) where
	return = Trampi . return . Right
	(Trampi m) >>= g = Trampi $ m >>= either (return . Left . (>>=g)) (bounce . g)


-- | lifts the base monad into Trampolinei context
instance MonadTrans Trampolinei where
	lift = Trampi . liftM Right 


-- | builds a trivial Trampolinei that output _|_
pauset :: Monad m => Trampolinei m ()
pauset = Trampi . return . Left $ return ()

-- | removes the pauses and run function to completion
-- | at each layer, either peel another layer off or return value to pure context
runt :: Monad m => Trampolinei m r -> m r
runt t = bounce t >>= either runt return


-- | interleave two Trampolineis
-- | note here a function g takes as param outcome of the two Trampolineis
-- | And by def of applicative, the two Trampolineis DO NOT interact until both have completed one bounce:
-- | "the function interleave will wait for all Trampolineis to complete their first
-- | bounces before it initiates their second bounces. 
-- | This is cooperative, rather than preemptive multi-tasking."

-- | can think of this as sandboxed coroutines where each thread runs independantly and only interact when hitting a pause
mzipWith :: Monad m => (a -> b -> c) -> Trampolinei m a -> Trampolinei m b -> Trampolinei m c
mzipWith g ta tb = g <$> ta <*> tb 

-- | interleave arbitrary number of Trampolineis
-- | put the results into a list
interleave :: Monad m => [Trampolinei m r] -> Trampolinei m [r]
interleave = foldr (mzipWith (:)) $ return [] 


-- some examples uses --
hello :: Trampolinei IO Int
hello = do 
	lift . putStr $ "hello, "
	pauset  					 -- * note this function creates a Trampolinei m () type
	lift . putStr $ "world! "	 -- * note this funtion creates a Trampolinei IO () type
	pauset 
	return 200

hello' :: Trampolinei IO Int
hello' = m >> pauset >> m >> pauset >> return 200
	where m = lift . putStr $ "repeat \n"

greet :: Trampolinei IO Int
greet = do
	lift . putStr $ "how do you do, \n"
	pauset 
	lift . putStr $ "fine morning! \n"
	pauset 
	lift . putStr $ "last word! \n"
	return 404


-- | runt hello   -> hello, world! 200
-- | to run interleaved version
-- | runt $ interleave [hello, greet]



--------------------- II. Generators and Iteratees  ---------------------


-- II.a Generatoris
-- | Instead of suspend (Right r in Trampolinei), there's yieldi, which outputs a value upon suspension
-- | question: how is this a suspension functor?


-- | Data Step a m r = Yield a Generatori a m r | Done r
-- | note monad m comes first in type parametrization to support profunctor definition
data Generatori a m r = Gen { genbounce :: m (Either (a, Generatori a m r) r) }



-- | note how functor and monad below are parametrized over return type r

instance Monad m => Functor (Generatori a m) where 
	fmap g (Gen m) = Gen $ liftM (apply g) m
		where 
			apply g (Right r)    = Right $ g r
			apply g (Left (a,s)) = Left $ (a,fmap g s)


instance (Monad m, Monoid a) => Applicative (Generatori a m) where
	pure = Gen . return . Right 
	(Gen g) <*> (Gen m) = Gen $ liftM2 apply g m
		where 
			apply (Right f) (Right r)         = Right $ f r 
			apply (Right r) (Left (a,s))      = Left $ (a, pure r <*> s)
			apply (Left (a,s)) (Right r)      = Left $ (a, s <*> pure r)
			apply (Left (a,s)) (Left (a',s')) = Left $ (a <> a', s <*> s') 


instance Monad m => Monad (Generatori a m) where
	return = Gen . return . Right
	(Gen m) >>= g = Gen $ m >>= \e -> case e of 
		Right r      -> genbounce $ g r
		Left (a,m')  -> return . Left $ (a, m'>>=g)


instance MonadTrans (Generatori a) where
	lift = Gen . liftM Right


-- | question: 2nd elem in tuple should be of type Generatori a m ()
-- | but how does return know current context is Generatori a m r? from type signature?
yieldi :: Monad m => a -> Generatori a m ()
yieldi a = Gen . return . Left $ (a,return ()) 


runGeni :: Monad m => Generatori a m r -> m ([a], r)
runGeni = run id where 
	run g (Gen m) = m >>= either (\(a,m') -> run (g . (a:)) m') (\r -> return (g [], r))


-- | exmaple Generatori
gen :: Generatori Int IO String
gen = do
	lift . putStr $ "yieldi one "   -- * Generatori a IO ()
	yieldi 1  					   -- * Generatori Int m r
	lift . putStr $ "yieldi two "   
	yieldi 2
	return "all done" 			   -- * Generatori a m String

-- | desugared
gen' :: Generatori Char IO Int
gen' = (lift . putStr $ "yieldi a \n") >> yieldi 'a' >> yieldi 'b' >> return 404


-- II.b Iterateeis
-- | upon suspension, Iterateeis demand a value instead of yieldiing one. Like an inverted/dual to Generatoris

-- | data Step a m r = Await a -> Step a m r | Done r
data Iterateei a m r = Iter { bounceIter :: m (Either (a -> Iterateei a m r) r)}


instance Monad m => Functor (Iterateei a m) where
	fmap g (Iter m) = Iter $ liftM (apply g) m 
		where 
			apply g (Right r) = Right $ g r
			apply g (Left s ) = Left $ (fmap g) . s


instance Monad m => Applicative (Iterateei a m) where
	pure = Iter . return . Right
	(Iter m1) <*> (Iter m2) = Iter $ liftM2 g m1 m2
		where 
			g (Right r1) (Right r2) = Right $ r1 r2
			g (Right r) (Left f)    = Left $ \a -> pure r <*> f a
			g (Left f) (Right r)    = Left $ \a -> f a <*> pure r
			g (Left f1) (Left f2)   = Left $ \a -> f1 a <*> f2 a    -- * Does this make sense? both applicative take same input param a


instance Monad m => Monad (Iterateei a m) where
	return = Iter . return . Right
	(Iter m) >>= g = Iter $ m >>= \e -> case e of 
		Right r -> bounceIter . g $ r
		Left k  -> return . Left $ \a -> k a >>= g


instance MonadTrans (Iterateei a) where
	lift = Iter . liftM Right 


-- | implement these
awaiti :: Monad m => Iterateei a m a
awaiti = Iter . return . Left $ \a -> return a  -- * Left branch awaitis an a, and build Iter $ m (Right branch) 


-- | Iterateei awaitis list of a's until list empty
runIteri :: Monad m => [a] -> Iterateei a m r -> m r 
runIteri (a:as) (Iter m) = m >>= either (\f -> runIteri as $ f a) return   -- * f :: a -> Iterateei a m r
runIteri []     (Iter m) = m >>= either (\f -> runIteri [] $ f $ error "no more vals") return


-- | test iter
iter :: Iterateei Int IO Int
iter = do 
	lift . putStr $ "enter two numbers \n"
	a <- awaiti
	b <- awaiti
	let c = a + b
	lift . putStr $ "sum is " ++ (show c) ++ "\n"
	return c



--------------------- III. Generalizing Corouties from Generators and Iteratees  ---------------------


-- | note the right value of: 
-- | Trampoline: Trampoline m r
-- | Generator : (o, Generator m o r)
-- | Iterator:   i -> Iterator m i r

-- | Note all are some type containing the coroutine, and all are functors
-- | generalize Right: Functor s => s (Coroutine m i o)


data Coroutine s m r = Co { resume :: m (Either (s (Coroutine s m r)) r) }


instance (Functor s, Monad m) => Functor (Coroutine s m) where
	fmap g (Co m) = Co $ liftM ap m
		where 
			ap (Right r) = Right $ g r
			ap (Left s)  = Left $ fmap (fmap g) s


instance (Functor s, Applicative s, Monad m) => Applicative (Coroutine s m) where
	pure = Co . return . Right
	(Co m1) <*> (Co m2) = Co $ liftM2 ap m1 m2
		where 
			ap (Right r1) (Right r2) = Right $ r1 r2
			ap (Right r) (Left s)    = Left $ (\c -> pure r <*> c) <$> s 
			ap (Left s) (Right r)    = Left $ (\c -> c <*> pure r) <$> s
			ap (Left s1) (Left s2)   = Left $ (<*>) <$> s1 <*> s2


instance (Functor s, Monad m) => Monad (Coroutine s m) where
	return = Co . return . Right
	(Co m) >>= g = Co $ m >>= \s -> case s of  
		Right r -> resume . g $ r
		Left k  -> return . Left . fmap (>>=g) $ k


instance MonadTrans (Coroutine s) where
	lift = Co . liftM Right


-- | suspend puts a suspension functor inside coroutine in Left Branch
-- | it's the dual of return
suspend :: (Monad m, Functor s) => s (Coroutine s m r) -> Coroutine s m r
suspend = Co . return . Left 


-- | now redefinne Trampoline, Generator and Iteratee as variation of coroutines

type Trampoline m r = Coroutine Identity m r     -- * Co { resume :: m (Either (Identity (Trampoline m r)) r) }
type Generator a m r = Coroutine ((,) a)  m r    -- * Co { resume :: m (Either (a, Generator m r) r) }
type Iteratee a m r  = Coroutine ((->) a) m r    -- * Co { resume :: m (Either (a -> Iteratee m r) r) }


-- | builds a trivial functor wrapping trivial coroutine a m ()
pause :: Monad m => Trampoline m ()
pause = suspend . Identity $ return ()


-- | builds nontrivial functor with values a, and trivial coroutine a m ()
yield :: Monad m => a -> Generator a m ()
yield a = suspend $ (a, return ())


-- | builds nontrivial functor :: a -> iteratee a m a, 
await :: Monad m => Iteratee a m a
await = suspend $ \a -> return a


-- | peels off the layers of trampoline and expose underlying monad
-- | note runIdenitity peels off Identity wrapper
run :: Monad m => Trampoline m r -> m r
run t = resume t >>= \e -> case e of 
	Right r -> return r
	Left i  -> run . runIdentity $ i 


-- | peels off layers of generator, build list of generated values
runGen :: Monad m => Generator a m r -> m ([a],r)
runGen m = resume m >>= \e -> case e of 
	Right r      -> return ([],r)
	Left (a,m')  -> liftM (\(as,r) -> (a:as, r)) $ runGen m'



-- | taking as from list as param and peel off layers of Iteratee
runIter :: Monad m => Iteratee a m r -> [a] -> m r
runIter m as = resume m >>= \e -> case e of 
	Right r -> return r
	Left k  -> case as of 
		[]     -> error "no more inputs"
		(x:xs) -> runIter (k x) xs
 

-- tests --
tramp1 :: [String] -> Trampoline Identity String
tramp1 (x:y:[]) = do 
	pause
	let z = x ++ y
	pause
	return z


gen1 :: Generator Int Identity String
gen1 = do 
	yield 12
	yield 14
	return "done"


iter1 :: Iteratee Int Identity Int
iter1 = do 
	a <- await
	b <- await
	c <- await
	return $ a + b + c


-- | Build Some more Sophisictaed suspension functors from generator and iteratee functors

-- data Request req res x = Request req (res -> x)

-- | upon suspension, demand and supply value
--data Request a b = Compose ((,) a) ((->) b) 

-- | upon suspension, demand or suppply value
--type InOrOut a b = EitherFunctor ((,) a) ((->) b)




--------------------- III. Composing Coroutines to achieve pre-emptive multi-tasking  ---------------------

-- | (>>=) :: m a -> (a -> m b) -> m b
bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 g ma mb = do 
	a <- ma 
	b <- mb
	g a b



-- | a composition function that crashes when generator runs empty before iteratee finish running
pipe1 :: Monad m => Generator a m r -> Iteratee a m s -> m (r,s)
pipe1 c1 c2 = bindM2 go (resume c1) (resume c2) 
	where 
		go (Left (a,c)) (Left f)  = pipe1 c $ f a                  -- * Generator is running and yielding a, Iteratee awaits a from Generator, note $\a -> a -> Generator a m s  creates a new generator 
		go (Left (a,c)) (Right r) = pipe1 c $ return r             -- * Iteratee is done running, its results is returned. 
		go (Right r) (Left f)     = error "no more values yielded" -- * throw error since no more value yielded
		go (Right r) (Right s)    = return (r,s)				   -- * return both results



-- | a composition function that sends a to Maybe a, thus does not crash
-- | challenge: rewrite this using applicative?
-- | note pipe builds a new function that runs both coroutines to completion
pipe2 :: Monad m => Generator a m r -> Iteratee (Maybe a) m s -> m (r,s)
pipe2 c1 c2 = bindM2 go (resume c1) (resume c2)
	where
		go (Left (a,c)) (Left f)  = pipe2 c $ f $ Just a
		go (Left (a,c)) (Right r) = pipe2 c $ return r
		go (Right r) (Left f)     = pipe2 (return r) $ f Nothing
		go (Right r) (Right s)    = return (r,s)


gen2 :: Monad m => Generator Int m ()
gen2 = do 
	yield 1
	yield 2
	return ()

iter2 :: Iteratee (Maybe Int) IO ()
iter2 = do 
	lift . putStr $ "enter two numbers \n"
	a <- await
	lift . putStr $ "a is " ++ (show a) ++ "\n"
	b <- await
	lift . putStr $ "b is " ++ (show b) ++ "\n"
	c <- await
	lift . putStr $ "c is " ++ (show c) ++ "\n"
	return ()


-- | test
-- gen2 `pipe2` iter2

--------------------------------- Transducer in the middle  ---------------------------------


-- | pasted from Control.Monad.Coroutine.Nested

-- | Combines two alternative functors into one, applying one or the other. Used for nested coroutines.
data EitherFunctor l r x = LeftF (l x) | RightF (r x)

instance (Functor l, Functor r) => Functor (EitherFunctor l r) where
   fmap f (LeftF l) = LeftF (fmap f l)
   fmap f (RightF r) = RightF (fmap f r)


-- | Like 'either' for the EitherFunctor data type.
eitherFunctor :: (l x -> y) -> (r x -> y) -> EitherFunctor l r x -> y
eitherFunctor left _ (LeftF f) = left f
eitherFunctor _ right (RightF f) = right f


-- can't get this stuff to compile and rest of article doesn't seem to add much to what is already learned
--data Transducer a b m r = Coroutine (EitherFunctor ((->) (Maybe a)) ((,) b)) m r




--------------------------------- reimplement bits from machines  ---------------------------------


--------------------------- A Better Attempt can be found in Biroutines  ---------------------------------

--------------------------------- Plan  ---------------------------------


-- | A plan is the suspension functor

-- | From Plan.hs:
-- | A @'Plan' k o a@ is a specification for a pure 'Machine', that reads inputs selected by @k@
-- with types based on @i@, writes values of type @o@, and has intermediate results of type @a@.
--
-- A @'PlanT' k o a@ can be used as a @'PlanT' k o m a@ for any @'Monad' m@.
--

-- | uncps-ed form of plan


data PlanT k o m a = Monad m => 						          -- * Analgogous to Coroutine
	Done a 												          -- * Right Branch of Either
	| YieldP o (PlanT k o m a) 								      -- * (o, coroutine)
	| forall z. AwaitP (z -> PlanT k o m a) (k z) (PlanT k o m a) -- * (a -> coroutine). note how z does not appear in type parameter
	| Fail   												      -- * no analogies to coroutine


-- | use existentials to get rid of m param. make signature look more clean. (other benefits?)
type Plan k o a = forall m. PlanT k o m a


instance Monad m => Functor (PlanT k o m) where
  fmap g (Done a)       = Done $ g a
  fmap _ Fail           = Fail
  fmap g (YieldP o p)   = YieldP o $ g <$> p
  fmap g (AwaitP f k p) = AwaitP ((fmap g) . f) k (g <$> p)


instance Monad m => Applicative (PlanT k o m) where
	pure = Done
	(<*>) = ap

instance Monad m => Monad (PlanT k o m) where
	return  = pure
	p >>= g = case p of                          
		Done a        -> g a
		Fail          -> Fail
		YieldP o p'   -> YieldP o $ p' >>= g
		AwaitP f k p' -> AwaitP ((>>=g) . f) k (p' >>= g)

-- | lift :: m a -> t m a
-- | known: Right branch of Either is Done a, and m has to be lifted in it somehow
-- | unknown:
instance MonadTrans (PlanT k o) where
  lift m = undefined


--instance MonadTrans (PlanT k o) where
--  lift m = PlanT (\kp _ _ _ -> m >>= kp)  -- * unwrap value from m and pass value a into function a -> m r
--  {-# INLINE lift #-}

instance Monad m => Alternative (PlanT k o m) where
	empty = Fail
	(<|>) = undefined	


yield' :: Monad m => o -> PlanT k o m ()
yield' o = YieldP o $ return ()


-- | question: where do I get k i and i? Using as place holder for now
--await' :: (Monad m, Category k) => i -> PlanT (k i) o m i
--await' i = AwaitP (\i -> return i) id (return i)
await' :: (Monad m, Category k) => PlanT (k ()) o m ()
await' = AwaitP (\z -> return z) id (return ())

stop :: Plan k o a
stop = Fail


-- | plan for monday:

{-

in the interest of moving things forward, write a parser using coroutines and see if you can make applicative predicates
if yes: refine

if no: see if you can define minimal viable coroutine and make applicatives that def make sense for parsing 


-}





--------------------------------- Machine  ---------------------------------



-- | here the different suspension functors are declared as a sum type. Why isn't Done a in Step?
-- | Also note m is not in here 

data Step k o c 						-- * o is input a, c is coroutine. k is some input selector?
  = Stop                                -- * no analogies from Monad.Coroutine tutorial above
  | Yield o c   						-- * generator, analogous to (a,coroutine)
  | forall t. Await (t -> c) (k t) c   	-- * iteratee, analgous to (a -> coroutine)



instance Functor (Step k o) where
	fmap _ Stop          = Stop
	fmap g (Yield o c)   = Yield o $ g c
	fmap g (Await f k c) = Await (g . f) k (g c)


-- | a machine is a monad transformer that's equivalent to a coroutine
-- | transforms underlying monad m, it might yield output of type o upon suspension, and have input selector k
data MachineT m k o = M { runMachineT :: m (Step k o (MachineT m k o)) }


instance Monad m => Functor (MachineT m k) where
	fmap g (M m) = M $ liftM ap m
		where 
			ap (Yield o m')   = Yield (g o) $ g <$> m'
			ap (Await f k m') = Await ((fmap g) . f) k (g <$> m')
			ap Stop           = Stop



-- | note the difference: the parametrized suspension functor below is replaced by Step k o c
-- | data Coroutine s m r = Co { resume :: m (Either (s (Coroutine s m r)) r) }

-- | why is this line needed? to make a more friendly type signature? 
-- A 'Machine' can be used as a @'MachineT' m@ for any @'Monad' m@.
type Machine k o = forall m. Monad m => MachineT m k o


------ at this point still not sure what k does -- 

-- | peel back M, and then Identity monad, and get a Step k o (..). where Step could be Yield, Await or Stop
runMachine :: MachineT Identity k o -> Step k o (MachineT Identity k o)
runMachine = runIdentity . runMachineT


-- | figure out how to go from plan to machine
-- | the `Plan` type (un-CPSed) somehow get converted to `Step` type, then it's wrapped in Machine value constructor
-- | I think: somehow turn a plan into a recursively applied function inside M?
construct :: Monad m => PlanT k o m a -> MachineT m k o 
construct = undefined


{-


	newtype PlanT k o m a = PlanT
	  { runPlanT :: forall r.
	      (a -> m r) ->                                     -- Done a
	      (o -> m r -> m r) ->                              -- Yield o (Plan k o a)
	      (forall z. (z -> m r) -> k z -> m r -> m r) ->    -- forall z. Await (z -> Plan ko a) (k z) (Plan k o a)
	      m r ->                                            -- Fail
	      m r
	  }


	-- | really confused about how the program is running though, complete black magic
	-- | Compile a machine to a model.
	construct :: Monad m => PlanT k o m a -> MachineT m k o
	construct m = MachineT $ runPlanT m     			         -- * unwrap PlanT
	  (const (return Stop))								         -- * construct a step that is always m (Stop)
	  (\o k -> return (Yield o (MachineT k)))                    -- * send Yield o (Plan k o a) to Yield o c
	  (\f k g -> return (Await (MachineT . f) k (MachineT g)))   -- * send Await (i -> plan) (k i) (Plan) to Await (i -> Machine) (k i) (Machine)
	  (return Stop)												 -- * send Fail to Stop

	-- | Generates a model that runs a machine until it stops, then start it up again.
	--
	-- @'repeatedly' m = 'construct' ('Control.Monad.forever' m)@
	repeatedly :: Monad m => PlanT k o m a -> MachineT m k o
	repeatedly m = r where
	  r = MachineT $ runPlanT m
	    (const (runMachineT r))
	    (\o k -> return (Yield o (MachineT k)))
	    (\f k g -> return (Await (MachineT . f) k (MachineT g)))
	    (return Stop)


-}

























--------------------------------- Scratch  ---------------------------------



{-
	-- stuck on monad implementation, try the impl from Control.Monad.Coroutine

	-- | Coroutine2 is a monad transformer wrapping underlying monad m, which wraps 
	-- | Either Right r : Done result, or Left s : Left (more compuatation to be done)
	data Coroutine2 s m r = C { resume2 :: m (Either (s (Coroutine2 s m r)) r) }


	-- | fmap :: (a -> b) -> f a f b
	-- | fmap operates on the value of the compuatation when it's done, or Right r
	instance (Functor s, Functor m) => Functor (Coroutine2 s m) where
		fmap g (C m) = C $ fmap (apply g) m                  -- * map the function (apply g) inside m, onto Either s
			where 
				apply g (Right s) = Right $ g s              -- * g sends s (..) to t (..), this is wrapped in Right value constructor
				apply g (Left s)  = Left $ (fmap . fmap) g s -- * maps insde s, inside Coroutine and apply g


	instance (Monad m, Functor s) => Monad (Coroutine2 s m) where
		return r = C . return . Right $ r  
		(C m) >>= g = C $ m >>= apply g         			       -- * the underlying monad is bound to compuation (apply g), note bind unwraps monad and expose Either data type
			where  
				apply g (Right s) = resume2 $ g s                  -- * g sends s to (Corotine2 s m) t, resume2 unwraps Coroutine2, exposing m(..)
				apply g (Left s ) = return . Left $ fmap (>>=g) s  -- * return to current context m, wrap in Either type Left, map over functor s and recursively bind underlying monad to g

-}



{-
data PauseF x = PauseF x

instance Functor PauseF where
	fmap g (PauseF x) = PauseF $ g x 


-- | turn a suspension functor into a coroutine
toCoroutine :: Monad m => Coroutine PauseF m ()
toCoroutine = suspend . PauseF $ return ()
-}


{-
--------------------------------- Pause Monad Transformer ---------------------------------


-- | toy pause transformer -> note note a real transformer
{-
	The Gist: build a  type that wraps some monad action ad infinitum. The functions that operate on this type
	peels off the layers, exposing the underlying action.
	
	note repeated application of (>>=) has signature m a -> (a -> m b) -> m b -> (b -> m c) -> m c -> ...

	or ::  		( a -> m b)	-> 		  ( b -> m c ) ->       ... 
		 m a -> 			   m b -> 				  m c -> 	 ...

	w/i context, some value a is changing from b to c to ..
	out of context, there's a function that takes value a (again, out of context) and sending it value b,
	then returning it to context

	In this case, m is the context, peeling it off exposes the underlying value Pause m, and some action is done on it
	that returns it back into context.

	In the case of runN, a == b == c for letters.

	Note this type doesn't output the result of underlying compuatation
-}
data Pause m = Run (m (Pause m)) | Done


-- | runN sends the Pause m monad to an m wrapping a Pause m monad 

-- | note m is m(Pause m), so when its unrapped w/ >>=, what's left if pause m
runN :: Monad m => Int -> Pause m -> m (Pause m)
runN 0 p = return p
runN n (Run m) = m >>= \p -> runN (n-1) p


fullRun :: Monad m => Pause m -> m ()
fullRun Done    = return ()
fullRun (Run m) = m >>= \p -> fullRun p



-- | note what pause is doing:
{-
	Run wraps two IO actions, "lets begin" and "step 1"
	another value a is created where a is "step", a is returned to current monadic context w/ return
	w/i aa, another action a' is created: "step 3", "yay we're done". this is returned to monad wrapping a
	w/i a', Done is returned to current context

-}
pauseEx1 :: Pause IO
pauseEx1 = Run $ do
  putStrLn "Let's begin"
  putStrLn "Step 1"
  return $ Run $ do
    putStrLn "Step 2"
    return $ Run $ do
      putStrLn "Step 3"
      putStrLn "Yay, we're done!"
      return Done


main2 :: IO ()
main2 = do 
	rest <- runN 2 pauseEx1
	Done <- runN 1 rest
  	putStrLn "=== full run rest ==="
	fullRun rest					 -- * note since rest has the first two layers peeled off, only the last layers are run
  	putStrLn "=== full run ex1 ==="
	fullRun pauseEx1				 -- * peel off all layers of pauseEx1 and runs them





-- | a real pause transformer
{-
	a Sum type that wraps a monad, could be Run or Done
	functions are mapped onto the underlying value, either r or m (PauseT m r)
	to interact w/ the type, use RunT which either returns the value to current context
	if DoneT, or expose the underlying value and then run the PauseT it wraps

	In other words, Run peels off the layers of RunT constructor until DoneT is seen

-}
data PauseT m r = RunT (m (PauseT m r)) | DoneT r


instance (Monad m) => Functor (PauseT m) where
	fmap g (DoneT r) = DoneT $ g r
	fmap g (RunT m ) = RunT $ liftM (g <$>) m


{-
	f a -> f (a -> b) -> f b -> f (b -> c) -> f c -> ...
	is this an applicative?
-}
instance (Monad m) => Applicative (PauseT m) where
	pure a  = DoneT a
	(<*>)   = ap


-- | monad instance for pauseT
instance (Monad m) => Monad (PauseT m) where
	return = pure
	DoneT r >>= f = f r
	-- | note here  m :: ( m (PauseT m r)
	-- | liftM        :: (a -> b) -> m a -> m b
	-- | (>>=f)       :: (m a -> m b
	-- | liftM (>>=f) :: m1 (m2 a) -> m1 (m2 b)
	-- | this lifts f over the m into the inner (PauseT m r)
	RunT m  >>= f = RunT $ liftM (>>=f) m  

instance MonadTrans PauseT where
	lift m = RunT $ liftM DoneT m 


pause :: Monad m => PauseT m ()
pause = DoneT () 

runTN :: Monad m => Int -> PauseT m r -> m (PauseT m r)
runTN 0 p           = return p
runTN _ p@(DoneT _) = return p
runTN n (RunT m)    = m >>= runTN (n-1)

fullRunT :: Monad m => PauseT m r -> m r
fullRunT (DoneT r) = return r
fullRunT (RunT m)  = m >>= fullRunT

ex2 :: PauseT IO ()
ex2 = do 
	lift  . putStrLn $ "step 1"
	pause 
	lift  . putStrLn $ "step 2"
	pause 
	lift  . putStrLn $ "step 3"



main :: IO ()
main = do 
	rest    <- runTN 2 ex2
	DoneT _ <- runTN 1 rest
	putStrLn "----- fullRunT rest ---- "
	fullRunT rest	
	putStrLn "----- fullRunT ex2 ---- "
	fullRunT ex2




--------------------------------- Coroutine ---------------------------------

--instance (Functor s, Monad m) => Monad (Coroutine s m) where
--   return x = Coroutine (return (Right x))

--   t >>= f = Coroutine (resume t >>= apply f)
--      where apply fc (Right x) = resume (fc x)
--            apply fc (Left s) = return (Left (fmap (>>= fc) s))


--(Co m) >>= g = Co $ m >>= apply g 
--	where
--		apply g (DoneC r) = return . DoneC $ fmap (>>=g) r
--		apply g (RunC s)  = resume (g s)


-- | s : suspension functor
-- | m : some monad
-- | r : return type
data CoroutineState s m r = RunC (s (Coroutine s m r)) | DoneC r


instance (Functor s, Functor (Coroutine s m)) => Functor (CoroutineState s m) where
	fmap g (DoneC r) = DoneC $ g r
	fmap g (RunC m)  = RunC $ (fmap . fmap) g m


-- | (>>=) :: m a -> (a -> m b) -> m b
-- | (>>=) :: (CoroutineState s m) r -> (r -> (CoroutineState s m) t) -> (CoroutineState s m) t
-- | (>>=) :: RunC (s (Coroutine s m r)) -> 
-- |          r -> RunC (s (Coroutine s m t)) -> 
-- | 		  RunC (s (Coroutine s m t))

instance (Functor s, Monad m, Functor (Coroutine s m)) => Monad (CoroutineState s m) where
	return r = DoneC r
	(DoneC r) >>= g = g r
	(RunC s ) >>= g = undefined

-- * 1st map over s, at Coroutine s m r
-- * >>= unwraps cor, revealing m, 


-- (fmap . fmap . liftM) (>>=g) $ s  -- * 1st map over s, now at Co { m(..) }
												               -- * 2nd map over Co, now at m (..)
												               -- * liftM over m, now at CoroutineState s m r
												               -- * g :: r -> CoroutineState s m t
       
												               -- * CoroutineState s m r >>= g :: CoroutineState s m t
												               -- * 


-- | RunC $ (fmap . fmap . fmap) (>>=g) $ s   


-- * map over suspension functor s, map over Coroutine s m r, map over monad m, 
-- | s 
-- | s' (Coroutine s m r)
-- | s' (Co $ m (CoroutineState s m r))


-- | g s                          :: CoroutineState s m t
-- | return (g s)                 :: m (CoroutineState s m t)
-- | Co . return $ (g s)          :: Coroutine s m t
-- | (Co $ m (g s)) <$> s         :: s (Coroutine s m t)
-- | RunC $ (Co $ m (g s)) <$> s  :: CoroutineState s m t


data Coroutine s m r = Co { resume :: m (CoroutineState s m r) }


instance (Monad m, Functor (CoroutineState s m)) => Functor (Coroutine s m) where
	fmap g (Co m) = Co $ liftM (fmap g) m


-- | (>>=) :: m a  -> (a -> m b) -> m b
-- | (>>=) :: (Coroutine s m) r -> (r -> (Coroutine s m) t) -> (Coroutine s m) t

-- | (>>=) :: Co ( m (CoroutineState s m r)) -> 
-- | 		  r -> Co ( m (CoroutineState s m t)) -> 
-- | 		  Co (m (CoroutineState s m t))
instance (Monad m, Functor s) => Monad (Coroutine s m) where
	return = Co . return . DoneC
	(Co m) >>= g = Co $ m >>= apply g
		where 
			apply g (DoneC r) = resume $ g r
			apply g (RunC s ) = return . RunC $ fmap (>>=g) s


instance MonadTrans (Coroutine s) where
	lift= Co . liftM DoneC



-- | wrap suspension functor in coroutine monad
suspend :: (Monad m, Functor s) => s (Coroutine s m r) -> Coroutine s m r
suspend = Co . return . RunC 




------- create the suspension functor------

-- | this is better than pauseT since there's an input value i and output value o
-- | here m is the value wrapped by functor `Interface`
data Interface i o m = Produced o (i -> m)

instance Functor (Interface i o) where
	fmap g (Produced o k) = Produced o $ g . k


-- | note from this point forward, at best the final product replicate data.machine
-- | question: can you implement applicative parsing using data.machine somehow?

-- | what is gained: a deeper understanding of how to interleave/think about functions and types in haskell
-- | some domain knowlege of coroutines and knowing what exactly is going on in machines

-- | what is not gained: a better library than machines or pipes or control.monad.coroutine

-- | what is lost: 4 - 5 days of time


-- | ideal implementation: parser process is lifted into an Auto and becomes and thus can be
-- | trivially composed with transducer



-}














