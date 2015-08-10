{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleContexts #-}  
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}




-- * inspiration: machiens and pipes
-- * https://github.com/Gabriel439/Haskell-Pipes-Library/tree/master/src/Pipes

-- * Gameplan:

-- * 1. build trivial conversion from plan to Planoutine
	-- * most important bit of logic: how to convert a plan to Planoutine
	-- * what to do at Fail and Done
			-- * describe in english, what the states of the functions are

{-
	
	plan is in four states
	
	await,yield,done r, fail

	the r in done r cannot have any typeclass constraints
	
	when overlaying computation returns, the output r is put in done r
	
	how does that translate to what happens in coroutine, which explicity gets rid of?

	Plan in state:

	Plan (m (Done r))


	coroutine is in three states:

	await,yield,fail

	fail crashes under and overlaying computation 

	need safe exit...how is that done in coroutine??

	what's a way to return value of overlaying computation without 


-}


module Coroutine2 where 

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Applicative

import Data.Monoid

import Control.Monad.State
import Control.Monad.Morph


import Coroutine


{-
	Design goals:

	overlaying computation may return result
	hoist has to be intuitive
		
		we know:

		with hoist, the last param has to be existentially quantified, 

		we also know:

		the use of return restrains what the "return" value will be

		non-patches:

		have some coroutine that does not have return value. because there has to be some value
		r, in this case it would be be from `respond b`, which is even more likely to be of 
		different values.


		make the return value ALWAYS generalized, that means no dangling `respond` or `request`
		forever does not change semantics for one iteration, but runs forever
		
		make the underlying monad not a monad, but then you cannot stack another value ontop of 
		it and make the whole thing a monad still

		* 
		have some trivial monad t that wraps coroutineT, and its return value is forall r. r

		so drop it in the free monad?

	Non goals:

	respond and request have to work with arb signatures

	can you make some monad with phantom type p, and replicate all monad properties?

-}

-- * general approach
-- * either make sure a is always general to satisfy eta :: forall a. m a -> n a
-- * or make some way to make specific a work. 
		-- * computation :: Monad m => m Int -> forall a. m a -> n a -> n String
		-- * computation :: Monad m, MonadTrans t -> m Int -> forall a. t m a -> s m a -> m String
				-- * where inside t, somehow some composition is happening
-- * combine with some other type that is quasi-identity-ish

-- * generic pattern :: m Type -> (mType -> forall a. t m a) -> forall a. t m a -> (m Type -> m Type2)
-- * 				   t m a -> (t m a -> m Type2) -> m Type 2




-------------------------------------- test functions --------------------------------------

-- * without forever, the return type of c1 is not general
c2 :: Monad m => CoroutineT Int b' Int b' m b'
c2 = respond 12


-- * use of forever makes return type of c1 general
c1 :: Monad m => CoroutineT Int b' Int b' m r
c1 = forever $ respond 12


tc1 :: 
	(Monad m, MonadTrans t, MFunctor t, Monad (t (CoroutineT a a' Int b' m))) 
	=> t (CoroutineT a a' Int b' m) ()
tc1 = (lift . respond $ 100) >> return ()


tc1c1 :: 
	(Monad m, MonadTrans t, MFunctor t, Monad (t (CoroutineT a a' Int b' m))) 
	=> t (CoroutineT a a' Int b' m) ()
tc1c1 = hoist (\c -> c <~> c1) tc1

c1' :: Monad m => CoroutineT Int b' Int b' m b'
c1' = respond 12

----------------------------- possible solution: put inside free monad -----------------------------


-- * make some free monad for now. later see if it can be imported
-- * note how `a` is the value, and `f` is some functor, mapping `a` to some `a` in context

data Free f a = Pure a | Roll (f (Free f a))


instance Functor f => Functor (Free f) where
	fmap g fm = case fm of 
		Pure a    -> Pure . g $ a
		Roll fm'  -> Roll . (fmap . fmap $ g) $ fm' 


instance Functor f => Monad (Free f) where
	return  = Pure
	m >>= g = case m of 
		Pure a   -> g a
		Roll m'  -> Roll $ (>>=g) <$> m'


-- * put incompatible coroutine inside free
fc2 :: (Monad m, Functor f) => Free f (CoroutineT Int b' Int b' m b')
fc2 = return c2

-- * put comaptible coroutine inside free
fc1 ::(Monad m, Functor f) => Free f (CoroutineT Int b' Int b' m r)
fc1 = return c1


-- * simply swap out underlying coroutine with some free monad
st :: Monad m => StateT Int (CoroutineT a a' Int b' m) Int
st = modify (+100) >> (lift . respond $ 200) >> return 404

{-

stf :: Functor f => StateT Int (Free f) Int
stf = hoist (\_ -> fm) st where 
	fm = undefined :: Functor f => Free f a
	st = undefined :: Monad m => StateT Int (CoroutineT a a' Int b' m) Int
-}

-- * compose compatible coroutine with free

-- * compose incompatible coroutine with free








-- * hoist :: (Monad m, Mfunctor t) => (forall a. m a -> n a) -> t m b -> t n b
-- * eta   :: forall a. m a -> n a
-- * forget about implementation for now, just check the types
instance MFunctor Free where
	hoist eta m = case m of 
		Pure a  -> undefined
		Roll m' -> undefined









--freeCo0 :: (Functor f, Monad m) => Free f (CoroutineT a a' Int Bool m r)
--freeCo0 = return stop


--freeCo1 :: (Functor f, Monad m) => Free f (CoroutineT a a' Int Bool m Bool)
--freeCo1 = return . respond $ 404


--tmfree :: (Functor f, Monad f) => StateT Int (Free f) Int
--tmfree = StateT $ \_ -> fint
--	where fint = return (200,404) :: Functor f => Free f (Int,Int)


--tmSt :: (MonadState Int ((,) a)) => StateT (Int,Int) (Free f) a
--tmSt = StateT $ \_ -> fSt where 

--tmSt :: StateT Int (State Int) Int
--tmSt = StateT $ \_ -> return $ return (404,200)


--fSt :: (Functor f, Monad m) => Free f (StateT Int m Int) 
--fSt = return $ put 200 >> return 404


--fco :: (Functor f, Monad m) => Free f (CoroutineT a a' Int b' m (Int,Int))
--fco = Pure $ respond 404 >> return (200,404)



----------------------------- possible solution: some cap on end of pipe -----------------------------


end :: Monad m => CoroutineT a b' c b' m r
end = undefined


-- * alternateively, have some cap that makes the type fit... but what should cap do??
tc1c2 = hoist (\c -> c <~> c2 <~> end) tc1



------------------------------ possible solution: swap, a non-monad-morphims -------------------------------




-- * compiler's intrepretation of the signature
--swap :: (m1 (a1, s) -> m (a, s)) -> StateT s m1 a1 -> StateT s m a

-- * make it a non monad morphism
--swap :: (m (a, s) -> m (b, s)) -> StateT s m a -> StateT s m b

-- * using forall, we can remove the tuple part, but what has it actually done?

-- * dual of hoist, does not modify the monad, but rather modifies the return value
swap :: (forall a. forall b. m a -> m b) -> StateT s m a -> StateT s m b
swap g m = StateT $ \s -> g $ runStateT m s


g :: Monad m => m a -> m b -> m b
g = undefined

ma0 :: Monad m => m a
ma0 = undefined

ma0' :: Monad m => m String
ma0' = undefined


mb0 :: Monad m => m b
mb0 = undefined

tma0 :: Monad m => StateT s m r
tma0 = undefined

-- * so far this seem to work ok ...
combined01 = swap (\m -> m `g` ma0) tma0 :: Monad m => StateT s m r
combined02 = swap (\m -> m `g` mb0) tma0 :: Monad m => StateT s m r

-- * but when we add a specific signature, we get the error from hell
-- * combined03 = swap (\m -> m `g` ma0') tma0

ma1 :: Monad m => CoroutineT b b' c c' m Int
ma1 = undefined

mb1 :: Monad m => CoroutineT b b' c c' m r
mb1 = undefined

tma1 :: Monad m => StateT s (CoroutineT a a' b b' m) s'
tma1 = undefined


combined11 = swap (\m -> m `g` mb1) tma1 :: Monad m => StateT s (CoroutineT a a' c c' m) s'

-- * error from hell:
-- * Expected type: CoroutineT a a' b b' m b1
-- * Actual type: CoroutineT a a' b b' m Int
--combined12 = swap (\m -> m `g` ma1) tma1


-------------- possible solution 3. convert Coroutine to something else with dummy r ------------------


-- * a dummy r here that's the last param
-- * Seq never returns... just yields..but how does it work?
-- * could there be some special yield?


-- * has to combine in coroutine and construct a pipe?


-- * this has to do
	-- * 1. satisfy hoist constraint
	-- * 2. satisfy return monadtrans result
	-- * 3. bonus: make f = respond on arb function signature possible
	-- * 4. bonus: composition of pipe

data Seq a a' b b' r c 
	= Req a' (a -> c)
	| Res b  (b'-> c)
	| Stop
	| Dummy r 


-- * an internal representation of pipes
-- * note the r here is some dummy type

-- * PipeR doesnt have sensible monad instance since r doesn't do anything, it's just some phantom 
data PipeR a a' b' m b r = P { flow :: m (Seq a a' b b' r (PipeR a a' b' m b r)) }


data PipeT a a' b' m b = forall r. PipeR a a' b' m b r


-- * as a bouns, functor instance make sense now
instance Functor (PipeT a a' b' m) where
	fmap g p = undefined

instance Monad (PipeR a a' b' m b) where
	return = undefined
	(>>=)  = undefined


-- * see if the type chekck for now

infixl 8 .>

(.>) :: Monad m => PipeR a a' b' m b r  -> PipeR b b' c' m c r -> PipeR a a' c' m c r
p1 .> p2 = undefined


respond' :: Monad m => b -> PipeR a a' b' m b r
respond' = undefined


request' :: Monad m => a' -> PipeR a a' b' m b r
request' = undefined


p1 :: Monad m => PipeR b b' c' m c r
p1 = undefined

tp1 :: Monad m => StateT t (PipeR a a' b' m b) s
tp1 = undefined


-- * with hacky monad instance, this now works
-- * BUT, no  monad instance should exist for PipeR
tpp1 :: Monad m => StateT t (PipeR a a' c' m c) s
tpp1 = hoist (\p -> p .> p1 ) tp1


-- * question, where does Done go??


-- * might have to consider some other control flow?








construct :: CoroutineT a a' b b' m v -> PipeR a a' b' m b r
construct = undefined


encase :: Monad m => Seq a a' b b' r (PipeR a a' b' m b r) -> PipeR a a' b' m b r
encase = P . return





---------------------------------- possible solution 2. invert <~> -----------------------------------

-- * this won't work, but if we invert the composition

--infixl 8 <~

--(<~) :: Monad m => CoroutineT b b' c c' m r -> CoroutineT a a' b b' m t -> CoroutineT a a' c c' m r
--m1 <~ m2 = m2 <~> m1

-- * obviously, invert did not work, its' self evident in function signature
-- * invert = hoist (\c -> c2 <~ c) tc1


---------------------------------- possible solution 1a. cap it -----------------------------------



-- * we can try to have some min function `cap` that 
-- * sends any `CoroutineT a a' b b' m t -> forall r. CoroutineT' a a' b b' m r`
-- * Note, cap should be put infront of primitives so end user doesn't have to worry about it


-- * this does not have the correct semantics, 
-- * only way this works is if cap return Nothing or _|_
cap :: Monad m => CoroutineT a a' b b' m t -> forall r. CoroutineT a a' b b' m r
cap = undefined



-- * Proof `cap` works in use cases provided

-- * now cap some coroutine a -> forall r. coroutine r and hoist compose



tc1c1' ::
	(Monad m, MonadTrans t, MFunctor t, Monad (t (CoroutineT a a' Int b' m))) 
	=> t (CoroutineT a a' Int b' m) ()
tc1c1' = hoist (\c -> cap c <~> cap c2) tc1



-- * similarly forever has similar type signature to `cap`
tc1c1'' = hoist (\c -> c <~> forever c2) tc1



-- * cap also solves the problem of arb signature used with primitives

-- * Note all signature below are valid
-- * c3 :: Monad m => CoroutineT a a' Int t m String
-- * c3 :: Monad m => CoroutineT a a' Int t m Bool
c3 :: Monad m => CoroutineT a a' Int t m r
c3 = cap $ respond 12


-- * or some function cape that sends `coroutine t -> forall r. pipe r`?

-- * try with maybe



---------------------------------- possible solution 1b. cap it -----------------------------------

-- * or some function cap' that sends `Coroutine t -> Coroutine Type`

data Exit = forall r. Exit r

cap' :: Monad m => CoroutineT a a' b b' m t -> coroutine a a' b b' m Exit
cap' = undefined


-- * proof cap works with runA


-- * proof cap' works in use cases provided

-- * cap' does not work in use case because Exit is a specific type
--tc1c1'' = hoist (\c -> cap' c <~> cap' c2) tc1





{-

	This design is also shelved since parametrizing coroutine monad over yield value makes no sense


	-------------------------------------------------------------------------------
	---------------------- Suspension Functor & Instances --------------------------
	--------------------------------------------------------------------------------


	-- * note term cannot be a functor
	data Term = forall r. Term r

	-- * does this implementation make sense?
	-- * what's a possible use case of <> ?
	instance Monoid Term where
		mempty = Term ()
		(Term a) `mappend` (Term b) = Term (a,b)



	-- * Suspension functor for plan
	-- * When `Plan` is cpsed, this functor will be redundant

	data Step a' a b' b c 
	  = Done Term
	  | Request a' (a -> c)       
	  | Respond b  (b' -> c)      
	  | Fail 


	instance Functor (Step a' a b' b) where
	  fmap g s = case s of 
	    Done r        -> Done r
	    Request a' k -> Request a' $ g . k 
	    Respond b h  -> Respond b $ g . h
	    Fail          -> Fail


	--------------------------------------------------------------------------------
	-------------------------------- Coroutine -------------------------------------
	--------------------------------------------------------------------------------

	-- * note coroutine is parametrized over downstream respond type `b`
	data CoroutineT a' a b' m b = Cot { resume :: m (Step a' a b' b (CoroutineT a' a b' m b)) }


	-- * problem here, functor needs to prametirze over return type, which now does not exist

	-- * what if we just roll with, and make it paramterize over yield type
	instance Monad m => Functor (CoroutineT a a' b m) where
		fmap g (Cot m) = Cot $ liftM ap m where 
			ap s = case s of 
				Done t       -> Done t
				Fail 		 -> Fail
				Request a' k -> Request a' $ (g <$>) . k
				Respond b h  -> Respond (g b) $ (g <$>) . h    -- * note fmap makes more sense now in the yield never return paradigm
	 


	-- * g takes in yield value b and create new value from it

	-- * design one: return b = encase $ Respond b $ const . encase $ Fail
	-- * here, instead of returning to done, things are returned to yield
	-- * but, it makes no sense to fail after yield

	-- * alternate design: put into Done, but is contrary to what the monad is parametrizing over
	-- * return b = encase $ Done . Term $ b

	-- * current design: yield the value, then return the value ? this makes no sense tbh
	instance Monad m => Monad (CoroutineT a' a b' m) where
		return b      = encase $ Respond b $ const . exit $ b
		(Cot m) >>= g = Cot $ m >>= \s -> case s of 
			Done v       -> return . Done $ v
			Fail         -> return Fail
			Request a' k -> return . Request a' $ (>>=g) . k
			Respond b h  -> resume $ g b  -- * type checks but obviously wrong!!




	{-


	(>>=) :: Monad m => m a -> (a -> m b) -> m b
	g b   :: CoroutineT a' a b' m c
	h     :: b' -> CoroutineT a' a b' m c


	some some sort of combination is in order:


	Respond b h -> resume $ (g b) `combine` (\b' -> h b)

	where combine has to type check against possible params of (g b) and (h b') coroutines


	but what does it mean...


	-}



	encase :: Monad m => Step a' a b' b (CoroutineT a' a b' m b) -> CoroutineT a' a b' m b
	encase = Cot . return


	exit :: Monad m => r -> CoroutineT a' a b' m b
	exit = encase . Done . Term 
-}



{-
	this design was discarded since my coroutine *has* to return a value or overlaying computation's 
	return value would be discarded

	-- * Supsension Functor for Coroutine
	-- * Note as a design principle, Coroutines do not `return`, but rather run forever or crash

	data Step a a' b b' c
		= Request a' (a -> c)
		| Respond b  (b' -> c) 
		| Stop


	instance Functor (Step a a' b b') where
		fmap = undefined

	--------------------------------------------------------------------------------
	-------------------- Plan and Typeclass Implementation -------------------------
	--------------------------------------------------------------------------------

	-- * `Plan` is the internal representation of Coroutine, its type signature allow monad implementation
	-- * A `Plan` is caped to a `CoroutineT` and `runT` 

	-- * consider doing an alternate implementation where this is CPSed
	data Plan a a' b b' m r = Plan { resume :: m (Seq a a' b b' r (Plan a a' b b' m r)) }


	instance Monad m => Functor (Plan a a' b b' m) where
	  fmap g (Plan m) = Plan $ liftM ap m where 
	      ap s = case s of 
	        Done v        -> Done $ g v
	        RequestP a' k -> RequestP a' $ (fmap g) . k 
	        RespondP b h  -> RespondP b $ (fmap g) . h 
	        Fail          -> Fail


	instance Monad m => Monad (Plan a a' b b' m) where
	  return         = Plan . return . Done
	  (Plan m) >>= g = Plan $ m >>= \step -> case step of 
	    Done v        -> resume $ g v
	    RequestP a' k  -> return . RequestP a' $ (>>=g) . k
	    RespondP b h   -> return . RespondP b $ (>>=g) . h
	    Fail          -> return Fail

	instance Monad m => Applicative (Plan a a' b b' m) where
	  pure  = return 
	  (<*>) = ap


	instance MonadTrans (Plan a a' b b') where
	  lift = Plan . liftM Done


	--------------------------------------------------------------------------------
	-------------------------------- Coroutine -------------------------------------
	--------------------------------------------------------------------------------

	-- * A CoroutineT is caped from a plan

	data CoroutineT a a' b m b' = Cot { runCo :: m (Step a a' b b' (CoroutineT a a' b m b')) }


	-- * when the plan is in `Done r`, the r should be returned in coroutine as well?
	cap :: Monad m => Plan a a' b b' m r -> CoroutineT a a' b m b'
	cap (Plan m) = Cot $ m >>= \s -> case s of 
		Fail 		  -> return Stop
		RequestP a' k -> return . Request a' $ cap . k
		RespondP b  h -> return . Respond b $ cap . h
		Done r        -> error "the problem is not solved"




	{-

	-- | Compile a machine to a model.
	cap :: Monad m => PlanT k o m a -> MachineT m k o
	cap m = MachineT $ runPlanT m
	  (const (return Stop))
	  (\o k -> return (Yield o (MachineT k)))
	  (\f k g -> return (Await (MachineT . f) k (MachineT g)))
	  (return Stop)
	-}

-}



