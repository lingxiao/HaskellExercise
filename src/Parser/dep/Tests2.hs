{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}  
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}  


--http://www.youtube.com/watch?v=_yRzWcWCOak

import Control.Arrow
import Data.Function (on)

import Prelude hiding ((.),id)
import Control.Category
import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Maybe

import Type
import Transducer
import Variants
import Utils


import Data.Functor.Contravariant
import Data.Profunctor
import Data.Bifunctor



----------------------------- Make simple Sub coroutine that's not a monad transformer ---------------------------------------
--| From SO

type Feature = String
type Buffer  = String

-- | replace [] with generic mplus instance
data Parser m a b = P { runP :: [a] -> m (b, [a]) }


data Sub a b = Done [b] | Partial (a -> Sub a b) 


instance Functor (Sub a) where
	fmap g s = case s of
		Done bs   -> Done $ g <$> bs 
		Partial k -> Partial $ (fmap g) . k

instance Applicative (Sub a) where
	pure b = Done [b]
	(<*>)  = ap

instance Monoid (Sub a b) where
	mempty        = Done []
	s `mappend` t = case (s,t) of
		(Done bs, Done cs)     -> Done $ bs <> cs
		(Done bs, Partial k)   -> Partial $ (mappend $ Done bs) . k
		(Partial k, Done bs)   -> Partial $ (mappend $ Done bs) . k
		(Partial k, Partial k') -> Partial $ \a -> (k a) <> (k' a)


instance Monad (Sub a) where
	return b = Done [b]
	s >>= g  = case s of 
		Done bs   -> foldr (<>) mempty $ g <$> bs 
		Partial k -> Partial $ (>>=g) . k 


-- * essentially the logic that would be in runSub is used here
-- | note this is morally a subroutine
withf :: MonadPlus m => Sub a b -> Parser m a b
withf s = P $ go s where 
	go (Done bs) xs       = msum [ return (b,xs) | b <- bs ]   -- * if Done, just put rest of stream in tuple, concat all results
	go (Partial k) []     = mzero 							   -- * if partial && stream empty, return empty value
	go (Partial k) (x:xs) = go (k x) xs      				   -- * if partial && nonempty stream, input x into k and run again


request :: Sub a a
request = Partial $ \a -> return a

yield :: [b] -> Sub a b
yield bs = Done bs


-- | scanner = Partial k -> (Scanner OR Done [b])
scanner' :: (Char -> [Feature]) -> Sub Char Feature
scanner' t = do
	a <- request
	case t a of 
		[] -> scanner' t
		bs -> yield bs


t :: Char -> [Feature] 
t 'a' = ["blank trial"]
t _   = []


p :: Parser [] Char Feature
p = withf $ scanner' t


----------------------------- Make Sub Monad transformer coroutine  ---------------------------------------


--data Suspension a b r = Stop | Await (a -> r) (k r) | Yield b r

--instance Functor (Suspension a) where
--	fmap g s = case s of 
--		Stop    -> Stop
--		Yield r -> Yield $ g r
--		Await k -> Await $ (fmap g) . k

--data SubT' a m r = Su { runS :: m (Suspension a r) }

--data Step k o r
--  = Stop
--  | Yield o r
--  | forall t. Await (t -> r) (k t) r

--newtype MachineT m k o = MachineT { runMachineT :: m (Step k o (MachineT m k o)) }


-- | alternatively, the canonical way to do this:
data SubT a m r = S { runS :: m (Either (a -> SubT a m r) r) }

instance Monad m => Functor (SubT a m) where
	fmap g (S m) = S $ liftM go m where
		go (Right r) = Right $ g r
		go (Left k ) = Left $ (fmap g) . k

instance Monad m => Applicative (SubT a m) where
	pure  = S . return . Right
	(<*>) = ap


-- | (>>=) :: SubT a m r -> (r -> SubT a m s) -> SubT a m s 
instance Monad m => Monad (SubT a m) where
	return      = pure
	--(S m) >>= g = S $ m >>= either (\k -> return . Left $ \a -> k a >>= g) (runS . g)
	(S m) >>= g = S $ m >>= \e -> case e of 
		Left k  -> return . Left $ \a -> k a >>= g 
		Right r -> runS . g $ r


instance MonadTrans (SubT a) where 
	lift = S . liftM Right 


-- * primitive SubT's

yieldT :: Monad m => r -> SubT a m r 
yieldT r = return r

awaitT :: Monad m => SubT a m a
awaitT = S . return . Left $ \a -> return a 


-- * Run SubT to completion

-- | take as many a out of [a] as possible to run SubT to completion, revealing underlying monad m
runT :: Monad m => [a] -> SubT a m r -> m r
runT [] (S m)     = m >>= either (\k -> runT [] $ k $ error "empty [a]") return
runT (a:as) (S m) = m >>= either (\k -> runT as $ k a) return 



-- * Some examples of SubTs

scanner :: (Char -> [Feature]) -> SubT Char (State [String]) ()
scanner t = do
	a <- awaitT
	b <- awaitT
	lift . modify $ (++ [[a,b,a,b], [a,b]])    -- * this here is the dummy for the feature matrix being built
	yieldT ()


-- | the underlying monad `State String` is run, revealing final state of `String` 
r = execState (runT "hello" $ scanner t) []


-- | the final state of outputs bs of underlying SubT monad needs to be "distributed"
-- | with final state of input stream as  
with :: SubT a (State [b]) () -> Parser [] a b
with s = P $ go s where 
	go s as = flip (,) [] <$> execState (runT as s) []
	
p' = with $ scanner t


 


















{-///
	w/hat is going on with computation:
/
////////////	the flow of SubT is controled by withf', dending on whether it yields or awaits
	SubT is /



	controlling by pattern matching on value constructor of the Either type


	SubT goes through this flow :: Monad m =>

//


|«\
	See the problem is the Either functor is inside Monad m, so two things:
		the function go cannot run inside Parser value constructor since the type is wrong
		the r from m (Right r) needs to be 'extracted', w/o relying on Comonad m instance

	
	---- * For Reference

	data Sub a b = Done [b] | Partial (a -> Sub a b) 

	withf :: MonadPlus m => Sub a b -> Parser m a b
	withf s = P $ go s where 	
		go (Done bs) xs       = msum [ return (b,xs) | b <- bs ]
		go (Partial k) []     = mzero 							
		go (Partial k) (x:xs) = go (k x) xs      				
	

	p' = withf' $ scanner t


	-- * in general:

	pattern match on cases of SubT
	make sure type checks
	this allows a top routine to control how SubT runs and what values a to bring into context

	when finished, have to extract r from subroutine somehow

	what's a less imperitive way to do this?


	-- * describe the pattern in more abstract manner



	-- * potentially add some sort of two way message passing abstraction

-}


{-

	scanner' :: (Char -> [Feature]) -> Sub Char Feature
	scanner' t = do
		a <- request
		case t a of 
			[] -> scanner' t
			bs -> yield bs


	-- | scanner takes a transducer and does this computation
	-- | note: fails, request, yield, and receive are all of type :: Monad m => Step a m b 
	scanner :: (Buffer -> Feature) -> StepT Char (State (Buffer, Feature)) Feature
	scanner t = do
		a <- request										   
		lift . modify $ \(as,bs) -> (as ++ [b], bs)
		(as, bs) <- lift . get
		let bs' = (<>) <$> bs <*> (t as) <||> bs <||> (t as)
		case (a, bs') of 
			('#', _) -> yield bs'
			(_, [])  -> scanner t
			_        -> (lift . put $ ([], bs')) >> scanner t

-}

------------------------------------------ July 29th. Lift parser into Coroutine -------------------------------------------

--data Step a m b = S { runS :: m (Either (a -> Step a m b) [b]) }




{-

	-- | a Sub monad transformer over some monad m
	data SubT a m b = DoneT (m [b]) | PartialT (m (a -> SubT a m b))

	instance Monad m => Functor (SubT a m) where
		fmap g s = case s of 
			DoneT m    -> DoneT $ liftM (g <$>) m
			PartialT m -> PartialT $ liftM ((g<$>) .) m 


	instance Monad m => Applicative (SubT a m) where
		pure b = DoneT . return $ [b]
		(<*>) = undefined


	instance Monad m => Monoid (SubT a m b) where
		mempty = DoneT . return $ []
		s `mappend` t = case (s,t) of
			(DoneT m, DoneT n)       -> DoneT $ liftM2 (<>) m n 
			(DoneT m, PartialT n)    -> PartialT $ liftM2 (\b k -> ((DoneT . return $ b) <>) . k) m n  
			(PartialT n, DoneT m)    -> PartialT $ liftM2 (\b k -> ((DoneT . return $ b) <>) . k) m n  
			(PartialT m, PartialT n) -> PartialT $ liftM2 (\k k' -> \a -> k a <> k' a) m n



	-- | (>>=) :: DoneT (m [b]) -> (b -> DoneT (m [c])) -> DoneT (m [c])
	instance (Comonad m, Monad m) => Monad (SubT a m) where
		return b = DoneT . return $ [b]
		s >>= g  = case s of 
			DoneT m    -> extract $ liftM (\bs -> foldr (<>) mempty $ g <$> bs) m    -- * this is bad: do not want comonad 
			PartialT m -> undefined
-}


{-

data Iterateei a m r = Iter { bounceIter :: m (Either (a -> Iterateei a m r) r)}

-- | test iter
iter :: Iterateei Int IO Int
iter = do 
	lift . putStr $ "enter two numbers \n"
	a <- awaiti
	b <- awaiti
	let c = a + b
	lift . putStr $ "sum is " ++ (show c) ++ "\n"
	return c
-}


-- | The BIGGEST challenge is to get the feedback and feedforward loop right!!!!!!!!!!












{-

-- now hack some combinator function that passes things to each other

type Mparser a b = [a] -> [(b,[a])]
type MFST a b    = [a] -> [b]

-- | a trivial parser
mp :: Mparser Char Char
mp []     = []
mp (a:as) = [(a,as)]


mt :: MFST Char (Maybe String)
mt []      = []
mt "hello" = [Just "hello"]
mt _       = [Nothing]


-- | build some trivial feedback-feedforward combinator
-- | this combinator act as top level routine, keeping track of state of buffer and stream
-- | parser and transducer demoted to subroutines
feed :: Mparser a b -> MFST b c -> [a] -> [c]
feed _ _ []   = []
feed mp mt as = goo [] as
	where goo = undefined



go [] _ rs  = undefined
go as [] rs = undefined 
splitp p as = let as' = p as in (flip (:) [] . fst <$> as', snd <$> as')


-}

--let as' = splitp mp as in go (snd as') (fst as') rs


--go (snd as') (fst as') rs

--go as bs rs = case mt bs of 
--	[] -> let as' = splitp mp as in go (snd as') $ (++) <$> bs <*> fst as' $ rs
--	cs -> go as [] $ (<>) <$> rs <*> cs




--feed :: Functor f => Parser a b -> NFST f b c -> Parser a b
--feed p m = undefined














-- | lift a parser into coroutine monad
-- | note how this screws with the way the parsr is used and there fore make no sense
--lparser :: Parser a b -> Process [a] [b]
--lparser p = repeatedly $ do
--	as <- await
--	let rs = runP p as
--	yield $ fst <$> rs


--p = lparser (satisfy (== 'a'))

--r = source ["ahello"] ~> p


--instance Automaton Mealy where
--  auto = construct . go where
--    go (Mealy f) = await >>= \a -> case f a of
--      (b, m) -> do
--         yield b
--         go m







































