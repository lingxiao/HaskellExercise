{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# LANGUAGE ImpredicativeTypes #-} 

---------------------------------------------------------------------------------------------------

--  Nondeterministic Finite State Transducer 

--   A Probed ﬁnite-state NFST T = (A,B, Q, I, F, E) over a semiring
--        K is speciﬁed by a ﬁnite _inputss alphabet A, a ﬁnite outputss alphabet B, a ﬁnite
--        set of states Q, a set of initial states I ⊆ Q, a set of ﬁnal states F ⊆ Q, a ﬁnite
--        set of transitions E ⊆ Q×(A∪{ε})×(B∪{ε})×K×Q

--   Sources:

--        main: http://tagh.de/tom/wp-coTeT/uploads/fsm_unweigtedautomata.pdf
--        http://www.cs.cornell.edu/courses/   cs786/2004sp/Lectures/l02-axioms.pdf
--        http://acl.ldc.upenn.edu/P/P02/P02-1001.pdf
--        http://www.cs.nyu.edu/~mohri/pub/hwa.pdf
--        http://www.cs.nyu.edu/~mohri/pub/fla.pdf
--        http://www.cs.nyu.edu/~mohri/pub/NFST.pdf
--        http://www.cis.upenn.edu/~pereira/papers/tcs.pdf
--        https://courses.engr.illinois.edu/cs373/Lectures/lec06.pdf
--        http://www.iaeng.org/publication/WCECS2010/WCECS2010_pp141-143.pdf
--        https://wiki.inf.ed.ac.uk/twiki/pub/CSTR/ListenSemester1_2010_11/mohri-wNFST_asr.pdf
--        http://www.gavo.t.u-tokyo.ac.jp/~novakj/wNFST-algorithms.pdf

-- a NFST reg exp impl:   http://www.cis.uni-muenchen.de/~schmid/papers/SNFST-PL.pdf

---------------------------------------------------------------------------------------------------


module Transducer (

	  NFST 

	, step
	, rewind
	, toNFST
	, closure

) where


import Prelude hiding ((.),id)
import Control.Applicative
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans
import Control.Comonad
import Control.Category 
import Data.Monoid
import Data.Maybe
import Data.Profunctor

import Semiring
import Type
import Utils

------------------------------
---------- Data Type ---------
------------------------------

-- | note in each M, a copy of self is always waiting at initial state
-- | f is a functor that wraps a and b, where (f a) and (f b) are monoids, and mempty denotes Eps transition
data NFST f a b = T { 
	rewind :: NFST f a b, 
	step   :: f a -> [(f b, Fstate, NFST f a b)] 
}


-- | not sure how this fits in yet, but does make type params less complexk
type Transducer a b = forall f. NFST f a b

-----------------------------
--------- Constructor ------- 
-----------------------------

toNFST :: (Functor f, Monoid (f a), Monoid (f b)) => NFSTtable f a b -> NFST f a b
toNFST = toT $ I 0


toT :: (Functor f, Monoid (f a), Monoid (f b)) => Fstate -> NFSTtable f a b -> NFST f a b
toT s t = go s
	where go s = T (toNFST t) $ \a -> do 						  	 -- Note the function is nondetermistic and thus is in list monad
		(b,s') <- t s a 							         -- step transducer forward based on given input
		case s' of
			(F _) -> return (b, s', go s')				                 -- if new state is terminal state, do not attemp Eps transition
			(I _) -> case t s' mempty of			                         -- else attempt to do Eps transition 
				[] -> return (b, s', go s')  
                                xs -> (\(c, s'') -> (b <> c, s'', go s'')) <$> xs                -- combine results of two transitions:

------------------------------
---------- Instances ---------
------------------------------

instance Show (NFST f a b) where 
	show _ = show "<Automata>"


instance Functor f => Functor (NFST f a) where
  fmap f m@(T m0 _) = T (f <$> m0) $ \a -> do 
  	(b,s,m') <- step m a
  	return (f <$> b, s, f <$> m')


instance (Functor f, Applicative f) => Applicative (NFST f a) where
	pure a  = r where r = T (pure a) $ const [(pure a, F 0, r)]
	m@(T m0 _ ) <*> n@(T n0 _) = T (m0 <*> n0) $ \a -> do
		(g, s, m') <- step m a
		(b, t, n') <- step n a
		return (g <*> b, t, m' <*> n')     --- * note how state of n' is output


instance Applicative f => Alternative (NFST f a) where
	empty   = T empty $ const []
	m@(T m0 _) <|> n@(T n0 _) = T (m0 <|> n0) $ \a -> case step m a of 
		[] -> step n a 
		xs -> xs


-- | POORLY THOUGHT OUT
-- | problem: too many/unwieldy typeclass restrictions?
-- | comonad restriction would throw unexpected error message
instance (Comonad f, Applicative f) => Monad (NFST f a) where
	return = pure
	m@(T m0 _) >>= g = T (m0 >>= g) $ \a -> do 
		(b, s, m') <- step m a   
		case b of 
			mempty -> []    -- * BUG: this here is a variable, not `mempty`
			_      -> do 
				(c,_,_) <- step (g . extract $ b) a
				return (c,s,m'>>=g)


instance (Comonad f, Applicative f) => MonadPlus (NFST f a) where
	mzero = empty
	mplus = (<|>)


instance Category (NFST f) where
	id = T id $ \a -> [(a, F 0, id)]
	m@(T m0 _) . n@(T n0 _) = T (m0 . n0) $ \a -> do 
		(b, s, n') <- step n a 
		(c, t, m') <- step m b
		return (c, t, m' . n' )                --- * note how m's state is output


instance Functor f => Profunctor (NFST f) where
	dimap g f m@(T m0 _) = T (dimap g f m0) $ \a -> let a' = g <$> a in do
		(b,s,m') <- step m a'
		return (f <$> b, s, dimap g f m')


instance (Monoid (f a), Monoid (f b), Applicative f) => Monoid (NFST f a b) where
	mempty  = empty
	mappend = union


instance (Monoid (f a), Monoid (f b), Applicative f) => Semiring (NFST f a b) where
	zero  = mempty
	(.+.) = mappend
	one   = empty        		               -- note zero and one are the same? this violates some laws?
	(.*.) = catenate 


--------------------------------
------ Kleene Operations -------
--------------------------------

-- | In plus closure, there is always a thread where the transducer is waiting at initial state
-- | That is unless the transducer is already at initial state
-- | Note the transducer is rewound to initial state before its closure is taken
closure :: (Monoid (f a), Monoid (f b), Applicative f) => NFST f a b -> NFST f a b 
closure m = go False $ rewind m where 
	go runM0 m@(T m0 _) = case runM0 of 

		False -> T (closure m0) $ \a -> do 
			(b,s,m') <- step m a
			return (b,s, go (rewindK s) m')

		True  -> T (closure m0) $ \a -> case (step m a, step m0 a) of 
			([], m0s) -> (\(c,t,n)  -> (c, t, go (rewindK t) n ) ) <$> m0s
			(ms, [] ) -> (\(b,s,m') -> (b, s, go (rewindK s) m') ) <$> ms
			(ms, m0s) -> (\(b,s,m') (c,_,_) -> (b <> c, s, go (rewindK s) m') ) <$> ms <*> m0s


-- | the next two operations are not exported. Use general interface `<>` and `.*.` instead
-- | Under catenation, transducer two can run if transducer one at initial state or it's at a final state
catenate :: (Monoid (f a), Monoid (f b), Applicative f) => NFST f a b -> NFST f a b -> NFST f a b
catenate = combine True True $ \s -> case s of 
	I 0 -> True
	F _ -> True
	_   -> False

-- | Under union, transducer two can run if transducer one is at initial state
union :: (Monoid (f a), Monoid (f b), Applicative f) => NFST f a b -> NFST f a b -> NFST f a b
union = combine True True $ \s -> case s of 
	I 0 -> True
	_   -> False


--------------------------------
------------ Utils -------------
--------------------------------

-- | Under kleene closure, the transducer is rewound if it's not already at initial state
rewindK :: Fstate -> Bool
rewindK (I 0) = False
rewindK _     = True


-- | Template logic for both (.+.) and (.*.), where the only difference is the condition in which transducer n can regot
combine :: (Monoid (f a), Monoid (f b), Applicative f) => Bool -> Bool -> (Fstate -> Bool) -> NFST f a b -> NFST f a b -> NFST f a b
combine runN runM f m@(T m0 _) n@(T n0 _) = let o = combine True True f in case (runM, runN) of 
	
	(False, False) -> mempty							-- if both machines stopped, neither can run ever again

	(False, True)  -> n 						          	-- once m is stopped, only n runs from now

	(True, False)  -> T (m0 `o` n0) $ \a -> do 					-- once n is paused, it can regot at 0 conditional on which state m is in
		(b,s,m') <- step m a 
		return (b, s, combine True (f s) f m' n0)

	(True, True)   -> T (m0 `o` n0) $ \a -> case (step m a, step n a) of
		([], nb) -> nb         							-- once m stops, only n runs from now				
		(mb, []) -> (\(b,s,m') -> (b,s, combine True (f s) f m' n0)) <$> mb     -- once m is paused, it can run again on certain condition
		(mb,nb)  -> (\(b,s,m') (b',t,n') -> (b <> b', s, m' `o` n')) <$> mb <*> nb		-- if both m and n runs, then both can run again in future


-------------------------------------
------------ Depricated -------------
-------------------------------------

-- | problem: created a a typeclass constraint on type of b
-- | The logic of flipping input-output order of transducer belongs here
-- | That is: a transducer can be defined as mapping from input to output (generation)
-- | but should also be able to map output to input (analysis)

--t :: NFSTtable Maybe String String
--t (I 0) Nothing    = [(Nothing, (I 0))]
--t (I 0) (Just "a") = [(Just "a", (F 3)),(Just "b", (I 1))]
--t (I 1) (Just "b") = [(Just "b", (I 0))]
--t (F 3) (Just "c") = [(Just "c", (I 0))]
--t _ _          = []
--ft = toNFST t

