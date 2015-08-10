{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
---------------------------------------------------------------------------------------------------

-- | REMAKE Nondeterministic weighted finite state FST module

-- | Weighted Finite State FST 

--   A Probed ﬁnite-state FST T = (A,B, Q, I, F, E, λ, ρ) over a semiring
--        K is speciﬁed by a ﬁnite _inputss alphabet A, a ﬁnite outputss alphabet B, a ﬁnite
--        set of states Q, a set of initial states I ⊆ Q, a set of ﬁnal states F ⊆ Q, a ﬁnite
--        set of transitions E ⊆ Q×(A∪{ε})×(B∪{ε})×K×Q, an initial state Prob
--        assignment λ : I → K, and a ﬁnal state Prob assignment

--   Sources:

--        main: http://tagh.de/tom/wp-content/uploads/fsm_unweigtedautomata.pdf
--        http://www.cs.cornell.edu/courses/   cs786/2004sp/Lectures/l02-axioms.pdf
--        http://acl.ldc.upenn.edu/P/P02/P02-1001.pdf
--        http://www.cs.nyu.edu/~mohri/pub/hwa.pdf
--        http://www.cs.nyu.edu/~mohri/pub/fla.pdf
--        http://www.cs.nyu.edu/~mohri/pub/fst.pdf
--        http://www.cis.upenn.edu/~pereira/papers/tcs.pdf
--        https://courses.engr.illinois.edu/cs373/Lectures/lec06.pdf
--        http://www.iaeng.org/publication/WCECS2010/WCECS2010_pp141-143.pdf
--        https://wiki.inf.ed.ac.uk/twiki/pub/CSTR/ListenSemester1_2010_11/mohri-wfst_asr.pdf
--        http://www.gavo.t.u-tokyo.ac.jp/~novakj/wfst-algorithms.pdf

-- on lenses: http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html
-- a turing machine in haskell: http://en.literateprograms.org/Turing_machine_simulator_(Haskell)
-- a FST reg exp impl:   http://www.cis.uni-muenchen.de/~schmid/papers/SFST-PL.pdf



---------------------------------------------------------------------------------------------------


module FST_1 (

	  FST
	, toFST
	, closure

	) where


import Prelude hiding ((.),id)
import Control.Applicative
import Control.Monad
import Control.Category 
import Data.Monoid
import Data.Machine

import Test.QuickCheck
import Test.QuickCheck.All 
import Test.QuickCheck.Modifiers

import Semiring

-- | import Data.Function 


-----------------------------
---------- Data Type --------
-----------------------------


-- | Any symbol a maps to Just a, Eps transition maps to Nothing
type Symbol a   = Maybe a

-- | A state is either an intermediate state I Int or a final state F Int
data Fstate = I Int | F Int 
	deriving (Eq, Ord)

instance Show Fstate where
	show (I s) = show s
	show (F s) = show s


type TransitionTable a b = Fstate -> Symbol a -> [(Symbol b, Fstate)]


-- | note in each M, a copy of self is always waiting at state 0
data FST a b = T { 
	rewind :: FST a b, 
	step   :: Symbol a -> [(Symbol b, Fstate, FST a b)] 
}


-----------------------------
--------- Constructor ------- 
-----------------------------


toFST :: TransitionTable a b -> FST a b
toFST = flip toM $ I 0

toM :: TransitionTable a b -> Fstate -> FST a b
toM t s = go s
	where go s = T (toFST t) $ \a -> do 
		(b,s') <- t s a
		return (b, s', go s')


-----------------------------
---------- Typeclass --------
-----------------------------


instance Show (FST a b) where 
	show m = show "<FST>"


instance Functor (FST a) where
  fmap f m@(T m0 _) = T (f <$> m0) $ \a -> do 
  	(b,s,m') <- step m a
  	return (f <$> b, s, f <$> m')


instance Applicative (FST a) where
	pure a  = r where r = T (pure a) $ const [(Just a, F 0, r)]
	m@(T m0 _ ) <*> n@(T n0 _) = T (m0 <*> n0) $ \a -> do
		(g, s, m') <- step m a
		(b, t, n') <- step n a
		return (g <*> b, t, m' <*> n')     --- * note how state of n' is output


-- | Note result may flicker between two FSTs if it's rewound? this is bad 
instance Alternative (FST a) where
	empty   = T empty $ const []
	m@(T m0 _) <|> n@(T n0 _) = T (m0 <|> n0) $ \a -> case step m a of 
		[] -> step n a 
		xs -> xs


-- | unionT : run FST, build a new FST from output, run it and use output from new FST
instance Monad (FST a) where
	return = pure 
	m@(T m0 _) >>= g = T (m0 >>= g) $ \a -> do 
		(jb, s, m') <- step m a 
		case jb of 
			Nothing  -> []
			(Just b) -> do
				(c,_,_) <- step (g b) a 
				return (c, s, m' >>= g)         --- * note how m state is output


instance Category FST where
	id = T id $ \a -> [(a, F 0, id)]
	m@(T m0 _) . n@(T n0 _) = T (m0 . n0) $ \a -> do 
		(b, s, n') <- step n a 
		(c, t, m') <- step m b
		return (c, t, m' . n' )                --- * note how m's state is outut


-- | note there's a level of abstraction mismatch: the other classes are implemented at the level 
-- | of an individual computation for each transition, the two typeclass below are in terms of sets of
-- | transducers

instance Monoid b => Monoid (FST a b) where
	mempty  = empty
	mappend = semiOp restartU True True


instance Monoid b => Semiring (FST a b) where
	zero  = mempty
	(.+.) = mappend
	one   = empty        		-- note zero and one are the same? this violates some laws?
	(.*.) = semiOp restartC True True


----------- moniod and semiring helpers ----------- 

-- | under union, transducer two can run if transducer one at initial state
restartU :: Fstate -> Bool 
restartU (I 0) = True
restartU _     = False

-- | under catenation, transducer two can run if transducer one at initial state or at a final state
restartC :: Fstate -> Bool 
restartC (I 0) = True
restartC (F _) = True
restartC _     = False


-- | Template logic for both (.+.) and (.*.), where the only difference is the condition in which transducer n can restart 
semiOp :: Monoid b => (Fstate -> Bool) -> Bool -> Bool -> FST a b -> FST a b -> FST a b
semiOp f runN runM m@(T m0 _) n@(T n0 _) = let op = semiOp f True True in case (runM, runN) of 
	
	(False, False) -> mempty												-- if both machines stopped, neither can run ever again

	(False, True)  -> n 													-- once m is stopped, only n runs from now

	(True, False)  -> T (m0 `op` n0) $ \a -> do 							-- once n is paused, it can restart at 0 conditional on which state m is in
		(b,s,m') <- step m a 
		return (b, s, semiOp f True (f s) m' n0)

	(True, True)   -> T (m0 `op` n0) $ \a -> case (step m a, step n a) of
		([], nb) -> nb         															-- once m stops, only n runs from now				
		(mb, []) -> (\(b,s,m') -> (b,s, semiOp f True (f s) m' n0)) <$> mb      -- once m is paused, it can run again on certain condition
		(mb,nb)  -> (\(b,s,m') (b',t,n') -> (b <> b', s, m' `op` n')) <$> mb <*> nb		-- if both m and n runs, then it can run again in future


-------------------------------
------ Other Operations -------
-------------------------------


-- | In plus closure, there is always a thread where the transducer is waiting at state 0, receiving inputs
closure :: FST a b -> FST a b 
closure m@(T m0 _) = T m0 $ \a -> step m0 a ++ step m a 



-- instance Automata (FST a b) where


-- | in kleene star, there is always a thread where the mealy machine is waiting at state 0 to receive inputs
--mstar :: M a b -> M a b
--mstar m@(M _ g _ ) = M 0 g $ \a -> case st m of 
--	0 -> star : step m a 
--	_ -> star : step (rewindM m) a ++ step m a
--	where star = (Eps, mstar . rewindM $ m)



--------------------
------ Tests -------
--------------------


m1f :: TransitionTable String String
m1f (I 0) Nothing    = [(Nothing, (I 0))]
m1f (I 0) (Just "a") = [(Just "a", (F 3)),(Just "b", (I 1))]
m1f (I 1) (Just "b") = [(Just "b", (I 0))]
m1f (F 3) (Just "c") = [(Just "c", (I 0))]
m1f _ _          = []

m1 :: FST String String
m1 = toFST m1f 


m2f :: TransitionTable String String
m2f (I 0) (Just "a") = [(Just "aa", (F 1))]
m2f (I 0) (Just "z") = [(Just "zz", (I 0))]
m2f _ _ 		 = []

m2 :: FST String String
m2 = toFST m2f



mff :: Fstate -> Symbol String -> [(Symbol (String -> String), Fstate)]
mff (I 0) (Just "a") = [(Just $ \b -> b <> b, (F 1))]
mff (F 1) (Just "b") = [(Just $ \b -> b <> b <> b, (I 0))]
mff _ _ 		 = []

mf :: FST String (String -> String)
mf = toFST mff 










































































