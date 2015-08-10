{-# LANGUAGE ImpredicativeTypes #-} {-# LANGUAGE NoMonomorphismRestriction #-}

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Play with bad solution part iii
-- | Creator: Xiao Ling
-- | Created: November 17th
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module AppTests where

import Control.Monad
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import Data.List 
import Data.Maybe
import Data.Monoid
import Text.Show.Functions

import ParserT
import ParserCombinator
import ParserUtils

import Core
import JType
import SigParser

{-----------------------------------------------------------------------------
	Test Parser
------------------------------------------------------------------------------}

cenv :: ClassPred
cenv = [("Functor", k1), ("Monad", k1), ("Monoid", k0)]

runPsig :: ClassPred -> Signature -> Either Error ((SigTree, EqMatrix), Signature)
runPsig env s = runReader (runReaderT (runPt sigParser s) env) []
	
pTree :: Signature -> IO ()
pTree t = let Right (x,_) = runPsig cenv t in ppTree $ fst x

t0 = pTree "(Monoid (f a), Monoid a) => f a"

t1 = pTree "a -> b -> c"
t2 = pTree "String -> Char -> (Bool,Double)"
t3 = pTree "(Functor f, Monad m, Monoid a, Monoid (f (m a))) => f (m a) -> m (f a) -> (String,Char)"
t4 = let Right (x,_) = runPsig cenv "Functor f => a -> f a" in x


{-----------------------------------------------------------------------------
	Test JType primitive functions
------------------------------------------------------------------------------}

ju,jd,jl,jt1,jo1,jo2,jo3,jf1 :: JType
ju = U ()
jd = D 12
jl = L [jd,jd]
jt1 = create "Tuple" [("fst", jd), ("snd", S "Second")]

jo1 = create "Maybe" [("v", jd)]
jo2 = create "Maybe" [("v", jl)]
jo3 = L [jo1,jo1]

jf1 = F $ \[D x] -> D $ x + x 

js :: [JType]
js = [ju,jd,jl,jt1,jo1,jo2,jo3,jf1]

-- * enumerate tasks for rest of the day:
-- * test satType for all possible cases
-- * test satConsist for all possible cases
-- * finish impl Strategy AND Typeclass storing datatypes
-- * manually bring them together

-- * satType * -- 

sat' = satType []

satPr = sat' (Pr "Boolean") $ B True 
satPo = sat' (Po $ Sy 'a' (J 0) [("Monoid", k0)]) $ create "Tree" [("v", L [])]
satLs = sat' (Lst $ Pr "Number") $ jl
satTu = sat' (Tup (Pr "Boolean") (Pr "Number")) $ tu (B True) (D 12)
satPa = sat' (Pa (Sy 'f' (J 1) []) [] $ Pr "Number") $ jo1


--jo1S = satType [] jo1 $  
fa     = Pa (Sy 'f' (J 1) []) [] (Po $ Sy 'a' (J 0) [])   -- * f a
maybea = Ad "Maybe" [] [Po $ Sy 'a' (J 0) []] 



{-----------------------------------------------------------------------------
	Test Strategy
------------------------------------------------------------------------------}




{-----------------------------------------------------------------------------
	Now mock some evaluator :: (SigTree, EqMatrix) -> Strategy a
------------------------------------------------------------------------------}

-- * traverse the list
-- * traverse the structure in each list 
-- * traverse the EqMatrix


-- * reason you're slow to implement what's going on is because you cannot visualize Alpha
-- * tangentially you're also not sure which moves are provided

-- * all you have to do is to map each Value constructor to some move
-- * for nested value constructors, just compose moves

-- * to understand real set of problems, hack away at one's we see.
-- * Primitive is the easiest one

-- * everything below should be in Strategy.hs?

{-

	set of tasks moves must accomplish, in order of operation

	traverse the tree

		check param has type
		
		deep type check parameters, so traverse the entire parameter type
		if it's of some nested type 
			-> this does not need to be tied to implementation of datatype
			at the composition level

	traverse EqMatrix

		pairwise deep check parameters

	traverse the tree

		retrieve function implemetation

		evaluate function at *unknown* number of parmeters

		deep type check result
	
	traverse EqMatrix

		pairwise deep check result against parameter to prevent this type of error

		foo :: (Monad m, Functor f, Monoid a) => f (m a) -> m (f a) -> m a
		
		context-free checking doesn't garantee that f, a and m in params 1, 2 must be the 
		same type. Ditto m and a in result wrt params 1 and 2


-}


-- * Evaluate Alphabet in context of some environment
-- * So you have to set the evaluation context


-- * enumerate the fundamental moves
-- * challenge, do this w/o using coroutine?
-- * what is the essence of this computation? is it a pipeline?
-- * Each computation takes something from param as output and 
-- * does something with it, it doesnt do anything to the original value that would change
-- * how other functions in the line evaluate it
-- * the only hicup is findFunction, which could be run first such that all other 
-- * functions in pipeline does have it

-- * type Move m a     = StateT (EvalCtx a) m a 
-- * type Strategy m a = Move m a






























