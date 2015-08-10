{-# LANGUAGE ImpredicativeTypes #-} 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Play with bad solution
-- | Creator: Xiao Ling
-- | Created: November 13th
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Play where 

import Control.Monad
import Control.Applicative
import Control.Monad.Reader

import Data.Maybe
import Data.Monoid
import Data.Dynamic

import Text.Show.Functions

import Coroutine
import Strategy3


{-----------------------------------------------------------------------------
	Intermediate representation
------------------------------------------------------------------------------}

-- * compiler/types/TypeRep.lhs:105 
-- * additional datatype

-- * atomic unit of a signature
type SigAlpha = String 

-- * Note properly constructed AST are, infact, lists 
data AST = Send AST AST | S SigAlpha deriving (Show, Eq)


-- * type AST = [SigAlpha] where data SigAlpha = S String

toList :: AST -> [SigAlpha]
toList t = case t of 
	S a 		  -> [a]
	Send (S a) t2 -> a : toList t2


-- * Some strategy mockup * --

-- * question: where should variable x reside, here or in environment? 
-- * if in environmemt, how does it know which one to bind to?
-- * is this an impl. detail or something vital?
hasType :: a -> String -> Strat a 
hasType x t = undefined


hasType' :: Int -> String -> Strat a
hasType' n t = undefined

-- * given a function name, evaluate it 
evalF :: String -> Strat a 
evalF = undefined

-- * assert two vals at indices are equal by some predicated
-- * note, in future this `Strat` should parameterized over some prediate as well
assertEq :: Int -> Int -> Strat a 
assertEq = undefined


-- * Send AST to constraint table * -- 

-- * some function that crawls the tree and build a constraint, 
-- * it really just comes down to matching things that are equal to each other

-- * first question, what kind of form should the constraint table look like?
-- * what kind of function does it need to serve?
-- * it needs to be crawled by the function and map (order preserving) to some sequence of 

-- * for now the table is just a list of tuples, asserting which parameters are equalt to which
-- * for now we check surface equality, in the future there should be soem notion of equality over
-- * parameterized types
type ConstraintTable = [(Int,Maybe Int)]

-- * building some constraint table is not hard, it could even be done with existing prelude or Data.List fnctions
-- * if the AST was represented by a list
-- * however, how do I know if it's the right approach?
-- * to cheat, first convert AST to list, then use list functions

-- * manually make some table first
toConstr :: AST -> ConstraintTable
toConstr t = undefined

-- * before the rest of the sequence is built, this routine is run and inserted into the end
-- * here the information about which elems are equal is perculoated down to second level by reprsenting them using indices
-- * this is an ok soluton provided the `Env` which the move reads from repsect the indices somehow
-- * some better soln must exist but cannot think of it right now
fromConstr :: ConstraintTable -> Strat a
fromConstr = foldr (\(a,b) s -> if b == Nothing then s else assertEq a $ fromJust b) pass



-- * Send AST to Strategy * -- 

-- * specifically, it should type check parameter and output
-- * should it also build some intermediate form relating equal types?


-- * here the problem is where do we keep the param?
-- * typecheck all params, do we keep an index?

-- * in this version, only two params can be checked
chkTwo :: a -> a -> AST -> Strat a
chkTwo x y (S a)        = hasType y a
chkTwo x y (Send t1 t2) = case t1 of 
	S a -> hasType x a ~> chkTwo x y t2

-- * in this soln, we use index to keep track of which param is to be checked
-- * the index is passed into `hasType'`, which presumbably maps index to parameter
chkMany :: AST -> Strat a
chkMany = go 0 where
	go n (S a) 		  = hasType' n a  				-- * here something differnt actually has to be done
	go n (Send t1 t2) = case t1 of
		S a -> hasType' n a ~> go (succ n) t2


-- * this one hardcodes the act of type checking and evaluating things
-- * it still doesn't do constraint analysis though because the knowlege there is not expclitiy represented
-- * and therefore cannot be mapped to some sequence of `Move`
chkEval :: FuncName -> AST -> Strat a 
chkEval = go 0 where 
	go n fn t = case t of
		S a 	      -> evalF fn ~> hasType' n a 
		Send (S a) t2 -> hasType' n a ~> go (succ n) fn t2

-- * for now, stop imagining alternatives and just put the existing pieces together
-- * this function actually should work provided the Env is structured as it's suppose to
-- * and the moves are implemented as they're suppose to

-- * the flaw with this is how should functions as param be represented?
-- * would it change the essence of what's going on? 
-- * should we deal with the detail now? I think yes..
chkEval':: FuncName -> AST -> Strat a 
chkEval' fn = go 0 where 
	go n t = case t of 
		S a       	  -> evalF fn ~> hasType' n a ~> (fromConstr . toConstr $ t)
		Send (S a) t2 -> hasType' n a ~> go (succ n) t2


-- * now make chkEval' that works with functions as params, this may require some change in how AST is represnted
-- * in that things are "properly" curried
-- * should functions have a different value constructor to denote its difference?
-- * should there be some subroutine crawling through functions that are params? should there be soem differnce in subtroutne
-- * for `toConstr` and `fromContr`?

-- * 1. difference in AST
-- * 2. difference in chkEval
-- * 3. difference in fromContr
-- * 4. difference in toConstr






-- * now at this point we can either: work on the moves and Env to complete the mockup
-- * move to the lifting portion to see how it would all fit <-- let's go with this one

-- * we have to create some env where variables are mapped to some index, or some sort of annotation
-- * our strategy then runs inside such an env
-- * so there's two aspect to it, the rest of of what's going on, and the piece of how to create
-- * the env itself

-- * now it's time to model the variable env
-- * variable env should map some index to a variable, but note maybe a should be wrapped in Dynamic?
-- * this is the fundamental flaw of putting things inside a list, they're grouped by some shallow propety of all being params
-- * what if it's some function instead? it is possble to have some function that emits a value based on index?
type Index  = Int
type VarEnv = [(Index, Dynamic)]



{-----------------------------------------------------------------------------
	Test intermediate representation
------------------------------------------------------------------------------}

-- * note intermediate representation is just a list, so consider rewiriting AST to some list

-- * "a -> a"
t1 = Send (S "a") $ S "b"

-- * "a -> b -> c"
t2 = Send (S "a") (Send (S "b") $ S "c")

-- * "a -> b -> c -> d"
t3 = Send (S "a") $ Send (S "b") (Send (S "b") $ S "c")

-- * we can work backwards by build a table manually first
s1 = Send (S "a") $ S "a"
c1 = [(0,Just 1)]  			-- * this table tells you the last value should have the same as the first value

-- * now what are you going to do with c1, s1 pair, it should be some form of more advanced chkEval

-- * step one, crawl through the table and build some move sequence
-- * how do you send this representation to some sequence of moves?


{-----------------------------------------------------------------------------
	Move && Strategy Formulation two
------------------------------------------------------------------------------}



-- * some interediate scaffold
-- * this shows that all the params of function are "uncurried" and something is done on them
type ParamRet = ([String], String)

-- * this really is the same as `last`
group :: AST -> ParamRet
group = flip go ([], []) where 
	go (S a) (xs,_)        = (xs,a)
	go (Send t1 t2) (xs,x) = case t1 of 
		(S a) -> go t2 (xs ++ [a], mempty)
		_     -> error "illegal AST type is imporoperly curried"




{-

	-- * NOTE: be prepared to settle for this solution if necessary

	-- * passing in index implies that some function and some variable
	is stored somewhere, which means the singature either propagate upwards
	OR it's hidden behind dynamic, but it still needs to be writtend down
	somewhere

	strat :: [Either String String] -> Strat
	strat xs = capH ~> go xs where 
		go []     _  = pass
		go (y:ys) i  = case y of 
			Left  s -> typed i ~> go xs $ i + 1
			Right s -> eval ~> (chkClosure xs) ~> go xs i


	The problem:

	convert this "a -> a -> a" or "m a -> (a -> m b) -> m b"
	into some `Strategy o`

	so far, we've built the moves associated with each possible signature symbol encountered
	
	a straight mapping would not work since the order of the moves is not actually the same as the order of the signature
	as moves are currently structured

	The primary difference with specifying function signatures in a dynamic language is that it cannot actually be enforeced. 
	Instead some strategy is built from the signature  
	

	so there needs to be some intermediate representation that gets sent to. So focus on creating this intermediate representation, questions asked:

		1. what does it need to encode
				- maps type to variable
				- maps variable type to variable type
				- preserve the order, send it to some other representation
		2. is the target fixed?
				- should reduce the amount of variables passed into it
				- what is the principle here when it comes to what information to include?


	the biggest tripup seems to be the order of expression, and some latent (?) recrusive structure


-}
	







































