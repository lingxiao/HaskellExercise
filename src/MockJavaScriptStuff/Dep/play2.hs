{-# LANGUAGE ImpredicativeTypes #-} 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Play with bad solution part ii
-- | Creator: Xiao Ling
-- | Created: November 16th
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Play2 where 

import Control.Monad
import Control.Applicative
import Control.Monad.Reader

import Data.Maybe
import Data.Monoid
import Data.Dynamic

import Text.Show.Functions

import Coroutine
import Strategy3


-- * There is an over-arching problem of proliferation of fundamental types, even though many are synonyms
-- * it's getting difficult to reason about this program since thigns are not properly "walled off"
-- * it's becoming one monolithic being

-- * the parser should be really really small and very different than the strategizer
-- * Should the code concerning locating the right implementtaion be different from 
-- * type checking?

-- * it's really : strategy = checkConstr . evalFunc . atInstance . typeCheck 
-- * each one could throw error or output some value

{-----------------------------------------------------------------------------
	Intermediate representation of function signature
------------------------------------------------------------------------------}

-- * Some index denoting location of a paramter in the value environment `ValEnv`
type Index  = Int

-- * Signature alphabet is either some fully evaluated value "a", "m a", etc
-- * Or some function signature "a -> b" or "a -> f b", etc
type SigAlpha = Either (Index, String) (Index, String)  

-- * A signature tree is just a list of Signature Alphabets where the `Cons` constructor 
-- * is the verb "send" or "->" and elements are Signature alphabets
-- * Note since lists are just unbalanced trees, the sturcture of the list *almost* maps to the curried
-- * representation of the function. 
-- * For now this unfaithful map is sufficient because all you want to do with it is to traverse and fold it
type SigTree  = [SigAlpha]


-- * right now the problem is : should there be some symbolic connection between how constraint is 
-- * represented and how SigAlpha is represented?
-- * should the information on parameterization be encoded in sigAlpha?


-- * this is the back one step, forward two steps part of the project

-- * there's three types of constraints, either the type have to be the same
-- * or the "container" have to be the same
-- * thus the information on what to type check is passed onto the target
data Constraint = Atom | Container | Contained


-- * there's two types of constraints, should they both be listed? are there *only* two types?
-- * Constraint table maps each 
type ConstrTable = [(Index, [Index])]

-- * for now we are only concerned with environment holding one or two variables
-- * in reality, it makes much more sense to hold some JType
-- * how would this VarEnv store functions?
type VarEnv a b = Either ((Index,a),(Index,b)) (Index,a)

-- * Prim function over datatypes * -- 

sigIdx :: SigAlpha -> Index 
sigIdx s = case s of 
	Right (i,_) -> i
	Left  (i,_) -> i

-- * Note varEnv cannot be a data type?
-- * for now it's sufficient to have two functions that take one or two params
varEnv :: a -> b -> VarEnv a b
varEnv a b = Left ((0,a),(1,b))

varEnv' :: a -> VarEnv a b
varEnv' a = Right (0,a)


{-----------------------------------------------------------------------------
	Moves
------------------------------------------------------------------------------}

hasType :: Index -> SigAlpha -> Move a
hasType n t = undefined

-- * given a function name, evaluate it 
evalF :: SigAlpha -> Move a 
evalF = undefined

-- * assert two vals at indices are equal by some predicated
-- * note, in future this `Move` should parameterized over some prediate as well
assertEq :: Index -> Index -> Move a 
assertEq = undefined

{-----------------------------------------------------------------------------
	Constraint Table
------------------------------------------------------------------------------}

-- * as of last impression, there's nothing wrong with this function
-- * BUT, no test have been written yet, you need to test this piece to make sure it works as intended

-- * Question of the Hour: how do you Test this??
-- * could have some `Env` with variables in it, and some signature wrt the variables, or despite the variable specs
-- * so we have to create some `varEnv` 
-- * So now we have to pay for the ineqegant solution of using indices since it means the params have to
-- * be stored somewhere, for now let's just roll with it
-- * you want this function:
-- * g x y = flip runReader varEnv  . flip runReader instEnv . runT $ constr sigTree where varEnv to toVarEnv x y
-- * what does `toVarEnv` look like? is it a list of Dynamic values? looks like abusing the pattern
-- * is it some arbitrariliy nested tuples? but how would the signature work?
-- * what it comes down to is you want polymorphic container 
-- * what if you have some notion of a function generating the value you want given any index?
-- * does it make sense considering you can't really write a function signature for it?

-- * for now just continue with the hack, rember in reality you want everything to be wrapped in Jtype primitive anyways
-- * so heterogenous containers shoulnd't be a problem

-- * what is there to test? you have to implement assertEq if you want to test anyways

-- * the bad test results should throw error? the good results should just emit some value, maybe () for now

-- * note all the functions below could collapse into one function to reduce overall mental load
constr :: SigTree -> Move a 
constr = constrMove . toConstr

constrMove :: ConstrTable -> Move a 
constrMove = foldr (\c m -> block c ~> m) pass

-- * moves built by this function throws some ambigous monad m error
block :: (Index, [Index]) -> Move a 
block (x,ys) = foldr (\y m -> assertEq x y ~> m) pass ys 

toConstr :: SigTree -> ConstrTable
toConstr ts = foldr (\t cs -> constrOne ts t : cs) [] ts

constrOne :: SigTree -> SigAlpha -> (Index, [Index])
constrOne ys x = (,) xi [ sigIdx y | y <- ys, someConstr x y, xi > sigIdx y ] where xi = sigIdx x 


-- * this needs to generated mre sophisticated constrints
-- * check to see that two signature are the same type
-- * for now, just do string equality, in the future may check parameterization, etc
someConstr :: SigAlpha -> SigAlpha -> Bool
someConstr (Left (_,x)) (Left (_,y)) = x == y
someConstr _	_				     = False


{-----------------------------------------------------------------------------
	Tests
------------------------------------------------------------------------------}

-- * test constraint table

x1, x2 :: SigAlpha
x1 = Left (0, "a")
x2 = Right (1, "a -> a")

-- * it makes more sense to add index to each leaf as you traverse it
t1, t2 :: SigTree
t1 = Left (0, "a") : Left (1, "a") : []
t2 = Left (0, "a") : Right (1, "a -> a") : Left (2, "a") : []
t3 = Left (0, "a") : Right (1, "a -> b") : Right (2, "b") : []

c1,c2,c3 :: ConstrTable
c1 = toConstr t1
c2 = toConstr t2
c3 = toConstr t3

-- * note here if we try to do do anything with m1..3, some error is thrown since `move` is not
-- * properly formed. "ambigous m in constraint Monad m"
m1, m2, m3 :: Move a
m1 = constrMove c1
m2 = constrMove c2
m3 = constrMove c3












