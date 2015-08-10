{-# LANGUAGE ImpredicativeTypes #-} {-# LANGUAGE NoMonomorphismRestriction #-}
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : 
-- | Creator: Xiao Ling
-- | Created: November 30th
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Strategy (

	  Move
	, MoveT
	, StrategyT
	, toStrat

) where 

import Control.Monad
import Control.Monad.State

import Data.Maybe
import Data.Monoid
import Data.Dynamic
import Text.Show.Functions

import Core

{-----------------------------------------------------------------------------
	I. Data Types
------------------------------------------------------------------------------}

-- * Evaluation context maps some representation of functio to be evaluated to a list of params
-- * The function is either some Name or the actual implemetation

-- * decision, should the result be just put into the param list?
type EvalCtx a = (FnName, [a])

type MoveT m a = StateT (EvalCtx a) m () 
type Move a    = forall m. Monad m => MoveT m a

type StrategyT m a = MoveT m a

{-----------------------------------------------------------------------------
	II. Primitive Functions over DataTypes
------------------------------------------------------------------------------}

at :: [a] -> I -> Maybe a 
at as (I i) = if length as >= i then Nothing else Just $ as !! i

param :: Monad m => I -> StateT (EvalCtx a) m a
param i = do 
	(_,ps) <- get
	case ps `at` i of 
		Nothing -> fail ""
		Just t  -> return t

fndFun' :: Monad m => StateT (EvalCtx a) m Dynamic
fndFun' = undefined

-- * could be broken down into some primitive
-- * does not belong in this section
-- * check 
comp :: (a -> Maybe DataType) -> a -> a -> J -> J -> Bool
comp g a b i j = undefined


{-----------------------------------------------------------------------------
	III. Fundamental Moves
------------------------------------------------------------------------------}

-- * goal: as much as possible, move all the logic pertaining with `a` to parameters
-- * how do you make some custom fail move ?
-- * the bigger question is how do we pass these `a` specific functions into Strategy?
-- * should it accept them as parametrs


{-
	enumerate things making your mind go blank right now:

	the details of interaction with underlying data type is not clear
	the details of how toStrat is used is not clear	
	maybe it's time to revisit JsType?

-}

pass :: Move a 
pass = return ()

-- * right now, look at what the fundamental moves are using
-- * wrt Jtype and find either implementation ofit, or fundamental impl of it

-- * check underlying paramater carries type information
-- * is this even a reachable case for `JType` ?
typed' :: (a -> Maybe DataType) -> I -> Move a 
typed' g i = param i >>= \p -> if g p == Nothing then fail "" else return ()


-- * check underling parameter is properly typed
deepTyped' :: (a -> Alpha -> Bool) -> I -> Alpha -> Move a 
deepTyped' g i a = param i >>= \p -> if g p a then return () else fail ""



-- * some function needs to find appropriate types in Ctx, then some other function
-- * needs to traverse both types upto appropriate depth and typecheck
consistTyped' :: Move a
consistTyped' = undefined

-- * find the appropriate function and evaluate at list of paramters
-- * put the result back into CtxEnv
evalFun' :: Move a 
evalFun' = do 
	(_,ps) <- get 
	g      <- fndFun' 
	return ()

-- * prelim signature * --

-- * this guy has to interact with the instance environment
fndFun :: Monad m => MoveT m a 
fndFun = undefined

typed :: I -> Move a 
typed = undefined

deepTyped :: I -> Alpha -> Move a
deepTyped = undefined

evalFun :: Move a
evalFun = undefined

consistTyped :: Move a
consistTyped = undefined

{-----------------------------------------------------------------------------
	IV. Move toStratr :: Beta -> Move
------------------------------------------------------------------------------}

move :: Monad m => Beta -> MoveT m a 
move (i,Left _ ) = typed i
move (i,Right a) = typed i >> deepTyped i a 

{-----------------------------------------------------------------------------
	V. Strategy toStratr :: SigTree -> Strategy
------------------------------------------------------------------------------}

-- * toStrat automatically builds this
-- * aLine = typed >> deepTyped >> pairwiseTyped >> fndFunctions >> evalFun >> deepTyped >> pairwiseTyped 
-- * so by inspection, we see it must first traverse everything but the terminal result, then 
-- * do pariwise check, then fndFunc >> evalFun
-- * then do deepTyped again, then do pairwiseTyped
toStrat :: Monad m => SigTree -> StrategyT m a
toStrat []     = pass
toStrat (x:xs) = case xs of 
	[] -> consistTyped >> evalFun >> move x >> consistTyped >> toStrat xs 
	_  -> move x >> toStrat xs


