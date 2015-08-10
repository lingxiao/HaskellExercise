{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances #-}


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | WHILE Interpretor  and WHILE Interpretor ++ 
-- | Created: April 25th
-- | lecture and hw source: http://www.seas.upenn.edu/~cis552/12fa/hw/hw7/

------------------------------------------------------------------------------
------------------------------------------------------------------------------


module While_Interpreter where


import Control.Monad
import Control.Monad.State 
import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Applicative

import Data.Maybe
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M

--import While_Parser


import Test.HUnit hiding ( State )
import Test.QuickCheck
import Test.QuickCheck.Function


tick :: (MonadState Int m) => m ()
tick = modify (+1) 

record :: (MonadWriter String m) => String -> m ()
record msg = tell $ msg ++ "; "

----------------------------
-- The machine's 'memory' -- 
----------------------------

{-
type Memory = Map Variable Value



-------------------------------
-- Translated Math Functions --
-------------------------------

-- translate Op to native operations -- 

bool :: Ord a => Bop -> a -> a -> Bool
bool (Gt) = (>)
bool (Lt) = (<)
bool (Ge) = (>=)
bool (Le) = (<=)


-- consider getting rid of all this boilerplate
ops :: Bop -> Value -> Value -> Maybe Value

ops _ (BoolVal _) (IntVal _ )  = Nothing 
ops _ (IntVal _ ) (BoolVal _ ) = Nothing 

ops (Plus)   ( IntVal v1 ) ( IntVal v2 )   = Just $ IntVal $ (+) v1 v2
ops (Minus)  ( IntVal v1 ) ( IntVal v2 )   = Just $ IntVal $ (-) v1 v2
ops (Times)  ( IntVal v1 ) ( IntVal v2 )   = Just $ IntVal $ (*) v1 v2
ops (Divide) ( IntVal v1 ) ( IntVal v2 )   = Just $ IntVal $ div v1 v2
ops g        ( IntVal v1 ) ( IntVal v2 )   = Just $ BoolVal $ bool g v1 v2

ops (Plus)    ( BoolVal b1 ) ( BoolVal b2 ) = Just $ BoolVal True
ops (Minus)   ( BoolVal b1 ) ( BoolVal b2 ) = Just $ BoolVal True
ops (Times)   ( BoolVal b1 ) ( BoolVal b2 ) = Just $ BoolVal True
ops (Divide)  ( BoolVal b1 ) ( BoolVal b2 ) = Just $ BoolVal True
ops g         ( BoolVal b1 ) ( BoolVal b2 ) = Just $ BoolVal $ bool g b1 b2

-----------------------------
-- Store Utility Functions --
-----------------------------

type Counter  = Int
type ErrMsg   = String
type Log      = String
type Output a = (String, a)


tick :: (MonadState Counter m) => m ()
tick = modify (+1) 

record :: (MonadWriter Log m) => Log -> m ()
record msg = tell $ msg ++ "; "


--------------
-- Store ++ --
--------------

{-
	Visually from the outside in:

	Error ErrMsg m
	Writer Log m
	State Counter m
	State Memory m
	Identity

	note: to access StateT Memory m, it has to be lifted three times
-}
type StorePlus a = ErrorT ErrMsg ( WriterT Log ( StateT Counter ( State Memory ))) a

runStorePlus :: Memory -> StorePlus a -> (((Either ErrMsg a, Log), Counter), Memory)
runStorePlus mem g = runIdentity $ runStateT ( runStateT ( runWriterT . runErrorT $ g ) 0 ) mem

displayOutput :: (((t1,t2),t3),t4) -> (([Char], t1), ([Char], t2), ([Char], t3), ([Char], t4))
displayOutput (((a,b),c),d) = (("Output:", a), ("Log: ", b), ("Profile: ", c), ("Memory: ", d))

runEval :: Memory -> Expression -> (Output (Either ErrMsg Value), Output Log, Output Counter, Output Memory)
runEval m e = displayOutput . runStorePlus m $ evalE e

runStatement :: Memory -> Statement -> ( Output (Either ErrMsg ()), Output Log, Output Counter, Output Memory)
runStatement m s = displayOutput . runStorePlus m $ evalS s 


-----------------------------
-- Expression Evaluator ++ --
-----------------------------

-- | note the signature is getting unwiedly
evalE :: ( 
		MonadTrans t1,
		MonadTrans t2,
		MonadTrans t3,
		Monad (t3 m),
		Monad (t2 (t3 m)),
		Monad (t1 (t2 (t3 m))),
		MonadState Memory m, 
		MonadState Counter (t1 (t2 (t3 m))), 
		MonadError ErrMsg (t1 (t2 (t3 m))),
		MonadWriter Log (t1 (t2 (t3 m))) 
		) => 
	Expression -> t1 (t2 (t3 m)) Value

evalE (Val v) = do
	tick
	case v of 
		IntVal a  -> record "intVal encountered"
		BoolVal b -> record "boolVal encountered"
	return v

evalE (Var k) = do
	tick 
	m <- lift . lift . lift $ get
	let mv = M.lookup k m
	record "Accessed Memory"
	case mv of 
		Just v  -> return v
		Nothing -> throwError "null pointer exception"

evalE (Op bop e1 e2) = do
	tick
	x <- evalE e1
	y <- evalE e2
	let xy = ops bop x y in case xy of 
		Just v  -> return v
		Nothing -> throwError "Cannot add Int with Bool"


----------------------------
-- Statement Evaluator ++ --
----------------------------

evalS :: ( 
		MonadTrans t1,
		MonadTrans t2,
		MonadTrans t3,
		Monad (t3 m),
		Monad (t2 (t3 m)),
		Monad (t1 (t2 (t3 m))),
		MonadState Memory m, 
		MonadState Counter (t1 (t2 (t3 m))), 
		MonadError ErrMsg (t1 (t2 (t3 m))),
		MonadWriter Log (t1 (t2 (t3 m))) 
		) => 
	Statement -> t1 (t2 (t3 m)) ()

evalS (Skip) = tick 

evalS (Assign k e) = do
	tick
	v <- evalE e
	lift . lift . lift $ modify ( M.insert k v )
	record "Memory modified"

evalS (If e s1 s2) = do
	tick
	v <- evalE e 
	case v of 
		(BoolVal True)  -> evalS s1
		(BoolVal False) -> evalS s2
		_     			-> throwError "Error: expression must evaluate to True or False"
	record "If statement evaluated"

evalS x@(While e s) = do
	tick
	v <- evalE e 
	case v of 
		(BoolVal True)  -> evalS s >> evalS x
		(BoolVal False) -> return ()
		_     			-> evalS Skip 
	record "While statement evaluated"

evalS (Sequence s1 s2) = tick >> evalS s1 >> evalS s2


-----------------------
-- Interpretor Tests --
-----------------------

-- mock store
mem0 = M.empty :: Memory
mem1 = M.fromList [("x",IntVal 10),("y",IntVal 5)] :: Memory 

-- test evalE --
e1 = Var "x"
e2 = Var "z"
e3 = Val $ IntVal 1
e4 = Val $ BoolVal True
e5 = Op Plus e1 e3
e6 = Op Plus e1 e4 
e7 = Op Gt e3 (Val $ IntVal 3)
e8 = Op Lt e3 (Val $ IntVal 3)


-- test evalS --
s1,s2,s3,s4,s5,s6,s7,s9,s10,s11,s12 :: Statement
s1 = Assign "z" ( Var "x" )
s2 = Assign "z" ( Val $ IntVal 1 )
s3 = Assign "z" ( Val $ BoolVal False )
s4 = Assign "z" e5
s5 = If e7 s1 ( Assign "SFIVE" ( Var "x" ))  				-- false
s6 = If e8 ( Assign "SIX" ( Var "y" ) ) s2  				-- true
s13 = If (Val $ IntVal 1 ) ( Assign "SIX" ( Var "y" ) ) s2  -- error
s7 = Sequence s5 s6
s8 = Sequence (Sequence (Assign "X" ( Val $ IntVal 1 ) ) ( Assign "Y" ( Val $ BoolVal False)) ) ( Sequence (Assign "Z" ( Val $ IntVal 100 ) ) ( Assign "U" ( Val $ BoolVal True)) )
s9 = Sequence  (Assign "Z" ( Val $ IntVal 100 )) ( Sequence (Assign "X" ( Val $ IntVal 1 ) ) ( Assign "Y" ( Val $ BoolVal False)) )
s10 = Skip
s11 = While condy decry where 
	condy = Op Gt (Var "y") ( Val $ IntVal 0)
	decry = Assign "y" (Op Minus ( Var "y" ) ( Val $ IntVal 1 ))

-- run all tests 
ss = [s1,s2,s3,s4,s5,s6,s7,s9,s10,s11, s13] :: [Statement]

-- infinite loop
s12 = While ( Val $ BoolVal True ) s1



-----------------------
-- Basic Store Monad --
-----------------------

type Store = StateT Memory Identity 

-- runs the expression and mutate store --
execS :: Statement -> Memory -> Memory
execS s store = execState ( evalS' s ) store 

-- runs expression and return result --
runS :: Statement -> Memory -> ((), Memory)
runS st store = runIdentity $ runStateT ( evalS' st ) store 


--------------------------------
-- Basic Expression Evaluator --
--------------------------------


evalE' :: Expression -> Store Value
evalE' ( Val v ) = return v
evalE' ( Var k ) = do
	s <- get
	return $ fromJust . M.lookup k $ s
evalE' ( Op bop e1 e2 ) = do
	x <- evalE' e1
	y <- evalE' e2
	return . fromJust $ ops bop x y


-------------------------------
-- Basic Statement Evaluator --
--------------------------------


evalS' :: Statement -> Store ()

evalS' Skip = return ()

evalS' (Assign k e) = do
	m <- get
	v <- evalE' e
	put ( M.insert k v m )
	return ()

evalS' (If e s1 s2) = do
	v <- evalE' e
	case v of 
		(BoolVal True)  -> evalS' s1
		(BoolVal False) -> evalS' s2
		_     			-> evalS' Skip
	return ()

evalS' x@(While e s) = do
	v <- evalE' e
	case v of 
		(BoolVal True)  -> evalS' s >> evalS' x
		(BoolVal False) -> return ()
		_     			-> evalS' Skip

evalS' (Sequence s1 s2) = evalS' s1 >> evalS' s2




-}



-- provided tests --

{-

run :: Statement -> IO ()
run stmt = do putStrLn "Output Store:" 
              putStrLn $ show $ execS stmt M.empty


w_test :: Statement
w_test = (Sequence (Assign "X" (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) (Sequence (Assign "Y" (Val (IntVal 0))) (While (Op Gt (Var "X") (Val (IntVal 0))) (Sequence (Assign "Y" (Op Plus (Var "Y") (Var "X"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1))))))))


w_fact :: Statement
w_fact = (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1))))))))))


t4a :: Test 
t4a = execS w_test M.empty ~?= 
        M.fromList [("X",IntVal 0),("Y",IntVal 10)]
t4b :: Test
t4b = execS w_fact M.empty ~?=
        M.fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]


-}







