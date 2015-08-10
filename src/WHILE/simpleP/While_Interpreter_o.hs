{- 
	While Interpretor
	Created: April 21st

	lecture and hw source: http://www.seas.upenn.edu/~cis194/lectures.html
	http://www.cs.tufts.edu/~nr/cs257/archive/doaitse-swierstra/combinator-parsing-tutorial.pdf

	resources:
	state monad documentation: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2


-}

module While_Interpreter_o where


import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Char

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State 

import Test.HUnit hiding ( State )
import Test.QuickCheck
import Test.QuickCheck.Function


----------------
-- Data Types --
----------------

-- The WHILE language --

-- variable is just an alias for string
type Variable = String

-- a type wrapper around haskell primitives int and bool
data Value = IntVal Int | BoolVal Bool
	deriving ( Eq, Show )

-- Binary-operation
data Bop = Plus | Minus | Times | Divide | Gt | Ge | Lt | Le 
	deriving ( Eq, Show )

-- expressions of the While language
data Expression = 
	Var Variable 
	| Val Value 
	| Op Bop Expression Expression 
	deriving ( Eq, Show )


-- statements of the While language
data Statement = 
	Assign Variable Expression  			 -- x = e
	| If Expression Statement Statement      -- if (e) { s1 } else { s2 }
	| While Expression Statement             -- while (e){ s }
	| Sequence Statement Statement			 -- s1; s2
	| Skip									 -- no-op
	deriving ( Eq, Show ) 



-- The machine's 'memory' --

type Store = Map Variable Value


---------------------
-- WHILE Evaluator --
---------------------

-- translate Op to native operations -- 

bool :: Ord a => Bop -> a -> a -> Bool
bool (Gt) = (>)
bool (Lt) = (<)
bool (Ge) = (>=)
bool (Le) = (<=)


-- consider getting rid of all this boilerplate
ops :: Bop -> Value -> Value -> Value

ops _ (BoolVal _) (IntVal _ )  = IntVal 0
ops _ (IntVal _ ) (BoolVal _ ) = IntVal 0 

ops (Plus)   ( IntVal v1 ) ( IntVal v2 )   = IntVal $ (+) v1 v2
ops (Minus)  ( IntVal v1 ) ( IntVal v2 )   = IntVal $ (-) v1 v2
ops (Times)  ( IntVal v1 ) ( IntVal v2 )   = IntVal $ (*) v1 v2
ops (Divide) ( IntVal v1 ) ( IntVal v2 )   = IntVal $ div v1 v2
ops g        ( IntVal v1 ) ( IntVal v2 )   = BoolVal $ bool g v1 v2

ops (Plus)    ( BoolVal b1 ) ( BoolVal b2 ) = BoolVal True
ops (Minus)   ( BoolVal b1 ) ( BoolVal b2 ) = BoolVal True
ops (Times)   ( BoolVal b1 ) ( BoolVal b2 ) = BoolVal True
ops (Divide)  ( BoolVal b1 ) ( BoolVal b2 ) = BoolVal True
ops g         ( BoolVal b1 ) ( BoolVal b2 ) = BoolVal $ bool g b1 b2

--------------------------
-- Expression Evaluator --
--------------------------


evalE :: Expression -> State Store Value
evalE ( Val v ) = return v
evalE ( Var k ) = do
	s <- get
	return $ fromJust . M.lookup k $ s
evalE ( Op bop e1 e2 ) = do
	x <- evalE e1
	y <- evalE e2
	return $ ops bop x y


-------------------------
-- Statement Evaluator --
-------------------------

evalS :: Statement -> State Store ()

evalS Skip         = return ()

evalS (Assign k e) = do
	m <- get
	v <- evalE e
	put ( M.insert k v m )
	return ()

evalS (If e s1 s2) = do
	v <- evalE e
	case v of 
		(BoolVal True)  -> evalS s1
		(BoolVal False) -> evalS s2
		_     			-> evalS Skip
	return ()

evalS p@(While e s) = do
	v <- evalE e
	case v of 
		(BoolVal True)  -> evalS s >> evalS p
		(BoolVal False) -> return ()
		_     			-> evalS Skip

evalS (Sequence s1 s2) = evalS s1 >> evalS s2


-- runs the expression and mutate store --
execS :: Statement -> Store -> Store
execS s store = execState ( evalS s ) store 



-----------------------
-- Interpretor Tests --
-----------------------

-- mock store
store0 = M.empty :: Store
store1 = M.fromList [("x",IntVal 10),("y",IntVal 5)] :: Store 

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
s1 = Assign "z" ( Var "x" )
s2 = Assign "z" ( Val $ IntVal 1 )
s3 = Assign "z" ( Val $ BoolVal False )
s4 = Assign "z" e5
s5 = If e7 s1 ( Assign "SFIVE" ( Var "x" ))  -- false
s6 = If e8 ( Assign "SIX" ( Var "y" ) ) s2  -- true
s7 = Sequence s5 s6
s8 = Sequence (Sequence (Assign "X" ( Val $ IntVal 1 ) ) ( Assign "Y" ( Val $ BoolVal False)) ) ( Sequence (Assign "Z" ( Val $ IntVal 100 ) ) ( Assign "U" ( Val $ BoolVal True)) )
s9 = Sequence  (Assign "Z" ( Val $ IntVal 100 )) ( Sequence (Assign "X" ( Val $ IntVal 1 ) ) ( Assign "Y" ( Val $ BoolVal False)) )
s10 = Skip
s11 = While condy decry
	where 
		condy = Op Gt (Var "y") ( Val $ IntVal 0)
		decry = Assign "y" newy
		newy  = Op Minus ( Var "y" ) ( Val $ IntVal 1 )

-- infinite loop
s12 = While ( Val $ BoolVal True ) s1



-- provided tests --

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

















