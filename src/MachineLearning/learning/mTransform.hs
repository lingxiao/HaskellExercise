{- 

Learn Monad Transform
Created: March 22nd 2013 

http://www.cs.virginia.edu/~wh5a/personal/Transformers.pdf
http://blog.sigfpe.com/2006/05/grok-haskell-monad-transformers.html

-}


import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Numeric.LinearAlgebra as NL


import Data.Maybe
import Data.Either
import Data.List
import Data.Map ( Map, (!) )
import qualified Data.Map as M



getPassword :: IO (Maybe String)
getPassword = do 
	s <- getLine
	if isValid s then return $ Just s else return Nothing


-- The validation test could be anything we want it to be.
isValid :: String -> Bool
isValid s = length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s
	where
		isAlpha a = True
		isNumber a = True
		isPunctuation a = True


-- | single layer IO monad
askPassword :: IO ()
askPassword = do 
	putStrLn "Insert your new password:"
	maybe_value <- getPassword
	if isJust maybe_value 
		then do putStrLn "Storing in database..."
		else do putStrLn "no"
	return ()


-- | Use monad transform to layer monads
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) } 

instance Monad m => Monad (MaybeT m) where
    return  = MaybeT . return . Just
    x >>= f = MaybeT $ ( runMaybeT x ) >>= \maybe_v -> case maybe_v of 
    		Nothing -> return Nothing
    		Just v  -> runMaybeT $ f v

{-
	>>= :: Monad m => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
	runMaybeT :: MaybeT [] Int -> [ Maybe Int ]
-}


compute :: Int -> MaybeT [] Int
compute x = if x > 5 then MaybeT $ [ Just $ x-2, Just $ x+2 ] else MaybeT $ [ Nothing ]


ret = runMaybeT $ compute 1


-- | rewrite IO by stacking maybe monad ontop of it
getValidPassword :: MaybeT IO String
getValidPassword = do 
	s <- lift getLine
	guard (isValid s) -- MonadPlus provides guard.
	return s

askPassword' :: MaybeT IO ()
askPassword' = do 
	lift $ putStrLn "Insert your new password:"
	value <- getValidPassword
	lift $ putStrLn "Storing in database..."




-- for convinience
instance Monad m => MonadPlus (MaybeT m) where
    mzero     = MaybeT $ return Nothing
    mplus x y = MaybeT $ do mvalue <- runMaybeT x
                            case mvalue of
                                 Nothing    -> runMaybeT y
                                 Just _     -> return mvalue
 
instance MonadTrans MaybeT where
    lift = MaybeT . (liftM Just)




{-
 another tutorial
-}

-- | where i left off: Don't understand default impl of state monad
-- | need to write MORE stateful programms to grok this stuff

test0 :: StateT Int Identity ( (), Int )
test0 = do
	x <- get 
	put ( x + 1 )
	a <- get
	return ((),a)

test1 :: StateT Int Identity (Int, Int)
test1 = do
    a <- get
    modify (+1)
    b <- get
    return (a,b)

test2 :: StateT String Identity (String, String)
test2 = do
    a <- get
    modify (++"1")
    b <- get
    return (a,b)

test3 :: StateT Int (StateT String Identity) (Int, String)
test3 = do 
	modify (+1)
	lift $ modify ( ++ "1" )
	a <- get
	b <- lift get
	return ( a, b )

test5 :: StateT Int IO ()
test5 = do
	modify (+ 1)
	a <- get
	lift (print a)
	modify (+ 1)
	b <- get
	lift (print b)


test6 :: StateT Int IO ( (), Int )
test6 = do
	modify (+ 1)
	a <- get
	lift (print a)
	modify (+ 1)
	b <- get
	lift (print b)
	return ( (), b )

go0 :: ( (), Int )
go0 = evalState test0 11

go1 :: ( Int, Int )
go1 = evalState test1 0

go2 :: ( String, String )
go2 = evalState test2 "0" 

go3 :: ( Int, String )
go3 = runIdentity $ evalStateT (evalStateT test3 0) "0"

go5 :: IO ()
go5 = evalStateT test5 0

go6 :: IO( (), Int )
go6 = evalStateT test6 3



------------------------------------------------------------------
------------------------ Recreate Finger  ------------------------
------------------------------------------------------------------

-- Could also declare a type that is made of stateT and Identity monadT composed together
type Grok1 returnType = StateT Int Identity returnType

-- | a function that unwraps the monadT and run the encapsulated function
runGrok1 :: Grok1 a -> Int -> a
runGrok1 m a = runIdentity $ evalStateT m a



-- | this function here should take a param and return a Grok1 monad 
-- | encapsulating a change of state action
gtick :: Grok1 Int
gtick = do 
	modify (+20)
	x <- get
	return x


gincr :: Int -> Grok1 Int
gincr i = do
	modify ( +i )
	x <- get
	return x


-- use in example
g1 :: Int
g1 = runGrok1 gtick 1
g2 :: Int
g2 = runGrok1 ( gincr 2 ) 1

-- now wrap grok1 into a reader monad


-- | First create surrounding type
type Surr = M.Map Char Integer
surr = M.fromList [('x',100),('y',2)]
type Wrap a = ReaderT Surr ( StateT Int ( StateT Int Identity ) ) a


tock :: (Num s, MonadState s m) => m ()
tock = do modify (+1)


runWrap :: Surr -> Int -> Int -> Wrap a -> a
runWrap surr st1 st2 m = runIdentity $ evalStateT ( evalStateT ( runReaderT m surr ) st2 ) st1


aWrap :: Wrap ( Int, Int )
aWrap = do
	lift tock
	lift . lift $ tock
	x <- get
	y <- lift . lift $ get
	return ( x, y )


-- | Note since wrap is instance of monadState, lifting is not need for tocking outer state
-- | same is true of get
aWrap' :: Wrap ( Int, Int )
aWrap' = tock 
	>>= \_ -> ( lift . lift $ tock ) 
	>>= \_ -> get 
	>>= \x -> lift . lift $ get 
	>>= \y -> return ( x, y )

------------------------------------------------------------------
------------------------ Finger Exercises ------------------------
------------------------------------------------------------------



{-
	step 1. grok 1-tiered monad transform
-}

grok1 :: StateT Int Identity ( Int, Int )
grok1 = do
	a <- get 
	put ( a + 1 )
	b <- get
	return ( a, b )


grok2 :: StateT String Identity ( String, String )
grok2 = do
	a <- get
	modify ( ++ " world" )
	b <- get
	return ( a, b )



grok1' :: StateT Int Identity ( Int, Int )
grok1' = get >>= \a -> modify (+10) >>= \_ -> get >>= \b -> return (a, b)


grok1t :: ( Int, Int )
grok1t = evalState grok1 11

grok2t :: ( String, String )
grok2t = evalState grok2 "hello"



{-
	step 2. grok 2-tiered monads built by monad-transform
-}

groktier2 :: StateT Int (StateT String Identity) (Int, Int, String, String)
groktier2 = do 
	a <- get
	b <- lift get
	modify (+1)
	lift $ modify ( ++ " world" )
	a' <- get
	b' <- lift get
	return ( a, a', b, b' )


groktier2e :: StateT Int (StateT String Identity) (Int, Int, String, String)
groktier2e = 
	get >>= \a -> 
	lift get >>= \b -> 
	modify (+1) >>= \_ -> 
	(lift $ modify ( ++ "world" )) >>= \_ ->
	get >>= \a' -> 
	lift get >>= \b' ->
	return ( a, a', b, b' )




groktier2t :: ( Int, Int, String, String )
groktier2t = runIdentity $ evalStateT (evalStateT groktier2 0) "hello"

-- | Note here we use evalState, not evalStateT 
go7 :: ( Int, Int, String, String )
go7 = evalState (evalStateT groktier2 0) "0"



{-
	step 2a. grok 3-tiered to be safe
-}

groktier3 :: StateT Int ( StateT String ( StateT String Identity ) ) ( Int, String, String )
groktier3 = do
	modify (+10)
	lift $ modify ( ++ " world" )
	lift . lift $ modify ( ++ " word" )
	a <- get
	b <- lift get
	c <- lift . lift $ get
	return ( a, b, c )


groktier3t :: ( Int, String, String )
groktier3t = runIdentity $ evalStateT ( evalStateT ( evalStateT groktier3 1 ) "hello" ) "harro"


-- | desugared version
groktier3' :: StateT Int ( StateT String ( StateT String Identity ) ) ( Int, String, String )
groktier3' = modify (+10) >>= \_ ->
         (lift $ modify ( ++ " world" )) >>= \_ ->
         (lift . lift $ modify ( ++ " word" )) >>= \_ ->
         get >>= \a ->
         lift get >>= \b -> 
         (lift . lift $ get) >>= \c ->
         return (a,b,c)



{-
	step 3a. stack IO and state monads
-}

--state wraps
grokIOst1 :: StateT Int IO ((), Int)
grokIOst1 = do
	modify ( + 12 )
	a <- get
	return ((), a )


grokIOst1t :: IO ( (), Int )
grokIOst1t = evalStateT grokIOst1 1


grokIOst2 :: StateT Int IO ()
grokIOst2 = do
	modify ( +12 )
	a <- get
	lift $ print a


grokIOst2t :: IO()
grokIOst2t = evalStateT grokIOst2 12



---------------------------------------------------------------------
------------------------ Interpretor Example ------------------------
---------------------------------------------------------------------

{-
	A larger example
	q to be asked: how to incorporate error, maybe, logger monad w/ state monad tranform StateT
-}

type Name = String 
data Exp = Lit Integer | Var Name | Plus Exp Exp | Abs Name Exp | App Exp Exp deriving (Show)
data Value = IntVal Integer | FunVal Env Name Exp deriving (Show)
type Env = M.Map Name Value


-- | Case 0. simple functions
eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust $ M.lookup n env 
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1 in 
					     let IntVal i2 = eval0 env e2 in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1 in let val2 = eval0 env e2 in case val1 of
						FunVal env' n body -> eval0 (M.insert n val2 env') body


-- quick test
e = M.fromList [("x",IntVal 1),("y",IntVal 2)] :: M.Map Name Value
x = Var "x"
y = Var "y"
s1 = eval0 e ( Lit 1 )
s2 = eval0 e x
s3 = eval0 e ( Plus x y )
s4 = eval0 e ( Abs "x" x )



-- | create synonynm for Identitity monad and synonym for runIdenitity
-- | Idenitity monad
type Eval1 a = Identity a 

-- | Note since Identitiy is the minimum context monad, there's no context param passed into function
runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev


-- | Case 1. Wrap eval in trivial monad
-- | for trivial case, wrap function in idenitity monad, giving fn minimum context
-- | If we define generic monad in fn signature, which ghci interpret as IO monad
-- | Note there's no way to handle errors
--eval1 :: Monad m => Env -> Exp -> m Value
eval1 :: Env -> Exp -> Eval1 Value
eval1 env ( Lit a ) = return $ IntVal a
eval1 env ( Var a ) = return $ fromJust $ M.lookup a env
eval1 env ( Plus e1 e2 ) = do
	IntVal v1 <- eval1 env e1
	IntVal v2 <- eval1 env e2
	return $ IntVal $ v1 + v2
eval1 env ( Abs n e ) = return $ FunVal env n e
eval1 env ( App e1 e2 ) = do
	v1 <- eval1 env e1
	v2 <- eval1 env e2
	case v1 of FunVal env' a expr -> eval1 ( M.insert a v2 env' ) expr



-- | Case 2. wrap eval in trivial monad, then ErrorT monad transform
-- | stack error monad ontop of identity monad by using ErrorT transform Monad
type Eval2 a = ErrorT String Identity a

-- | Note since errorT provides a context but do not require param, no additional param passed in
-- | since the inner layer is the identity monad, need to call runIdenitity to get value
runEval2 :: Eval2 a -> Either String a
runEval2 a = runIdentity $ runErrorT a

-- | modify Identity monad with error monad
eval2 :: Env -> Exp -> Eval2 Value
eval2 env ( Lit a ) = return $ IntVal a
-- | Note here we look for variable directly in the map which
-- | is passed in as a parameter
eval2 env ( Var a ) = case M.lookup a env of
	Nothing -> throwError $ "unbound var " ++ a
	Just v  -> return v
eval2 env ( Plus e1 e2 ) = do
	m1 <- eval2 env e1
	m2 <- eval2 env e2
	case ( m1, m2 ) of 
		( IntVal v1, IntVal v2 ) -> return $ IntVal $ v1 + v2
		_ 						 -> throwError "type error"
eval2 env ( Abs n e ) = return $ FunVal env n e
-- | Note here we pass enviornment as a fn param, 
-- | so we can store variable into environment directly, then evaluate it
eval2 env ( App e1 e2 ) = do
	v1 <- eval2 env e1
	v2 <- eval2 env e2
	case v1 of 
		FunVal env' a expr -> eval2 ( M.insert a v2 env' ) expr
		_  				   -> throwError "type error"



-- | Case 3. wrap fn in Identity, ErrorT, and then ReaderT Monad
-- | Here we move the env variable from eval3 to runEval3 instead
-- | hide environment from all functions using ReaderT monad transformer
-- | Note unlike StateT monad, fn in ReaderT cannot modifiy environment
type Eval3 a = ReaderT Env (ErrorT String Identity) a

-- | note we runReader first, then runError, then runIdentity
runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity . runErrorT $ runReaderT ev env


-- | Note these functions now have pure signature
eval3 :: Exp -> Eval3 Value 
eval3 ( Lit a ) = return $ IntVal a
-- | Note we ask for environment var from ReaderT monad
eval3 ( Var a ) = do 
	env <- ask
	case M.lookup a env of
		Nothing -> throwError $ "unbound var " ++ a
		Just v  -> return v
eval3 ( Plus e1 e2 ) = do
	m1 <- eval3 e1
	m2 <- eval3 e2
	case ( m1, m2 ) of 
		( IntVal v1, IntVal v2 ) -> return $ IntVal $ v1 + v2
		_ 						 -> throwError "type error"
eval3 ( Abs n e ) = do 
	env <- ask
	return $ FunVal env n e
-- | here we need to store variable in local environment first, then evaluatee it
-- | Note the environment is *not* modified here
eval3 ( App e1 e2 ) = do
	v1 <- eval3 e1
	v2 <- eval3 e2
	case v1 of 
		FunVal env' a expr -> local ( const $ M.insert a v2 env' ) $ eval3 expr
		_  				   -> throwError "type error"


-- | Adding state so that the function can change the environment
-- | Note we add one more param of type Int for state monadT
-- | Add writer monad transform as well
type Eval4 a = ReaderT Env ( ErrorT String ( WriterT [String] (StateT Int Identity) ) ) a

runEval4 :: Env -> Int -> Eval4 a -> ((Either String a, [String]),Int )
runEval4 env counter expr = runIdentity $ runStateT (runWriterT $ runErrorT $ runReaderT expr env) counter


-- | we want to keep track of number of times eval4 is called 
-- | Do not want to hard wire into eval4 below or even tigh the tick fn to Eval4 monad
-- | Thus we give it generic signature
tick :: (Num s, MonadState s m) => m ()
tick = do modify (+1)


-- | we want these functions to keep track of number of calls to the eval4 function
eval4 :: Exp -> Eval4 Value

eval4 ( Lit a ) = tick >>= \_ -> return $ IntVal a 
eval4 ( Var a ) = do
	tick
	tell [a]
	env <- ask
	case M.lookup a env of 
		Nothing -> throwError $ "unbound var " ++ a
		Just v  -> return v 
eval4 ( Plus e1 e2 ) = do
	tick
	m1 <- eval4 e1
	m2 <- eval4 e2
	case ( m1, m2 ) of 
		( IntVal v1, IntVal v2 ) -> return $ IntVal $ v1 + v2
		_ 						 -> throwError "type error"
eval4 ( Abs n e ) = do 
	tick
	env <- ask
	return $ FunVal env n e
-- | here we need to store variable in local environment first, then evaluatee it
-- | Note the environment is *not* modified here
eval4 ( App e1 e2 ) = do
	tick
	v1 <- eval4 e1
	v2 <- eval4 e2
	case v1 of 
		FunVal env' a expr -> local ( const $ M.insert a v2 env' ) $ eval4 expr
		_  				   -> throwError "type error"




-- | Finally, add I/O

-- | Just replace Idenitity with IO
type Eval5 a = ReaderT Env ( ErrorT String ( WriterT [String] (StateT Int IO) ) ) a

-- | Note IO wraps entire return signature
runEval5 :: Env -> Int -> Eval5 a -> IO((Either String a, [String]),Int )
runEval5 env counter m = runStateT (runWriterT $ runErrorT $ runReaderT m env) counter


-- | Note use of liftIO as a shortCut
eval5 :: Exp -> Eval5 Value
eval5 ( Lit a ) = do
	liftIO $ print a
	tick
	return $ IntVal a
eval5 ( Var a ) = do
	tick
	tell [a]
	env <- ask
	case M.lookup a env of 
		Nothing -> throwError $ "unbound var " ++ a
		Just v  -> return v 
eval5 ( Plus e1 e2 ) = do
	tick
	m1 <- eval5 e1
	m2 <- eval5 e2
	case ( m1, m2 ) of 
		( IntVal v1, IntVal v2 ) -> return $ IntVal $ v1 + v2
		_ 						 -> throwError "type error"
eval5 ( Abs n e ) = do 
	tick
	env <- ask
	return $ FunVal env n e
-- | here we need to store variable in local environment first, then evaluatee it
-- | Note the environment is *not* modified here
eval5 ( App e1 e2 ) = do
	tick
	v1 <- eval5 e1
	v2 <- eval5 e2
	case v1 of 
		FunVal env' a expr -> local ( const $ M.insert a v2 env' ) $ eval5 expr
		_  				   -> throwError "type error"




--------------------------------------------------------------------
------------------------ Learn reader monad ------------------------
--------------------------------------------------------------------


-- | mock reader monad, note two functions modifiy the same param locally, then operation is done
-- | on modified param
addStuff :: Int -> Int
addStuff = do
	a <- (*2)
	b <- (+5)
	return ( a+b )


-- | Reader Monad, put value in context, then doing some operation on it
fndLen :: Reader String Int
fndLen = ask >>= \content -> return $ length content

-- | can modify the param locally, and then do operation on that
fndModLen :: Reader String Int
fndModLen = local ( "pre" ++ ) fndLen


testReader :: Int
testReader = runReader fndLen "fives"

testModReader :: Int
testModReader = runReader fndModLen "fives"

-- | wrap reader aroudn IO
printReaderContent :: ReaderT String IO()
printReaderContent = do
	content <- ask
	lift $ putStrLn content

-- | de-sugared 
printReaderContent' :: ReaderT String IO()
printReaderContent' = ask >>= \content -> lift $ putStrLn content


-- | wrap state around reader
printContent :: StateT String ( ReaderT String Identity ) ( String, String )
printContent = do
	content <- lift ask
	stateV  <- get
	return ( stateV ++ " step", content )

printContentT :: ( String, String )
printContentT = runIdentity $ runReaderT ( evalStateT printContent "hello" ) "reader content"




---------------------------------------------------------------------------------
------------------------ First time using Monad Morphism ------------------------
---------------------------------------------------------------------------------


-- | where we are going to restart:

--finish doing stateT, wrap it in IO so can log the algo process
--consider breaking IO piece into its own thing for reuse -- how?

--type Heap e ret = StateT (NodeMap e) IO ret
--runHeap :: Heap e ret -> NodeMap e -> (ret, NodeMap e)
--runHeap m a = runStateT m a 
--evalHeap :: Heap e ret -> NodeMap e  -> ret
--evalHeap m a = evalStateT m a

-- Bayes Net --

-- | in the interest of self documenting code
type Prob  = Double
type PaIdx = Int
type NumPa = Int

data Node e = Node { pa :: [e], ch :: [e], probs :: [ Vector Prob ] } deriving ( Show, Eq )
type NodeMap e = Map e ( Node e )

data BayesNet e = BayesNet { ns :: [e], nmap :: NodeMap e } deriving ( Show )




-- Put net in MT stack --

type Heap e ret = StateT (NodeMap e) Identity ret



runHeap :: Heap e ret -> NodeMap e -> (ret, NodeMap e)
runHeap m a = runIdentity $ runStateT m a 

evalHeap :: Heap e ret -> NodeMap e  -> ret
evalHeap m a = runIdentity $ evalStateT m a

takeNode :: Char -> Heap Char ( Maybe ( Node Char) )
takeNode c = do
	m <- get
	let n = M.lookup c m 
	modify ( M.delete c )
	return n 

insertNode :: Char -> Node Char -> Heap Char ()
insertNode c n = do
	m <- get
	modify ( M.insert c n )
	return ()

heapOp :: Heap Char ( Maybe ( Node Char ) )
heapOp = do
	m <- get
	insertNode 'z' ( Node [] [] [] )
	return $ M.lookup 'a' m




-- Elegantly handle errors using monad morphisms --

type MHeap e ret = MaybeT ( StateT [e] Identity ) ret


runheap :: MHeap e ret -> [e] -> ( Maybe ret, [e] )
runheap m l = runIdentity $ runStateT ( runMaybeT m ) l



fndAtIdx :: Int -> [a] -> Maybe a
fndAtIdx i xs = if length xs > i then Just $ xs !! i else Nothing


hoistMaybe :: (Monad m) => Maybe a -> MaybeT m a
hoistMaybe ma = MaybeT (return ma)

mheapOp :: Int -> MHeap Int Int
mheapOp i = do 
	xs <- lift $ get
	a  <- hoistMaybe (fndAtIdx i xs)
	lift $ modify (++ [a])
	return a


mheapOp' :: Int -> MHeap Int Int
mheapOp' i = 
	(MaybeT $ liftM Just get)               
    >>= \xs -> (MaybeT $ return $ fndAtIdx i xs)      
    >>= \a  -> (MaybeT $ liftM Just $ modify (++ [a])) 
    >>= \_  ->  MaybeT $ return $ Just a


-- Now refactor mheapOp to call generic monadstate actions ---

-- | try calling non-context specific monads within monadT stack
trial :: Ord k => k -> MaybeT (StateT (Map k v) Identity) v
trial k = do
	nmap <- lift $ get
	n    <- hoistMaybe $ M.lookup k nmap 
	lift $ delValue k
	return n


delValue :: Ord k => k -> State ( Map k v ) ()
delValue k = do 
	modify ( M.delete k )
	return ()


insertValue :: Ord k => k -> v -> State ( Map k v ) ()
insertValue k v = do
	modify ( M.insert k v )
	return ()


-- Using a different monadT stack and same monad morphism --

type Stack env st r = MaybeT ( ReaderT env ( StateT st Identity ) ) r


runStack :: Stack env st r -> st -> env -> ( Maybe r, st )
runStack m st env = runIdentity $ runStateT ( runReaderT ( runMaybeT m ) env ) st


-- | w/o propagation of error
tryS :: Stack [Int] [Int] Int
tryS = do
	env <- lift $ ask
	let a = head env
	lift . lift $ modify ( ++ [a])
	return a


-- | use function that returns Nothing on out of bound error
tryMS :: Stack [Int] [Int] ()
tryMS = 
	-- | instead of using lift $ ask, which return ReaderT env ( StateT st Identity ) r
	-- | must rebuild entire monad stack with MaybeT on outside, and the inner layers wrapping maybe value.
	-- | note: ( liftM Just ask )  :: m ( Maybe a ), MaybeT :: m (Maybe a) -> MaybeT m a
	( MaybeT $ liftM Just ask )
	-- | monadT layers are unwrapped, revealing env, here 'cast' head env to Maybe Int and 
	-- | wrap the whole thing in MaybeT
	>>= \env -> let ma = MaybeT $ return ( fndAtIdx 0 env ) in ma 
	-- | again, we modify underlying state first, which return mondstate
	-- | wrap it in Maybe using the liftM function, finally wrapped it in MaybeT wrapper 
	>>= \a   -> ( MaybeT $ liftM Just $ modify ( ++ [a] ) )
	>>= \_   -> return ()


-- | desugared version 
tryMS' :: Stack [Int] [Int] ()
tryMS' = do
	env <- lift $ ask
	a   <- hoistMaybe ( fndAtIdx 0 env )
	lift . lift $ modify ( ++ [] )
	return ()


{- 

Monad Morphism question from SO

import Control.Monad
import Control.Error
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Functor.Identity

-- (State s) is the exact same thing as (StateT s Identity):
-- type State s = StateT s Identity
type MHeap e r = MaybeT (State [e]) r

mheapOp :: Int -> MHeap Int Int

mheapOp i = do 
    xs <- lift get
    a <- hoistMaybe (atMay xs i)
    lift $ modify (++ [a])
    return a

-- Inline 'return' and 'lift' for 'MaybeT', and also inline 'hoistMaybe'
mheapOp i = do 
    xs <- MaybeT $ liftM Just get
    a <- MaybeT $ return $ atMay xs i
    MaybeT $ liftM Just $ modify (++ [a])
    MaybeT $ return $ Just a

-- Desugar 'do' notation
mheapOp i =
    (MaybeT $ liftM Just get)               >>= \xs ->
     (MaybeT $ return $ atMay xs i)          >>= \a ->
      (MaybeT $ liftM Just $ modify (++ [a])) >>= \_ ->
       (MaybeT $ return $ Just a)

-- Inline first '(>>=)' (which uses 'MaybeT' monad)
mheapOp i =
    MaybeT $ do
        mxs <- runMaybeT (MaybeT $ liftM Just get)
        case mxs of
            Nothing -> return Nothing
            Just xs -> runMaybeT $
                (MaybeT $ return $ atMay xs i)          >>= \a ->
                 (MaybeT $ liftM Just $ modify (++ [a])) >>= \_ ->
                  (MaybeT $ return $ Just a)

-- runMaybeT (MaybeT x) = x
mheapOp i =
    MaybeT $ do
        mxs <- liftM Just get
        case mxs of
            Nothing -> return Nothing
            Just xs -> runMaybeT $
                (MaybeT $ return $ atMay xs i)          >>= \a ->
                 (MaybeT $ liftM Just $ modify (++ [a])) >>= \_ ->
                  (MaybeT $ return $ Just a)

-- Inline definition of 'liftM'
mheapOp i =
    MaybeT $ do
        mxs <- do xs' <- get
                return (Just xs')
        case mxs of
            Nothing -> return Nothing
            Just xs -> runMaybeT $
                (MaybeT $ return $ atMay xs i)          >>= \a ->
                 (MaybeT $ liftM Just $ modify (++ [a])) >>= \_ ->
                  (MaybeT $ return $ Just a)

{- Use third monad law (a.k.a. the "associativity law") to inline the inner do
   block -}
mheapOp i =
    MaybeT $ do
        xs  <- get
        mxs <- return (Just xs)
        case mxs of
            Nothing -> return Nothing
            Just xs -> runMaybeT $
                (MaybeT $ return $ atMay xs i)          >>= \a ->
                 (MaybeT $ liftM Just $ modify (++ [a])) >>= \_ ->
                  (MaybeT $ return $ Just a)

{- Use first monad law (a.k.a. the "left identity law"), which says that:

   x <- return y

   ... is the same thing as:

   let x = y
-}
mheapOp i =
    MaybeT $ do
        xs' <- get
        let mxs = Just xs'
        case mxs of
            Nothing -> return Nothing
            Just xs -> runMaybeT $
                (MaybeT $ return $ atMay xs i)          >>= \a ->
                 (MaybeT $ liftM Just $ modify (++ [a])) >>= \_ ->
                  (MaybeT $ return $ Just a)

-- Inline definition of 'mxs'
mheapOp i =
    MaybeT $ do
        xs' <- get
        case (Just xs') of
            Nothing -> return Nothing
            Just xs -> runMaybeT $
                (MaybeT $ return $ atMay xs i)          >>= \a ->
                 (MaybeT $ liftM Just $ modify (++ [a])) >>= \_ ->
                  (MaybeT $ return $ Just a)

{- The 'case' statement takes the second branch, binding xs' to xs.

   However, I choose to rename xs' to xs for convenience, rather than rename xs
   to xs'. -}
mheapOp i =
    MaybeT $ do
        xs <- get
        runMaybeT $ (MaybeT $ return $ atMay xs i)          >>= \a ->
                     (MaybeT $ liftM Just $ modify (++ [a])) >>= \_ ->
                      (MaybeT $ return $ Just a)

-- Inline the next '(>>=)'
mheapOp i =
    MaybeT $ do
        xs <- get
        runMaybeT $ MaybeT $ do
            ma <- runMaybeT $ MaybeT $ return $ atMay xs i
            case ma of
                Nothing -> return Nothing
                Just a -> runMaybeT $
                    (MaybeT $ liftM Just $ modify (++ [a])) >>= \_ ->
                     (MaybeT $ return $ Just a)

-- runMaybeT (MaybeT x) = x
mheapOp i =
    MaybeT $ do
        xs <- get
        do ma <- return $ atMay xs i
           case ma of
               Nothing -> return Nothing
               Just a -> runMaybeT $
                   (MaybeT $ liftM Just $ modify (++ [a])) >>= \_ ->
                    (MaybeT $ return $ Just a)

-- You can inline the inner 'do' block because it desugars to the same thing
mheapOp i =
    MaybeT $ do
        xs <- get
        ma <- return $ atMay xs i
        case ma of
            Nothing -> return Nothing
            Just a -> runMaybeT $
                (MaybeT $ liftM Just $ modify (++ [a])) >>= \_ ->
                 (MaybeT $ return $ Just a)

-- Use first monad law
mheapOp i =
    MaybeT $ do
        xs <- get
        let ma = atMay xs i
        case ma of
            Nothing -> return Nothing
            Just a -> runMaybeT $
                (MaybeT $ liftM Just $ modify (++ [a])) >>= \_ ->
                 (MaybeT $ return $ Just a)

-- Inline definition of 'ma'
mheapOp i =
    MaybeT $ do
        xs <- get
        case (atMay xs i) of
            Nothing -> return Nothing
            Just a -> runMaybeT $
                (MaybeT $ liftM Just $ modify (++ [a])) >>= \_ ->
                 (MaybeT $ return $ Just a)

-- Inline the next '(>>=)'
mheapOp i =
    MaybeT $ do
        xs <- get
        case (atMay xs i) of
            Nothing -> return Nothing
            Just a -> runMaybeT $ MaybeT $ do
                mv <- runMaybeT $ MaybeT $ liftM Just $ modify (++ [a])
                case mv of
                    Nothing -> return Nothing
                    Just _  -> runMaybeT $ MaybeT $ return $ Just a

-- runMaybeT (MaybeT x) = x
mheapOp i =
    MaybeT $ do
        xs <- get
        case (atMay xs i) of
            Nothing -> return Nothing
            Just a -> do
                mv <- liftM Just $ modify (++ [a])
                case mv of
                    Nothing -> return Nothing
                    Just _  -> return (Just a)

-- Inline definition of 'liftM'
mheapOp i =
    MaybeT $ do
        xs <- get
        case (atMay xs i) of
            Nothing -> return Nothing
            Just a -> do
                mv <- do x <- modify (++ [a])
                         return (Just x)
                case mv of
                    Nothing -> return Nothing
                    Just _  -> return (Just a)

-- Inline inner 'do' block using third monad law
mheapOp i =
    MaybeT $ do
        xs <- get
        case (atMay xs i) of
            Nothing -> return Nothing
            Just a -> do
                x  <- modify (++ [a])
                mv <- return (Just x)
                case mv of
                    Nothing -> return Nothing
                    Just _  -> return (Just a)

-- Use first monad law to turn 'return' into 'let'
mheapOp i =
    MaybeT $ do
        xs <- get
        case (atMay xs i) of
            Nothing -> return Nothing
            Just a -> do
                x  <- modify (++ [a])
                let mv = Just x
                case mv of
                    Nothing -> return Nothing
                    Just _  -> return (Just a)

-- Inline definition of 'mv'
mheapOp i =
    MaybeT $ do
        xs <- get
        case (atMay xs i) of
            Nothing -> return Nothing
            Just a -> do
                x  <- modify (++ [a])
                case (Just x) of
                    Nothing -> return Nothing
                    Just _  -> return (Just a)

-- case takes the 'Just' branch, binding 'x' to '_', which goes unused
mheapOp i =
    MaybeT $ do
        xs <- get
        case (atMay xs i) of
            Nothing -> return Nothing
            Just a -> do
                modify (++ [a])
                return (Just a)

{- At this point we've completely inlined the outer 'MaybeT' monad, converting
   it to a 'StateT' monad internally.  Before I inline the 'StateT' monad, I
   want to point out that if 'atMay' returns 'Nothing', the computation short
   circuits and doesn't call 'modify'.

   The next step is to inline the definitions of 'return, 'get', and 'modify':
-}
mheapOp i =
    MaybeT $ do
        xs <- StateT (\as -> return (as, as))
        case (atMay xs i) of
            Nothing -> StateT (\as -> return (Nothing, as))
            Just a -> do
                StateT (\as -> return ((), as ++ [a]))
                StateT (\as -> return (Just a , as))

-- Now desugar both 'do' blocks:
mheapOp i =
    MaybeT $
        StateT (\as -> return (as, as)) >>= \xs ->
         case (atMay xs i) of
             Nothing -> StateT (\as -> return (Nothing, as))
             Just a ->
                 StateT (\as -> return ((), as ++ [a])) >>= \_ ->
                  StateT (\as -> return (Just a , as))

-- Inline first '(>>=)', which uses 'StateT' monad instance
mheapOp i =
    MaybeT $ StateT $ \as0 -> do
        (xs, as1) <- runStateT (StateT (\as -> return (as, as))) as0
        runStateT (case (atMay xs i) of
            Nothing -> StateT (\as -> return (Nothing, as))
            Just a ->
                StateT (\as -> return ((), as ++ [a])) >>= \_ ->
                 StateT (\as -> return (Just a , as)) ) as1
                     --                                 ^
                     -- Play close attention to this s1 |

-- runStateT (StateT x) = x
mheapOp i =
    MaybeT $ StateT $ \as0 -> do
        (xs, as1) <- (\as -> return (as, as)) as0
        runStateT (case (atMay xs i) of
            Nothing -> StateT (\as -> return (Nothing, as))
            Just a ->
                StateT (\as -> return ((), as ++ [a])) >>= \_ ->
                 StateT (\as -> return (Just a , as)) ) as1

-- Apply (\as -> ...) to as0, binding 'as0' to 'as'
mheapOp i =
    MaybeT $ StateT $ \as0 -> do
        (xs, as1) <- return (as0, as0)
        runStateT (case (atMay xs i) of
            Nothing -> StateT (\as -> return (Nothing, as))
            Just a ->
                StateT (\as -> return ((), as ++ [a])) >>= \_ ->
                 StateT (\as -> return (Just a , as)) ) as1

-- Use first monad law to convert 'return' to 'let'
mheapOp i =
    MaybeT $ StateT $ \as0 -> do
        let (xs, as1) = (as0, as0)
        runStateT (case (atMay xs i) of
            Nothing -> StateT (\as -> return (Nothing, as))
            Just a ->
                StateT (\as -> return ((), as ++ [a])) >>= \_ ->
                 StateT (\as -> return (Just a , as)) ) as1

{- The let binding says that xs = as0 and as1 = as0, so I will rename all of
   them to 'xs' since they are all equal -}
mheapOp i =
    MaybeT $ StateT $ \xs -> do
        runStateT (case (atMay xs i) of
            Nothing -> StateT (\as -> return (Nothing, as))
            Just a ->
                StateT (\as -> return ((), as ++ [a])) >>= \_ ->
                 StateT (\as -> return (Just a , as)) ) xs

-- do m = m, so we can just get rid of the 'do'
mheapOp i =
    MaybeT $ StateT $ \xs ->
        runStateT (case (atMay xs i) of
            Nothing -> StateT (\as -> return (Nothing, as))
            Just a ->
                StateT (\as -> return ((), as ++ [a])) >>= \_ ->
                 StateT (\as -> return (Just a , as)) ) xs

-- Distribute the 'runStateT ... xs' over both 'case' branches
mheapOp i =
    MaybeT $ StateT $ \xs ->
        case (atMay xs i) of
            Nothing -> runStateT (StateT (\as -> return (Nothing, as))) xs
            Just a -> runStateT (
                StateT (\as -> return ((), as ++ [a])) >>= \_ ->
                 StateT (\as -> return (Just a , as)) ) xs

-- runStateT (StateT x) = x
mheapOp i =
    MaybeT $ StateT $ \xs ->
        case (atMay xs i) of
            Nothing -> (\as -> return (Nothing, as)) xs
            Just a -> runStateT (
                StateT (\as -> return ((), as ++ [a])) >>= \_ ->
                 StateT (\as -> return (Just a , as)) ) xs

-- Apply (\as -> ...) to 'xs', binding 'xs' to 'as'
mheapOp i =
    MaybeT $ StateT $ \xs ->
        case (atMay xs i) of
            Nothing -> return (Nothing, xs)
            Just a -> runStateT (
                StateT (\as -> return ((), as ++ [a])) >>= \_ ->
                 StateT (\as -> return (Just a , as)) ) xs

-- Inline the '(>>=)'
mheapOp i =
    MaybeT $ StateT $ \xs ->
        case (atMay xs i) of
            Nothing -> return (Nothing, xs)
            Just a -> runStateT (StateT $ \as0 -> do
                (_, as1) <- runStateT (StateT (\as -> return ((), as ++ [a]))) as0
                runStateT (StateT (\as -> return (Just a , as))) as1 ) xs

-- runStateT (StateT x) = x
mheapOp i =
    MaybeT $ StateT $ \xs ->
        case (atMay xs i) of
            Nothing -> return (Nothing, xs)
            Just a -> (\as0 -> do
                (_, as1) <- (\as -> return ((), as ++ [a])) as0
                (\as -> return (Just a , as)) as1 ) xs

-- Apply all the functions to their arguments
mheapOp i =
    MaybeT $ StateT $ \xs ->
        case (atMay xs i) of
            Nothing -> return (Nothing, xs)
            Just a -> (\as0 -> do
                (_, as1) <- return ((), as0 ++ [a])
                return (Just a , as1) ) xs

-- Use first monad law to convert 'return' to 'let'
mheapOp i =
    MaybeT $ StateT $ \xs ->
        case (atMay xs i) of
            Nothing -> return (Nothing, xs)
            Just a -> (\as0 -> do
                let (_, as1) = ((), as0 ++ [a])
                return (Just a , as1) ) xs

-- Let binding says that as1 = as0 ++ [a], so we can inline its definition
mheapOp i =
    MaybeT $ StateT $ \xs ->
        case (atMay xs i) of
            Nothing -> return (Nothing, xs)
            Just a  -> (\as0 -> do
                return (Just a , as0 ++ [a]) ) xs

-- do m = m
mheapOp i =
    MaybeT $ StateT $ \xs ->
        case (atMay xs i) of
            Nothing -> return (Nothing, xs)
            Just a  -> (\as0 -> return (Just a , as0 ++ [a])) xs

-- Apply (\as0 -> ...) to 'xs', binding 'xs' to 'as0'
mheapOp i =
    MaybeT $ StateT $ \xs ->
        case (atMay xs i) of
            Nothing -> return (Nothing, xs)
            Just a  -> return (Just a , xs ++ [a])

-- Factor out the 'return' from the 'case' branches, and tidy up the code
mheapOp i =
    MaybeT $ StateT $ \xs ->
        return $ case (atMay xs i) of
            Nothing -> (Nothing, xs)
            Just a  -> (Just a , xs ++ [a])

-- One last step: that last 'return' is for the 'Identity' monad, defined as:
{-

mheapOp i =
    MaybeT $ StateT $ \xs ->
        Identity $ case (atMay xs i) of
            Nothing -> (Nothing, xs)
            Just a  -> (Just a , xs ++ [a])
-}

{- So now we can clearly say what the function does:

   * It takes an initial state named 'xs'

   * It calls 'atMay xs i' to try to find the 'i'th value of 'xs'

   * If 'atMay' returns 'Nothing, then our stateful function returns 'Nothing'
     and our original state, 'xs'

   * If 'atMay' return 'Just a', then our stateful function returns 'Just a'
     and a new state whose value is 'xs ++ [a]'

   Let's also walk through the types of each layer:

   layer1 :: [a] -> Identity (Maybe a, [a])
   layer1 = \xs ->
       Identity $ case (atMay xs i) of
           Nothing -> (Nothing, xs)
           Just a  -> (Just a, xs ++ [a])

   layer2 :: StateT [a] Identity (Maybe a)
   --   i.e. State  [a] (Maybe a)
   layer2 = StateT layer1

   layer3 :: MaybeT (State [a]) a
   layer3 = MaybeT layer2
-}
-}

