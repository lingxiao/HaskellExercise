{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances #-}


------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- MonadT lecture - goes over the implementation details by manually 
-- implementing transformers 

-- Created: April 26th

-- Problems encountered:
-- Read passively, did not focus hard enough to map out relations between objects
-- drank too much coffee and can't focus

------------------------------------------------------------------------------
------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

import Test.HUnit hiding ( State )
import Test.QuickCheck hiding ( promote )
import Test.QuickCheck.Function


-------------------------
-- Syntax Constructors --
-------------------------


data Expr = Value Int | Div Expr Expr 
	deriving (Show)


-- Example Exprs --
ok  = Div (Div (Value 1972) (Value 2)) (Value 23) 		      :: Expr
err = Div (Value 2) (Div (Value 1) (Div (Value 2) (Value 3))) :: Expr



----------------------------------
-- Single purpose monads: Maybe --
----------------------------------


-- evalMaybe takes type Expr and sends to type Maybe Int --
evalMaybe ::  Expr -> Maybe Int
evalMaybe (Value n) = Just n
evalMaybe (Div x y) = do
  y0 <- evalMaybe y
  guard (y0 /= 0)
  x0 <- evalMaybe x
  return (x0 `div` y0)


----------------------------------
-- Single purpose monads: Error --
----------------------------------

-- Erreption data type
data Err a = Raise String | Result a
       deriving (Show)


-- Make Err type a monad
instance Monad Err where
	return = Result
	-- Err a -> (a -> Err b) -> Err b
	ma >>= f = case ma of
		Raise s  -> Raise s
		Result x -> f x


-- evalErr takes type Expr and sends to type Err Int
evalErr ::  Expr -> Err Int
evalErr (Value n) = return n
evalErr (Div x y) =	do 
	n <- evalErr x
	m <- evalErr y
	if m == 0 then Raise $ errorS y m else Result $ n `div` m


errorS y m = "Error dividing " ++ show y ++ " by " ++ show m


----------------------------------
-- Single purpose monads: State --
----------------------------------

-- define profile
type Profile = Int


tickProfile :: State Profile ()
tickProfile = modify (+1) 

--evalST :: Expr -> StateT Profile Identity Int
evalST :: Expr -> State Profile Int 
evalST ( Value n ) = return n
evalST ( Div x y ) = do
	n <- evalST x
	m <- evalST y
	tickProfile
	return $ div n m


goST :: Expr -> IO ()
goST e = putStrLn $ "value: " ++ show x ++ ", count: " ++ show s
           where (x,s) = runState ( evalST e ) 0 :: (Int, Profile) 


------------------------------------
-- Build a moand w/ both features -- 
------------------------------------


-- defined MonadErr monad type class
-- So every monad that is an instance of this class can throwErreption
class Monad m => MonadErr m where
	throwErr :: String -> m a

-- trivially, we make Err an instance of MonadErr class
instance MonadErr Err where 
	throwErr = Raise


-- rewrite evalErr with monad interface
-- note m is an instance of MonadErr
evalErrM :: ( MonadErr m ) => Expr -> m Int
evalErrM (Value n) = return n
evalErrM (Div x y) = do 
	n <- evalErrM x
	m <- evalErrM y
	if m == 0 
		then throwErr $ errorS y m
		else return $ n `div` m


-- build generic monads --

-- generic tick function for any monad that is a MonadState
tick :: (MonadState Profile m) => m ()
tick = modify (+1)

-- note since evalMega throw Erro and ticks, it's both a MonadState and MonadErr
-- note we have not a way to run this since the runEvalMega expression needs 
-- a signature denothing how stateT wraps monadErr or vice versa
evalMega :: ( MonadState Profile m, MonadErr m ) => Expr -> m Int
evalMega (Value n) = return n
evalMega (Div x y) = do 
	n <- evalMega x
	m <- evalMega y
	tick
	if m == 0 
		then throwErr $ errorS y m 
		else return $ n `div` m


-- this does not work for some reason
type Mega a = StateT Int ( Err ) a 




------------------------------------
-- Add features to exiting Monads --
------------------------------------


-- the monad transformer class
class Transformer t where
	promote :: Monad m => m a -> ( t m ) a


------ ErrT monad transformer ------

-- defin ErrT, a Err transformer type
-- Just wrap Err monad around value a, and wrap both in m
-- finally wrap monad in MkErr constructor
data ErrT m a = MkErr (m ( Err a ) )


-- this function undos the embedding function above
unMkErr :: ErrT m a -> m ( Err a )
unMkErr ( MkErr ma ) = ma

-- make Err a monad transfomrer
instance Transformer ErrT where
  -- promote :: Monad m => m a -> (ErrT m) a
  promote = MkErr . ( liftM return )


-- show transformoer output is a monad
instance Monad m => Monad (ErrT m) where
	return x = promote $ return x 
	(MkErr p) >>= g = MkErr $ do
		x <- p
		case x of 
			Raise s  -> return $ Raise s
			Result y -> unMkErr $ g y

 -- show transforemer is an exception monad, which is sublcass of monad
instance (Monad m) => MonadErr ( ErrT m ) where
	throwErr s = MkErr ( return ( Raise s ) )



---------------------------
-- Use standard libarary --
---------------------------

type Store1 = StateT Int ( Either String ) Int 

-- build a function that runs in generic monad subclasses monadError and monadState
eval1 :: (MonadError String m, MonadState Int m) =>  Expr -> m Int            
eval1 (Value n) = return n
eval1 (Div x y) = do 
	n   <- eval1 x
	m   <- eval1 y
	if m == 0 then throwError $ errorS y m else do tick
	return  $ n `div` m


-- A monad where either is run first, then state
--type SE = StateT Int ( Either String ) Int

evalSE :: Expr -> StateT Int ( Either String ) Int
evalSE = eval1

runEvalSE s st = runStateT (eval1 s) st

-- run state first, then either
evalES :: Expr -> ErrorT String ( State Int ) Int
evalES = eval1


runEvalES s st = runState (runErrorT (evalES s)) st


-- Add writer to eval -- 

eval2 :: (MonadError String m, MonadState Int m, MonadWriter String m) => Expr -> m Int            
eval2 ( Value n ) = tell ( msg (Value n) n ) >> return n
eval2 (Div x y)   = do 
	n <- eval2 x
	m <- eval2 y
	if m == 0 then throwError $ errorS y m else do tick 
	tell $ msg (Div x y) (n `div` m)
	return  $ n `div` m



msg :: (Show a, Show b) => a -> b -> String
msg t r = "term: " ++ show t ++ ", yields " ++ show r ++ "\n"


-- error wraps log, so if computation fails log gets thrown away
type WSE a = WriterT String (StateT Int (Either String)) a 

evalWSE :: Expr -> WSE Int
evalWSE = eval2



-- log wraps error
type ESW a = ErrorT String (WriterT String (State Int)) a        

-- make the console output pretty
instance Show a => Show (ESW a) where 
  show m = "Log:\n"  ++ log ++ "\n" ++ 
           "Count: " ++ show cnt ++ "\n" ++
           result
    where ((res, log), cnt) = runState (runWriterT (runErrorT m)) 0
          result   = case res of 
                       Left s -> "Error: " ++ s
                       Right v -> "Value: " ++ show v


evalESW :: Expr -> ESW Int
evalESW = eval2 

runESW s = runState ( runWriterT ( runErrorT ( eval2 s ) ) ) 0






---------------------------------------
-- Pratice composing lots of monadTs --
---------------------------------------


{-
	All possible combo of these monadTs: ErrorT, MaybeT, StateT, ReaderT, WriterT
-}

----------------------------
-- Atomic Monad functions --
----------------------------

type Counter = Int
type Msg     = String


-- Note all these functions sends a haskell type to a monad, these functions can run
-- in any monad that is an instance of the class defined in the predicate

-- State
incr :: (MonadState Counter m) => Counter -> m ()
incr i = modify (+i)

addMsg :: ( MonadState Msg m ) => Msg -> m()
addMsg msg = modify ( ++ msg )

-- Error
errorm :: ( MonadError String m ) => Bool ->  m Int
errorm cond = if cond then return 1 else throwError "this is an error"

errorms :: ( MonadError String m ) => Bool ->  m String
errorms cond = if cond then return "works" else throwError "does not work"


--Reader
readerm :: (MonadReader String m ) => m String
readerm = ask >>= \x -> return x

-- Writer
writerm :: ( MonadWriter Msg m ) => Msg -> m ()
writerm msg = tell msg


-- Maybe -> no MonadMaybe class?
maybem :: Bool -> Maybe Bool
maybem cond = if cond then Just True else Nothing


-----------------------------------
-- Compose above monad functions --
-----------------------------------

---- call statemonad function twince on state monad ----

incr2 :: ( MonadState Counter m ) => m ()
incr2 = incr 1 >> incr 10

incr2t = runState incr2 12 :: ((),Int)

addmsg2 :: ( MonadState Msg m ) => m ()
addmsg2 = addMsg "hello" >> addMsg " world"


addmsg2t = runState addmsg2 "yoyo "  :: ( (), String )


---- StateT and StateT ----

-- stack two monadState together
-- Note State String is on the bottome or inside, while int is on the outside
-- Thus string has to be lifted into the current layer before it could be used
incrMsg:: (MonadTrans t, MonadState Msg m, MonadState Counter (t m)) => t m ()
incrMsg = do 
	lift . addMsg $ "one"
	incr 1
	return ()


incrMsgt = runIdentity $ runStateT ( runStateT incrMsg 0 ) "zero" :: (((), Int), String)


-- note StateT Int in on the inside or bottom, and StateT string is on the outside
-- Also note no matter how many times you 'lift', you always return to the outer most layer
incrMsg1:: (MonadTrans t, MonadState Msg (t m), MonadState Counter m) => t m ()
incrMsg1 = do 
	lift . incr $ 1
	addMsg " one"
	lift . incr $ 10
	addMsg " two"
	return ()

incrMsgt1 = runIdentity $ runStateT ( runStateT incrMsg1 "zero" ) 0 :: (((), String), Int) 


---- ErrorT and ErrorT, same type ----

errErr :: MonadError String m => Bool -> Bool -> m Int
errErr b1 b2 = do
	a <- errorm b1
	b <- errorm b2
	return $ a + b 


errErrt0 = runErrorT ( errErr False True ) :: Monad m => m (Either String Int)
errErrt1 = runErrorT ( errErr True True )  :: Monad m => m (Either String Int)


---- ErrorT and ErrorT, different right type ----

-- note since the left type are the same, this is actually not a stack monad
errErr1 :: MonadError String m => Bool -> Bool -> m (Int, String)
errErr1 b1 b2 = do
	a <- errorm b1 
	b <- errorms b2
	return (a,b)

errErr1t = runIdentity . runErrorT $ errErr1 True True :: Either [Char] (Int, String)

----- StateT and ErrorT ----

-- Note the order in which we call state function vs errorm function does not matter
-- since the function works for all of subclasses defined in the predicate
incrErr :: ( MonadState Counter m, MonadError String m ) => Bool -> m ()
incrErr cond = do
	incr 22
	a <- errorm cond
	incr a 

type SE a = StateT Int ( ErrorT String Identity ) a 

incrErrSE :: Bool -> (Either [Char] (), Int)
incrErrSE b = runIdentity $ runStateT ( runErrorT ( incrErr b ) ) 0

-- | stack error ontop of state
-- | Note the output type is of form: monad wraps result
incrErrSE0 = runIdentity $ runStateT ( runErrorT ( incrErr False ) ) 0 :: (Either String (), Int)
incrErrSE1 = runIdentity $ runStateT ( runErrorT ( incrErr True ) ) 0  :: (Either String (), Int)

-- | stack state ontop of error
-- | Note output type is of form: monad wraps Either String monad, wrapping result
incrErrES0 = runIdentity $ runErrorT ( runStateT ( incrErr False ) 0 ) :: Either String ((), Int)
incrErrES1 = runIdentity $ runErrorT ( runStateT ( incrErr True ) 0 )  :: Either String ((), Int)


---- reader and writer ----
readWrite :: ( MonadReader String m, MonadWriter Msg m ) => m ()
readWrite = do
	x <- readerm
	writerm $ x ++ " from inside the monad"
	return ()

-- Writer inside, reader outside
rdWrite1 = runIdentity $ runWriterT ( runReaderT readWrite "hello" ) :: ( (), String )
-- Reader inside, writer outside
rdWrite2 = runIdentity $ runReaderT ( runWriterT readWrite ) "hello" :: ( (), String )


---- reader and State ----

readState :: ( MonadState Msg m, MonadReader String m ) => m ()
readState = do
	x <- readerm
	let x' = x ++ " in monad" 
	modify (++ x')

-- | state on inside
readSt1 = runIdentity $ runStateT ( runReaderT readState "hello" ) ""     :: ( (), String )
-- | reader on inside
readSt2 = runIdentity $ runReaderT ( runStateT readState "yo" ) "hello"   :: ( (), String )


---- reader and Error ----

errorb :: ( MonadError String m ) => String ->  m String
errorb cond = case cond of 
	"true" -> return "works"
	_      -> throwError "doesn't work"


readerErr :: ( MonadReader String m, MonadError String m ) => m String
readerErr = readerm >>= \b -> errorb b


readerErrER0 = runIdentity $ runReaderT ( runErrorT readerErr ) "false" :: Either String String
readerErrER1 = runIdentity $ runReaderT ( runErrorT readerErr ) "true"  :: Either String String

readerErrRE0 = runIdentity $ runErrorT ( runReaderT readerErr "true" )  :: Either String String
readerErrRE1 = runIdentity $ runErrorT ( runReaderT readerErr "false" ) :: Either String String


---- State, Error, Reader ----
rdErrSt :: ( MonadReader String m, MonadError String m, MonadState Msg m ) => m ()
rdErrSt = do
	b <- readerm
	addMsg "msg before error; "
	case b of 
		"true" -> addMsg $ b ++ " inside monad; "
		_      -> throwError "monad error"
	addMsg "msg after error" 
	return ()


-- Reader wraps Error, wraps state
rdErrStSER = runIdentity $ runStateT ( runErrorT $ runReaderT rdErrSt "true" ) "" :: (Either String (), String)


---- State, State, Error, Reader ----


-- | Note here the StateT String monad is on the inner layer, so it needs to be lifted to the current layer
rdErrSt2:: (MonadTrans t, MonadState Msg m, MonadState Counter (t m), MonadReader String (t m), MonadError String (t m)) => t m ()
rdErrSt2 = do 
	b <- readerm
	incr 1
	lift . addMsg $ b ++ "; "
	case b of 
		"true" -> let a = lift . addMsg $ b ++ " inside monad; " in incr $ 12
		_      -> throwError "monad error"
	incr 100
	return ()


--rdErrSt3 = do 
--	b <- readerm
--	lift $ incr 1
--	addMsg $ b ++ "; "
--	case b of 
--		"true" -> let a = addMsg $ b ++ " inside monad; " in lift . incr $ 12
--		_      -> throwError "monad error"
--	lift . incr $ 100
--	return ()


tSSRE :: String -> Either String (((), Int), String)
tSSRE b = runIdentity . runErrorT $ runReaderT ( runStateT ( runStateT rdErrSt2 0 ) "in ")  b

tSSER :: String -> Either String (((), Int), String)
tSSER b = runIdentity $ runReaderT ( runErrorT ( runStateT ( runStateT rdErrSt2 0 ) "in ")) b 









-------------------------------------------------------------
-- Pratice Running generic MonadT in arbitrary Monad Stack --
-------------------------------------------------------------

-- Data types --

type Memory  = Map String Int 
type ErrMsg  = String
type Log     = String

mem0 = M.fromList [("x", 1)] :: Memory

-- Utility functions --

record :: (MonadWriter Log m) => Log -> m ()
record msg = tell $ msg ++ "; "


------ Example One ----

-- Note due to the presence of two `lift`, there are two instances of monadtransformers
mStack :: ( 
	MonadTrans t1,
	MonadTrans t2,
	Monad (t2 m),
	MonadState Memory m,
	MonadState Counter (t1 (t2 m)),
	MonadError String (t1 (t2 m)),
	MonadWriter Log (t1 (t2 m))
	) => Bool -> t1 (t2 m) Int
mStack b = do
	tick
	m <- lift . lift $ get
	record "accesed memory"
	let x = fromJust ( M.lookup "x" m )
	case b of 
		True  -> return 100
		False -> throwError "false"


type ESSW a = ErrorT ErrMsg ( StateT Counter ( StateT Memory ( Writer Log ))) a

runESSW :: ESSW a -> ( ((Either ErrMsg a, Counter ), Memory ), Log )
runESSW g = runIdentity . runWriterT $ runStateT (runStateT (runErrorT g ) 0) mem0

type WSSE a = WriterT Log ( StateT Counter ( StateT Memory ( ErrorT ErrMsg Identity ))) a

runWSSE :: WSSE a -> Either ErrMsg ( ((a, Log), Counter), Memory)
runWSSE g = runIdentity . runErrorT $ runStateT (runStateT (runWriterT g ) 0) mem0


------ Example Two ----

type EWSS a = ErrorT ErrMsg ( WriterT Log ( StateT Counter ( State Memory ) ) ) a

runEWSS :: EWSS a -> (((Either ErrMsg a, Log), Counter), Memory)
runEWSS g = runIdentity $ runStateT ( runStateT ( runWriterT $ runErrorT g ) 0 ) mem0

-- | Note mStack1 has three monad transformers
-- | Also note we explicity define the type of monadT stack this function runs in
mStack1 :: Bool -> EWSS Int
mStack1 b = do
	tick
	m <- lift . lift . lift $ get
	record "accesed memory"
	let x = fromJust ( M.lookup "x" m )
	case b of 
		True  -> return 100
		False -> throwError "false"


mStack2 :: ( MonadState Counter m, MonadWriter Log m, MonadError ErrMsg m ) => Bool -> m Int
mStack2 b = do
	tick
	record "ticked"
	case b of 
		True  -> return 100
		False -> throwError "false"



type EWS a = ErrorT ErrMsg ( WriterT Log ( State Counter ) ) a

runEWS :: EWS a ->  ((Either ErrMsg a, Log), Counter)
runEWS g = runIdentity $ runStateT ( runWriterT $ runErrorT g ) 0







eval3 :: (MonadError String m, MonadState Int m, MonadWriter String m) => Expr -> m Int            

eval3 (Value n ) = do 
	tell $ msg (Value n) n
	return n

eval3 (Div x y) = do
	n <- eval3 x
	m <- eval3 y
	if m == 0 then throwError $ errorS y m else do 
			tick 
			tell $ msg (Div x y) (n `div` m)
			return  $ n `div` m



type WSE1 a = WriterT String (StateT Int (Either String)) a 

evalWSE1 :: Expr -> WSE1 Int
evalWSE1 = eval3





















