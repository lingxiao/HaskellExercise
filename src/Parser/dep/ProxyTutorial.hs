---------------------------------------------------------------------------------------------------
-- | Pipes package tutorial
-- | Tutorials: http://hackage.haskell.org/packages/archive/pipes/3.3.0/doc/html/Control-Proxy-Tutorial.html#g:6
-- | Proxy transformer: http://hackage.haskell.org/packages/archive/pipes/2.5.0/doc/html/Control-Proxy-Trans-Tutorial.html
-- | StateP transformer: http://hackage.haskell.org/packages/archive/pipes/3.0.0/doc/html/Control-Proxy-Trans-State.html
---------------------------------------------------------------------------------------------------


import Control.Monad
import Control.Monad.Trans
import Control.Proxy
import Control.Proxy.Trans.Writer
import Control.Proxy.Trans.State

import qualified Data.Map as M

----------------
-- Data Types --
----------------

{-

    | Sends    | Receives | Receives   | Sends      | Base  | Return
    | Upstream | Upstream | Downstream | Downstream | Monad | Value
proxy  a'         a          b'           b            m       r


-- specific instances of proxies 


    | Question | Answer | Base monad | Return value
Server   Int        Int        IO           r


    | Question | Answer | Base monad | Return value
Client   Int      | Int    | IO         | ()


-- Ways to interact w/ proxy

request :: (Monad m, Proxy p) => a' -> p a' a b' b m a
                              ^                   ^
                              |                   |
                   Argument --+          Result --+


respond :: (Monad m, Proxy p) => b -> p a' a b' b m b'
                              ^                  ^
                              |                  |
                     Result --+  Next Argument --+


-}

------------------------------
-- Components from Tutorial --
------------------------------

-- | A consumer
-- | note consumers request information from client
-- | the request have a parameter that could be ()
printer :: (Proxy p, Show a) => () -> Consumer p a IO r
printer () = runIdentityP $ forever $ do
	a <- request ()
	lift $ putStrLn "receive value: " 
	lift $ print a

-- | Another consumer
take' :: (Proxy p) => () -> Pipe p a a IO ()
take' () = runIdentityP $ do
 replicateM_ 3 $ do
     a <- request ()
     respond a
 lift $ putStrLn "You shall not pass!"


-- | A producer
-- | Note producer respond to request from client
prompt :: (Proxy p) => () -> Producer p Int IO r
prompt () = runIdentityP $ forever $ do
	lift $ putStrLn "Enter an Int:"
	n <- lift readLn 
	respond n

-- | Another Producer
-- | Note fromListS is a short cut function to make a list into a producer
stuffList :: (Monad m, Proxy p) => () -> Producer p Int m ()
stuffList = fromListS [1..10]



-- | Client: generalized consumers
-- | Note client can supply arbitrary arg other than ()
-- | Note Int sends ints, and receive bools
client :: (Proxy p) => () -> Client p Int Bool IO ()
client () = runIdentityP $ forM_ [1,3,1] $ \arg -> do
	lift . putStrLn $ "client needs: " ++ show (arg :: Int)
	result <- request arg
	lift . putStrLn $ "client receive: " ++ show (result :: Bool)
	lift . putStrLn $ "*"


-- | Server: generic producer
-- | Note server produce receive ints and produce bools
server :: (Proxy p) => Int -> Server p Int Bool IO r
server = runIdentityK loop where
	loop arg = do 
		lift . putStrLn $ "server receive: " ++ show (arg :: Int)
		let result = arg > 2
		lift . putStrLn $ "server sends: " ++ show (result :: Bool)
		nextArg <- respond result
		loop nextArg


-- | Add a cache between server and client
cache :: (Proxy p, Ord key) => key -> p key val key val IO r
cache = runIdentityK (loop M.empty) where
	loop vcache key = case M.lookup key vcache of
		Nothing -> do
			val  <- request key
			key2 <- respond val
			loop (M.insert key val vcache) key2
		Just val -> do
				lift $ putStrLn "Used cache for: "
				key2 <- respond val
				loop vcache key2


----------------------------
-- sessions from tutorial --
----------------------------

session :: (Proxy p) => () -> Session p IO r
session = prompt >-> printer 


session1 :: () -> ProxyFast C () () C IO () 
session1 = prompt >-> take' >-> printer 

-- | runProxy session2
session2 :: (Proxy p) => () -> Session p IO ()
session2 = server >-> client

session3 :: (Proxy p) => () -> Session p IO ()
session3 = server >-> cache >-> client

------------------------
-- Mock FSA Pipe Unit --
------------------------

-- | Step one: create a StateP proxy stack - embedding local state into proxy computation

-- | Note StateP's type signature, the second param is a proxy, not a monad: 
-- | StateP :: (s -> p (a', s) (a, s) (b', s) (b, s) m (r, s)) -> StateP s p a' a b' b m r
-- | Thus unlike stateT which transforms monad to monad, stateP transforms proxy to proxy
-- | Note the parens (..) functions as a proxy 
-- | Some details: StateP guarentee transformed proxy stack implement channel interface
-- | Some details: Note no need to runIdenitityP since StateP stack is a proxy already

-- | A Producer
--incr :: (Monad m, Proxy p) => () -> Producer (StateP Int p) Int m r
incr :: (Proxy p) => () -> (StateP Int p) C () () Int IO r
incr () = forever $ do 
	m <- get
	lift . print $ "stateP get value: "
	lift . print $ m
	
	modify (+1)
	
	n <- get
	lift . print $ "the value is modified: "
	lift . print $ n
	lift . print $ "************"
	
	respond n 

-- | A Pipe
-- | Note both signatures are correct
incr1 :: Proxy p => () -> Pipe (StateP Int p) Int Int IO r
--incr1 :: Proxy p => () -> (StateP Int p) () Int () Int IO r
incr1 () = forever $ do
	i <- request ()
	lift . print $ "incr1 get value from upstream: "
	lift . print $ i
	lift . print $ "************"
	
	modify (+i)
	
	n <- get
	respond n


-- | Step two: figure out how to use it
execSesh :: Monad m => (() -> StateP s ProxyFast a' () () b m r1) -> s -> m s
execSesh sesh n = runProxy $ execStateK n $ sesh

runSesh :: Monad m =>  (() -> StateP s ProxyFast a' () () b m r) -> s -> m (r, s)
runSesh sesh n = runProxy $ runStateK n $ sesh


--incrSession :: Proxy p => () -> StateP Int p C () () C IO ()
incrSesh1 :: Proxy p => () -> Session (StateP Int p) IO ()
incrSesh1 = incr >-> client2

incrSesh2 :: Proxy p => () -> Session (StateP Int p) IO ()
incrSesh2 = incr >-> pipe1 >-> client2

incrSesh3 :: Proxy p => () -> Session (StateP Int p) IO ()
incrSesh3 = fromListS [1..3] >-> incr1 >-> pipe1 >-> client1



-------------------------------------------------
-- chain stateP stack of different state types --
-------------------------------------------------

-- | Note how they are now modifiying the same global repository

deltaStr :: (Monad m, Proxy p) => () -> Pipe (StateP (String, Int) p) String Int m r
deltaStr () = forever $ do
	s        <- request ()
	(str, i) <- get
	put (str ++ s, i)
	respond 945


deltaInt :: (Monad m, Proxy p) => () -> Pipe (StateP (String, Int) p) Int Int m r
deltaInt () = forever $ do
	int      <- request ()
	(str, i) <- get
	put (str, i + int)


-- | session | --
logger = client2
use    = runProxy $ runStateK ("hello", 12) $ fromListS ["hello"] >-> deltaStr >-> deltaInt >-> logger


--------------------
-- Branch Proxies --
--------------------

{-
	Consumer

	Producer 

	Producer

-}

fork :: (Monad m, Proxy p1, Proxy p2, Proxy p3) => () -> Consumer p1 a (Producer p2 a (Producer p3 a m)) r
fork () = runIdentityP . hoist (runIdentityP . hoist runIdentityP) $ forever $ do
	a <- request ()          -- Request output from the 'Consumer'
	lift $ respond a         -- Send output to the outer 'Producer'
	lift $ lift $ respond a  -- Send output to the inner 'Producer'


-- 1st application
p1 = runProxyK $ fork <-< fromListS [1..3]

-- 2nd application
p2 = runProxyK $ (hoist lift .) printD <-< mapD (> 2) <-< p1

-- 3rd application
p3 = runProxy  $ printD <-< mapD show <-< p2


-------------------
-- More Practice --
-------------------

-- | Producers | -- 

-- note the producer signature
server1 :: (Proxy p) => () -> Producer p Int IO r 
server1 () = runIdentityP . forever $ do 
	respond 404

-- note the generic proxy signature
server2 :: (Proxy p) => () -> p C () () Int IO ()
server2 () = runIdentityP $ do
	respond 202




-- | Pipes | --

pipe1 :: (Proxy p) => () -> p () Int () Int IO r
pipe1 () = runIdentityP . forever $ do 
	a <- request ()
	lift . print $ "pipe1 receive value and add 1000"
	lift . print $ a
	lift . print $ "************"
	respond $ a + 1000


-- | Consumers | --

client1 :: (Proxy p) => () -> p () Int () C IO r
client1 () = runIdentityP . forever $ do
	a <- request ()
	lift . print $ "client1 received value: "
	lift . print $ a 
	lift . print $ "******************"



client2 :: (Proxy p) => () -> p () Int () C IO ()
client2 () = runIdentityP $ do
	a <- request ()
	lift . print $ "client2 received value: "
	lift . print $ a 
	lift . print $ "******************"



-- | Sessions | --

session4 :: (Proxy p) => () -> Session p IO r
session4 = server1 >-> printer


session5 :: (Proxy p) => () -> p C () () C IO ()
session5 = server2 >-> printer


session6 :: (Proxy p) => () -> p C () () C IO ()
session6 = server2 >-> client1


session7 :: (Proxy p) => () -> p C () () C IO ()
session7 = server2 >-> pipe1 >-> pipe1 >-> client1


-- | Things not explored | --

-- Composing producers and consumers 
-- Folds over a stream 







