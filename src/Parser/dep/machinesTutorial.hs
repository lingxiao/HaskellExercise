{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-} -- For :+: in signatures
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-} 



import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Cont

import Data.Monoid
import Data.Maybe
--import Data.Foldable
import Data.Functor.Extend
import Data.Copointed
import Data.Semigroup.Traversable
import Data.Semigroup.Foldable
import Data.Traversable
import Data.Machine
import Data.Machine.Mealy
import qualified Data.Sequence as S



import RoseTree

---------------------------------------------------------------------------------------------------
-- | Machines study plan

-- | understand some new ideas need to understand machines implementation
-- | rankN types
-- | CPS

-- | In this order, use all exported functions and create instances of data type if exported
-- | if datatype is instance of a typeclass, use all functions supported by typeclass

-- | plan.hs
-- | process.hs
-- | machines.hs
-- | Tee.hs 
-- | Wye.hs

-- | Note the stuff on github is out of date
---------------------------------------------------------------------------------------------------


---------------------------------------------------------------------------------------------------
-- | RankN types
-- | Source: http://stackoverflow.com/questions/12031878/what-is-the-purpose-of-rank2types
---------------------------------------------------------------------------------------------------

type ShowBox = forall b . ( forall a . (Show a, Enum a) => a -> b ) -> b


mkshowb :: (Show a, Enum a)=> a -> ShowBox
mkshowb x = \k -> k x


{- 

Expanded type:

 runShowBox :: 
 				forall b. (forall a. Show a => a -> b)  -> 
 				(forall b. (forall a. Show a => a -> b) -> b) -> 
 				b

-}
runShow :: forall b. (forall a . (Show a, Enum a) => a -> b ) -> ShowBox -> b
runShow g box = box g


 
-- | expanded signature e1 :: [forall b. (forall a. Show a => a -> b) -> b]
e1 :: [ShowBox]
e1 = [mkshowb 4, mkshowb 'b']


se1 :: [String]
se1 = fmap (runShow show) e1


se11 :: [String]
se11 = fmap (runShow $ show . succ) e1


---------------------------------------------------------------------------------------------------
-- | CPS
-- | Source: http://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
---------------------------------------------------------------------------------------------------

-- un-CPSed form

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)



---------------- Simple CPS ----------------


-- | CSP-ed form
-- | essentially, instead of returning a fully evaluated value of type x, 
-- | cpsed form return a call back of form (x -> r), where the call back that
-- | takes the value of type x as parameter and return value of type r

-- | this style reifies the action of one function surrendering control to the next function in chain

add' :: Int -> Int -> (Int -> r) -> r
add' x y g = g (add x y)

square' :: Int -> (Int -> r) -> r
square' x g = g . square $ x


pythagoras' :: Int -> Int -> (Int -> r) -> r
pythagoras' x y g = square' x $ \x2 -> square' y $ \y2 -> add' x2 y2 $ \sumed -> g sumed


---------------- CPSed higher order functions ----------------


-- write simple function thrice that applies function g three times
thrice :: (a -> a) -> a -> a
thrice g = g . g . g



-- in cps form -> note thrice' now takes in function g that is also in cps form
thrice' :: (o -> (o -> r) -> r) -> o -> (o -> r) -> r
thrice' g a h = g a $ \a1 -> g a1 $ \a2 -> g a2 $ \a3 -> h a3


-- | note an unary cpsed function is passed as first param
thricerun :: IO ()
thricerun = thrice' (add' 100) 2 print


---------------- Cont Monad ----------------

-- | since cps is a model of computation, we can express it as a monad which simplifies the code

-- | provided in monad.cont
-- | newtype Cont r a = Cont { runCont :: (a -> r) -> r }

-- | instance Monad (Cont r) where
--  	return n = Cont (\k -> k n)
--  	m >>= f  = Cont (\k -> runCont m (\a -> runCont (f a) k))

-- | Note, by calling runCont on a monad that outputs a and function of form (a->r) r is output

-- | rewrite above cps functions using cont monad 
addc :: Int -> Int -> Cont r Int
addc x y = return $ add x y

squarec :: Int -> Cont r Int
squarec x = return $ square x

pythagorasc :: Int -> Int -> Cont r Int
pythagorasc x y = do
	x2    <- squarec x
	y2    <- squarec y
	sumed <- addc x2 y2
	return sumed


pythrun :: String
pythrun = runCont (pythagorasc 2 4) show

pythrun2 :: Int
pythrun2 = runCont (pythagorasc 2 3) $ \x -> x + 100


gen :: Int
gen = runCont (return 3) $ \x -> x + 100


---------------- Call CC ----------------
-- call with current continuation

-- square
{-

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a

callCC takes a function of form ( g -> Cont r a), 
where g :: a -> Cont r b that takes a value of type a, and returns a cotinuation
then entire callCC then return result Cont r a


-}

squarecc :: Int -> Cont r Int
squarecc n = callCC $ \g -> g $ n^2


-- | when :: Monad m => Bool -> m () -> m ()
-- | when is like if that controls which branch executed conditonal on Boolean value passed in
foo :: Int -> Cont r String
foo n = callCC $ \g -> do
	let n' = n^2 + 3
	when (n'>20) $ g "over twenty"
	return . show $ n' - 4


-- | a more complex example of control flow using callCC
bar :: Char -> String -> Cont r Int
bar c s = do
	msg <- callCC $ \g -> do
		let s' = c : s
		when (s' == "hello") $ g "they said hello"
		return $ "which branch is this? " ++ (show s')
	return . length $ msg



-- | when g is applied to value, entire callCC call takes that value. execution stops at g function application
-- | baz pop out when g is called, return 10000 is never evaluated
baz :: Cont r Int
baz = callCC $ \g -> do
	let n = 5
	g n 
	return 100000

{-
	like imperitive code block:

	var c = a + b;
	if (c>12){
		callBack("c is greater than 12")
	} else {
		return "c is less than 12"
	}
	
	return ("final call: "++ (toString c))

-}
bak :: Int -> Int -> Cont r String
bak a b = do 
	int <- callCC $ \g -> do
		let c = a + b
		when (c > 12) $ g "c is greater than 12: "
		return $ "c is less than 12: " ++ (show c)
	return $ "final call " ++ int



---------------------------------------------------------------------------------------------------
-- I. Plan
-- from source: 
-- | A @'Plan' k o a@ is a specification for a pure 'Machine', that reads inputs selected by @k@
-- with types based on @i@, writes values of type @o@, and has intermediate results of type @a@.

-- A @'PlanT' k o a@ can be used as a @'PlanT' k o m a@ for any @'Monad' m@.

-- In an older version of machines:

-- A @'Plan' o m a@ is a specification for a pure 'Machine', that can perform actions in @m@, which
-- writes values of type @o@, and has intermediate results of type @a@.

-- plan (output type) (input type) (intermediate result type)
---------------------------------------------------------------------------------------------------

-- 1. construct simple plans from exported functions.

{-

	plan in un-cpsed form: 
	
	data 'Plan' k o a = 
		Done a
	  | Yield o (Plan k o a)
	  | forall z. Await (z -> Plan k o a) (k z) (Plan k o a)
	  | Fail

-}


-- | some simple plans that just output stuff
-- | Note func signature, yield :: o -> PlanT k o m ()

y1 :: PlanT k String m ()
y1 = yield "hello world"

-- run . construct $ y1  --> ["hello world"]

y2 :: PlanT k [Int] m ()
y2 = yield [1..10]  


-- run . construct $ y1 --> [[1..10]]



-- | some simple awaits
aw1 :: PlanT ((->) Int) o m Int
aw1 = awaits (\x -> x + x)

-- run . construct $ aw1  --> []

-- using type interface w/ aw1
aw1' = fmap (+100) aw1


-- | join two plans together


---------------------------------------------------------------------------------------------------
-- II. Create Various Tees
---------------------------------------------------------------------------------------------------




-- source -- 

-- | Source is type synonym for machine

streama :: Source Int
streama = source [1..10]

-- run streama   -> [1..10]

-------------- Use stock Processes --------------


-- processes --

-- | Process is type synonym for machine


-- | these are stock functios exported by Source.hs

notSix :: Process Int Int
notSix = filtered (/= 6)


dropSix :: Process Int Int
dropSix = dropping 6

takeSix :: Process Int Int
takeSix = taking 6

takeLessSix :: Process Int Int
takeLessSix = takingWhile $ \x -> x < 6

dropLessSix :: Process Int Int
dropLessSix = droppingWhile $ \x -> x < 6


-- compose machines using (~>), or its alias "cap" --

-- | cap a b is just b ~> a
-- | Note the signature of cap :: Monad m => Process a b -> Source a -> MachineT m k b
-- | Note the signature of ~>  :: Monad m => MachineT m k b -> ProcessT m b c -> MachineT m k c


-- | can build with other processes using same form
notsix_1 :: Monad m => MachineT m k Int
notsix_1 = cap notSix streama


-- run notsix_1   -> [1..5,7..10]



-------------- Roll my own tees --------------


{-

construct a bigger Tee machine:

tee :: Monad m => ProcessT m a a' -> ProcessT m b b' -> TeeT m a' b' c -> TeeT m a b c

which takes two processes (machines) and a tee, and consrut a TeeT that takes in types a and b, and returns c

-}

-- | Note how inputs work w/ any monad m, return any type c
inputs :: (Monad m) => TeeT m Int Char c -> TeeT m Int Char c 
inputs = tee (source [1..4]) (source "hello world")



-- Some TeeT's --


-- | Tee is type synonym for machine


-- | Note here we are using Tee, not Tee-Transformer
teeNaked :: Tee Int Char (Int, String)
teeNaked = construct (loop 0)
  where
    -- `loop` keeps internal state - the count of strings
    -- produced so far.
    loop count = do
        -- Read a number from L.
        n <- awaits L
        -- Read this many characters from L.
        s <- replicateM n (awaits R)
        let count' = count + 1
        -- Output the result.
        yield (count', s)
        -- note since count' changes, we have to explicity call loop again
        loop count'

-- | here we tee up inputs w/ tee1, note tee1 is a Tee, which is a synonym for MachineT m k o
-- | since tee1 is the monad, just call run on it
runTNaked :: [(Int, String)]
runTNaked = run $ inputs teeNaked


{-

	Note how awaits is used in here. Some signatures:

	awaits :: k i -> PlanT k o m i

	awaits L :: PlanT (T i b) o m i
	awiats R :: PlanT (T a i) o m i

-}

teeNaked2 :: Tee Int String Int
teeNaked2 = construct roll
	where roll = forever $ do 
		int <- awaits L
		str <- awaits R
		let n = int + (length str)
		yield n


runTNaked2 :: [Int]
runTNaked2 = run . tee (source [101..103]) (source ["bla", "blah", "blahblah"]) $ teeNaked2


-- | wrap Tee around a base monad, in this case state
-- | redo example above w/ Tee-transformer
-- | Note the type of repeatedly, which construct a "machine" from a "plan"
-- | repeatedly :: Monad m => PlanT k o m a -> MachineT m k o
teeState :: TeeT (State Int) Int Char (Int, String)
teeState = repeatedly $ do
		n <- awaits L
		s <- replicateM n (awaits R)
		lift . modify $ (+1)
		c <- lift get
		yield (c, s)



-- | Note how the m in TeeT m Int Char c is now (State Int) monad
-- | create a bigger machine w/ tee1'
-- | Note how the machine has the same type as tee1' since a = a' and b = b' wrt tee signature above
tee1Machine :: TeeT (State Int) Int Char (Int, String)
tee1Machine = inputs teeState


-- | evaluate the machine. Note how runT is used similar to monadTranformer idiom. 
-- | Observe partially evaluated fn signatures:

-- | runT :: Monad m => MachineT m k b -> m [b]
-- | runT tee1Machine :: State Int [(Int, String)]

-- | runT exposes the underlying monad. So after calling runT, a standard state monad is output. Just call standard run state functions
runTstate :: [(Int, String)]
runTstate = flip evalState 0 . runT $ inputs teeState 



-- TeeT a few other monads just to be sure --
teeWriter :: TeeT (Writer (Sum Int)) Int Char String
teeWriter = repeatedly $ do
	n <- awaits L
	s <- replicateM n (awaits R)
	tell (Sum 1)
	yield s


runTwriter :: ([String], Sum Int)
runTwriter = runWriter . runT $ inputs teeWriter



-------- TeeT a StateT (Writer) Monad stack ---------


teeBoth :: TeeT (StateT Int (State Int)) Int Int (Int,Int)
teeBoth = repeatedly $ do
	i <- awaits L
	j <- awaits R
	let k = i + j
	lift . modify $ (+k)
	lift . lift . modify $ (* k)
	yield (i,j)


inboth :: Monad m => TeeT m Int Int c -> TeeT m Int Int c 
inboth = tee (source [1..4]) (source [101..104])


-- | Again, note how runT peels back the Tee transformer, revealing StateT transformer stack
-- | runT teeBoth :: StateT Int (State Int) [(Int,Int)]

runTboth :: [(Int,Int)]
runTboth = runIdentity $ evalStateT (evalStateT (runT $ inboth teeBoth) 0) 100


-- | to make it easier, we can create a type of the stack and its own run. This is "Seen" stuff

type Stack1 = StateT Int (State String)


runStack1 :: Int -> String -> StateT Int (StateT String Identity) ret -> (Int, String)
runStack1 t1 t2 m = runIdentity . runStateT ( execStateT m t1 ) $ t2



teeBoth1 :: TeeT Stack1 Int String (Int,String)
teeBoth1 = repeatedly $ do 
	int <- awaits L
	str <- awaits R
	lift . modify $ (+int)
	lift . lift . modify $ (++str)
	yield (int,str)


runTBoth1 :: (Int,String)
runTBoth1 = runStack1 0 "empty: " $ runT $ ins teeBoth1
	where ins = tee (source [1..]) (source ["hello","world","stuff"])



-------- Chain multiple monadic proceses ---------


stateStr :: ProcessT (State String) String Int
stateStr = repeatedly $ do 
	a <- await
	lift . modify $ flip (++) a
	yield $ length a


stateStr2 :: ProcessT (State String) Int String
stateStr2 = repeatedly $ do 
	a <- await
	let a' = show a
	lift . modify $ flip (++) a'
	yield a'


stateInt :: ProcessT (State Int) Int Int
stateInt = repeatedly $ do
	a <- await
	b <- get
	let c = a * a + b
	put c 
	yield c



------ Cap off a Tee to make an I, or a process ----- 

-- | Compare Cap with Tee 
-- | cap :: Monad m => Process a b     -> Source a                          -> MachineT m k b
-- | tee :: Monad m => ProcessT m a a' -> ProcessT m b b' -> TeeT m a' b' c -> TeeT m a b c


--capState :: MachineT (State Int) k b
timesN :: Int -> Process Int Int  
timesN n = repeatedly $ do 
	x <- await
	yield $ x * n


rtimesN :: Process Int Int
rtimesN = cap (timesN 12) $ source [1..4]


divideN :: Int -> Process Int Int
divideN n = repeatedly $ do
	x <- await
	let y = div x n 
	if y == 1 then stop else yield y

------ Create a process w/ three machines -----

bothN :: Process Int Int 
bothN = (timesN 3) ~> (timesN 3) ~> (divideN 9)

rbothN :: Process Int Int
rbothN = cap bothN $ source [2..9]


------ Create a Tee that feed into a process of two machines -----


teeToNum :: TeeT (State Int) String String Int 
teeToNum = repeatedly $ do
	l <- awaits L
	r <- awaits R
	lift . modify $ (+) $ length l + length r
	n <- get
	yield n


-- | run it to check
numT :: TeeT (State Int) String String Int
numT = tee (source ["lorem", "blahblah"]) (source ["ipsum", "haha"]) teeToNum

--runNumT :: ([Int,Int], Int)
--runNumT = flip runState 0 . runT $ numT



---------------------------------------------------------------------------------------------------
-- II. Create a process that builds a RoseTree
---------------------------------------------------------------------------------------------------

growTree :: [a] -> RoseTree a -> RoseTree a
growTree es t = foldr rappend t $ pure <$> es


stProcess :: ProcessT (State (RoseTree Int)) Int Int
stProcess = repeatedly $ do
	x <- await
	lift . modify $ growTree [x]
	yield x


-- | note here we use generic (~>) instead of cap
try :: ProcessT (State (RoseTree Int)) Int Int
try = source [1..8] ~> stProcess


-- | build the tree
runTry :: RoseTree Int
runTry = flip execState Bud . runT $ try




mkTree :: TeeT (State (RoseTree Int)) Int Int (Int,Int)
mkTree = repeatedly $ do
	l <- awaits L
	r <- awaits R
	lift . modify $ growTree [l,r]
	yield (l,r)
		


mkATree :: TeeT (State (RoseTree Int)) Int Int (Int,Int)
mkATree = tee (source [1..4]) (source [11..]) mkTree


stepkTree :: RoseTree Int
stepkTree = flip execState Bud . runT $ mkATree


-- | turn a Tee into an I

mkTree' :: TeeT (State (RoseTree Int)) Int Int Int
mkTree' = repeatedly $ do
	l <- awaits L
	lift . modify $ growTree [l]
	yield l

mkATree' :: ProcessT (State (RoseTree Int)) Int Int
mkATree' = capR (source [1..6]) mkTree'


--stepkTree' :: RoseTree Int    
stepkTree' = flip runState Bud . runT $ mkATree'    -- ([], empty tree)




---------------------------------------------------------------------------------------------------
-- III. Explore typeclass interface 
---------------------------------------------------------------------------------------------------


-- * Instance Functor 

-- | recall source :: (Monad m, Foldable f) => f b -> MachinneT m k b
-- | or     source :: Foldable f => f a -> Source a a
sourcePlus :: [Int]
sourcePlus = run $ (+100) <$> source [1..4]   --- [101..104]


-- roll a machine, then fmap a func onto it
--aprocess :: Machine (Is Int) Int
aprocess :: Process Int Int
aprocess = repeatedly $ do
	x <- await
	yield $ x + x

mySource' :: Process Int Int
mySource' = source [1..4] ~> aprocess  

-- run mySource'   			    -> [2..8]
-- run $ (+100) <$> mySource'   -> [102..108]


-- * Instance Applicative 


-- <*> :: (Monad m, Appliance k) => MachineT m k (a -> b) -> MachineT m k a -> MachineT m k b
aFunction :: Process Int (Int -> Int)
aFunction = repeatedly $ do 
	x <- await
	yield $ (+) x 


bFunction :: Process (Int -> Int) Int
bFunction = repeatedly $ do 
	g <- await
	yield $ g 12


g :: Process Int Int 
g = source [1..3] ~> aFunction ~> bFunction 


---------------------------------------------------------------------------------------------------
-- IV. Explore Mealey machine
---------------------------------------------------------------------------------------------------

-- | drive mealy forward -> pasted from Mealy.hs since its not exported
driveMealy :: Mealy a b -> S.Seq a -> a -> (b, Mealy a b)
driveMealy m xs z = case S.viewl xs of
  y S.:< ys -> case runMealy m y of
    (_, n) -> driveMealy n ys z
  S.EmptyL  -> runMealy m z


-- | create mealy machines
m1f :: Int -> Char -> (Char,Int) 
m1f 0 'a' = ('b',1)
m1f 1 'b' = ('a',0)

m1 :: Mealy Char Char 
m1 = unfoldMealy m1f 0


-- | m2 is used for applicative composition
m2f :: Int -> Char -> (Char -> String, Int)
m2f 0 'a' = (\b -> b : b : [] ,0)
m2f 0 'b' = (\b -> b : b : [] ,1)
m2f 1 'c' = (\b -> b : b : [] ,1)

m2 :: Mealy Char (Char -> String)
m2 = unfoldMealy m2f 0


-- | run mealymachine 1
(b,m12) = runMealy m1 'a' :: (Char, Mealy Char Char)


-- | use all type class interfaces


-- | Functor -> use this to extend one machine to emit multiple kinds of outputs 
m1' :: Mealy Char String
m1' = (\a -> a:a:[]) <$> m1

(b',m12') = runMealy m1' 'a'  :: (String, Mealy Char String)


-- | applicative -> figure out a use case for this
m3 :: Mealy Char String
m3 = m2 <*> m1 


(b3, m31) = runMealy m3 'a' :: (String, Mealy Char String)



-- | monad 
-- | a mealy machine that always output 'a', in this case
m4 :: Mealy Char Char
m4 = return 'a' 

-- | the function takes an output, transformes it
{-
Note the implementation:

  m >>= f = Mealy $ \a -> case runMealy m a of
    (b, m') -> (fst (runMealy (f b) a), m' >>= f)


function f returns a mealy that takes in input b and construct mealy machine fb, fb is then run
taking in input a again

Problem: can't find a use case for this?

-} 
m5 :: Mealy Char String
m5 = m1 >>= \b -> return $ b : b : [] 

-- | functions bound to mealy machine using monad bind
fm1 b = if b == 'b' then return 'b' else return 'z' :: Mealy Char Char
fm2 b = snd . runMealy m1 $ b :: Mealy Char Char

m6 :: Mealy Char Char
m6 = m1 >>= fm1

m7 :: Mealy Char Char
m7 = m1 >>= fm2



------ translate + and * into monadic implementations -----

-- | note for all operations below, the reults of different computation branches should be mappended together
-- | note there's some pattern of combining computation branches that should be abstracted out -> or that what the monoid is trying to accomplish



-- | mock util functions 


-- | rewind mealy back to initial state
rewind :: Mealy a b -> Mealy a b 
rewind = id

-- | check if mealy at terminal state
terminal :: Mealy a b -> Bool
terminal m = False 

-- | strip a mealy of all final states
-- | should be (\(b,s,b,m) -> (b,s,false,m)) <$> n
stripFinal :: Mealy a b -> Mealy a b
stripFinal m = id <$> m


-- | (.+.) or mappend
-- | initial state = initial t1
-- | final state   = final state t1, final state t2
-- | links         = eps from initial t1 to initial t2
{-

two interleaving branches: 

branch 1 -> m1 accept a's
branch 2 -> m2 accept a's

-}
-- | note by this implementation, mempty shouldn't output anything
unionT :: Mealy a b -> Mealy a b -> Mealy a [b]
unionT m n = Mealy $ \a -> case runMealy m a of 
	(b,m1) -> case runMealy n a of 
		(c,n1) -> ([b,c], m1 `unionT` n1 )



-- | (.*.)
-- | initial state = initial t1
-- | final state   = final state t2
-- | links         = eps from initial t1 to initial t2 + eps from final t1 to initial t2 
{-

at least two interleaving branches

branch 1  -> m1 accepts a's
branch 2  -> m2 accepts a's
branch 3* -> everytime m1 arrives at a terminal state, a new branch is spawned whereby m2 is rewind back to initial, then accept a's

-}
catenate :: Mealy a b -> Mealy a b -> Mealy a [b]
catenate m n = let m' = stripFinal m in Mealy $ \a -> case runMealy m' a of 
	(b,m1) -> case terminal m1 of 
		True  -> case runMealy (rewind n) a of 
			(c',n1') -> ([b,c'], m1 `catenate` n1')
		False -> case runMealy n a of 
			(c, n1) -> ([b,c], m1 `catenate` n1)



-- | Kleene plus or Iteration
-- | initial state eps trans to self
-- | terminal states eps trans to initial
-- | Symbolically: {"ab", "c"}* = {ε, "ab", "c", "abab", "abc", "cab", "cc", "ababab", "ababc", "abcab", "abcc", "cabab", "cabc", "ccab", "ccc", ...}.

{-

inifinitely many branches (make sure to test above on symbolic example) 

branch 1  -> m accept a's
branch 2+ -> an m is always waiting at beginning, and ready to accept a's should it need to be accepted
				correlary -> if m reaches terminal state, it rewinds to initial 

-}

-- | note since rewind doesn't actually rewind, function below does not work 
-- | also it's wrong, once rewound, mealy does not run again
star :: Mealy a b -> Mealy a [b] 
star m = Mealy $ \a -> case runMealy m a of 
	(b,m1) -> case runMealy (rewind m1) a of 
		(b',_) -> ([b,b'], star m1)




---------------------------------------------------------------------------------------------------
-- V. Create Various Wyes
---------------------------------------------------------------------------------------------------


{-

construct a bigger Wye machine

wye :: Monad m => ProcessT m a a' -> ProcessT m b b' -> WyeT m a' b' c -> WyeT m a b c

-}

wye1 :: Monad m => WyeT m Int Int Int
wye1 = undefined

{-
myWye :: Wye Char Char Char
myWye = repeatedly $ do
  x <- await
  case x of
    Left l -> yield l
    Right r -> yield r

-}










































---------------------------------------------------------------------------------------------------
-- | Someone else's Machines Tutorial

-- | type operators: http://chrisdone.com/posts/haskelldb-and-typeoperator-madness
---------------------------------------------------------------------------------------------------




-- | construct a trivial plan that outputs one piece of data
-- | note this is a plan
plana :: PlanT k Char m ()
plana = yield 'A'

-- | construct a machine from plan, then run
runPlana :: [Char]
runPlana = run $ construct plana



-- | next level of trivial plan where data is repeated pushed down stream
-- | note this is a machine
stream1 :: (Monad m) => MachineT m k Char
stream1 = source "helloworld"


stream2 :: (Monad m) => MachineT m k Char
stream2 = source "helloearth"


-- | a process which maps inputs to outputs
filterO :: Monad m => MachineT m (Is Char) Char
filterO = filtered (/='o')


-- | note cap attach process (filterm) to a source (stream1), creating a machine
runFilter :: String
runFilter = run $ cap filterO stream1

--tinput :: T t0 t1 t2 -> Char
----tinput (L _) = 'a'
--tinput (R _) = 'b'

--myinterleave :: Tee Char Char Char -> Machine (((->) Char) :+: ((->) Char)) Char
myinterleave :: Monad m => TeeT m Char Char c -> TeeT m a b c
myinterleave = tee stream1 stream2



--myTee :: Tee Char Char Char
--myTee = repeatedly $ do 
--	x <- request (L await)
--	yield x 
--	y <- request (R await)
--	yield y









