{-# LANGUAGE FlexibleInstances #-}  {-# LANGUAGE FlexibleContexts #-}  {-# LANGUAGE NoMonomorphismRestriction #-}  {-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} {-# LANGUAGE RecordWildCards #-} {-# LANGUAGE UnicodeSyntax #-}


import Control.Arrow
import Data.Function (on)

import Prelude hiding ((.),id)
import Control.Category
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Monoid
import Data.Machine
import Data.Maybe

import Type
import Transducer
import Variants
import Utils

import Parser
import ParserCombinator
import ParserUtils


import Data.Functor.Contravariant
import Data.Profunctor
import Data.Bifunctor

import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as C
import Data.Word


-- an incremental parser http://www.reddit.com/r/programming/comments/1dwxr9/john_carmack_is_porting_wolfenstein_3d_to_haskell/c9vcndf
-- a lexer: http://www.haskell.org/alex/

--------------------------------------
------------ sanity check ------------
--------------------------------------


m2f :: NFSTtable String String
m2f (I 0) (Just "_1") = [(Just "0_1 ", (F 1))]
m2f (I 0) (Just "_0") = [(Just "0_0 ", (I 0))]
m2f (F 1) (Just "_0") = [(Just "1_0 ", (I 0))]
m2f (F 1) (Just "_1") = [(Just "1_1 ", (F 1))]
m2f _ _ 		      = []


pb :: [[Symbol String]]
pb = runNFST m2f [Just "_1", Just "_0"]


m1f :: NFSTtable String String
m1f (I 0) Nothing    = [(Nothing, (I 0))]
m1f (I 0) (Just "a") = [(Just "a", (F 3)),(Just "b", (I 1))]
m1f (I 1) (Just "b") = [(Just "b", (I 0))]
m1f (F 3) (Just "c") = [(Just "c", (I 0))]
m1f _ _          = []

m1 :: NFST String String
m1 = toNFST m1f 



mff :: Fstate -> Symbol String -> [(Symbol (String -> String), Fstate)]
mff (I 0) (Just "a") = [(Just $ \b -> b <> b, (F 1))]
mff (F 1) (Just "b") = [(Just $ \b -> b <> b <> b, (I 0))]
mff _ _ 		 = []

mf :: NFST String (String -> String)
mf = toNFST mff 




nmf :: NFSTtable String String 
nmf (I 0) (Just "01") = [(Just "->1", I 1)]
nmf (I 1) (Nothing  ) = [(Just "~>1", I 1), (Just "~>2", I 2)]
nmf (I 2) (Just "#" ) = [(Just "->3", F 3)]

nmf (I 2) Nothing     = [(Just "~>2", I 2)]

nmf (I 1) (Just "#" ) = [(Just "->3", F 3)]
nmf _ _			      = []


nm1 :: NFST String String
nm1 = toNFST nmf


---------------------------------------
----------- Morpheme type  ------------
---------------------------------------


data Morpheme = Root String | 
				Baby        |   
				Plural      | 
				EOL         | 
				M Morpheme Morpheme
	deriving (Eq, Ord)


instance Monoid Morpheme where
	mempty         = EOL
	EOL `mappend` n = n
	m `mappend` EOL = m
	m `mappend` n   = M m n 


instance Show Morpheme where
	show (Root str) = str
	show Baby       = "Baby"
	show Plural     = "Plural"
	show EOL        = "EOL"
	show (M m n)    = (show m) ++ "-" ++ (show n)


----------------------------------------------------------
---------- Mock Deterministic Parser/Analyser -----------
----------------------------------------------------------

-- | make a toy morph parser -> note consider making reg expression for this
gF :: FSTtable String Morpheme
gF (I 0) (Just "fish")    = Just (Just . Root $ "fish", F 3)   --- * problem: "fish" nondeterministicly transition to PL and Sing
gF (I 0) (Just "bird")    = Just (Just . Root $ "bird", F 4)
gF (I 0) (Just "cat" )    = Just (Just . Root $ "cat",  F 5)
gF (I 0) (Just "frog")    = Just (Just . Root $ "frog", F 6)
gF (I 0) (Just "dog" )    = Just (Just . Root $ "dog",  F 7)
gF (I 0) (Just "kittie" ) = Just (Just . Root $ "cat",  F 8)

gF (F 3) (Just "ie" )  = Just (Just Baby, F 2)
gF (F 4) (Just "ie" )  = Just (Just Baby, F 2)
gF (F 6) (Just "gie" ) = Just (Just Baby, F 2)
gF (F 7) (Just "gie" ) = Just (Just Baby, F 2)
gF (F 8) Nothing       = Just (Just Baby, F 2)

gF (F 3) Nothing    = Just (Just Plural, F 1)
gF (F 4) (Just "s") = Just (Just Plural, F 1)
gF (F 5) (Just "s") = Just (Just Plural, F 1)
gF (F 6) (Just "s") = Just (Just Plural, F 1)
gF (F 7) (Just "s") = Just (Just Plural, F 1)

gF (F 2) (Just "s") = Just (Just Plural, F 1)
gF _  _ 	  	    = Nothing


m :: FST String Morpheme
m = toFST gF


------------------ An Deterministic Analyzer built from an FST ---------------------

analyzerD :: FST String Morpheme -> String -> Morpheme
analyzerD m = head . analyzer m

------------------ A Parser with Deterministic Analyzer Subroutine ---------------------


taggerD :: FST String Morpheme -> Parser Char Morpheme
taggerD m = analyzerD m <$> word  				-- * here the output of parsed word in analyzed and tagged


tagm :: Parser Char Morpheme
tagm = taggerD m



-- | Task: Can the above function composition be better described as an explicit flow of computation instead?
-- | Expecially relevant with nondeterministic parsing, and cases where distant feature matricies have to refer to each other

----------------------------------------------------------
-------- Mock Non-Deterministic Parser/Analyser ---------
----------------------------------------------------------

-- | a non deterministic transducer
-- | Ideally, want to define it cleanely (and automatically) in the direction of generation

-- | in defining this transducer (or at higher LOA), the user should not be aware of the feature matrix at all
nF :: NFSTtable String Morpheme
nF (I 0) (Just "fish") = [(Just . Root $ "fish", I 3)]
nF (I 3) (Just "#")    = [(Just EOL, F 1)]
nF (I 3) Nothing       = [(Nothing, I 3),(Just Plural, I 2)]     --- * note here how fish transition to state 3 and state 2
nF (I 2) (Just "#")    = [(Just EOL, F 1)]

nF (I 0) (Just "bird") = [(Just . Root $ "bird", I 4)]
nF (I 4) (Just "ie")   = [(Just Baby, I 5)]
nF (I 4) (Just "s")    = [(Just Plural, I 2)]
nF (I 4) (Just "#")    = [(Just EOL, F 1)]
nF (I 5) (Just "s")    = [(Just Plural, I 2)]
nF (I 5) (Just "#")    = [(Just EOL, F 1)]
nF _ _ 			 	   = []

nm :: NFST String Morpheme
nm = toNFST nF


-- | Note this is replacing a word, the analyzer should be using the transducer to build a feature matrix, keeping the original word intact

-- | Note: this should be a routine in larger compuation involving interaction among distant feature matrices
-- | Fully understand the problem and reify the larger compuation, this fn below should be obsolete by then
-- | Thoughts: this could be redescribed as a ProcessT (StateT buffer []) a b

-- | greedily accepts the first string of symbols that can be accepted by transducer, repeat until list empty
--analyzer :: Monoid b => NFST String -> String -> [b]
-- | The PROBLEM here is that the parsing computation finished running already and output a list of one PValue,
-- | where as the analyzer ouputs a list of many PValues
analyzer :: Monoid b => NFST [a] b -> [a] -> [b]
analyzer m wrd = go m wrd [] []
	where 
		go m []     str ret = fromJust <$> ret
		go m (w:ws) str ret = let s' = str ++ [w] in 
			case step m (Just s') of 
				[] -> go m ws s' ret
				rs -> do 
					(b,_,m') <- rs
					go m' ws [] $ (\a -> a <> b) <$> ret <||> [b]    	-- * NOTE: here the logic of parsing overeach into the logic
																		-- of what to do with the PValue of parsing with the mappend operation

-- | Note: this is wrong, it should return type ParserT [] [b], or the parser
-- | branches with nondeterminism from PValue of transducer

-- | CENTRAL PROBLEM: interaction between compuation of analzyer and the parser, right now it's sepearate
-- | and have some sort of explicit descirption of how parser should branch.

tagger :: Monoid b => NFST String b -> Parser Char [b]
tagger m = (analyzer m . (++"#")) <$> word


-- | tags all words in Input
tagAll :: Monoid b => NFST String b -> Parser Char [[b]]
tagAll m = many1 $ tagger m



temp :: NFST Char String
temp = toNFST $ \s a -> case (s,a) of 
	(I 0, Just 'a') -> [(Just "01_a", I 1), (Just "00_eps", I 0)]
	(I 1, Just 'b') -> [(Just "12_b", F 2)]
	(F 2, Just 'c') -> [(Just "20_c", I 0)]
	(_,_)  		    -> []


g :: Char -> [String]
g a = [ fromJust b | (b,_,_) <-  step temp $ Just a]


-------------------------------------------------------------------------
---------- Make a serious attempt at modeling Feature Matrix ------------
-------------------------------------------------------------------------


-- | it should be a monoid where mappend is union, and mempty is empty feature?

-- | Decide when feature matrix should be built


{-
	task:
	describe, in english the flow of compuation of mergining feature matrices from distant words

	make this process abstract and determine if it's seen before

	if not can it be described as a type? which implement certain typeclasses? -> shouldn't be necessary


	union :: matrix -> matrix -> Maybe matrix



	first approximation:

	given predicate: nounPhrase = g <$> nounP <*> verbP :: Maybe (Parser Char NounPhrase)
	where g n v = NP n v  iff the union of their feature matrix is is not Nothing (or empty?)

	nounP accepts a noun, but its Num feature is blank and has to be inferred from context.
	verbP accepts a verb, tags as 3rd person

	g take union of two feature matrices, and if union is Nothing then Nothing else Just (nounphrase and its feature matrix)
	g then allow nounPhrase to inherit the feature traits of the noun and verb

	this process apply to all phrases, and other sentence substructures at all levels

-}

---------- Feature Matrix test cases: make sure you write them ------------

-- | note the presence of undefined is a hacky solution to the need for a feature with empty fields
-- | it's also a very object oriented way of thinking...
data POS = Noun | Verb | Adj | Adv | Det | Prep | EmptyPOS
	deriving (Eq, Show)


-- | note: this isn't really a monoid!!
instance Monoid POS where
	mempty = EmptyPOS
	EmptyPOS `mappend` p = p
	p `mappend` EmptyPOS = p
	p `mappend` q    = p

-- | note the presence of undefined is a hacky solution to the need for a feature with empty fields
-- | it's also a very object oriented way of thinking...
data NUM = Sg | Pl | Present | EmptyNum
	deriving (Eq, Show)


-- | This isn't really a monoid!!
-- | here mappend proxy union
instance Monoid NUM where
	mempty = EmptyNum
	EmptyNum `mappend` n = n
	n `mappend` EmptyNum = n
	n `mappend` m    = if n == m then n else mempty


-- | note the empty feature makes this a monoid
-- | when the union (mappend) of two features do not make sense, EmptyFeature is given


-- | design critique: 
-- | good: allow use of monoid interface which is used in transducer
-- | bad:  bad unions return neutrial value EmptyFeature, which means the parsing does Not Failed, when it should
-- | remidy for bad: explicity checking for EmptyFeature and crash thread

-- | problem: this is the bare minimum, what about stuff for verbs and things?
data Feature = EmptyFeature | FM { 
	  pos  :: POS
	, root :: String
	, num  :: NUM
} deriving (Eq)


instance Show Feature where
	show EmptyFeature = "EmptyFeature"
	show (FM p r n)   = "pos: " ++ show p ++ " root: " ++ r ++ "num: " ++ show n


-- | Feature Matrix typeclass implementation
{-
	use case:

	each transition in trasudcer spit out a feature matrix with one or zero field filled, and rest of field mempty
	union all features together to get one filled feature matrix

-}
instance Monoid Feature where
	mempty = EmptyFeature
	a `mappend` EmptyFeature = a 
	EmptyFeature `mappend` b = b  
	a@(FM p r m) `mappend` b@(FM q s n) 
		| m /= mempty && n /= mempty && m <> n == mempty = EmptyFeature
		| otherwise 									 = FM (p <> q)(r <> " " <> s) (m <> n)


a = (posF Noun) <> (rootF "bird")
b = numF Pl
c = numF Sg

posF :: POS -> Feature
posF p = FM p mempty mempty

rootF :: String -> Feature
rootF r = FM mempty r mempty

numF :: NUM -> Feature
numF n = FM mempty mempty n 


-- | in defining this transducer (or at higher LOA), the user should not be aware of the feature matrix at all
f1 :: NFSTtable String Feature
f1 (I 0) (Just "fish") = [(Just (rootF "fish" <> posF Noun), I 3)]
f1 (I 3) Nothing       = [(Just . numF $ Sg, I 3),(Just . numF $ Pl, I 2)]     --- * note here how fish transition to state 3 and state 2
f1 (I 3) (Just "#")    = [(Just EmptyFeature, F 1)]

f1 (I 0) (Just "bird") = [(Just (rootF "bird" <> posF Noun), I 4)]
f1 (I 4) (Just "s")    = [(Just . numF $ Pl, I 2)]
f1 (I 4) (Just "#")    = [(Just EmptyFeature, F 1)]

f1 (I 2) (Just "#")    = [(Just EmptyFeature, F 1)]
f1 _ _ 			 	   = []

msf :: NFST String Feature
msf = toNFST f1

--parseFeat :: Parser Char [[Feature]]
--parseFeat = tagAll msf




------------------------ Mock a Noun-Verb parser --------------------------

data NounA = N String Feature
	deriving (Eq, Show)

data Verb = V String Feature | VN Verb NounA
	deriving (Eq, Show)


-- | mock a short noun dictionary -> not a real transducer
nounf :: NFSTtable String Feature
nounf (I 0) (Just "fish") = [(Just (rootF "fish" <> posF Noun), I 3)]
nounf (I 3) Nothing       = [(Just . numF $ Sg, I 3),(Just . numF $ Pl, I 2)]     --- * note here how fish transition to state 3 and state 2
nounf (I 0) (Just "birds") = [(Just $ rootF "bird" <> posF Noun <> numF Pl, I 4)]
nounf (I 0) (Just "raptor") = [(Just $ rootF "raptor" <> posF Noun <> numF Sg, I 5)]
nounf _ _ 				  = []


-- | mock a short verb dictionary
verbf :: NFSTtable String Feature
verbf (I 0) (Just "run") = [(Just $ rootF "mock" <> posF Verb <> numF Pl, I 7)]
verbf (I 0) (Just "flies") = [(Just $ rootF "mock" <> posF Verb <> numF Sg, I 6)]
verbf _ _ 				  = []


-- | noun transducer
nount :: NFST String Feature
nount = toNFST nounf

-- | verb transducer
verbt :: NFST String Feature
verbt = toNFST verbf


tokenizer :: Parser Char [String]
tokenizer = many1 go 
	where go = do
		w <- word
		return $ w ++ "#"


------------------------ compare and contrast parse then analyze, vs analyze as we parse --------------------------


--http://imgur.com/gallery/cJfVY


-- | Gist: dump a subroutine into the parser and runs the subroutine upon each char consumed by parser

-- | PROBLEM: the parser stops too early, PValueing in incomplete parsing


-- | PROBLEM: the monoid is not being accumlated
-- | PROBLEM: the token # needs to be added
-- | the original word needs to be kept
--noun :: Parser Char Feature
--noun = with $ \xs -> fromJust . fst3 <$> step nount (pure xs)


---- | this allow parsing and analyzing but does not have logic about accumulating monoidal outputs
---- | cannot figure out how to get "#" EOL token into Input

---- | SOLN: figure out how to describe either: a more "rich" compuation, or one or two more "fundamental" computations
--verb :: Parser Char Feature
--verb = with $ \xs -> fromJust . fst3 <$> step verbt (pure xs)

noun :: Parser Char Feature
noun = untilP' . toNFST $ nounf


verb :: Parser Char Feature
verb = untilP' . toNFST $ verbf


phrase :: Parser Char Feature
phrase = (<>) <$> noun <*> verb


untilP = undefined

-- | Util function
untilP' :: NFST String b -> Parser Char b
untilP' m = untilP (=='#') $ \xs -> fromJust . fst3 <$> step m (pure xs) 



-- | test this mock

t1,t2,t3,t4,t5,t6 :: [Feature]
t1 = fst <$> runP phrase "birds run "
t2 = fst <$> runP phrase "birds flies "
t3 = fst <$> runP phrase "raptor run "
t4 = fst <$> runP phrase "raptor flies "
t5 = fst <$> runP phrase "fish flies "
t6 = fst <$> runP phrase "fish run "




-- | elements of a richer computation:
-- | 


-- | list of fundamental compuations:
-- | 


-- | Gist: run parser first, then run subroutine on PValue of parser

-- | RULE OUT THIS SOLUTION
-- | this has parsing first, then analyzing, but since the nondeterministic analyzing happens after parsing
-- | the nondeterminism is not propagated
verb' :: Parser Char [Feature]
verb' = (analyzer verbt) . (++ "#") <$> word 



-- | RULE OUT this solution: since chaining processes do not return type Parser
-- | redo this thing by chaining different processes

{-

in broadstrokes:

the parser is a process that consumes a letter `a` and push downInput

the processT state string stack keeps state of `ys` and tries to `step m (ys++[a])`
if it succeds then outputs else request another letter


-}

-- | put parser inside a process
parseP :: Parser a b -> Process [a] [(b,[a])]
parseP p = repeatedly $ do 
	str <- await
	yield $ runP p str


--pp :: Process Char String
--pp = source ["Hello world"] ~> (parseP word)



------------------------------- Play with an incremental parser design -------------------------------


{-
  what the parser is doing:

  let a char through -> dump into coroutine -> pause
  -> get PValue from coroutine and compose w/ existing PValue -> resume -> let char through -> .. 

  --- * Learn the process of thought with machines

  -- Note: A 'Machine' is usually constructed from 'Plan', so it does not need to be CPS'd.
  data Step k o r
    = Stop
    | Yield o r
    | forall t. Await (t -> r) (k t) r

  instance Functor (Step k o) where
    fmap _ Stop = Stop
    fmap f (Yield o k) = Yield o (f k)
    fmap f (Await g kg fg) = Await (f . g) kg (f fg)

  -- | A 'MachineT' reads from a number of inputs and may yield PValues before stopping
  -- with monadic side-effects.
  newtype MachineT m k o = MachineT { runMachineT :: m (Step k o (MachineT m k o)) }



-}

data PValue a b = Failed String | PValue [b] | PartialV ([a] -> PValue a b)  -- * morally equivalent to autoparser


-- | this makes more sense to me, but you can't have a tuple of (_, [a]) because you don't know what the rest of the Input is 

-- | question: does this actually capture what I'm trying to do?
--data PValue a b = Failed String | PValue [(b, [a])] | PartialV ([a] -> PValue a b)  -- * morally equivalent to autoparser
	

instance Show b => Show (PValue a b) where
  show (Failed msg)   = "Failed " ++ show msg
  show (PValue bs ) = "PValue " ++ show bs
  show (PartialV g ) = "PartialV <Function>"


instance Functor (PValue a) where
	fmap _ (Failed msg)  = Failed msg
	fmap g (PValue bs) = PValue $ g <$> bs
	fmap g (PartialV f) = PartialV $ \as -> g <$> f as



-- | WTF is c???
data ParserIncr a b c = PI { runPIncr :: [a] -> (c -> [a] -> PValue a b) -> PValue a b }

-- | fmap :: (c -> d) -> f c -> f d
-- | fmap :: (c -> d) -> (ParserIncr a b) c -> (ParserIncr a b) d

-- | fmap :: (c -> d) -> 
-- |         PI { runPIncr :: [a] -> (c -> [a] -> PValue a b c) -> PValue a b c} -> 
-- | 		 PI { runPIncr :: [a] -> (d -> [a] -> PValue a b d) -> PValue a c d } 

instance Functor (ParserIncr a b) where
	fmap g p = PI $ \as k -> runPIncr p as (k . g)





{-

sat :: (a -> Bool) -> ParserIncr a b
sat p = PI $ \as k -> case as of
	[]     -> PValue []
	(a:as) -> case p a of 
		True -> k a as
		_    -> Failed "Failed to match"


satr :: PValue Char Bool
satr = runPIncr (sat (=='a')) "abced" $ \a as -> if a == 'a' then PValue [True,True] else Failed "a is not char a"
-}
{-


	getC :: Parser a a
	getC = P $ \as -> case as of 
	  (x:xs) -> [ (x,xs) ]
	  []     -> []

	-- | Return the next character if it satisfies the given predicate
	satisfy :: (a -> Bool) -> Parser a a
	satisfy p = do 
	  c <- getC
	  if (p c) then return c else Failed "End of input"




 -- * autoparsec satisfy
 -- | Match a single byte based on the given predicate.
satisfy :: (Word8 -> Bool) -> Parser r Word8
satisfy p = P $ \st@(S sb lb adds eof FailedDepth) k ->        --- * take a Input and continouation function k 
    case S.uncons sb of
      Just (w, sb') 
      	| p w -> k w (S sb' lb adds eof FailedDepth)    -- * apply function k onto w and Input -> now what is w?
        | otherwise -> IFaileded st "Faileded to match"
      Nothing -> case L.uncons lb of
                   Just (w, lb') | p w -> k w (mkState lb' adds eof FailedDepth)   -- * apply function k onto w
                                 | otherwise -> IFaileded st "Faileded to match"
                   Nothing -> continue (`IFaileded` "satisfy: EOF")
                                       (satisfy p) k st


-}








------------------------------- Design Own Incremental Parser -------------------------------



-- | Input is data type yielded by parser to the coroutine
-- | It contains the current list of input chars being operated on by coroutine and end of line condition

data Input a = S [a] Bool deriving (Show)

instance Monoid (Input a) where 
	mempty = S [] True
	(S as x) `mappend` (S bs y) = S (as ++ bs) $ x && y


instance Functor Input where
 	fmap g (S as x) = S (g <$> as) x


instance Applicative Input where
	pure a = S [a] False
	(S gs x) <*> (S as y) = S (gs <*> as) $ y


instance Alternative Input where
	empty = mempty
	s@(S _ b) <|> t = case b of 
		True -> t 
		_    -> s 

instance Monad Input where
	return = pure
	(S as x) >>= g = foldr mappend mempty $ g <$> as



-- | Output is data type yielded by coroutine to Parser
-- | It is either a Failed message, Donez [b], or Partial ([a]->Output a b), where [a] is the current buffer passed back to the parser

data Output a b = Fail String | Donez [b] | Partial ([a] -> Output a b)


instance Show b => Show (Output a b) where
  show (Fail msg)  = "Fail " ++ show msg
  show (Donez bs)   = "Donez " ++ show bs
  show (Partial g) = "Partial <Function>"


-- | fmap :: (b -> c) -> Donez [b] -> Donez [c]
instance Functor (Output a) where
	fmap _ (Fail s)    = Fail s
	fmap g (Donez bs)   = Donez $ g <$> bs
	fmap g (Partial f) = Partial $ \as -> g <$> f as


instance Applicative (Output a) where
	pure a  = Donez [a]
	o <*> p = undefined


-- | The parser takes a list of [a] and yields a buffer to coroutine, which yields back Output

-- | should it be -> output a b or [(Output a b, [a])], where it's Donez b instead of Donez [b]
data ParserI a b = PP { runPi :: [a] -> (Input a -> Output a b) -> Output a b }


instance Show (ParserI a b) where
	show p = show "<Incremental Parser>"



-- | fmap :: (b -> c) -> f b -> f c

-- | fmap :: (b -> c) -> ParserI a b -> ParserI a c

{-
	fmap :: (b -> c) -> 
			PP [a] -> (Input a -> Output a b) -> Output a b -> 
			PP [a] -> (Input a -> Output a c) -> Output a c
	

	fmap :: (b -> c) -> 
			PP [a] -> (Input a -> Donez [b]) -> Donez [b]
			PP [a] -> (Input a -> Donez [c]) -> Donez [c]


	According to SO, it's not a profunctor but a bifunctor?
    a profunctor is a bifunctor that's just contravariant in its first argument, and covariant in the second

-}




cor :: (Ord a, Num a) => Input a -> Output a Bool
cor (S as b) = if b == True then Donez ((>1) <$> as) else Donez $ (<1) <$> as


gg :: Show a => a -> String
gg = show


ff :: (Ord a, Num a) => Input a -> Output a String
ff ins = fmap gg (cor ins)




-- | problems as of July 18th:
{-
	do not really know what is going on and even if typeclass implemened, what can be Donez
	with it to reify yield/await flow of control


	try writing some pseudo code to see ideal use case:

	the primitive DSL are

	getC :: ParserI a a
	getC = PP $ \as k -> case as of 
		[]     -> []
		(x:xs) -> give [x] to k then ? how is this process described?


	can this whole thing be implemented with on top of machines instead? 


	where Parser a b is just a synonym for Process a b

	this way it can interop w/ autoT transducer.

	except you still cannot do circular toplogy

	satisfy

	choose



-}


--------------------------------- First intro with contravariant functors and profunctors -------------------------------


-- | a type that's contravratiant functor
data Pred a = Pred { eval :: a -> Bool }

-- | contramap maps the input
-- contramap :: (a -> b) -> f b -> f a
instance Contravariant Pred where
	contramap g (Pred p) = Pred $ p . g


p :: Pred Int
p = Pred (>12)
p' :: Pred Int
p' = (`div` 2) >$< p :: Pred Int

data Constr a b = C a 

instance Contravariant (Constr a) where
	contramap _ (C a) = C a


-- | bifunctor. A covariant functor over product catagories

-- | dummy either data type
data Branch a b = Lb a | Rb b 
	deriving (Eq, Show)


instance Bifunctor Branch where
	bimap g _ (Lb a) = Lb $ g a
	bimap _ h (Rb b) = Rb $ h b


-- | profunctor. A contravariant and covariant functor over product catagories
-- | or it's a bifunctor that's contravariant in 1st arg and covariant in 2nd arg
{-
class Profunctor f where
    dimap ∷ (c → a) → (b → d) → f a b → f c d
-}


-- | note the type-foo shanigans, w/o need to make into profunctor:

{-

	data Limits a = Lim {
		next  :: a -> (a,a),
		check :: a -> a -> Bool
	}

-}

-- | to make into profunctor, or functor over catagory A(op) X B, use two params, a : A, b : B
data Limits' a b = Lim {
	next  :: a -> (b,b),
	check :: a -> a -> Bool
}

type Limits a = Limits' a a 

-- | this implementation use some new concepts here: arrow
-- | (***) :: Arrow a => a b c -> a b' c' -> a (b, b') (c, c')
instance Profunctor Limits' where
	dimap g h Lim {..} = Lim {
	   next = (h *** h) . next . g
	 , check = check `on` g 
	}


-------------------------------------- Profunctor instance of Parser ------------------------------------


-- | Make profunctor of parser, as seen on SO

data ParserIC' a b b' = PIC { runPic :: [a] -> (Input a -> Output a b) -> Output a b' }

type ParserIC a b = ParserIC' a b b


-- dimap :: (c -> b) -> (b -> c) -> ParserI a b -> ParserI a c
instance Profunctor (ParserIC' a) where
	dimap g h p = PIC $ \as k -> fmap h . runPic p as $ fmap g . k


instance Functor (ParserIC' a b) where
	fmap = rmap


--- try out ParserIC and Profunctor functions ---


{-

Plan for 7/22nd

1. implement the recursive analyzer w/ transducer parser that we wanted to make

2. make any adjustments to type as needed. Implement profunctor, functor

3. repeat 1 and 2 until satisfied then

4. implement applicative, monad, alternative, monoid(?)


-}


-- | note: to ensure input output data type details don't leak, write really freaking good DSLs

getP :: ParserIC a b
getP = PIC $ \as k -> case as of 
	[]     -> Donez []
	(x:xs) -> k $ S [x] True


-- | propose: make a few of the above stuff to see how k fits in with getP

-- | data Output a b = Fail String | Donez [b] | Partial ([a] -> Output a b)
withf :: ParserIC a b
withf = PIC $ \(a:as) k -> case k (S [a] False) of
	Partial g -> Fail "dno"
	ret       -> ret


k :: Input Char -> Output a Char
k (S as b) = Donez $ as ++ " world"

-- | runPic getP "a hello" k

-- | mannually map function `succ` onto k
k' :: Input Char -> Output a Char
k' = fmap succ . k


-- | use profunctor `dimap`: problem: mapping g and h has the same effect??

-- | map the outputs of parsing as outputed by function k
getP1 :: Enum b => ParserIC a b
getP1 = dimap succ id getP


-- | map the outputs of parsing as ouputed by parser
getP2 :: Enum b => ParserIC a b
getP2 = dimap id succ getP




------ make applicative, alternative, and monad instances -------


-- | problem: note how all these typeclasses are going to be tied to representation of Input and Output
-- | Also note type Output must be wrong since info of inputs as is destroyed
{-
	instance Applicative (ParserIC' a b) where
		pure a = PIC $ \as k -> Donez [a]
		(PIC g) <*> (PIC h) = PIC $ \as _ -> case g as of 
			Fail s    -> Fail s
			Partial f -> undefined
			Donez gs   -> undefined
-}













-- | can you make an applicative out of this w/o functor instance? 
-- | to answer the question, can look at relationship between functors and profunctors
-- | note not all profunctor are functors
-- | what's the profunctor equivalent of applicative?

-- | pure is the identity element, what does it mean to be pure in product catagory?

{-
instance Applicative (ParserIC' a b) where
	pure a    = undefined
	p1 <*> p2 = PIC $ \as k -> do
		()


instance Applicative (Parser a) where
  pure a     = P $ \as -> [(a,as)]
  p1 <*> p2  = P $ \as -> do
    (g, as')  <- runP p1 as
    (x, as'') <- runP p2 as'
    return ( g x, as'' )
-}































---------------------------------------------------- Useless --------------------------------------------------



--scanner :: NFST Char Morpheme -> ProcessT (State String) Char Morpheme
--scanner m = do
--	a   <- await
--	lift . modify $ (++[a])
--	wrd <- lift get
--	case step m wrd of 
--		[] -> scanner m 
--		rs -> 


--scanner :: NFST Char Morpheme -> ProcessT (StateT String []) Char Morpheme
--scanner m = construct . plan where
--	plan m = do 
--		a   <- await
--		lift . lift . modify $ (++[a])
--		wrd <- get
--		case step m a of 
--			[] -> plan m
--			rs -> do
--				(b,_,m') <- rs


{-

analyzer :: Monoid b => NFST [a] b -> [a] -> [b]
analyzer m wrd = go m wrd [] []
	where 
		go m []     str ret = fromJust <$> ret
		go m (w:ws) str ret = let s' = str ++ [w] in 
			case step m (Just s') of 
				[] -> go m ws s' ret
				rs -> do 
					(b,_,m') <- rs
					go m' ws [] $ (\a -> a <> b) <$> ret <||> [b]    	-- * NOTE: here the logic of parsing overeach into the logic
																		-- of what to do with the PValue of parsing with the mappend operation
-}


--stProcess :: ProcessT (State (RoseTree Int)) Int Int
--stProcess = repeatedly $ do
--	x <- await
--	lift . modify $ growTree [x]
--	yield x

--autoT :: NFST a b -> ProcessT [] (Symbol a) (Symbol b)
--autoT = construct . plan where
--    plan m = do 
--        a <- await
--        case step m a of
--            []  -> stop
--            xs  -> do
--                (b,_,m') <- lift xs
--                yield b
--                plan m'
















{-

	Review:


	Ideal flow of computation
		Top process: Parser

			consume a char and append into exising string
			call subroutine
				if subroutine yield [] then consume another char
				else subroutine yields [PValue]. propagate nondeterminism of 
				subroutine to parser 


		Subroutine: Transducer
			subroutine takes string and try to run it, yield a list of [PValues] or []


	Critique this flow of compuation
		is it modular
		should it be more modular



-}




-- | an analyzer 


--analyze :: Monoid b => NFST String b -> Parser Char [b]
analyze m = (\w -> (w, analyzer m $ w ++ "#")) <$> word


-- | noun parser 
nounP :: Parser Char [Feature]
nounP = tagger nount

-- | verb parser
verbP :: Parser Char [Feature]
verbP = tagger verbt



n = head . fst . head $ runP nounP "bird" 
ns = head . fst . head $ runP nounP "birds" 


v = head . fst . head $ runP verbP "runs"
vs = head . fst . head $ runP verbP "run"



--verbBlock = (\) <$> verb <*> noun <|> verb


-- | task: find way to propagate threads of lower computation into upper one


-- | for now just mannually do it, eventually it should be some sort of mutally referential function
-- | where the list is built as the compuation flows

rs = [([ FM Noun "fish" Sg, FM Noun "fish" Pl], "") ]


--rs' = [ (l,as) | l <- ls | (ls, as) <- rs ]

seqT :: Monad m => m (m a, b) -> m (a, b)
seqT rs = do 
	(ls, as) <- rs
	do 
		l <- ls
		return (l,as)







-- | runParser nounP "sound"    -> [(N1 "sound","")]

{-


ideally: 

1. word :: parser String String 
		takes any string of letters until whitespace

2. wordT :: Parser String Word = g <$> word
		takes word and annotate with terminal signal

3. tWordT :: Parser String Morpheme =  t<$>wordT
		parses word again and annote with root word and morpheme

4. mWordT :: Parser String POS
		reads morpheme and wrap word around POS value constructor

so for example, englishParser = morphemeReader . dictTransducer . terminalAnnotator <$> wordParser



describe flow of compuation: it is an applicative? 
	wrd    <- runParser wordE
	morphs <- step transducer wrd
	pos    <- toPOS morphs
	return pos

------ All complexity associated with how transducer is run should not leak from here ------

constraint: the following ways to describe phrase and sentence must be possible
problem: the transducer doesnt make this distinction since one root word could spawn words in serveral POS
Soln: all pos parser could be variants of wordT that runs word and returns value if tag satisfies pos, else return nothing, ie:


wordT :: Parser Char [Morpheme]
wordT = toMorpheme . (++"@") <$> word

satPOS :: POS -> Parser Char String
satPOS pos = (==pos) <$> wordT

noun :: Parser Char String
noun = satPOS Noun

verb :: Parser Char String
verb = satPOS Verb

adj  :: Parser Char String
adj = satPOS Adj

adv  :: Parser Char String
adv = satPOS Adv

prep :: Parser Char String
prep = satPOS Prep

det  :: Parser Char String
det = satPOS Det


------ note the stuff below make use of atomic parser described above ------


nounGroup :: Parser Char String
nounGroup = adj_n <|> n_p_np <|> nounP
	where
		adj_n  = adj <*> noun
		n_p_np = (adj_n <|> noun) <*> prep <*> nounPh

verbGroup :: Parser Char String
verbGroup = verb <*> nounPh <|> verb


nounPhrase :: Parser Char String
nounPhrase = det <*> noun <|> noun

verbPhrase :: Parser Char String
verbPhrase = verb <*> adv <|> v_p_np
	where v_p_np = verb <*> preP <*> nounPh


sentence :: Parser Char String
sentence = nounPh <*> verbPh


------ Problems and things to keep in mind ------


Problem:  Ambigous POS? 
Solution: explore all avenues -> nondeterminism from tagging POS, so from the transducer itself?

Problem: how does branching computation of transducer affect compuation of parser


unknown area: feature matrix and how it's built, its effect on flow of computation
	thorny stuff: 
		 e replacement
		 y replacement
		 k insertion
		 and other special plural rules


question to keep in mind: how does additional complexity of transducer affect the way pareser is run?


-}


--------------------------------------------------------------
---------- Mock non-deterministic Parser/Analyser ------------
--------------------------------------------------------------

-- | mock non deterministic function
--nonD :: MockND String
--nonD a@"hello" = [(length a, a, nonD a), (length a + 12, a, nonD "")]
--nonD a@"world" = [(length a + 100, a, nonD $ a++a)]
--nonD _         = []

-- | make a automate function that concats all PValues
--autoAll :: NFST a b -> Process (Symbol a) (Symbol b)
--autoAll = construct . plan where 
--	plan m = do 
--		a <- await
--		mapM (\(b,_,m') -> yield b >> plan m' ) $ step m a


--case step m a of 
--[]            -> stop  					-- * this line here Faileds silently
--((b,_,m'):xs) -> yield b >> plan m'




{-

	data ND a b = N { runN :: a -> [(b, ND a b)] }


	instance Show (ND a b) where
		show n = show "<ND>"


	nd :: ND String Int
	nd = N $ \a -> case a of 
		"hello" -> [(length a, nd), (length a + 100, nd)]
		"world" -> [(length a + 10, nd)]
		_       -> []


	ad :: Process String Int
	ad = toProcess nd 


	toProcess :: ND a b -> Process a b
	toProcess = construct . plan where
		plan n = do 
			a <- await
			case runN n a of 
				[]          -> stop
				((b,n'):xs) -> yield b >> plan n'
-}




-- mock paralell parser where the PValues are just given --

-- | these lists represent raw PValue of NFST
-- | note: it may make more sense to make these into feature matrices first 


-- | Note: there are some seemingly complex compuation going on w/ feature matrix unions that could be 
-- | simplified with appropriate data type and abstractions

one, many :: [[Maybe Morpheme]]
one  = [[Just . Root $ "bird", Just Plural]]
many = [[Just . Root $ "fish"], [Just . Root $ "fish", Just Plural]]

-- | want to run parser that outputs
-- | [([Root "fish"],""), ([Root "fish", Plural], "")]

-- | then tag with
-- | [Noun "fish" + Sing, Noun "fish" + Plural]






{-
	runTest :: FSTtable a b -> [a] -> [b]
	runTest m as = fmap fromJust . runFST m $ fmap Just as

	analyze :: [String] -> [Morpheme]
	analyze = runTest gF


	t1, t2, t3, t4, t5 :: [Morpheme]
	t1 = analyze ["bird", "ie", "s"] 
	t2 = analyze ["kittie", "s"] 
	t3 = analyze ["frog", "gie", "s"]
	t4 = analyze ["fish"]
	t5 = analyze ["fish", "ie", "s"]

-}



-- | build a trivial parser that returns a list of parsers




---------- Try making another Parser -----------


-- | CONFLICT: any function that works w/ parser is forced to return this type
-- | the parsing function either successfuly yields list of results (Just x) or request (Nothing) more information
type Parsed b = Maybe [b]


-- | CONFLICT: some times the k function is not needed, but don't know how to get rid of it??

-- | the parser builds a buffer one char at a time and yields to function k.
-- | function k parse the result and give back to parser, requesting more chars as needed
data Parse a b = Pa { runPa :: [a] -> ([a] -> Parsed b) -> [(b, [a])] }


-- | dummy k
kd :: [Char] -> Parsed Char
kd a = pure []

-- | primitives of original parser

getc :: Parse a a
getc = Pa $ \as k -> case as of 
	[]     -> []
	(a:as) -> [(a,as)]


-- | note will reuse getc once monad instance of Parse is written
satc :: (a -> Bool) -> Parse a a
satc p = Pa $ \as k -> case as of 
	[]     -> []
	(a:as) -> if p a then [(a,as)] else []


trans :: [Char] -> Parsed String
trans "hello" = Just ["hello", "another result"]
trans "phone" = Just ["phone", "another phone"]
trans "un"    = Just ["inflection ", "eps "]
trans "do"    = Just ["root-do ", "noun "]
trans _       = Nothing


-- | create another function that runs until hitting EOL symbol
-- | some composition of satisfy and untilSucc
-- | additionally, whose reponsibility is it to put together the final results of several runs?


-- | is ^-- this worth making a separate function for. is there a better way to distribute the logic 
-- | among combinatators and primitives


-- | there appear to be a notion of `forever` in here somewhere, also a notion of 'until'
-- | these are both combinators describing terminal condition (if any) of parser

-- | this is not really any better than regular parser

-- | are parser and k really coroutines? or are p and k both subroutines of parser? seems like parser is 
-- | coordinating p and k
untilp :: Monoid b => (a -> Bool) -> Parse a b
untilp p = Pa $ \xs k -> loop k xs [] []
	where 
		loop _ [] rs _          = flip (,) [] <$> rs
		loop k (a:as) rs buffer = case k buffer of 
			Nothing -> loop k as rs $ buffer ++ [a]
			Just bs -> let rs' = (<>) <$> rs <*> bs <||> bs in case p a of
				True -> flip (,) as <$> rs'
				_    -> loop k as rs' [a]


r4 = runPa (untilp (=='#')) "undo# world" trans


{-
	Next steps:

	reconcile how to use k, how is it different from some other function passed into parser
	what is the point of k? is it used to store state? 
		-- store buffer
		-- pass continuation information from co-routine inside parser

	inmplement instance profunctor, functor and applicative

	write a parser applicative style 

	use it to see if parser makes sense


	Alt gameplan:

	figure out wtf a coroutine is. 
		Read coroutine tutorials
		read control.monad.coroutine
		read machines package
-}






































































































