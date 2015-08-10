---------------------------------------------------------------------------------------------------
-- | Finite State Automaton
-- | Tutorials: http://kar.kent.ac.uk/22057/2/Regular_Expressions_and_Automata_using_Haskell.pdf
-- | Cool Impl: https://github.com/leonidas/codeblog/blob/master/2011/2011-12-18-haskell-nfa.md
---------------------------------------------------------------------------------------------------

{-


challenges: how do i construct one that does analysis and generation?

-}

module FSA (
	FA(..),
	NFA(..),
	evalNFA,
	runNFA
	)where


import Control.Monad
import Control.Monad.Identity
import Control.Applicative
import Control.Proxy
import Control.Proxy.Trans.Writer
import Control.Proxy.Trans.State

import Parser 
import ParserCombinator
import Utils

import Data.Char

-----------------------
-- Deterministic FSA --
-----------------------

data FA n t = FA { startSt :: n, finalSt :: [n], trans1 :: n -> t -> n }


-------------------------------- | Generation | ---------------------------------------


----------------------
-- Proxy Components --
----------------------


-- | RunFA is a pipe (made by StateP proxy transformer) that accepts transition functions (arc) as param
-- | and outputs new node to down stream modules
-- | The pipe encapsulates the current node that determinstic finite state machine is in
-- | Use: runSesh sesh n = runProxy $ runStateK n $ sesh

{-
 Upstream | Downstream
       +---------+
       |         |
 ()   <==       <== ()
       |  runFA  |
 arc  ==>       ==> node
       |         |
       +---------+
-}

runFA :: (Monad m, Proxy p) => FA n t -> () -> Pipe (StateP n p) t n m r
runFA (FA _ _ trans) () = forever $ do
    arc  <- request ()
    node <- get
    put (trans node arc)
    respond node



{-

   Upstream | Downstream
       +----------+
       |          |
 ()   <==        <== ()
       |  runFA1  |
 arc  ==>        ==> (node,arc)
       |          |
       +----------+
-}

-- | same as runFA but pass both arc and current node downstream
-- | Note the current node should be a local state accesible to other modules as read-only
runFA1 :: (Monad m, Proxy p) => FA n arc -> () -> Pipe (StateP (n, String) p) arc (n,arc) m r 
runFA1 (FA _ _ trans) () = forever $ do
    arc        <- request ()
    (node,wrd) <- get
    put (trans node arc, wrd)
    respond (node,arc)



{-
              +--------------+
              |              |
 ()          <==            <== ()
              |  translator  |
 (node,arc)  ==>            ==> (node,arc)
              |              |
              +--------------+
-}

-- | A mock translater
-- | Note how the state is exposed to both modules which is BAD
-- | Both the representation And access should be restricted
translator1 :: (Proxy p, Show n) => () -> Pipe (StateP (n,String) p) (n,Arc) (n,Arc) IO r
translator1 () = forever $ do
	(node,arc) <- request ()
	(node,wrd) <- get
	put (node, translate arc wrd)
	respond (node,arc)
	lift . print $ "******************"



----------------------------
-- Proxy Components Utils --
----------------------------

 -- | A diagnostic tool 
logger :: (Show n, Proxy p) => () -> Consumer p n IO r 
logger () = runIdentityP . forever $ do
	n <- request ()
	lift . print $ "Log: "
	lift . print $ n
	lift . print $ "******************"

--------------
-- Sessions --
--------------

runSesh st session = runProxy $ runStateK st $ session

-- | run a session to check runFA works
-- | Use: runProxy $ runStateK RootSt $ sesh1 fmorph [Baby,Pl]
sesh1 :: (Proxy p, Show n) => FA n a -> [a] -> () -> Session (StateP n p) IO ()
sesh1 fa arcs = fromListS arcs >-> runFA fa >-> logger


-------------------------------- | Analysis | ---------------------------------------


-- | Intermediate form of noun
data IntermediateNoun = Sg String | Pl String String
	deriving (Eq,Show)


-- | A parser that accepts all letters except the letter 'S'
otherN :: Parser Char [Char]
otherN = toList <$> satisfy (\a -> isAlpha a && a /= 's')


charS :: Parser Char [Char]
charS = toList <$> char 's'


-- | A hacky solution that works for one case, is not extendable, too imperitive
interNoun :: String -> IntermediateNoun
interNoun = Sg <$> singular <|> Pl <$> singular <*> charS
	where 
		singular = (++) <$> charS <*> rest <|> rest
		rest = (++) <$> otherN <*> rest <|> pure [] 


-- | problem: make a parser generate two state


in1 = "cats"  -- [Sg "cats", Pl "cat" "s"]



{-
want this:

map('a', 'a')
.
.
map('s',)


-}

-- Analysis Utils --


toList :: Char -> [Char]
toList = flip (:) []














-- | A parser that accepts all words but chop off 'S' at the end
--pl   = (:) <$> (char 's' <|> nonPl) <*> (pl <|> (\a -> [a]) <$> char 's' <|> pure [])

--sing = (++) <$> (toList <$> char 's' <|> nonPl) <*> (nonPl <|> pure [])

--singular = (++) <$> toList <$> char 's' <*> rest 
--	where rest = (++) <$> nonPl <*> (rest <|> pure [])



--interNoun' = (:) <$> (char 's' <|> nonPl) <*> (pl <|> sing) 
--	where 
--		sing = interNoun' <|> pure []
--		pl   = interNoun' <|> (\a -> [a]) <$> char 's' <|> pure []


--interNoun = (root <*> pl) <|> (root <*> sing)
--	where 
--		root = (:) <$> (char 's' <|> nonPl)
--		sing = interNoun <|> pure []
--		pl   = interNoun <|> (\a -> [a]) <$> char 's' <|> pure []



------------------
-- FSA Examples --
------------------

-- | Note the fact that pre/suffix is appended to root word is a `side effect`
-- | Note Fault state is arrived via sequence of transition not allowed in the machine
data Arc  = Pluralize | Baby | Final deriving (Eq,Show)
data Node = RootSt | PlSt | BabySt | BabyPlSt | FinalSt | FaultSt deriving (Eq,Show)


-- | Analysis FA | -- 






-- | Generation FA | -- 

-- | An instance of deterministic finite automata
fmorph :: FA Node Arc
fmorph = FA RootSt [FinalSt] transf


-- | Note non matched transition schemes default to Fault state
transf :: Node -> Arc -> Node
transf RootSt Baby       = BabySt
transf RootSt Pluralize  = PlSt
transf BabySt Pluralize  = BabyPlSt

transf RootSt   Final = FinalSt
transf BabySt   Final = FinalSt
transf PlSt     Final = FinalSt
transf BabyPlSt Final = FinalSt

transf _      _    = FaultSt


translate :: Arc -> String -> String
translate Pluralize    = flip (++) "s"
translate Baby  = flip (++) "gie"
translate Final = flip (++) ""





-------------------------------- | Depricated | ---------------------------------------







----------------------------------------
-- Non-Proxy versions of run and eval --
----------------------------------------

-- | Checks if sequence of transitions arrives at terminal nodes
sevalFA :: Eq n => FA n s -> [s] -> Bool
sevalFA fa@(FA _ sfs _ ) = (`elem` sfs) . (srunFA fa)

-- | Outputs final state reached by sequence of transitons
srunFA :: FA n s -> [s] -> n
srunFA (FA s0 sfs trans) = foldl trans s0


---------------------------
-- Non-deterministic FSA --
---------------------------

data NFA n s = NFA { initSt :: n, endSt :: [n], trans :: n -> s -> [n] } 

evalNFA :: Eq n => NFA n s -> [s] -> Bool
evalNFA fa@(NFA _ sfs _) = any (`elem` sfs) . (runNFA fa)

-- | note foldM enumerates all possible outward transition for each state
runNFA :: Eq n => NFA n s -> [s] -> [n]
runNFA (NFA s0 sfs trans) = foldM trans s0



--------------------
-- Other Examples --
--------------------

{-

-- states
data State = S1 | S2 | S3 | S4 | S5 deriving (Eq, Show)
data Trans = A | B | C | D | E deriving (Eq, Show)

-- An FA Example --

fa :: FA State Trans
fa = FA (S1) [S4,S5] t1

-- | Note the hack where non-matched transitions automatically goes to s0
t1 :: State -> Trans -> State
t1 S1 E = S1
t1 S1 A = S2
t1 S2 B = S3
t1 S2 C = S4
t1 S3 D = S5
t1 _  _ = S1


-- An NFA Example -- 

nfa :: NFA State Trans
nfa = NFA (S1) [S4,S5] t

-- state transitions
t :: State -> Trans -> [State]
t S1 A = [S2]
t S2 A = [S3,S4]
t S2 B = [S1,S2]
t S2 C = [S3,S4]
t S3 D = [S5]
t S4 A = [S2,S4]
t _  _ = []
-}

-- use fa session
--mmain = runIdentity $ runProxy $ runWriterK $ execStateK (startSt fmorph) $
    --fromListS [Baby,Pl] >-> runFA1 fmorph >-> liftP . toListD




















































