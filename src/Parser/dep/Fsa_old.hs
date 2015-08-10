---------------------------------------------------------------------------------------------------
-- | Finite State Automaton
-- | Tutorials: http://kar.kent.ac.uk/22057/2/Regular_Expressions_and_Automata_using_Haskell.pdf
-- | Cool Impl: https://github.com/leonidas/codeblog/blob/master/2011/2011-12-18-haskell-nfa.md
---------------------------------------------------------------------------------------------------

module FSA (
  Transition(..),
  FiniteAutomaton
  )where

import Data.Set
import Control.Monad


---------------
-- Data Type --
---------------

-- | Note this value constructor is exported
data Transition a b = Trans b a b | Etrans b b
  deriving (Eq,Show,Ord)

-- | for readability
type Alphabet a = Set a
type States   b = Set b
type Final    b = Set b

-- | a is type of transition, b is type of state
-- | Note this value constructor is not exported
-- | Question: are all these fields necessary?
data FiniteAutomaton a b = FSA {
  alphabet   :: Alphabet a,
  states     :: States b,
  start      :: b,
  final      :: Final b,
  transition :: Set (Transition a b)
} deriving (Show)


------------
-- Traits --
------------


-------------------------
-- Primitive Functions --
-------------------------

toFSA :: (Ord a, Ord b) => (b, [b], [Transition a b]) -> FiniteAutomaton a b
toFSA (start, final, trans) = FSA (fromList alphabet) (fromList states) start (fromList final) (fromList trans)
  where 
    (states, alphabet) = Prelude.foldr collect ([],[]) trans
    collect (Trans b1 a b2) (sts, alpha)  = (b1 : b2 : sts, a : alpha) 
    collect (Etrans b1 b2 ) (sts, alpha)  = (b1 : b2 : sts, alpha) 



-- | one regular transition


-- | one closure transition


-- | one transition


----------
-- Test --
----------

{-
  (1) - 'a' -> (2)
-}
fsa0 :: FiniteAutomaton Char Int
fsa0 = FSA (fromList['a']) (fromList[1,2]) 0 (singleton 1) (fromList[Trans 1 'a' 2, Etrans 1 2]) 

{-
  (0) - 'a' -> (0)
  (0) - 'a' -> (1) - 'b' -> (2) - 'c' -> (3)
-}
fsa1 :: FiniteAutomaton Char Int
fsa1 = FSA (fromList['a','b','c']) (fromList[0..3]) 0 (singleton 3) t1
  where t1 = fromList[Trans 0 'a' 0, Trans 0 'a' 1, Trans 1 'b' 2, Trans 2 'b' 3]


fsa2 :: FiniteAutomaton Char Int
fsa2 = toFSA (0,[2,5],[Trans 0 'a' 1, Trans 0 'b' 3, Trans 1 'b' 2, Etrans 3 4, Trans 3 'b' 4, Trans 4 'b' 5])





----------------------------
-- Another Implementation --
----------------------------

-- states
data State = S1 | S2 | S3 | S4 | S5 deriving (Eq, Show)
data Trans = A | B | C | D deriving (Eq, Show)

-- deterministic finite automaton --
data FA n s = FA { initSt1 :: n, endSt1 :: [n], trans1 :: n -> s -> n }


runFA :: Eq n => FA n s -> [s] -> Bool
runFA (FA n0 ns trans) = (`elem` ns) . Prelude.foldl trans n0

-- non-deterministic finite automaton
data NFA n s = NFA { initSt :: n, endSt :: [n], trans :: n -> s -> [n] } 

runNFA :: Eq n => NFA n s -> [s] -> Bool
runNFA (NFA n0 ns trans) =  any (`elem` ns) . foldM trans n0


--------------
-- Examples --
--------------


-- An FA Example --
i1 = S1
f1 = [S5,S4]

t1 :: State -> Trans -> State
t1 S1 A = S2
t1 S2 B = S3
t1 S2 C = S4
t1 S3 D = S5


nfa1 = FA i1 f1 t1

-- An NFA Example -- 

-- initial state
i = S1
f = [S5, S4]

-- state transitions
t :: State -> Trans -> [State]
t S1 A = [S2]
t S2 A = [S3,S4]
t S2 B = [S1,S2]
t S2 C = [S3,S4]
t S3 D = [S5]
t S4 A = [S2,S4]
t _  _ = []

nfa = NFA i f t







































