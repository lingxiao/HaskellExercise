

-- | strategy for making this file: think of shitty abstractions as we go?

-- | every function signature here should have form NFST a b ->  NFST a b -> NFST a b or [NFST a b] -> NFST a b
-- | note the semiring operations could satisfy this signature



module Combinators ( 

	  choice
	, sigma
	, pie

) where 

import Data.Monoid
import Data.Machine
import Semiring
import Type
import Transducer


-- | choose the first transducer that accepts input
choice :: [NFST a b] -> NFST a b
choice = foldr (<|>) empty


-- | union all transducers
sigma :: [NFST a b] -> NFST a b
sigma = foldr (.+.) zero


-- | catenate all transducers
pie :: [NFST a b] -> NFST a b
pie = foldr (.*.) one


-- | need some way of mapping a large corpus of inputs to outputs, where the rules are the for each in-out map?
-- | use case, transition from state 0 to list of states of all words in dictionary. 

-- | one way: (0) - wrd1 -> (2) `cons` (0) - wrd 2 -> (3) `cons` ...
-- | so its the `cons` of all trivial transducers from initial state to some final state
-- | need to define cons, and a function to foldr cons identity ts

-- | challenge: is this function already defined somewhere in semiring? can it be built from semiring functions?
-- | Is this function just choice? .. it is


-- | ideal: t1 = [Root "fish", Baby "ie", Plural "s"]  - mkTranducer -> FST String Morpheme
-- |        t2 = [Root "egg", Babye "gie", Plural "s"]  - mkTranseducer -> FST String Morpheme

-- | tWords = choice [t1,t2]







