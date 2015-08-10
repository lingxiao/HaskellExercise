---------------------------------------------------------------------------------------------------
-- | A simple parser that sends strings to its Syntax Tree
-- | The morphology of the words are not examined
-- | There is no constraints relating Num-feature of nouns
---------------------------------------------------------------------------------------------------


module SimpleSyntax (

	  NaiveP (..)

	, runNP
	
	, verbP
	, nounP
	, detP
	, advP
	, adjP
	, prepP
	
	, noun
	, verb
	
	, nounPh
	, verbPh
	
	, sentence

) where

import Data.Char
import Control.Monad
import Control.Applicative 

import Parser
import ParserCombinator
import SyntaxTree
import ParserUtils


------------------
-- Naive Parser --
------------------

type NaiveP b = Parser Char b

runNP :: NaiveP b -> [Char] -> [(b, [Char])]
runNP p s = runP p $ fmap toLower s 

---------------
-- Word Bank --
---------------

determinants, verbs, nouns, adjectives, adverbs, preposition :: [Word]
determinants = ["the", "this","that", "these", "those"] 
verbs        = ["create", "lift", "run", "create"]    
nouns        = ["sound", "steve", "room"]    
adjectives   = ["heavy", "big"]      
adverbs      = ["quickly", "readily"]
preposition  = ["in","from"]

------------------------
-- Tag Part of Speech --
------------------------

verbP :: NaiveP Verb
verbP = wordsP V1 verbs

nounP :: NaiveP Noun
nounP = wordsP N1 nouns

detP :: NaiveP Det
detP = wordsP D determinants

adjP :: NaiveP Adj
adjP = wordsP Adj adjectives

advP :: NaiveP Adv
advP = wordsP Adv adverbs

prepP :: NaiveP Prep
prepP = wordsP P preposition

-- | Aux function
wordsP :: (Word -> a) -> [Word] -> NaiveP a
wordsP g = liftM g . choice . fmap ( betweenS . string )

-------------------------------
-- Tag POS functional groups --
-------------------------------

-- | Parse Non-Terminal Noun
noun :: NaiveP Noun
noun = adj_n <|> n_p_np <|> nounP
	where
		adj_n  = N2 <$> adjP <*> noun
		n_p_np = N3 <$> ( adj_n <|> nounP ) <*> prepP <*> nounPh


-- | Parse Non-Terminal Verb
verb :: NaiveP Verb
verb = V2 <$> verbP <*> nounPh <|> verbP


-------------------
-- Parse Phrases --
-------------------

nounPh :: NaiveP NounPhrase
nounPh = NP2 <$> detP <*> noun <|> (NP1 <$> noun)

verbPh :: NaiveP VerbPhrase
verbPh = VP2 <$> verb <*> advP <|> v_p_np
	where v_p_np = VP3 <$> verb <*> prepP <*> nounPh


--------------------
-- Parse Sentence --
--------------------

sentence :: NaiveP Sentence
sentence = S <$> nounPh <*> verbPh


-----------
-- Tests --
-----------

-- for readability --
type Word      = String
type Phrase    = String

-- | map parser over all possible tests for parser
-- | choose to display result of parser or unparsed portions
runall :: Functor f => NaiveP b1 -> f [Char] -> ((b1, [Char]) -> b) -> f b
runall parser list g = (\a -> g $ (runNP parser a ) !! 0 ) <$> list

-- Test nonterminal-nouns --
n1, n2, n3, n4, n5, n6, n7, n8, n9, n10 :: Word

n1 = "steve"   			       -- n

n2 = "big " ++ n1 		       -- Adj n', where n' = n
n3 = "big " ++ n2              -- Adj n', where n' = Adj n

n4 = n1 ++ " in room"          -- n' p np, where n' = n
n5 = n2 ++ " in room"          -- n' p np, where n' = Adj n
n6 = n3 ++ " in room"          -- n' p np, where n' = Adj Adj n
n7 = n6 ++ " in room"		   -- n' p np, where n' = (Adj Adj n) p np

n8 = "big " ++ n4              -- Adj n', where n' = n p np
n9 = "big " ++ n5			   -- Adj n', where n' = (Adj n) p np
n10 = "big " ++ n6 			   -- Adj n', where n' = (Adj Adj n) p np

ns :: [Word]
ns = [n1,n2,n3,n4,n5,n6,n7,n8,n9,n10]

-- Test nonterminal-verbs --
v1, v2 :: Word
v1 = "lift"    				-- v
v2 = "lift steve"			-- v np, where np = n', n' = n

vs :: [Word]
vs = v1 : v2 : ((\np -> "lift " ++ np) <$> nps)

-- noun-phrase --
np1, np2 :: Phrase
np1 = "room"                        -- n', where n' = n
np2 = "the " ++ np1	                -- det n', where n' = n
nps :: [Phrase]
nps = (\n -> "the " ++ n ) <$> ns   -- det n', for all possible ns specified below


-- verb-phrase --
vp1,vp2 :: Phrase
vp1 = "run quickly"                	   -- v' adv, where v' = v
vp2 = "run steve quickly"          	   -- v' adv, where v' = v np, np = n', n' = n
vps :: [Phrase]
vps = (\v -> v ++ " quickly") <$> vs   -- v' adv, for all possible vs specified below



-- Test sentences --
s1 = "steve lift in the room"  :: String

s1' = 
	(S 
		(NP1 
			(N1 "steve")
		) 
		(VP3 
			(V1 "lift") 
			(P "in") 
			(NP2 
				(D "the") 
				(N1 "room")
			)

		)
	)









