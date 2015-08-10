module SimplePo where

import Data.Char
import Control.Monad
import Control.Applicative 

import Parser
import ParserCombinator
--import SyntaxTree
import Utils

-- Parser Levels --
type Word      = String
type Words     = String
type Phrase    = String
type SentenceS = String


-------------------
-- Earley Parser --
-------------------

type Earley b = Parser Char b

runEarley :: Earley b -> [Char] -> [(b, [Char])]
runEarley p s = runParser p $ fmap toLower s 

---------------
-- Word Bank --
---------------

determinants, verbs, nouns, adjectives, adverbs, preposition :: [Word]
verbs        = ["create", "lift", "run", "create"]    
nouns        = ["sound", "steve", "room"]    
adjectives   = ["heavy", "big"]      
adverbs      = ["quickly", "readily"]
preposition  = ["in","from"]
determinants = ["the"] 

--------------------------
-- Parse Part of Speech --
--------------------------

detP :: Earley Word
detP = wordsP determinants

verbP :: Earley Word
verbP = wordsP verbs

nounP :: Earley Word
nounP = wordsP nouns

adjP :: Earley Word
adjP = wordsP adjectives

prepP :: Earley Word
prepP = wordsP preposition

advP :: Earley Word
advP = wordsP adverbs


-- | Aux function
wordsP :: [Word] -> Earley Word
wordsP = choice . fmap ( betweenS . word )


-----------------------------------
-- Parse Non-Terminal POS-phrase --
-----------------------------------

-- | Parse Non-Terminal Noun
noun :: Earley Words
noun = adj_n <|> n_p_np <|> nounP

adj_n :: Earley Words
adj_n = sconcat2 <$> adjP <*> noun


n_p_np :: Earley Words
n_p_np = sconcat2 <$> ( adj_n <|> nounP ) <*> p_np
	where p_np = sconcat2 <$> prepP <*> nounPh


-- | Parse Non-Terminal Verb
verb :: Earley Words
verb = sconcat2 <$> verbP <*> nounPh <|> verbP


-------------------
-- Parse Phrases --
-------------------

-- | noun-phrase
nounPh :: Earley Phrase
nounPh = sconcat2 <$> detP <*> noun <|> noun


-- | verb-phrase
verbPh :: Earley Phrase
verbPh = sconcat2 <$> verb <*> advP <|> v_p_np
	where v_p_np = sconcat3 <$> verb <*> prepP <*> nounPh 


--------------------
-- Parse Sentence --
--------------------

sentence :: Earley SentenceS
sentence = sconcat2 <$> nounPh <*> verbPh


-----------
-- Utils --
-----------

sconcat2 :: String -> String -> String
sconcat2 a b = a ++ " " ++ b 

sconcat3 :: String -> String -> String -> String
sconcat3 a b c = a ++ " " ++ b ++ " " ++ c


-----------
-- Tests --
-----------


-- | map parser over all possible tests for parser
-- | choose to display result of parser or unparsed portions
testall :: Functor f => Earley b1 -> f [Char] -> ((b1, [Char]) -> b) -> f b
testall parser list g = (\a -> g $ (runEarley parser a ) !! 0 ) <$> list

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


-- Test nonterminal-nouns --
n1, n2, n3, n4, n5, n6, n7, n8, n9, n10 :: Words

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

ns :: [Words]
ns = [n1,n2,n3,n4,n5,n6,n7,n8,n9,n10]

-- Test nonterminal-verbs --
v1, v2 :: Words
v1 = "lift"    				-- v
v2 = "lift steve"			-- v np, where np = n', n' = n

vs :: [Words]
vs = v1 : v2 : ((\np -> "lift " ++ np) <$> nps)


-- Test sentences --











