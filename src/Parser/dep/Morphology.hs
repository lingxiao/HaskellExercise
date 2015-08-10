{-
	sources: http://spraakbanken.gu.se/personal/markus/publications/FM_ICFP2004.pdf

-}

module Morphology where

import Data.Char
import Control.Monad
import Control.Monad.State 
import Control.Applicative 

import Parser
import ParserCombinator
import SyntaxTree
import SimpleSyntax
import Utils

----------------
-- Data Types --
----------------

type Word     = String
type Morpheme = String


----------------------------
-- Attribute-Value Matrix --
----------------------------

-- this is redundant and unwiedly and introduce two potential source of truth
-- somehow should get rolled up into syntax tree representation
data POS = V | N | De | Pre | Av | Aj
	deriving (Eq,Show)

-- | This Needs to be an Abstract data type
-- | Need to have associated functions to update the feature matrix
-- | Instead of get/put hack
-- | Need to have associated function to do union of two FMs

-- | Important question: how is this used in context of Earley parser?
-- | Alternate design, root is Verb | Noun | .., categ is a combinator that reads 
-- | constructor of root and gives POS info

-- | Consider describing this w/ some type of functor
data FeatureMat =  FM { categ :: POS, root :: String, num :: Int }
	deriving (Eq,Show)


-- update function


-- union of two fm

--------------------------
-- Finite State Machine --
--------------------------


-- | this is a specific soln to this prob at hand,
-- | Should create general fsa and compose w/ parser somehow
type FSA r = StateT FeatureMat (Parser Char) r


runFsa :: FSA a -> FeatureMat -> String -> [(FeatureMat, String)]
runFsa fsa fm str = runParser ( execStateT fsa fm) str 


-- some tests -- 

-- | Update feature matrix for each word parsed
updateFM :: FSA ()
updateFM = do 
	x <- lift pa
	case x of 
		"hello" -> put ( FM V "updated" 2 ) >> return ()
		"world" -> return ()


fm :: FeatureMat
fm = FM V "hello" 1

pa :: Parser Char String
pa = word "hello" <|> word "world"



























------------------
---- Word Roots --
------------------

--wordBank :: [Word]
--wordBank = ["fish", "cat"]


-------------------
---- Word Parser --
-------------------

--rootP :: Parser Char Word
--rootP = choice . fmap ( betweenS . word ) $ wordBank


----------------------
---- FSA Components --
----------------------

---- | baby talk
--babyP1 :: Parser Char Morpheme
--babyP1 = word "ie"

--babyP2 :: Parser Char Morpheme
--babyP2 = word "gie"

---- | Plural
--plural1 :: Parser Char Morpheme
--plural1 = word "s"


------------
---- FSA  --
------------

----fsa1 :: Parser Char Word
----fsa1 = (++) <$> rootP <*> babyP1


-------------
---- Tests --
-------------


















