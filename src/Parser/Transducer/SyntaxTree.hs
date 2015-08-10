
{-
	Created:   May 8th, 2013
	Reference: http://bergmair.eu/pub/nlpsum-rep.pdf
-}

module SyntaxTree (
	Verb (..),
	Noun (..), 
	Det (..), 
	Prep (..), 
	Adj (..), 
	Adv (..), 
	NounPhrase (..), 
	VerbPhrase (..), 
	Sentence (..)
	) where


-----------------------------
-- Syntax Tree Constructor --
-- ? What is a better way to build this ? --
-----------------------------

-- Parts of speech --
data Verb = V1 String | V2 Verb NounPhrase
	deriving (Eq, Show)

data Noun = N1 String | N2 Adj Noun | N3 Noun Prep NounPhrase
	deriving (Eq, Show)

data Det  = D String deriving (Eq, Show)
data Prep = P String deriving (Eq, Show)
data Adv  = Adv String deriving (Eq, Show)
data Adj  = Adj String deriving (Eq, Show)

---- Phrases --
data NounPhrase = NP1 Noun | NP2 Det Noun
	deriving (Eq, Show)

data VerbPhrase = VP2 Verb Adv | VP3 Verb Prep NounPhrase
	deriving (Eq, Show)

-- Sentence -- 
data Sentence = S NounPhrase VerbPhrase
	deriving (Eq, Show)


-----------
-- Tests --
-----------


-- Syntax Tree --

d = D "the"         :: Det
p = P "over"        :: Prep
adv = Adv "quickly" :: Adv
adj = Adj "fast"    :: Adj

v1 = V1 "run"  	    :: Verb
v2 = V2 v1 np1

n1 = N1 "forrest"   :: Noun
n2 = N2 adj n1      :: Noun
n3 = N3 n1 p np1    :: Noun

np1 = NP1 n1 		:: NounPhrase
np2 = NP2 d n3 		:: NounPhrase

vp2 = VP2 v2 adv 	:: VerbPhrase
vp3 = VP3 v2 p np2 	:: VerbPhrase

s1  = S np1 vp2     :: Sentence
s2  = S np2 vp3     :: Sentence


-- | s2
s2' = S 
	(NP2 
		(D "the") 
		(N3 
			(N1 "forrest") 
			(P "over") 
			(NP1 
				(N1 "forrest")
			)
		)
	) 

	(VP3 
		(V2 
			(V1 "run") 
			(NP1 
				(N1 "forrest")
			)
		) 
		(P "over") 
		(NP2 
			(D "the") 
			(N3 
				(N1 "forrest") 
				(P "over") 
				(NP1 
					(N1 "forrest")
				)
			)
		)
	)	



