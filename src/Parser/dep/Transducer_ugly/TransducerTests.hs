---------------------------------------------------------------------------------------------------
-- Weighted Finite State Transducer Tests
---------------------------------------------------------------------------------------------------


import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.List

import Utils
import RoseTree
import Transducer
import TransducerTypes
import TransducerGraph
import TransducerFunctions



----------------------------------------------------------------
------- figure out how to integrate transducer w/ parser  ------
----------------------------------------------------------------

-- | this is determining the functions in TransducerFunctions.hs

-- | Top level: parser consume word and output a list of possibilities 
-- | the parser then branch the computation based on each possibility
-- | give a list of sentences tagged by pos
-- | prune branches that didnt parse all words in sentence

-- | testable bit: parser consume one word and output list of possible tags
-- | question: integration of word parser with sentence parser? should be the same parser


-- | problem -> need to integrate into parser at a character level

-- | "undo" -> "u" => "n" -> Sym "un" -> "d" => "o" -> Sym "do" -> WFST -> [[]]




-----------------
-- Graph Tests --
-----------------

-- | Arcs
a1 = toArc (0,3,Eps,Eps,1)
a2 = toArc (3,4,Eps,Eps,1)
a3 = toArc (2,1,Eps,Eps,1)


-- | graphs
ga1 =  toArc (0,1,Sym A,Sym B,0.5)
ga2 =  toArc (1,2,Sym B,Sym C,0.5)
ga3 =  toArc (2,3,Sym B,Sym C,0.5)
g   =  [ga1,ga2, ga3] :: TransitionGraph Alpha Alpha



-- | merge graph test | --

-- | These tests also indirectly test splitGraph function

-- | Case0 extend w/ trivial arcs
g0 = mergeGraph [] g                          :: Maybe (TransitionGraph Alpha Alpha)

-- | case1 extend w/ arc from initial
g1 = mergeGraph [a1] g                        :: Maybe (TransitionGraph Alpha Alpha)

-- | case2 extend w/ arc from final
g2 = mergeGraph [a2] g                        :: Maybe (TransitionGraph Alpha Alpha)

-- | case 3 extend w/ arc from state somewhere in middle
g3 = mergeGraph [a3] g                        :: Maybe (TransitionGraph Alpha Alpha)

-- | case 4 extend w/ many arcs, all connected to graph and each other
g4 = mergeGraph [a2, toArc(3,2,Eps,Eps,1)] g              :: Maybe (TransitionGraph Alpha Alpha)

-- | case 5 extend w/ many arcs, all connected to graph, but not to each other
g5 = mergeGraph [a1,a3] g                       :: Maybe (TransitionGraph Alpha Alpha)

-- | case 6 extend w/ many arcs, some connected to graph, but all to each other
g6 = mergeGraph [a1, toArc(3,5,Eps,Eps,1)] g              :: Maybe (TransitionGraph Alpha Alpha)

-- | case 7 extend w/ many arcs, some connected to graph, some to each other, but have left overs
g7 = mergeGraph [a1,toArc(3,5,Eps,Eps,1),toArc(9,10,Eps,Eps,1)] g  :: Maybe (TransitionGraph Alpha Alpha)

-- | case 8 extend w/ many arcs, none connected to graph
g8 = mergeGraph [toArc(9,10,Eps,Eps,1), toArc(10,11,Eps,Eps,1)] g  :: Maybe (TransitionGraph Alpha Alpha)



-------------------------------
------- Transducer tests ------
-------------------------------

-- test --
table0 :: TransitionTable Alpha Alpha
table0 = (2, 4, Eps, Eps, 1) : toProbTable [(0,1,A,B,1),(1,2,A,B,1),(1,3,A,C,1),(2,4,B,B,1),(3,5,C,C,1)]
t0 :: WeightedTransducer Alpha Alpha
t0     = toTransducer [4,5] table0   


table1 = toProbTable [(0,1,A,B,1),(0,2,B,B,1),(2,3,C,C,1),(3,2,B,B,1)]  :: TransitionTable Alpha Alpha
t1     = toTransducer [1,3] table1                                      :: WeightedTransducer Alpha Alpha

table2 = toProbTable [(0,1,B,C,1),(1,0,B,B,1)]                          :: TransitionTable Alpha Alpha
t2     = toTransducer  [1] table2                                       :: WeightedTransducer Alpha Alpha


------------------------------------------
------- run prelim morphology tests ------
------------------------------------------


type WordPart = String

data Morphemes = Root String | Baby | NumPl | NumSing 
  deriving (Show, Eq, Ord)


{-

  Goal -> a morphological parser 

  "unlikely" -> [Sym "un", Sym "like", Sym "ly"] -> [Inflextion + Verb + ToAdj]
  "fish"     -> [Sym "fish", Eps]             -> [Noun, Noun + Plural]
  "fishy"    -> [Sym "fish", Sym "y"]        -> [Noun + Baby]
  "fishies"  -> [Sym "fish", Sym "ie", Sym "s"]  -> [Noun + Baby + Plural]

-}

morph1 :: WeightedTransducer WordPart Morphemes
morph1 = toTransducer [1] $

  (8,2, Eps, Sym Baby, 1.0)  : toTable (
  
  (0,3, "fish", Root "fish")  : 
  (0,4, "bird", Root "bird")  :
  (0,5, "cat",  Root "cat" )  : 
  (0,6, "frog", Root "frog")  : 
  (0,7, "dog",  Root "dog" )  : 
  (0,8, "kittie",Root "cat" ) : 

  (3,2, "ie",  Baby  ) : 
  (4,2, "ie",  Baby  ) : 
  (6,2, "gie", Baby  ) : 
  (7,2, "gie", Baby  ) : 

  (4,1,"s", NumPl) :
  (5,1,"s", NumPl) :
  (6,1,"s", NumPl) :
  (7,1,"s", NumPl) :

  (2,1,"s", NumPl) : [] )

evalm = logTrans morph1

mt1 = evalm $ Sym <$> ["fish"]
mt2 = evalm $ Sym <$> ["fish", "ie"]
mt3 = evalm $ Sym <$> ["fish", "ie", "s"]
mt4 = evalm [Sym "kittie", Eps]
mt5 = evalm [Sym "kittie", Eps, Sym "s"]
mt6 = evalm $ Sym <$> ["dog", "gie"]
mt7 = evalm $ Sym <$> ["dog", "gie", "s"]

-- | note in eval machine the "ie" is skipped - machine does not detect error
mt8 = evalm $ Sym <$> ["frog", "ie", "s"]














{-
data Parts = Root | Prefix | Suffix 
  deriving (Show, Eq, Ord)

mtable = (2,5,Eps,Eps,0.7) : (4,6,Eps,Eps,0.5) : toProbTable [(0,1,"un", Prefix,1),(1,2,"like", Root,1),(2,3,"ly", Suffix,1),(1,4,"do", Root,0.3)]
mt     = toTransducer [5,6,3] mtable


mt1 = (Sym <$> ["un","do"]) ++ [Eps]
mt2 = (Sym <$> ["un","like"]) ++ [Eps]
mt3 = Sym <$> ["un","like","ly"]


-}




{-

DERPCIATED RUN TRANSDUCER FUNCTIONS 

-- | Generic run function for arbitrary functor that store result of step function
runAll :: Functor f => (f (History a b) -> Input a -> f (History a b)) -> f (History a b) -> [Input a] -> f (Output b, Prob)
runAll stepFST start inputs = (\(b,p,_) -> (b,p)) <$> foldl stepFST start inputs 


-- | Run and forget history
-- | The final probability is multiplied for all steps
exec :: WeightedTransducer a b -> [Input a] -> [(Output b, Prob)]
exec wfst (a:as) = runAll stepFST (step wfst a 1.0) as
  where stepFST past a = join [ step t a p | (_,p,t) <- past ]


-- | Run and remember history 

-- | NOTE: This should return maybe (rosetree a) in case nonallowed sequences are passed



-- | note the transition probabilites does not accumlate down the tree
-- | Thus to calculate the prob of a particular leaf, traverse the tree down to leaf and multiply all probs

-- | consider alternate data structure since self-rolled rose tree is a hack
eval :: (Eq a, Eq b) => WeightedTransducer a b -> [Input a] -> RoseTree (Output b, Prob)
eval wfst = runAll (stepFST wfst) Bud
  where
    -- | this computation should escape early if bad input read
    stepFST wfst Bud  a = foldr rappend Bud $ pure <$> step wfst a 1.0
    stepFST wfst tree a = foldr (\(k,vs) t -> case grow k vs $ t of 
        Just nt -> nt 
        Nothing -> t
      ) tree $ [ (p, step t a 1.0) | p@(_,_,t) <- ends tree ]



-}
























