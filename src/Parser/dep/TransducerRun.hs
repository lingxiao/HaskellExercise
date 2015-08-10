---------------------------------------------------------------------------------------------------
-- | Weighted Finite State Transducer Combinators

-- | Figure out how to run non-determistic transitions -> is it monadic?

-- | reimplement using machines -> learn machines first


-- | Proposed solutions: http://stackoverflow.com/questions/17171147/how-do-you-use-the-list-monad-to-compute-represent-the-outcome-of-a-non-determin
-- | SO primer on nondterministic state monad: http://stackoverflow.com/questions/13843687/how-can-i-build-a-nondeterministic-state-monad-in-haskell
---------------------------------------------------------------------------------------------------


module TransducerRun where 

import Control.Applicative
import Control.Monad
import Control.Proxy
import Control.Proxy.Trans.State
import Control.Proxy.Trans.Writer
import Control.Monad.Writer

import Data.Sequence
import Data.Monoid
import Data.Maybe

import Utils
import TransducerPrimitives
import TransducerGraph
import Transducer
import RoseTree



-----------------------------------------------
----------- fun w/ proxies -----------
-----------------------------------------------


incr :: (Proxy p) => () -> Consumer (StateP Int p) Int IO r
incr () = forever $ do
   nOurs   <- get
   nTheirs <- request ()
   lift $ print (nTheirs, nOurs)
   put (nOurs + 2)


incr2 :: (Proxy p, Monad m) => () -> Consumer (StateP Int p) Int m r
incr2 () = forever $ do
   nOurs   <- get
   nTheirs <- request ()
   put (nOurs + 2)


incr3 :: (Proxy p, Monad m) => () -> Consumer (StateP [x] p) x m r
incr3 () = forever $ do
  xs <- get
  x  <- request ()
  put $ x:xs



sesh2 xs = fromListS xs >-> incr
test2 = runProxy $ runStateK 100 $ sesh2 [1..4]

sesh3 xs = fromListS xs >-> incr2
sesh4 xs = fromListS xs >-> incr3
sesh5 xs = fromListS xs >-> incr4









type Key a = (Tstate, Input a)
type Value b = [(Tstate, Output b, Prob)]
type TransitionTuple a b = (Key a, Value b)


-----------------------------------------------
----------- run with tree data type -----------
-----------------------------------------------

-- | simple impl -> does not allow interleaving of effects in nondeterministic cases
-- | does not branch computation for nondeterministic transitions
runSimple :: WeightedTransducer a b -> [Input a] -> [Value b]
runSimple t = foldr (genlist t) [] 


genlist t a []   = run t (0,a) : []
genlist t a past = undefined 
--let (s2,_,_) = (head . head) past in run t (s2,a) : past


------ nondeterminsitic run implemented with rose tree ------

-- | The result is explicitly represented as a tree, it's kind of a
-- | training wheel/scaffold towards a better solutions

{-
  
step1 -> determinsitic         
      [val1]
step2 -> determinsitic          
      [val2,val1]
step3 -> non-determined, 1 eps   
      [[val3,val2,val1],[val3,val2,val1]] = [scenario1, scenario2]
step4 -> non-determined, 1 eps, 1 reg symbole -> get (value1,value2)
      [[value1:scenario1, value1:scenario2], [value2:scenario1, value2:scenario2]]


context:  several history leading to present
function: cons present onto history

can be respented with a tree

read input 
    | does not match arc input s0 <- currentStates -> fail
    | match one arc input from s0 <- currentStates -> cons output to all edge of tree >> read input
    | match > 1 arc input from s0 <- currentStates -> 


-}

-- | Non determinisic -> no interleaving of effects
-- | note cannot reuse runSimple since rosetree is not monoid -> though in this case it doesnt matter
--runNond :: WeightedTransducer a b -> [Input a] -> RoseTree (Value b)
--runNond t = foldr (gentree t) Bud


runNondet :: (Eq a, Eq b) => WeightedTransducer a b -> [Input a] -> RoseTree (Key a, Value b)
runNondet t inputs = foldl (\tree sym -> gentree t sym tree) Bud inputs 


{-
  Better abstraction:
  build a nondeterminstic branching machine where the data is stored as tree
  the client can `read` current states and `step` to the next step according to current state
  -> basically a better non-determistic finite state transducer abstraction
-}
-- | full of leaky abstraction and could be simplified w/ some sensible tree combinators
gentree :: (Eq a, Eq b)=> WeightedTransducer a b -> Input a ->  RoseTree (Key a, Value b) -> RoseTree (Key a, Value b)
gentree wfst a Bud = let p = (0, a) in pure $ (p, run wfst p)
gentree wfst a tree  = tree'
  where 
      ls'   = join $ (\l@(k,vs) -> [ let p = (s,a) in (l,(p,run wfst p)) | (s,_,_) <- vs, run wfst (s,a) /= [] ]) <$> terminals tree
      tree' = foldr (\(k, vs) tree -> fromJust . grow k (toTuples vs) $ tree) tree ls'

-- test runnondet
t3 = runNondet t $ Sym <$> [A, A, B, C]



{-

as suggested by stackoverflow

cannot adapt to graph

-}
type NonDet w a = WriterT (Seq w) [] a


runMach m t = runWriterT $ m t


mark :: w -> NonDet w ()
mark = tell . singleton


(#>) :: w -> NonDet w a -> NonDet w a
(#>) x = (mark x >>)


-- | traverse tree | --
data Tree a = Leaf a | Bin (Tree a) (Tree a)
data Direction = L | R deriving (Show, Read, Eq, Ord, Enum, Bounded)


-- | here th branches are labled and is known
-- | tree w/ arb many braches? fold over branchs and mplus
-- | a graph 
travelT :: Tree a -> NonDet Direction a
travelT (Leaf x)  = return x
travelT (Bin l r) = (L #> travelT l) `mplus` (R #> travelT r)


a = runMach travelT $ Leaf 1
b = runMach travelT $ Bin (Bin (Leaf "a") (Leaf "b")) (Leaf "c")


-- | traverse graph | --
travelG :: WeightedTransducer a b -> NonDet () b
travelG = undefined

-----------------------------------------------
------------ run with list of WFST ------------
-----------------------------------------------

-- | eventually step should be part of WFST data type, for now just an external function
-- | Given transducer 
step :: WeightedTransducer a b -> a -> [(WeightedTransducer a b, b)]
step t a = undefined




---------------------------------------------
-------------------- Check ------------------
---------------------------------------------


-- test non-determinstic run --
table = toTable [(0,1,A,B,1),(1,2,A,B,1),(1,3,A,C,1),(2,4,B,B,1),(3,5,C,C,1)] :: TransitionTable Alpha Alpha
t     = toTransducer [4,5] table                                              :: WeightedTransducer Alpha Alpha


r0 :: RoseTree (Key Alpha, Value Alpha) 
r0 = Node ((0,Sym A),[(1,Sym B, 1)]) []                                        
r1 :: RoseTree (Key Alpha, Value Alpha)
r1 = Node ((0, Sym A), [(1, Sym B, 1)]) [ Node ((1,Sym A),[(2,Sym B,1)]) [], Node ((1,Sym A), [(3,Sym C,1)]) []]  







-- Transducers --

table1 = toTable [(0,1,A,A,1),(0,2,B,B,1),(2,3,C,C,1),(3,2,B,B,1)]  :: TransitionTable Alpha Alpha
t1     = toTransducer [1,3] table1                                  :: WeightedTransducer Alpha Alpha

table2 = toTable [(0,1,A,A,1),(1,0,B,B,1)]                          :: TransitionTable Alpha Alpha
t2     = toTransducer  [1] table2                                   :: WeightedTransducer Alpha Alpha

as = Sym <$> [A,B]


















{-

     Upstream | Downstream
         +------------+
         |            |
 ()     <==          <== ()
         |  runTrans  |
 input  ==>           ==> (state,output,prob)
         |            |
         +------------+

-- | Allow interleaving effects
runTrans :: (Monad m, Proxy p) => WeightedTransducer a b -> () -> Pipe (StateP Tstate p) (Symbol a) (Tstate,Symbol b, Prob) m r
runTrans t () = forever $ do
  input <- request ()
  s0    <- get
  let val@(s1,_,_) = run t (s0,input)
  put s1
  respond val



 -- | A diagnostic tool 
logger :: (Show x, Proxy p) => () -> Consumer p x IO r 
logger () = runIdentityP . forever $ do
  x <- request ()
  lift . print $ "Log: "
  lift . print $ x
  lift . print $ "******************"


    Upstream | Downstream
         +------------+
         |            |
 ()     <==          <== ()
         |   record   |
 input  ==>          ==> ()
         |            |
         +------------+
record :: (Monad m, Proxy p) => () -> Consumer p x m r
record () = undefined


sesh1 :: (Proxy p, Show a, Show b) => WeightedTransducer a b -> [Input a] -> () -> Session (StateP Tstate p) IO ()
sesh1 t as = fromListS as >-> runTrans t >-> logger


runSesh :: Monad m => s -> (() -> StateP s ProxyFast a' () () b m r) -> m (r, s)
runSesh st session = runProxy $ runStateK st $ session

test = runSesh 0 $ sesh1 t2 $ Sym <$> [A,B]


-}








