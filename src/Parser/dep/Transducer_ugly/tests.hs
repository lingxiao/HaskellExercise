---------------------------------------------------------------------------------------------------
--  Test Nondeterministic Finite State Transducer
---------------------------------------------------------------------------------------------------

import WFST
import TransducerPrimitives
import TransducerUtils


-----------------
-- Symbol Type --
-----------------


data Alpha = A | B | C | D  deriving (Eq,Ord,Show)
data Beta  = AA | BB | CC | DD   deriving (Eq,Ord,Show)


-----------------
-- Transducers --
-----------------

table1 = toTable [(0,1,A,A,1),(0,2,B,B,1),(2,3,C,C,1),(3,2,B,B,1)]  :: TransitionTable Alpha Alpha
t1     = toTransducer [[0]] [[1],[3]] table1                        :: WeightedTransducer Alpha Alpha

table2 = toTable [(0,1,A,A,1),(1,2,B,B,1),(2,1,C,C,1)]              :: TransitionTable Alpha Alpha
t2     = toTransducer  [[0]] [[2]] table2                           :: WeightedTransducer Alpha Alpha

table5 = toTable [(0,1,A,A,1),(1,0,B,B,1)]                          :: TransitionTable Alpha Alpha
t5     = toTransducer  [[0]] [[1]] table5                           :: WeightedTransducer Alpha Alpha

table6 = toTable [(0,0,C,C,1)]										:: TransitionTable Alpha Alpha
t6     = toTransducer [[0]] [[0]] table6 							:: WeightedTransducer Alpha Alpha

-----------------
-- Monoid laws --
-----------------

-- | identity and commutatity of (M,.+.zero) 
-- | tazr isomorphic to tazl to t2
tazr = t2 .+. zero :: WeightedTransducer Alpha Alpha
tazl = zero .+. t2 :: WeightedTransducer Alpha Alpha

-- | identity and commutatity of (M,.*., one)
-- | Passed test since two graphs are isomorphic to t2 and each other
tmor = t2 .*. one  :: WeightedTransducer Alpha Alpha
tmol = one .*. t2  :: WeightedTransducer Alpha Alpha


-------------------
-- Semiring laws --
-------------------

-- | zero annilate .*.
-- | passed test by definition
tzml = zero .*. t1  			  :: WeightedTransducer Alpha Alpha
tzmr = t1 .*. zero  			  :: WeightedTransducer Alpha Alpha

-- | Distributiveness -> passed though hard to see equivalence by eye 
ta = t6 .*. (t5 .+. t5)           :: WeightedTransducer Alpha Alpha
tb = (t6 .*. t5) .+. (t6 .*. t5)  :: WeightedTransducer Alpha Alpha


-----------
-- Other --
-----------

-- | Check corresponds to graphical representation | --

-- | graphical example found at http://tagh.de/tom/wp-content/uploads/fsm_unweigtedautomata.pdf slide 27
tu = t1 .+. t2    :: WeightedTransducer Alpha Alpha
tc = t1 .*. t2    :: WeightedTransducer Alpha Alpha


-- | check homorphism of .+. and one
tor = t2 .+. one  :: WeightedTransducer Alpha Alpha
tol = one .+. t2  :: WeightedTransducer Alpha Alpha 





