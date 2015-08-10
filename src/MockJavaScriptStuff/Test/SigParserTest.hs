---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Test Parser
-- | Creator: Xiao Ling
-- | Created: November 17th
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Play3Test where 

import Control.Monad
import Control.Applicative
import Control.Monad.Reader

import Data.Maybe
import Data.Monoid
import Data.Dynamic

import Text.Show.Functions

import ParserT
import Play3


{-----------------------------------------------------------------------------
	Test DataType
------------------------------------------------------------------------------}

a :: SigAlpha
a = Po 'a' Nothing

ma :: SigAlpha
ma = Pa 'm' (Just "Monad") a

fma :: SigAlpha
fma = Pa 'f' (Just "Functor") ma

--t0 = T (a,0,[])
--t1 = Send (a,0,[]) $ T (a,1,[])
--t2 = Send (ma,0,[]) $ T (fma, 1,[])


{-----------------------------------------------------------------------------
	Test Alphabet parser
------------------------------------------------------------------------------}

sym = pAlpha hash :: Parser Char SigAlpha

-- * have to distinguish between parametrized and unparameterized
hash   = [("f", "Functor"), ("m", "Monad"), ("t", "Traversable"),("n", "Monoid")]

--sas :: [SigAlpha]
sas = runP sym <$> sas' where 
	sas' = ["a", "n", "f a", "f (m (t a))", "f (m (t Char))", "[Char]", "[a]", "[f (m a)]", "(a,f (t (m Double)))",  "Char", "Bool", "Double", "()", "String"]

--sas' :: [SigAlpha]
sas' = runP sym <$> sas' where 
	sas' = ["Functor f", "Monad m", "Applicative f", "Traversable t"]


--tree0 :: [Tree Leaf]
--tree0 = fst . head . runP pSigFree <$> ["a -> a", "a -> Bool", "String -> f (t (m a)) -> a", "a -> (Bool -> String) -> () -> (a,t (m a))"]



{-----------------------------------------------------------------------------
	Test signature parser
------------------------------------------------------------------------------}

-- * typeclass constraint signature parser

-- * syntanx good, semantics good
tc1 = "Functor f => "
tc2 = "Monoid (f (m a)) => "
tc3 = "(Monoid (f (m a)), Functor f, Monad m) =>"

-- * syntax good, semantics bad
tc4 = "Functor (f a) => "
tc5 = "(Monad m, Monoid m) => "

-- * syntax bad, should not parse
tc6 = "Functor f a =>"
tc7 = "Monoid m (f a) =>"

tcs1 = runP pTcPred <$> [tc1,tc2,tc3,tc4,tc5]




{-----------------------------------------------------------------------------
	Test that could only if all functions in play3 was exported
------------------------------------------------------------------------------}

-- * Parameters * -- 

tenv :: TcDict
tenv = (\(a,b) -> (a,pure b)) <$> [("f", "Functor"), ("m", "Monad"), ("t", "Traversable"),("a", "Monoid"), ("f (m a)", "Monoid"), ("m a", "Monoid")]

cenv :: ClassPred
cenv = [("Functor", K 1), ("Monad", K 1), ("Monoid", K 0)]

cpred :: [ClassId]
cpred = [("Functor", K 1), ("Monoid", K 1), ("Xclass", K 1)]

-- * some tree 
t = let Right (x,_) = runP pSignature "a -> b -> (c -> d) -> Bool" in x


-- * test satConsist * -- 

a,c :: Alpha
a = let Right (x,_) = runP (pAlpha tenv) "m a" in x
c = let Right (x,_) = runP (pAlpha [("m", ["Monoid"])]) "m" in x

-- * test satAccu * -- 

s1 = satAccu cenv a  																					-- * pass
s2 = satAccu cenv x where x = let Right (x,_) = runP (pAlpha [("x", ["Monoid", "Xclass"])]) "x" in x 	-- * out of scope
s3 = satAccu cenv x where x = let Right (x,_) = runP (pAlpha [("f", pure "Functor")]) "f" in x 			-- * wrong kind



-- * test EqMatrix *

-- * Pairwise compare two `Right` nodes and build `EqMatrix`
aMatr :: (Alpha,X) -> (Alpha,X) -> EqMatrix
aMatr (p,x) (q,x') = catMaybes $ (\((c,k,_), (c',k',_)) -> if c == c' then Just ((x,k),(x',k')) else Nothing) <$> p `xSym` q 


af i = S 'f' i $ [("Functor", K 1)]  -- * f _
am i = S 'm' i $ [("Monad", K 1)]    -- * m _


ae i = S 'm' i $ [("Monad", K 2)]
a_   = Po . S 'a' 0 $ [("Monoid", K 1)]

a0 = Po . S 'b' 0 $ [] 				 -- * b
a1 = Po . S 'a' 0 $ [("Monoid", K 0)] 				 -- * a
a2 = Pa (af 1) [] a1				 -- * f a
a3 = Pa (af 2) [] a2				 -- * f (f a)
a4 = Pa (am 1) [] a1 				 -- * m a
a5 = Pa (am 1) [] a0				 -- * m b
a6 = Pa (am 2) [] a2 				 -- * m (f a)
a7 = Pa (ae 0) [] a_ 				 -- * m a   where both m and a have wrong kind

a11 = aMatr (a1,0) (a1,1)   		 -- * a, a
a12 = aMatr (a1,0) (a2,1)   		 -- * a, f a
a13 = aMatr (a1,0) (a3,1)   		 -- * a, f (f a)
a22 = aMatr (a2,0) (a2,1)   		 -- * f a, f a
a23 = aMatr (a2,0) (a3,1)   		 -- * f a, f (f a)
a25 = aMatr (a2,0) (a5,1) 			 -- * f a, m b
a36 = aMatr (a3,0) (a6,1)    	     -- * f (f a), m (f a)


-- * functions that was rolled up into bigger ones * --

























