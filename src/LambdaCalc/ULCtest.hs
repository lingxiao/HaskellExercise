---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Test Pure un-typed LamUnbda calculus
-- | Creator: Xiao Ling
-- | Created: January 8th, 2014
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module UntypedLamUnTest where

import Data.Monoid
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Identity
import Control.Applicative 
import Data.Maybe
import Data.List
import Core
import UntypedLam


{-----------------------------------------------------------------------------
	I. Tests: basic
------------------------------------------------------------------------------}

-- * Strings of λ calculus * --

[sf,sx,sy,sz] = VarUn <$> ["f", "x","y","z"]		:: [TermUn]

sI,sY,sK :: TermUn 		
sI   = LamUn "x" $ VarUn "x"												      -- * λx. x
sS   = LamUn "x" $ LamUn "y" $ sx 											      -- * λx. λy. x
sY   = LamUn "f" (AppUn r r) where r = LamUn "x" (AppUn sf $ AppUn sx sx) 		  -- * λf. (λx.f (x x)) (λx.f (x x))
sK   = LamUn "x" $ LamUn "y" $ LamUn "z" $ AppUn (AppUn sx sz) (AppUn sy sz)      -- * λx. λy. λz. (xz)(yz)
sxy  = LamUn "x" $ LamUn "y" $ AppUn (VarUn "x") (VarUn "y") 		    		  -- * λx.λy.xy
sfxy = LamUn "f" $ AppUn (LamUn "x" $ AppUn sf sx) (LamUn "y" $ AppUn sf sy )     -- * λf. (λx. f x) (λy. f y)
 

{-----------------------------------------------------------------------------
	II. Tests: Church Encoding, Booleans, Lists
------------------------------------------------------------------------------}

-- * test this bit of code
c0,c1,scc,plus :: TermUn
c0  = LamUn "s" $ LamUn "z" (VarUn "z")   									        -- * λs.λz. z
c1  = LamUn "s" $ LamUn "z" (AppUn (VarUn "s") $ VarUn "z")						    -- * λs.λz. sz
scc = LamUn "n" $ LamUn "s" $ LamUn "z" $ AppUn s nsz where							-- * λn.λs.λ.z s (n s z)
	nsz     = AppUn (AppUn (VarUn "n") (VarUn "s")) (VarUn "z") 
	[n,s,z] = VarUn <$> ["n","s","z"]

-- * λm. λn. λs. λz.ms(nsz)
plus = LamUn "m" $ LamUn "n" $ LamUn "s" $ LamUn "z" $ msnsz where 
	msnsz = AppUn (AppUn (VarUn "m") (VarUn "s")) nsz
	nsz   = AppUn (AppUn (VarUn "n") (VarUn "s")) (VarUn "z")

add :: TermUn -> TermUn -> TermUn
add m n = evalFull $ AppUn (AppUn plus m) n 			-- * this function does not terminate under full evaluation !!??

suc :: TermUn -> TermUn
suc m = evalFull $ AppUn scc m

-- * arithmetic * -- 

-- * note c1' reduce to λsz. (λz.z)(sz), but since (sz) cannot be reduced further
-- * it cannot be substituted into (λz.z), thus the function hits a fixed point "one step early"
c4   = suc . suc . suc . suc $ c0  		
c1'  = add c1 c0  		 			
c1'' = add c0 c1 						
c2'  = suc c1 `add` suc c1 			



{-----------------------------------------------------------------------------
	III. Test Non-exported functions of UntypedLam.hs
------------------------------------------------------------------------------}

{-
	-- * Strings of λ calculus * --

	[sf,sx,sy,sz] = VarUn <$> ["f", "x","y","z"]		:: [TermUn]

	sI,sY,sS,sK :: TermUn 		
	sI   = LamUn "x" $ VarUn "x"											          -- * λx. x
	sS   = LamUn "x" $ LamUn "y" $ sx 										          -- * λx. λy. x
	sY   = LamUn "f" (AppUn r r) where r = LamUn "x" (AppUn sf $ AppUn sx sx) 	      -- * λf. (λx.f (x x)) (λx.f (x x))
	sK   = LamUn "x" $ LamUn "y" $ LamUn "z" $ AppUn (AppUn sx sz) (AppUn sy sz)      -- * λx. λy. λz. (xz)(yz)
	sfxy = LamUn "f" $ AppUn (LamUn "x" $ AppUn sf sx) (LamUn "y" $ AppUn sf sy )     -- * λf. (λx. f x) (λy. f y)

	[zero,one,two] = flip VarI nofo <$> [0..2] 			:: [TermIn]


	-- * Debrujin variable renaming * -- 
	[sI',sS',sY',sK',sfxy'] = encode <$> [sI,sS,sY,sK,sfxy]

	-- * shift * -- 

	-- * shift 2 0 (λ.λ.1(02)) = λ. λ. (1 (0 4))
	sh1 :: TermIn
	sh1 = shift 2 0 $ LamI (LamI (AppI one (AppI zero two nofo) nofo) nofo) nofo 									

	-- * shift 2 0 (λ.01(λ.012)) = λ. ((0 3) λ. ((0 1) 4))
	sh2 :: TermIn
	sh2 = shift 2 0 $ LamI (AppI (AppI zero one nofo) (LamI (AppI (AppI zero one nofo) two nofo) nofo) nofo) nofo    

	-- * substitution * -- 

	-- * reduce expression: (λ.λ. 12) (λ. 210), manually evalUn reduced to substition of form
	-- * shift (-1) 0 $ (λ.12) [0 :-> shift 1 0 (λ. 210)]
	su0 :: TermIn
	su0 = decr $ subst a (0 :-> incr b) where
		a = LamI (AppI one two nofo) nofo
		b = LamI (AppI (AppI two one nofo) zero nofo) nofo


	-- * evaluation * -- 

	b12 :: TermIn
	b12 = (AppI s1 s2 nofo) where 
		s1 = LamI (LamI (AppI one two nofo) nofo) nofo
		s2 = LamI (AppI (AppI two one nofo) zero nofo) nofo

	b12' = fst $ runIdentity $ flip runStateT [] $ byValue b12


-}
