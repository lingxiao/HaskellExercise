{-# Language RankNTypes, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances  #-}

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Lambda Cube Classes
-- | Creator: Xiao Ling
-- | Created: January 13th, 2014
-- | Eventually: http://www.cse.unsw.edu.au/~chak/haskell/term-conv/
-- | edited from emacs 
-- | 

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Core where

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Either
import qualified Control.Monad.State as S
import Control.Applicative 
import Data.List


{-----------------------------------------------------------------------------
	I. types 
------------------------------------------------------------------------------}

-- * type synonyms * -- 
  
type Err  = String 			-- * error message
type Id   = String 			-- * variable name
type Idx  = Int 			-- * de-brujin's index
data Info = Rec Id String 	        -- * store originAl name and extra information for debugging
	deriving (Eq,Show)

-- * Computations * -- 

-- * labeling variables with de-brujin's index
type DeBrujin t    = forall m. (Monad m, Applicative m) => ReaderT [Id] m t

-- * Type checker that outputs type annotation `t`, with effect `m`
type TypeChkr t    = forall m. (Monad m, Applicative m) => ReaderT [t] (EitherT Err m) t

-- * Effectful interpreter running some AST `t` with sideffects `m`
type Interpreter t = forall m. Monad m			=> StateT [t] m t

{-----------------------------------------------------------------------------
	II. TypeClasses
------------------------------------------------------------------------------}

                     
-- * Bijection between a and b such that id := decode . encode
-- * Note: This type class is tied to the Computation `DeBrjujin`, is this too restricting?
class DeBrujinPair a b | a -> b, b -> a where 
	label   :: a -> DeBrujin b
	decode  :: b -> a
	encode  :: a -> b
	encode = runIdentity . flip runReaderT [] . label 


{-----------------------------------------------------------------------------
	III. Distinguished Computations
------------------------------------------------------------------------------}

-- * Log current state of Term
trace :: (S.MonadState [a] m) => a -> m ()
trace = S.modify . (:) 

-- * Annotate some named variable with Debrujin index, if variable is in scope
mrk :: Id -> (Int -> t) -> DeBrujin t
mrk x g = ask >>= \en -> case elemIndex x en of 
	Just i  -> return . g $ i 
	Nothing -> fail $ "Error: variable " ++ x ++ " not in scope"

{-----------------------------------------------------------------------------
	IV. Utils
------------------------------------------------------------------------------}

nofo :: Info
nofo = Rec "" ""

liftEither :: Monad m => Either a b -> EitherT a m b 
liftEither (Left e)  = left e
liftEither (Right r) = return r

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d 
mapEither f g e = case e of 
	Left a  -> Left $ f a
	Right b -> Right $ g b

maybeEither :: Maybe a -> (a -> b) -> c -> Either c b 
maybeEither (Just a) g _ = Right $ g a
maybeEither Nothing _  b = Left b


