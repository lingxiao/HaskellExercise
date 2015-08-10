{-# LANGUAGE ImpredicativeTypes #-} {-# LANGUAGE NoMonomorphismRestriction #-}
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Play with bad solution part iii
-- | Creator: Xiao Ling
-- | Created: November 17th
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module SigParser2 ( 

	sigParser

) where 

import Control.Monad
import Control.Monad.Reader
import Control.Applicative

import Data.List 
import Data.Maybe
import Data.Monoid 
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Show.Functions

import ParserT
import ParserCombinator
import ParserUtils
import Core2

-- * Try and make this significantly shorter

{---------------------------------------------------------------------------------
	I. Data Type and Functions Servicing the Core
----------------------------------------------------------------------------------}

-- * Map some singature unit x to a list of typeclass x maybe drawn from
type TcDict = [(Signature,[ClassName])]

{----------------------------------------------------------------------------------
	II. Alpha Parser :: Signature -> Alpha
-----------------------------------------------------------------------------------}


{-----------------------------------------------------------------------------------
	III. Signature Parser :: Signature -> SigTree
------------------------------------------------------------------------------------}



{-----------------------------------------------------------------------------------
	IV. Alpha Analyzer :: Alpha -> Maybe Error
------------------------------------------------------------------------------------}


{-----------------------------------------------------------------------------------
	V. Signature Analyzer :: SigTree -> Either Error SigTree
------------------------------------------------------------------------------------}

{------------------------------------------------------------------------------------
	VI. Build Equality Constraint  :: SigTree -> EqConstr
-------------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------------
	VII. Parse, Analyze, eqMatrix :: Signature -> (SigTree, EqConstr)
--------------------------------------------------------------------------------------}

-- * Main Parser/Analyzer computation
sigParser :: ParserT Char (ReaderT ClassPred (Reader (AdtEnv a))) (SigTree, EqMatrix)
sigParser = undefined


