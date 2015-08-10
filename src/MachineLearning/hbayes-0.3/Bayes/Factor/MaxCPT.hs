{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{- | Implementation of Max-product factors for MAP queries

-}
module Bayes.Factor.MaxCPT(
	-- * Type
      MAXCPT 
    , mpeInstantiations
	) where 

import Bayes.Factor 
import Bayes.Factor.PrivateCPT
import Bayes.PrivateTypes
import qualified Data.Vector as V
import qualified Data.IntMap as IM
import Data.List(maximumBy)
import Data.Function(on)

--import Debug.Trace 

--debug a = trace (show a ++ "\n") a

instance FactorElement (Double,PossibleInstantiations) where 
    doubleValue = fst 
    mkValue x = (x,[])
    scale s (d,l) = (s*d,l)
    multiply (da,[]) (db,lb) = (da*db,lb)
    multiply (da,la) (db,[]) = (da*db,la)
    multiply (da,la) (db,lb) = (da*db,[xa ++ xb | xa <- la, xb <- lb])
    elementUnit = (1.0,[])

instantiationProba :: ((Double,PossibleInstantiations), DVISet) -> Double
instantiationProba (a,b) = fst a 

maximization :: [((Double,PossibleInstantiations), DVISet)] -> (Double,PossibleInstantiations)
maximization [] = elementUnit 
maximization l = 
    let ((d,possible),newInst) = maximumBy (compare `on` instantiationProba) l
        addTo [] l = l
        addTo i [] = [i]
        addTo i l = map (i ++) l
    in 
    (d,addTo newInst possible)

instance Factor (MAXCPT) where
    emptyFactor = _emptyFactor
    factorVariables = _factorVariables
    isScalarFactor = _isScalarFactor
    containsVariable = _containsVariable
    factorDimension = _factorDimension
    variablePosition = _variablePosition
    isUsingSameVariablesAs = _isUsingSameVariablesAs

    factorFromScalar = _factorFromScalar
    factorWithVariables = _factorWithVariables
    factorToList = _factorToList
    factorNorm = _factorNorm
    factorScale = _factorScale
    factorValue = _factorValue
    factorStringValue f d = show (privateFactorValue f d)

    evidenceFrom = _evidenceFrom

    factorProduct = _factorProduct
    
    factorProjectOut _ f@(Scalar v) = f
    factorProjectOut s f = cptFactorProjectOutWith maximization s f

mpeInstantiations :: MAXCPT -> [DVISet]
mpeInstantiations (Scalar (_,i)) = i 
mpeInstantiations _ = error "The final MAXCPT factor should be a scalar one"

instance Show MAXCPT where
    show (Scalar v) = "\nScalar Factor:\n" ++ show v
    show c@(Table [] _ v) = "\nEmpty CPT:\n"

    show c = displayFactorBody c  


