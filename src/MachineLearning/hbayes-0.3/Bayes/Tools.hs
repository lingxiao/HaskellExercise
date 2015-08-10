{- | Miscellaneous tools
	
-}
module Bayes.Tools (
	nearlyEqual
	) where 



-- | Floating point number comparisons which should take into account
-- all the subtleties of that kind of comparison
nearlyEqual :: Double -> Double -> Bool
nearlyEqual a b = 
    let absA = abs a 
        absB = abs b 
        diff = abs (a-b)
        epsilon = 2e-5
    in
    case (a,b) of 
        (x,y) | x == y -> True -- handle infinities
              | x*y == 0 -> diff < (epsilon * epsilon)
              | otherwise -> diff / (absA + absB) < epsilon