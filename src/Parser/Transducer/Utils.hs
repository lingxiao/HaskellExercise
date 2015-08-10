
module Utils where 


import Data.List
import Data.Function
import Data.Set
import qualified Data.Map as M


rmvDup :: (Ord t) => [t] -> [t]
rmvDup = toList . fromList


infixr 1 <||>
(<||>) :: [a] -> [a] -> [a]
(<||>) (x:xs) _ = x:xs
(<||>) [] l2    = l2


equate :: Eq b => (a -> b) -> a -> a -> Bool
equate = on (==)


pluckL :: [a] -> Int -> Maybe (a, [a])
pluckL xs i = case splitAt i xs of
    (b, v:e) -> Just (v, b ++ e)
    _ -> Nothing


groupVals :: Ord a => [(a,b)] -> [(a,[b])]
groupVals ls = M.toList . M.fromListWith (++) . fmap (\(x,y) -> (x,[y])) $ ls



fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b


thd3 :: (a,b,c) -> c
thd3 (_,_,n) = n


flipTuple :: (a,b) -> (b,a)
flipTuple (a,b) = (b,a)
