{-# LANGUAGE TemplateHaskell #-}
---------------------------------------------------------------------------------------------------
--  https://github.com/ekmett/lens
--  http://www.scs.stanford.edu/11au-cs240h/notes/zipper.html
--  https://www.fpcomplete.com/school/pick-of-the-week/basic-lensing
---------------------------------------------------------------------------------------------------


import Language.Haskell.TH
import Control.Lens


data Point = P { _x :: Double, _y :: Double } deriving (Show)

data Line = L { _p1 :: Point, _p2 :: Point } deriving (Show)

$( makeLenses ''Point )
$( makeLenses ''Line )


setX, setY :: Double -> Point -> Point
setX  = set x
setY  = set y

--over x :: (Double -> Double) -> Point -> Point
--over 

mapP f = over f


-- examples --

pp1 = P 1 2
pp2 = P 100 78





l = L pp1 pp2



