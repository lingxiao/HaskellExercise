{-# LANGUAGE ForeignFunctionInterface #-}

-- $ ghc -O2 --make wrappers.hs functions.c

import Numeric.LinearAlgebra
import Data.Packed.Development
import Foreign(Ptr,unsafePerformIO)
import Foreign.C.Types(CInt)

-----------------------------------------------------

main = do
    print $ myScale 3.0 (fromList [1..10])
    print $ myDiag $ (3><5) [1..]

-----------------------------------------------------

foreign import ccall "c_scale_vector"
    cScaleVector :: Double                -- scale
                 -> CInt -> Ptr Double    -- argument
                 -> CInt -> Ptr Double    -- result
                 -> IO CInt               -- exit code

myScale s x = unsafePerformIO $ do
    y <- createVector (dim x)
    app2 (cScaleVector s) vec x vec y "cScaleVector"
    return y

-----------------------------------------------------

foreign import ccall "c_diag"
    cDiag :: CInt                        -- matrix order
          -> CInt -> CInt -> Ptr Double  -- argument
          -> CInt -> Ptr Double          -- result1
          -> CInt -> CInt -> Ptr Double  -- result2
          -> IO CInt                     -- exit code

myDiag m = unsafePerformIO $ do
    y <- createVector (min r c)
    z <- createMatrix (orderOf m) r c
    app3 (cDiag o) mat m vec y mat z "cDiag"
    return (y,z)
  where r = rows m
        c = cols m
        o = if orderOf m == RowMajor then 1 else 0
