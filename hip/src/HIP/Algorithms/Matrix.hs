{-# LANGUAGE BangPatterns #-}
module HIP.Algorithms.Matrix (
  crop,  (.*), flipH, flipV
  ) where

import HIP.Interface
import Data.Array.Repa as R hiding (transpose)
import qualified Data.Array.Repa as R (transpose)

crop :: Pixel px => Image px -> Int -> Int -> Int -> Int -> Image px
{-# INLINE crop #-}
crop img i j m n = make m n (\i' j' -> ref img (i'-i) (j'-j))

flipH :: Pixel px => Image px -> Image px
{-# INLINE flipH #-}
flipH img = fromDelayed . backpermute (Z :. m :. n) flipper $ arr where
  !arr = getDelayed img
  !(Z :. m :. n) = extent arr
  {-# INLINE flipper #-}
  flipper (Z :. i :. j) = (Z :. i :. mod (-j-1) n)

flipV :: Pixel px => Image px -> Image px
{-# INLINE flipV #-}
flipV img = fromDelayed . backpermute (Z :. m :. n) flipper $ arr where
  !arr = getDelayed img
  !(Z :. m :. n) = extent arr
  {-# INLINE flipper #-}
  flipper (Z :. i :. j) = (Z :. mod (-i-1) m :. j)


-- | Matrix type multiplication of two images. Dimensions must be MxN .* NxM
-- Note that operator is exactly opposite in MATLAB.
(.*) :: (Strategy strat img px, AImage img px, Pixel px) =>
        strat img px
        -> img px
        -> img px
        -> img px
{-# INLINE (.*) #-}
(.*) img1 img2
  | m1 == n2 && n1 == m2 = make m1 n2 multOp
  | otherwise = error "Image dimensions must agree. Expected MxN * NxM = MxM"
  where
    !(!m1, !n1) = dims img1
    !(!m2, !n2) = dims img2
    !arr1 = getComputed img1
    !arr2 = getComputed img2
    {-# INLINE multOp #-}
    multOp i j = sumAllS $ R.zipWith (*)
                 (slice arr1 (Any :. (i::Int) :. All))
                 (slice arr2 (Any :. (j::Int)))


