module Graphics.Image.Processing.Matrix where

import Graphics.Image.Base
import Data.Array.Repa as R

transpose :: Pixel px => Image px -> Image px
transpose = fromDelayed . R.transpose . getDelayed

(.*) :: Pixel px => Image px -> Image px -> Image px
(.*) img1 img2
  | m1 == n2 && n1 == m2 = make m1 n2 multOp
  | otherwise = error "Image dimensions must agree. Expected MxN * NxM = MxM"
  where
    (m1, n1) = dims img1
    (m2, n2) = dims img2
    arr1 = getComputed img1
    arr2 = getComputed img2
    multOp i j = sumAllS $ R.zipWith (*)
                 (slice arr1 (Any :. (i::Int) :. All))
                 (slice arr2 (Any :. (j::Int)))



