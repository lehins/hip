{-# LANGUAGE BangPatterns, ViewPatterns #-}
module Graphics.Image.Processing.Convolution (
  Outside(..), convolveOut, convolve, convolve', convolveRows, convolveCols
  ) where

import Graphics.Image.Base
import Graphics.Image.Processing.Matrix (crop, transpose)
import Data.Array.Repa hiding (transpose)
--import Data.Array.Repa.Stencil.Dim2
import Data.Array.Repa.Algorithms.Convolve


data Outside px = Extend | Wrap | Fill !px | Crop deriving Eq

convolveOut  :: Pixel px => Outside px -> Image px -> Image px -> Image px
{-# INLINE convolveOut #-}
convolveOut out kernel@(dims -> (m', n')) img@(dims -> (m, n)) =
  fromComputed $ convolveOutP outside karr arr
  where !karr = (getComputed kernel)
        !arr  = (getComputed img')
        !img' | out == Crop = crop img m' n' (m-2*m') (n-2*n')
              | otherwise   = img
        {-# INLINE outside #-}
        outside _ _ (Z :. i :. j) =
          case out of
            Extend    -> index arr (Z :. i' :. j')
            Wrap      -> index arr (Z :. mod i m :. mod j n)
            (Fill px) -> px
            Crop      -> ref img i j
            where (i', j') = (if i < 0 then 0 else (if i >= m then (m-1) else i),
                              if j < 0 then 0 else (if j >= n then (n-1) else j))


convolve :: Pixel px => Image px -> Image px -> Image px
{-# INLINE convolve #-}
convolve = convolveOut Wrap

convolve' :: Pixel px => Image px -> Image px -> Image px
{-# INLINE convolve' #-}
convolve' = convolveOut Extend

convolveRows :: Pixel px => [px] -> Image px -> Image px
{-# INLINE convolveRows #-}
convolveRows row = convolve . transpose . fromLists $ [row]

convolveCols :: Pixel px => [px] -> Image px -> Image px
{-# INLINE convolveCols #-}
convolveCols row = convolve . fromLists $ [row]
