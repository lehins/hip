{-# LANGUAGE BangPatterns #-}
module Graphics.Image.Processing (
  -- * Convolution
  module Graphics.Image.Processing.Convolution,
  module Graphics.Image.Processing.FFT,
  module Graphics.Image.Processing.Matrix,
  module Graphics.Image.Processing.Geometric,
  downsampleRows, downsampleCols, downsample, downsampleF,
  upsampleRows, upsampleCols, upsample, upsampleF,
  leftToRight, topToBottom
  ) where

import Graphics.Image.Base as I
import Graphics.Image.Processing.Convolution
import Graphics.Image.Processing.FFT
import Graphics.Image.Processing.Matrix
import Graphics.Image.Processing.Geometric
import Data.Array.Repa as R hiding (transpose)

downsampleF :: Pixel px => Int -> Int -> Image px -> Image px
{-# INLINE downsampleF #-}
downsampleF fm fn img = fromDelayed $ R.traverse arr
                        (\(Z :. m :. n) -> (Z :. m `div` fm :. n `div` fn))
                        (\_ (Z :. i :. j) -> index arr (Z :. i*fm :. j*fn))
  where !arr = getComputed img


upsampleF :: Pixel px => Int -> Int -> Image px -> Image px
{-# INLINE upsampleF #-}
upsampleF fm fn img = fromDelayed $ R.traverse arr
                      (\(Z :. m :. n) -> (Z :. m*fm :. n*fn))
                      (\_ (Z :. i :. j) ->
                        if i `mod` fm == 0 && j `mod` fn == 0
                        then index arr (Z :. i `div` fm :.  j `div` fn)
                        else fromInteger 0)
  where !arr = getComputed img

-- | Removes every second row from the image starting with second one.
downsampleRows :: Pixel px => Image px -> Image px
{-# INLINE downsampleRows #-}
downsampleRows = downsampleF 2 1

downsampleCols :: Pixel px => Image px -> Image px
{-# INLINE downsampleCols #-}
downsampleCols = downsampleF 1 2

downsample :: Pixel px => Image px -> Image px
{-# INLINE downsample #-}
downsample = downsampleF 2 2

upsampleRows :: Pixel px => Image px -> Image px
{-# INLINE upsampleRows #-}
upsampleRows = upsampleF 2 1

upsampleCols :: Pixel px => Image px -> Image px
{-# INLINE upsampleCols #-}
upsampleCols = upsampleF 1 2

upsample :: Pixel px => Image px -> Image px
{-# INLINE upsample #-}
upsample = upsampleF 2 2

leftToRight :: Pixel px => Image px -> Image px -> Image px
{-# INLINE leftToRight #-}
leftToRight img1 img2
  | m1 == m2  = make m1 (n1 + n2) getPixel
  | otherwise = error "Images must agree in numer of rows" where
    !(!m1, !n1) = dims img1
    !(!m2, !n2) = dims img2
    {-# INLINE getPixel #-}
    getPixel i j
      | j < n1    = ref img1 i j
      | otherwise = ref img2 i (j-n1)

topToBottom :: Pixel px => Image px -> Image px -> Image px
{-# INLINE topToBottom #-}
topToBottom img1 img2
  | n1 == n2  = make (m1 + m2) n1 getPixel
  | otherwise = error "Images must agree in numer of columns" where
    !(!m1, !n1) = dims img1
    !(!m2, !n2) = dims img2
    {-# INLINE getPixel #-}
    getPixel i j
      | i < m1    = ref img1 i j
      | otherwise = ref img2 (i-m1) j

