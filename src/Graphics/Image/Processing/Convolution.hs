{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Graphics.Image.Processing.Convolution
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Convolution (
  convolve, convolveRows, convolveCols, correlate
  ) where

import Prelude as P

import Graphics.Image.ColorSpace
import Graphics.Image.Interface as I
import Graphics.Image.Processing.Geometric




-- | Correlate an image using a kernel. Border resolution technique is required.
correlate :: I.Array arr cs e =>
                  Border (Pixel cs e) -> Image arr cs e -> Image arr cs e -> Image arr cs e
correlate !border !kernel !img =
  I.traverse (compute img) (const sz) stencil
  where
    !kernelM         = toManifest kernel
    !(krnM, krnN)    = dims kernelM
    !krnM2           = krnM `div` 2
    !krnN2           = krnN `div` 2
    !sz              = dims img
    getPxB getPx !ix = handleBorderIndex border sz getPx ix
    {-# INLINE getPxB #-}
    stencil getImgPx !(i, j) = integrate 0 0 0 where
      !ikrnM = i - krnM2
      !jkrnN = j - krnN2
      integrate !ki !kj !acc
        | kj == krnN            = integrate (ki+1) 0 acc
        | kj == 0 && ki == krnM = acc
        | otherwise             = let !krnPx = I.unsafeIndex kernelM (ki, kj)
                                      !imgPx = getPxB getImgPx (ki + ikrnM, kj + jkrnN)
                                  in integrate ki (kj + 1) (acc + krnPx * imgPx)
    {-# INLINE stencil #-}
{-# INLINE correlate #-}


-- | Convolution of an image using a kernel. Border resolution technique is required.
--
-- Example using <https://en.wikipedia.org/wiki/Sobel_operator Sobel operator>:
--
-- >>> frog <- readImageY RPU "images/frog.jpg"
-- >>> let frogX = convolve Edge (fromLists [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]]) frog
-- >>> let frogY = convolve Edge (fromLists [[-1,-2,-1], [ 0, 0, 0], [ 1, 2, 1]]) frog
-- >>> displayImage $ normalize $ sqrt (frogX ^ 2 + frogY ^ 2)
--
-- <<images/frogY.jpg>> <<images/frog_sobel.jpg>>
--
convolve :: Array arr cs e =>
             Border (Pixel cs e) -- ^ Approach to be used near the borders.
          -> Image arr cs e -- ^ Kernel image.
          -> Image arr cs e -- ^ Source image.
          -> Image arr cs e
convolve !out = correlate out . rotate180
{-# INLINE convolve #-}


-- | Convolve image's rows with a vector kernel represented by a list of pixels.
convolveRows :: Array arr cs e =>
                Border (Pixel cs e) -> [Pixel cs e] -> Image arr cs e -> Image arr cs e
convolveRows !out = convolve out . fromLists . (:[]) . reverse
{-# INLINE convolveRows #-}


-- | Convolve image's columns with a vector kernel represented by a list of pixels.
convolveCols :: Array arr cs e =>
                Border (Pixel cs e) -> [Pixel cs e] -> Image arr cs e -> Image arr cs e
convolveCols !out = convolve out . fromLists . P.map (:[]) . reverse
{-# INLINE convolveCols #-}

