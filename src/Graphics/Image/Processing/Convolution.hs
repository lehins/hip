{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Module      : Graphics.Image.Processing.Convolution
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Convolution (
  -- * Convolution
  convolve, convolveRows, convolveCols,
  -- * Correlation
  correlate
  ) where


import           Graphics.Image.ColorSpace               (Pixel (..), X)
import           Graphics.Image.Interface                as I
import           Graphics.Image.Processing.Geometric
import           Graphics.Image.Utils                    (loop)
import           Prelude                                 as P


-- | Correlate an image with a kernel. Border resolution technique is required.
correlate :: (Array arr X e, Array arr cs e)
          => Border (Pixel cs e) -> Image arr X e -> Image arr cs e -> Image arr cs e
correlate !border !kernel !img =
  makeImageWindowed
    sz
    (kM2, kN2)
    (m - kM2 * 2, n - kN2 * 2)
    (getStencil (I.unsafeIndex imgM))
    (getStencil (borderIndex border imgM))
  where
    !imgM = toManifest img
    !sz@(m, n) = dims img
    !kernelM = toManifest kernel
    !(kM, kN) = dims kernel
    !(kM2, kN2) = (kM `div` 2, kN `div` 2)
    getStencil getImgPx !(i, j) =
      loop 0 (< kM) (+ 1) 0 $ \ !iK !acc0 ->
        let !iD = i + iK - kM2 in
          loop 0 (< kN) (+ 1) acc0 $ \ !jK !acc1 ->
            let !jD = j + jK - kN2 in
              acc1 + liftPx (* getX (unsafeIndex kernelM (iK, jK))) (getImgPx (iD, jD))
    {-# INLINE getStencil #-}
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
convolve :: (Array arr X e, Array arr cs e) =>
            Border (Pixel cs e) -- ^ Approach to be used near the borders.
         -> Image arr X e -- ^ Kernel image.
         -> Image arr cs e -- ^ Source image.
         -> Image arr cs e
convolve !out = correlate out . rotate180
{-# INLINE convolve #-}


-- | Convolve image's rows with a vector kernel represented by a list of pixels.
convolveRows :: (Array arr X e, Array arr cs e) =>
                Border (Pixel cs e) -> [Pixel X e] -> Image arr cs e -> Image arr cs e
convolveRows !out = convolve out . fromLists . (:[]) . reverse
{-# INLINE convolveRows #-}


-- | Convolve image's columns with a vector kernel represented by a list of pixels.
convolveCols :: (Array arr X e, Array arr cs e) =>
                Border (Pixel cs e) -> [Pixel X e] -> Image arr cs e -> Image arr cs e
convolveCols !out = convolve out . fromLists . P.map (:[]) . reverse
{-# INLINE convolveCols #-}

