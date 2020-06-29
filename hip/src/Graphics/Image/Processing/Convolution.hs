{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
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


import qualified Data.Massiv.Array as A
import Graphics.Image.Internal
import Graphics.Pixel
import Prelude as P


-- | Correlate an image with a kernel.
correlate ::
     ColorModel cs e
  => Border (Pixel cs e) -- ^ Border resolution technique.
  -> Image cs e -- ^ Kernel image.
  -> Image cs e -- ^ Source image.
  -> Image cs e
correlate !border (Image kernel) (Image arr) =
  Image (A.compute (A.mapStencil border (A.makeCorrelationStencilFromKernel kernel) arr))
{-# INLINE correlate #-}


-- | Convolution of an image using a kernel.
--
-- Example using <https://en.wikipedia.org/wiki/Sobel_operator Sobel operator>:
--
-- >>> frog <- readImageY "images/frog.jpg"
-- >>> let frogX = convolve Edge (fromLists [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]]) frog
-- >>> let frogY = convolve Edge (fromLists [[-1,-2,-1], [ 0, 0, 0], [ 1, 2, 1]]) frog
-- >>> displayImage $ normalize $ sqrt (frogX ^ 2 + frogY ^ 2)
--
-- <<images/frogY.jpg>> <<images/frog_sobel.jpg>>
--
convolve ::
     ColorModel cs e
  => Border (Pixel cs e) -- ^ Border resolution technique.
  -> Image cs e -- ^ Kernel image.
  -> Image cs e -- ^ Source image.
  -> Image cs e
convolve !border (Image kernel) (Image arr) =
  Image (A.compute $ A.mapStencil border (A.makeConvolutionStencilFromKernel kernel) arr)
{-# INLINE convolve #-}


-- | Convolve image's rows with a vector kernel represented by a list of pixels.
convolveRows :: ColorModel cs e =>
                Border (Pixel cs e) -> [Pixel cs e] -> Image cs e -> Image cs e
convolveRows !border = convolve border . fromLists . pure . reverse
{-# INLINE convolveRows #-}


-- | Convolve image's columns with a vector kernel represented by a list of pixels.
convolveCols :: (ColorModel cs e) =>
                Border (Pixel cs e) -> [Pixel cs e] -> Image cs e -> Image cs e
convolveCols !out = convolve out . fromLists . P.map pure . reverse
{-# INLINE convolveCols #-}

