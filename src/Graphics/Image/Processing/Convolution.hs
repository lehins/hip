{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Graphics.Image.Processing.Convolution
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Convolution (
  convolve, convolveRows, convolveCols,
  ) where

import Prelude hiding (map)
import qualified Prelude as P (map)
import Graphics.Image.Interface
import Graphics.Image.Processing.Geometric



convolve' :: Array arr cs e =>
              Border (Pixel cs e) -> Image arr cs e -> Image arr cs e -> Image arr cs e
convolve' !border !kernel !img =
  traverse2 (compute kernel) (compute img) (const . const sz) stencil
  where
    !(krnM, krnN)     = dims kernel
    !krnM2            = krnM `div` 2
    !krnN2            = krnN `div` 2
    !sz               = dims img
    getPxB !getPx !ix = handleBorderIndex border sz getPx ix
    {-# INLINE getPxB #-}
    stencil !getKrnPx !getImgPx !(i, j) = integrate 0 0 0 where
      !ikrnM = i - krnM2
      !jkrnN = j - krnN2
      integrate !ki !kj !acc
        | kj == krnN            = integrate (ki+1) 0 acc
        | kj == 0 && ki == krnM = acc
        | otherwise             = let !krnPx = getKrnPx (ki, kj)
                                      !imgPx = getPxB getImgPx (ki + ikrnM, kj + jkrnN)
                                  in integrate ki (kj + 1) (acc + krnPx * imgPx)
    {-# INLINE stencil #-}
{-# INLINE convolve' #-}

-- | Convolution of an image using a kernel. Border resolution technique is required.
--
-- Example using <https://en.wikipedia.org/wiki/Sobel_operator Sobel operator>:
--
-- >>> frog <- readImageY RP "images/frog.jpg"
-- >>> let frogX = convolve Edge (fromLists [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]]) frog
-- >>> let frogY = convolve Edge (fromLists [[-1,-2,-1], [ 0, 0, 0], [ 1, 2, 1]]) frog
-- >>> displayImage $ normalize $ sqrt (frogX ^ 2 + frogY ^ 2)
--
-- <<images/frogY.jpg>> <<images/frog_sobel.jpg>>
--
convolve  :: Array arr cs e =>
             Border (Pixel cs e) -- ^ Approach to be used near the borders.
          -> Image arr cs e -- ^ Kernel image.
          -> Image arr cs e -- ^ Source image.
          -> Image arr cs e
convolve !out = convolve' out . rotate180
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

