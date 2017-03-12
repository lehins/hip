{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Module      : Graphics.Image.Processing.Convolution
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Convolution (
  -- * Kernel
  Kernel(..), toKernel, fromKernel,
  -- * Convolution
  convolve, convolveRows, convolveCols,
  -- * Correlation
  correlate
  ) where

import           Control.Monad.ST
import qualified Data.Vector.Unboxed                     as VU
import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface                as I
import           Graphics.Image.Interface.Vector.Unboxed (VU)
import           Graphics.Image.Processing.Geometric
import           Prelude                                 as P

data Kernel e = Kernel !Int !Int !(VU.Vector (Int, Int, e))


toKernel :: Array VU X e => Image VU X e -> Kernel e
toKernel img =
  Kernel m n $ VU.filter (\(_, _, x) -> x /= 0) $ VU.imap addIx $ toVector img
  where
    !(m, n) = dims img
    !(m2, n2) = (m `div` 2, n `div` 2)
    addIx !k (PixelX x) =
      let !(i, j) = toIx n k
      in (i - m2, j - n2, x)
    {-# INLINE addIx #-}
{-# INLINEABLE toKernel #-}


fromKernel :: MArray VU X e => Kernel e -> Image VU X e
fromKernel = fromKernel'
{-# INLINEABLE fromKernel #-}


fromKernel' :: forall e . MArray VU X e => Kernel e -> Image VU X e
fromKernel' (Kernel m n v) = createImage create where
  !(m2, n2) = (m `div` 2, n `div` 2)
  create :: ST s (MImage s VU X e)
  create = do
    mImg <- new (m, n)
    loopM_ 0 (<m) (+1) $ \ !i -> do
      loopM_ 0 (<n) (+1) $ \ !j -> do
        write mImg (i, j) 0
    VU.forM_ v $ \ (i, j, x) -> write mImg (i + m2, j + n2) (PixelX x)
    return mImg
  {-# INLINEABLE create #-}
{-# INLINE fromKernel' #-}


-- | Correlate an image with a kernel. Border resolution technique is required.
correlate :: Array arr cs e
          => Border (Pixel cs e) -> Image VU X e -> Image arr cs e -> Image arr cs e
correlate !border !kernel !img =
  makeImageWindowed
    sz
    (kM2, kN2)
    (m - kM2 * 2, n - kN2 * 2)
    (stencil (I.unsafeIndex imgM))
    (stencil getPxB)
  where
    !imgM = toManifest img
    !sz@(m, n) = dims img
    (Kernel kM kN kernelV) = toKernel kernel
    !(kM2, kN2) = (kM `div` 2, kN `div` 2)
    !kLen = VU.length kernelV
    getPxB = handleBorderIndex border sz (I.index imgM)
    {-# INLINE getPxB #-}
    stencil getImgPx !(i, j) = integrate 0 0
      where
        integrate !k !acc
          | k == kLen = acc
          | otherwise =
            let !(iDelta, jDelta, x) = VU.unsafeIndex kernelV k
                !imgPx = getImgPx (i + iDelta, j + jDelta)
            in integrate (k + 1) (acc + liftPx (x *) imgPx)
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
         -> Image VU X e -- ^ Kernel image.
         -> Image arr cs e -- ^ Source image.
         -> Image arr cs e
convolve !out = correlate out . rotate180
{-# INLINE convolve #-}


-- | Convolve image's rows with a vector kernel represented by a list of pixels.
convolveRows :: Array arr cs e =>
                Border (Pixel cs e) -> [Pixel X e] -> Image arr cs e -> Image arr cs e
convolveRows !out = convolve out . fromLists . (:[]) . reverse
{-# INLINE convolveRows #-}


-- | Convolve image's columns with a vector kernel represented by a list of pixels.
convolveCols :: Array arr cs e =>
                Border (Pixel cs e) -> [Pixel X e] -> Image arr cs e -> Image arr cs e
convolveCols !out = convolve out . fromLists . P.map (:[]) . reverse
{-# INLINE convolveCols #-}

