{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
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
  , toKernel, Kernel(..)
  ) where

import qualified Data.Vector.Unboxed                 as VU
import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface            as I
-- import           Graphics.Image.Interface.Vector     (VU)
import           Graphics.Image.Internal             as I
import           Graphics.Image.Processing.Geometric
import           Graphics.Image.Utils                (loop, toIx)
import           Prelude                             as P


data Orientation
  = Vertical
  | Horizontal deriving Show

data Kernel e
  = Kernel1D Orientation
             {-# UNPACK #-} !Int
             (VU.Vector (Int, e))
  | Kernel2D {-# UNPACK #-} !Int
             {-# UNPACK #-} !Int
             (VU.Vector (Int, Int, e)) deriving Show

toKernel :: (Eq e, Num e, VU.Unbox e) => (Int, Int) -> VU.Vector e -> Kernel e
toKernel !sz !v =
  case sz of
    (1, n) ->
      let !n2 = n `div` 2
      in Kernel1D Horizontal n2 $ mkKernel1d n2
    (m, 1) ->
      let !m2 = m `div` 2
      in Kernel1D Vertical m2 $ mkKernel1d m2
    (m, n) ->
      let !(m2, n2) = (m `div` 2, n `div` 2)
      in Kernel2D m2 n2 $
         VU.filter (\(_, _, x) -> x /= 0) $
         VU.imap (addIx m2 n2 n) v
  where
    mkKernel1d !l2 =
      VU.filter ((/= 0) . snd) $
      VU.imap (\ !k x -> (k - l2, x)) v
    {-# INLINE mkKernel1d #-}
    addIx !m2 !n2 !n' !k x =
      let !(i, j) = toIx n' k
      in (i - m2, j - n2, x)
    {-# INLINE addIx #-}
{-# INLINE toKernel #-}



-- | Correlate an image with a kernel. Border resolution technique is required.
correlate :: (Array arr X e, Array arr cs e)
          => Border (Pixel cs e) -> Image arr X e -> Image arr cs e -> Image arr cs e
correlate !border !kernelImg !img =
  makeImageWindowed
    sz
    (kM2, kN2)
    (m - kM2 * 2, n - kN2 * 2)
    (getStencil kernel (I.unsafeIndexA arr))
    (getStencil kernel (handleBorderIndex border (m, n) (I.unsafeIndexA arr)))
  where
    !(Image arr) = compute img
    !sz@(m, n) = dims img
    dropX (PixelX x) = x
    (Image kernelArr) = compute kernelImg
    !(kM, kN) = shapeA kernelArr
    !kernel = toKernel (kM, kN) $ VU.generate (kM*kN) $ (dropX . unsafeIndexA kernelArr . toIx kN)
    !(kLen, kM2, kN2) =
      case kernel of
        Kernel1D Horizontal n2 v -> (VU.length v, 0, n2)
        Kernel1D Vertical m2 v   -> (VU.length v, m2, 0)
        Kernel2D m2 n2 v         -> (VU.length v, m2, n2)
    getStencil (Kernel1D Horizontal _ kernelV) getImgPx !(i, j) =
      loop 0 (/= kLen) (+ 1) 0 $ \ !k !acc ->
        let !(jDelta, x) = VU.unsafeIndex kernelV k
            !imgPx = getImgPx (i, j + jDelta)
        in acc + liftPx (x *) imgPx
    getStencil (Kernel1D Vertical _ kernelV) getImgPx !(i, j) =
      loop 0 (/= kLen) (+ 1) 0 $ \ !k !acc ->
        let !(iDelta, x) = VU.unsafeIndex kernelV k
            !imgPx = getImgPx (i + iDelta, j)
        in acc + liftPx (x *) imgPx
    getStencil (Kernel2D _ _ kernelV) getImgPx !(i, j) =
      loop 0 (/= kLen) (+ 1) 0 $ \ !k !acc ->
        let !(iDelta, jDelta, x) = VU.unsafeIndex kernelV k
            !imgPx = getImgPx (i + iDelta, j + jDelta)
        in acc + liftPx (x *) imgPx
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

