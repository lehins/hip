{-# LANGUAGE ViewPatterns, BangPatterns, FlexibleContexts #-}
module Graphics.Image.Processing.Geometric (
  rotate, rotate'
  ) where

import Prelude hiding (map, zipWith)
import Graphics.Image.Interface
import Data.Complex


{-| Rotate an image around it's center by an angle Θ in counterclockwise
direction. Dimensions of a new image are adjusted, so rotated image fully fits
inside.

>>> lena
<Image RGB: 512x512>
>>> let lena30deg = rotate lena 0 (pi/6)
>>> lena30deg
<Image RGB: 700x700>

-}
rotate :: (Interpolation alg, Image img px, Pixel px, Num px, RealFloat (Inner px)) =>
          alg    -- ^ Interpolation algorithm to be used during rotation.
       -> img px -- ^ image to be rotated.
       -> px     -- ^ default pixel that will fill in areas that are out of bounds.
       -> Inner px -- ^ angle @theta@ in radians, that an image should be rotated by.
       -> img px
rotate alg !img@(dims -> !(m, n)) !defPx !theta = traverse img getNewDims getNewPx
  where
    !(oldM, oldN) = (fromIntegral m, fromIntegral n)
    !(newM, newN) = (oldM * cost + oldN * sint, oldN * cost + oldM * sint)
      where !(sint, cost) = (sin theta, cos theta)
    !(oldMhalf, oldNhalf) = (oldM / 2, oldN / 2)
    !(newMhalf, newNhalf) = (newM / 2, newN / 2)
    getNewDims _ _ = (ceiling newM, ceiling newN)
    {-# INLINE getNewDims #-}
    getNewPx !getOldPx (fromIntegral -> !i) (fromIntegral -> !j) =
      interpolate alg defPx m n getOldPx i' j' where
        !z = exp(0 :+ theta) * ((j - newNhalf) :+ (i - newMhalf))
        !i' = oldMhalf + imagPart z
        !j' = oldNhalf + realPart z
    {-# INLINE getNewPx #-}
{-# INLINE rotate #-}


{-| Rotate an image around it's center by an angle Θ in counterclockwise
direction. Dimensions of a new image will be kept the same.

>>> lena
<Image RGB: 512x512>
>>> let lena30deg = rotate lena 0 (pi/6)
>>> lena30deg
<Image RGB: 512x512>

-}
rotate' :: (Interpolation alg, Image img px, Pixel px, Num px, RealFloat (Inner px)) =>
           alg    -- ^ Interpolation algorithm to be used during rotation.
        -> img px -- ^ image to be rotated.
        -> px     -- ^ default pixel that will fill in areas that are out of bounds.
        -> Inner px -- ^ angle @theta@ in radians, that an image should be rotated by.
        -> img px
rotate' alg img@(dims -> !(m, n)) !defPx !theta =  traverse img getNewDims getNewPx
  where
    !(newMhalf, newNhalf) = (fromIntegral $ div m 2, fromIntegral $ div n 2)
    getNewDims _ _ = (m, n)
    {-# INLINE getNewDims #-}
    getNewPx !getOldPx (fromIntegral -> !i) (fromIntegral -> !j) =
      interpolate alg defPx m n getOldPx i' j' where
        !z = exp(0 :+ theta) * ((j - newNhalf) :+ (i - newMhalf))
        !i' = newMhalf + imagPart z
        !j' = newNhalf + realPart z
    {-# INLINE getNewPx #-}
{-# INLINE rotate' #-}
