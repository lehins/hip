{-# LANGUAGE ViewPatterns, BangPatterns, FlexibleContexts #-}
module Graphics.Image.Processing.Geometric (
  scale, resize, rotate, rotate'
  ) where

import Prelude hiding (map, zipWith)
import Graphics.Image.Interface
import Data.Complex

{- | Scale an image by a factor while using interpolation.

>>> lena
<Image RGB: 512x512>
>>> let lenaHalf = scale Bilinear 0.5 lena
>>> lenaHalf
<Image RGB: 256x256>
>>> let lenaTwice = scale Bilinear 2 lena
>>> lenaTwice
<Image RGB: 1024x1024>

-}
scale :: (Interpolation alg, Image img px, Pixel px, Num px, RealFrac (Inner px)) =>
         alg      -- ^ Interpolation algorithm to be used during scaling.
      -> Inner px -- ^ Scaling factor, must be grater than 0.
      -> img px   -- ^ Image to be scaled.
      -> img px
scale !alg !fact !img = traverse img getNewDims getNewPx where
  !(imgM, imgN) = dims img
  getNewDims _ _ = (round (fromIntegral imgM * fact), round (fromIntegral imgN * fact))
  {-# INLINE getNewDims #-}
  getNewPx !getPx (fromIntegral -> !i) (fromIntegral -> !j) =
    interpolate alg (pixel 0) imgM imgN getPx (i/fact) (j/fact)
  {-# INLINE getNewPx #-}
{-# INLINE scale #-}


{- | Resize an image while using interpolation.

>>> lena
<Image RGB: 512x512>
>>> let lenaResize = resize Bilinear 256 1024 lena
>>> lenaResize
<Image RGB: 256x1024>

-}
resize :: (Interpolation alg, Image img px, Pixel px, Num px, RealFrac (Inner px)) =>
         alg        -- ^ Interpolation algorithm to be used during resizing.
      -> Int -> Int -- ^ New image dimensions @m@ rows and @n@ columns.
      -> img px     -- ^ Image to be resized.
      -> img px
resize !alg !newM !newN !img = traverse img getNewDims getNewPx where
  !(imgM, imgN) = dims img
  !(mScale, nScale) = (fromIntegral newM / fromIntegral imgM,
                       fromIntegral newN / fromIntegral imgN)
  getNewDims _ _ = (newM, newN)
  {-# INLINE getNewDims #-}
  getNewPx !getPx (fromIntegral -> !i) (fromIntegral -> !j) =
    interpolate alg (pixel 0) imgM imgN getPx (i/mScale) (j/nScale)
  {-# INLINE getNewPx #-}
{-# INLINE resize #-}


{-| Rotate an image around it's center by an angle Θ in counterclockwise
direction. Dimensions of a new image are adjusted, so rotated image fully fits
inside.

>>> lena
<Image RGB: 512x512>
>>> let lena30deg = rotate Bilinear 0 (pi/6) lena
>>> lena30deg
<Image RGB: 700x700>

-}
rotate :: (Interpolation alg, Image img px, Pixel px, Num px, RealFloat (Inner px)) =>
          alg      -- ^ Interpolation algorithm to be used during rotation.
       -> px       -- ^ Default pixel that will fill in areas that are out of bounds.
       -> Inner px -- ^ Angle @theta@ in radians, that an image should be rotated by.
       -> img px   -- ^ Image to be rotated.
       -> img px
rotate !alg !defPx !theta !img@(dims -> !(m, n)) = traverse img getNewDims getNewPx
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
direction. Dimensions of a new image will stay unchanged.

>>> lena
<Image RGB: 512x512>
>>> let lena30deg = rotate Bilinear 0 (pi/6) lena
>>> lena30deg
<Image RGB: 512x512>

-}
rotate' :: (Interpolation alg, Image img px, Pixel px, Num px, RealFloat (Inner px)) =>
           alg      -- ^ Interpolation algorithm to be used during rotation.
        -> px       -- ^ Default pixel that will fill in areas that are out of bounds.
        -> Inner px -- ^ Angle @theta@ in radians, that an image should be rotated by.
        -> img px   -- ^ Image to be rotated.
        -> img px
rotate' !alg !defPx !theta !img@(dims -> !(m, n)) =  traverse img getNewDims getNewPx
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
