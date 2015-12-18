{-# LANGUAGE ViewPatterns, BangPatterns, FlexibleContexts #-}
module HIP.Algorithms.Geometric (
  scale, resize, rotate, rotate',
  downsampleRows, downsampleCols, downsample, downsampleF,
  upsampleRows, upsampleCols, upsample, upsampleF,
  leftToRight, topToBottom, pad
  ) where

import Prelude hiding (map, zipWith)
import Data.Complex
import HIP.Interface
import HIP.Pixel.Base (Pixel(..))
import HIP.Algorithms.Interpolation

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
scale :: (Interpolation meth px, AImage img px, Pixel px) =>
         meth   -- ^ Interpolation method to be used during scaling.
      -> Double -- ^ Scaling factor, must be greater than 0.
      -> img px -- ^ Image to be scaled.
      -> img px
scale _ ((0>=) -> True) _ = error "scale: scaling factor must be greater than 0"
scale !meth !fact !img    = traverse img getNewDims getNewPx where
  !(imgM, imgN) = dims img
  getNewDims _ _ = (round (fromIntegral imgM * fact), round (fromIntegral imgN * fact))
  {-# INLINE getNewDims #-}
  getNewPx !getPx (fromIntegral -> !i) (fromIntegral -> !j) =
    interpolate meth imgM imgN getPx (i/fact) (j/fact)
  {-# INLINE getNewPx #-}
{-# INLINE scale #-}


{- | Resize an image while using interpolation.

>>> lena
<Image RGB: 512x512>
>>> let lenaResize = resize Bilinear 256 1024 lena
>>> lenaResize
<Image RGB: 256x1024>

-}
resize :: (Interpolation method px, AImage img px, RealFrac (Channel px)) =>
          method     -- ^ Interpolation method to be used during resizing.
       -> Int -> Int -- ^ New image dimensions @m@ rows and @n@ columns.
       -> img px     -- ^ Image to be resized.
       -> img px
resize !meth !newM !newN !img = traverse img getNewDims getNewPx where
  !(imgM, imgN) = dims img
  !(mScale, nScale) = (fromIntegral newM / fromIntegral imgM,
                       fromIntegral newN / fromIntegral imgN)
  getNewDims _ _ = (newM, newN)
  {-# INLINE getNewDims #-}
  getNewPx !getPx (fromIntegral -> !i) (fromIntegral -> !j) =
    interpolate meth imgM imgN getPx (i/mScale) (j/nScale)
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
rotate :: (Interpolation method px, AImage img px) =>
          method -- ^ Interpolation method to be used during rotation.
       -> Double -- ^ Angle @theta@ in radians, that an image should be rotated by.
       -> img px -- ^ Image to be rotated.
       -> img px
rotate !meth !theta !img@(dims -> !(m, n)) = traverse img getNewDims getNewPx
  where
    !(oldM, oldN) = (fromIntegral m, fromIntegral n)
    !(newM, newN) = (oldM * cost + oldN * sint, oldN * cost + oldM * sint)
      where !(sint, cost) = (sin theta, cos theta)
    !(oldMhalf, oldNhalf) = (oldM / 2, oldN / 2)
    !(newMhalf, newNhalf) = (newM / 2, newN / 2)
    getNewDims _ _ = (ceiling newM, ceiling newN)
    {-# INLINE getNewDims #-}
    getNewPx !getOldPx (fromIntegral -> !i) (fromIntegral -> !j) =
      interpolate meth m n getOldPx i' j' where
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
rotate' :: (Interpolation method px, AImage img px, Pixel px, RealFloat (Channel px)) =>
           method -- ^ Interpolation method to be used during rotation.
        -> Double -- ^ Angle @theta@ in radians, that an image should be rotated by.
        -> img px -- ^ Image to be rotated.
        -> img px
rotate' !meth !theta !img@(dims -> !(m, n)) =  traverse img getNewDims getNewPx
  where
    !(newMhalf, newNhalf) = (fromIntegral $ div m 2, fromIntegral $ div n 2)
    getNewDims _ _ = (m, n)
    {-# INLINE getNewDims #-}
    getNewPx !getOldPx (fromIntegral -> !i) (fromIntegral -> !j) =
      interpolate meth m n getOldPx i' j' where
        !z = exp(0 :+ theta) * ((j - newNhalf) :+ (i - newMhalf))
        !i' = newMhalf + imagPart z
        !j' = newNhalf + realPart z
    {-# INLINE getNewPx #-}
{-# INLINE rotate' #-}


downsampleF :: AImage img px => Int -> Int -> img px -> img px
{-# INLINE downsampleF #-}
downsampleF !fm !fn !img = traverse img
                           (\m n -> (m `div` fm, n `div` fn))
                           (\getPx i j -> getPx (i*fm) (j*fn))


upsampleF :: AImage img px => Int -> Int -> img px -> img px
{-# INLINE upsampleF #-}
upsampleF !fm !fn !img = traverse img 
                         (\m n -> (m*fm, n*fn))
                         (\getPx i j ->
                           if i `mod` fm == 0 && j `mod` fn == 0
                           then getPx (i `div` fm) (j `div` fn)
                           else fromDouble 0)


-- | Removes every second row from the image starting with second one.
downsampleRows :: AImage img px => img px -> img px
{-# INLINE downsampleRows #-}
downsampleRows = downsampleF 2 1


downsampleCols :: AImage img px => img px -> img px
{-# INLINE downsampleCols #-}
downsampleCols = downsampleF 1 2


downsample :: AImage img px => img px -> img px
{-# INLINE downsample #-}
downsample = downsampleF 2 2


upsampleRows :: AImage img px => img px -> img px
{-# INLINE upsampleRows #-}
upsampleRows = upsampleF 2 1


upsampleCols :: AImage img px => img px -> img px
{-# INLINE upsampleCols #-}
upsampleCols = upsampleF 1 2


upsample :: AImage img px => img px -> img px
{-# INLINE upsample #-}
upsample = upsampleF 2 2


-- | Concatenate two images together into one. Both input images must have the
-- same number of rows.
leftToRight :: AImage img px => img px -> img px -> img px
{-# INLINE leftToRight #-}
leftToRight !img1@(cols -> !n1) !img2 = traverse2 img1 img2 newDims newPx where
  newDims !m1 _ !m2 !n2
    | m1 == m2  = (m1, n1 + n2)
    | otherwise = error ("Images must agree in numer of rows, but received: "++
                         (show img1)++" and "++(show img2))
  {-# INLINE newDims #-}
  newPx !getPx1 !getPx2 !i !j = if j < n1 then getPx1 i j else getPx2 i (j-n1)
  {-# INLINE newPx #-}


-- | Concatenate two images together into one. Both input images must have the
-- same number of columns.
topToBottom :: AImage img px => img px -> img px -> img px
{-# INLINE topToBottom #-}
topToBottom !img1@(rows -> !m1) !img2 = traverse2 img1 img2 newDims newPx where
  newDims _ n1 !m2 !n2
    | n1 == n2  = (m1 + m2, n1)
    | otherwise = error ("Images must agree in numer of columns, but received: "++
                         (show img1)++" and "++(show img2))
  {-# INLINE newDims #-}
  newPx !getPx1 !getPx2 !i !j = if i < m1 then getPx1 i j else getPx2 (i-m1) j
  {-# INLINE newPx #-}


{- | Changes dimensions of an image while padding it with a default pixel whenever
@i@ and @j@ is out of bounds for an original image -}
pad :: AImage img px =>
       px         -- ^ default pixel to be used for out of bounds region
    -> Int -> Int -- ^ image's new dimensions @m@ rows and @n@ columns
    -> img px     -- ^ image that is subjected to padding.
    -> img px
pad !defPx !m1 !n1 !img@(dims -> !(m, n)) =
  traverse img (const . const (m1, n1)) newPx where
    newPx !getPx !i !j = if i < m && j < n then getPx i j else defPx
    {-# INLINE newPx #-}
{-# INLINE pad #-}
  
