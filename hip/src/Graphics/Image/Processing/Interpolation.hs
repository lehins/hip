{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Image.Processing.Interpolation
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Interpolation
  ( Interpolation(..)
  , Nearest(..)
  , Bilinear(..)
  , Bicubic(..)
  ) where

import Graphics.Image.Internal

-- | Implementation for an interpolation method.
class Interpolation method where

  -- | Size of the neighborhood and the index of the center within it.
  interpolationBox :: method -> (Ix2, Sz2)

  -- | Construct a new pixel by using information from neighboring pixels.
  interpolate :: (RealFloat e, ColorModel cs e) =>
                 method -- ^ Interpolation method
              -> (Ix2 -> Pixel cs e)
                 -- ^ Lookup function that returns a pixel at @i@th and @j@th
                 -- location.
              -> (e, e) -- ^ Real values of @i@ and @j@ index
              -> Pixel cs e



-- | Nearest Neighbor interpolation method.
data Nearest = Nearest deriving Show


-- | Bilinear interpolation method.
data Bilinear = Bilinear deriving Show


-- | Bicubic interpolation method. The parameter is usually set from -0.5 to -1.0.
newtype Bicubic = Bicubic Double deriving Show


instance Interpolation Nearest where

  interpolationBox Nearest = (0 :. 0, Sz (1 :. 1))

  interpolate Nearest getPx (i, j) = getPx (round i :. round j)



instance Interpolation Bilinear where

  interpolationBox _ = (0 :. 0, Sz (2 :. 2))

  interpolate Bilinear getPx (i, j) = fi0 + fmap (jWeight *) (fi1 - fi0)
    where
      !i0 = floor i
      !j0 = floor j
      !i1 = i0 + 1
      !j1 = j0 + 1
      !iWeight = i - fromIntegral i0
      !jWeight = j - fromIntegral j0
      !f00 = getPx (i0 :. j0)
      !f10 = getPx (i1 :. j0)
      !f01 = getPx (i0 :. j1)
      !f11 = getPx (i1 :. j1)
      !fi0 = f00 + fmap (iWeight *) (f10 - f00)
      !fi1 = f01 + fmap (iWeight *) (f11 - f01)
  {-# INLINE interpolate #-}



instance Interpolation Bicubic where

  interpolationBox _ = (1 :. 1, Sz (4 :. 4))

  interpolate (Bicubic a) getPx (i, j) =
      (/ w) <$> ( f00 + f10 + f20 + f30
                + f01 + f11 + f21 + f31
                + f02 + f12 + f22 + f32
                + f03 + f13 + f23 + f33 )
    where
      a' = fromDouble a
      weight x
          | x' <= 1 = ((a' + 2) * x' - (a' + 3)) * x2' + 1
          | x' <  2 = a' * ((x2' - 5 * x' + 8) * x' - 4)
          | otherwise = 0
          where x' = abs x
                x2' = x' * x'
      {-# INLINE weight #-}
      !i0 = i1 - 1
      !j0 = j1 - 1
      !i1 = floor i
      !j1 = floor j
      !i2 = i1 + 1
      !j2 = j1 + 1
      !i3 = i1 + 2
      !j3 = j1 + 2

      !weightX0 = weight (fromIntegral i0 - i)
      !weightX1 = weight (fromIntegral i1 - i)
      !weightX2 = weight (fromIntegral i2 - i)
      !weightX3 = weight (fromIntegral i3 - i)
      !weightY0 = weight (fromIntegral j0 - j)
      !weightY1 = weight (fromIntegral j1 - j)
      !weightY2 = weight (fromIntegral j2 - j)
      !weightY3 = weight (fromIntegral j3 - j)

      !weightX0Y0 = weightX0 * weightY0
      !weightX1Y0 = weightX1 * weightY0
      !weightX2Y0 = weightX2 * weightY0
      !weightX3Y0 = weightX3 * weightY0

      !weightX0Y1 = weightX0 * weightY1
      !weightX1Y1 = weightX1 * weightY1
      !weightX2Y1 = weightX2 * weightY1
      !weightX3Y1 = weightX3 * weightY1

      !weightX0Y2 = weightX0 * weightY2
      !weightX1Y2 = weightX1 * weightY2
      !weightX2Y2 = weightX2 * weightY2
      !weightX3Y2 = weightX3 * weightY2

      !weightX0Y3 = weightX0 * weightY3
      !weightX1Y3 = weightX1 * weightY3
      !weightX2Y3 = weightX2 * weightY3
      !weightX3Y3 = weightX3 * weightY3

      !f00 = (weightX0Y0 *) <$> getPx (i0 :. j0)
      !f10 = (weightX1Y0 *) <$> getPx (i1 :. j0)
      !f20 = (weightX2Y0 *) <$> getPx (i2 :. j0)
      !f30 = (weightX3Y0 *) <$> getPx (i3 :. j0)

      !f01 = (weightX0Y1 *) <$> getPx (i0 :. j1)
      !f11 = (weightX1Y1 *) <$> getPx (i1 :. j1)
      !f21 = (weightX2Y1 *) <$> getPx (i2 :. j1)
      !f31 = (weightX3Y1 *) <$> getPx (i3 :. j1)

      !f02 = (weightX0Y2 *) <$> getPx (i0 :. j2)
      !f12 = (weightX1Y2 *) <$> getPx (i1 :. j2)
      !f22 = (weightX2Y2 *) <$> getPx (i2 :. j2)
      !f32 = (weightX3Y2 *) <$> getPx (i3 :. j2)

      !f03 = (weightX0Y3 *) <$> getPx (i0 :. j3)
      !f13 = (weightX1Y3 *) <$> getPx (i1 :. j3)
      !f23 = (weightX2Y3 *) <$> getPx (i2 :. j3)
      !f33 = (weightX3Y3 *) <$> getPx (i3 :. j3)

      !w = weightX0Y0 + weightX1Y0 + weightX2Y0 + weightX3Y0
         + weightX0Y1 + weightX1Y1 + weightX2Y1 + weightX3Y1
         + weightX0Y2 + weightX1Y2 + weightX2Y2 + weightX3Y2
         + weightX0Y3 + weightX1Y3 + weightX2Y3 + weightX3Y3
  {-# INLINE interpolate #-}
