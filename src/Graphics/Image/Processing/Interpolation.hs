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
module Graphics.Image.Processing.Interpolation (
  Interpolation(..), Nearest(..), Bilinear(..)
  ) where

import Graphics.Image.Internal

-- | Implementation for an interpolation method.
class Interpolation method where

  -- | Construct a new pixel by using information from neighboring pixels.
  interpolate :: (Elevator a, RealFloat a, ColorModel cs e) =>
                 method -- ^ Interpolation method
              -> (Ix2 -> Pixel cs e)
                 -- ^ Lookup function that returns a pixel at @i@th and @j@th
                 -- location.
              -> (a, a) -- ^ Real values of @i@ and @j@ index
              -> Pixel cs e


-- | Nearest Neighbor interpolation method.
data Nearest = Nearest deriving Show


-- | Bilinear interpolation method.
data Bilinear = Bilinear deriving Show


instance Interpolation Nearest where

  interpolate Nearest getPx (i, j) = getPx (round i :. round j)
  {-# INLINE interpolate #-}


instance Interpolation Bilinear where
  interpolate Bilinear getPx (i, j) = fi0 + fmap (jWeight *) (fi1 - fi0)
    where
      !i0 = floor i
      !j0 = floor j
      !i1 = i0 + 1
      !j1 = j0 + 1
      !iWeight = fromRealFloat (i - fromIntegral i0)
      !jWeight = fromRealFloat (j - fromIntegral j0)
      !f00 = getPx (i0 :. j0)
      !f10 = getPx (i1 :. j0)
      !f01 = getPx (i0 :. j1)
      !f11 = getPx (i1 :. j1)
      !fi0 = f00 + fmap (iWeight *) (f10 - f00)
      !fi1 = f01 + fmap (iWeight *) (f11 - f01)
  {-# INLINE interpolate #-}
