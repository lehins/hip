{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Image.Processing.Interpolation
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Interpolation (
  Interpolation(..), Nearest(..), Bilinear(..)
  ) where

import Graphics.Image.Interface

-- | Implementation for an interpolation method.
class Interpolation method where

  -- | Construct a new pixel by using information from neighboring pixels.
  interpolate :: (Elevator e, Num e, ColorSpace cs) =>
                 method (Pixel cs e) -- ^ Interpolation method
              -> (Int, Int)          -- ^ Image dimensions @m@ rows and @n@ columns.
              -> ((Int, Int) -> Pixel cs e)
                 -- ^ Lookup function that returns a pixel at @i@th and @j@th
                 -- location.
              -> (Double, Double) -- ^ Real values of @i@ and @j@ index
              -> Pixel cs e


-- | Nearest Neighbor interpolation method.
data Nearest px = Nearest !(Border px) deriving Show


-- | Bilinear interpolation method.
data Bilinear px = Bilinear !(Border px) deriving Show


instance Interpolation Nearest where

  interpolate (Nearest border) !sz !getPx !(round -> i, round -> j) =
    handleBorderIndex border sz getPx (i, j)
  {-# INLINE interpolate #-}


instance Interpolation Bilinear where

  interpolate (Bilinear border) !sz !getPx !(i, j) =
    a00 + a10*iPx + a01*jPx + a11*iPx*jPx where
    getPx' = handleBorderIndex border sz getPx
    {-# INLINE getPx' #-}
    !(i0, j0) = (floor i, floor j)
    !(i1, j1) = (i0 + 1, j0 + 1)
    !iPx = fromDouble $ fromChannel (i - fromIntegral i0)
    !jPx = fromDouble $ fromChannel (j - fromIntegral j0)
    !f00 = getPx' (i0, j0)
    !f10 = getPx' (i1, j0)
    !f01 = getPx' (i0, j1) 
    !f11 = getPx' (i1, j1)
    !a00 = f00
    !a10 = f10 - f00
    !a01 = f01 - f00
    !a11 = f11 + f00 - f10 - f01
  {-# INLINE interpolate #-}

  {-
  interpolate (Bilinear border) !sz !getPx !(i, j) = fi0 + jPx*(fi1-fi0) where
    getPx' = handleBorderIndex border sz getPx
    {-# INLINE getPx' #-}
    !(i0, j0) = (floor i, floor j)
    !(i1, j1) = (i0 + 1, j0 + 1)
    !iPx = fromDouble $ fromChannel (i - fromIntegral i0)
    !jPx = fromDouble $ fromChannel (j - fromIntegral j0)
    !f00 = getPx' (i0, j0)
    !f10 = getPx' (i1, j0)
    !f01 = getPx' (i0, j1) 
    !f11 = getPx' (i1, j1) 
    !fi0 = f00 + iPx*(f10-f00)
    !fi1 = f01 + iPx*(f11-f01)
  {-# INLINE interpolate #-}
  -}
