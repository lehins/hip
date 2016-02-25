{-# LANGUAGE BangPatterns, ViewPatterns #-}
module Graphics.Image.Processing.Interpolation (
  Interpolation(..), Nearest(..), Bilinear(..)
  ) where

import Graphics.Image.Interface
import Graphics.Image.ColorSpace




class Interpolation method where
  interpolate :: (Elevator e, Num e, ColorSpace cs) =>
                 method           -- ^ Interpolation method
              -> (Int, Int)       -- ^ Image dimensions @m@ rows and @n@ columns.
              -> ((Int, Int) -> Pixel cs e)
                 -- ^ Lookup function that returns a pixel at @i@th and @j@th
                 -- location.
              -> Pixel cs e -- ^ Default pixel to use when out of bounds.
              -> (Double, Double) -- ^ real values of @i@ and @j@ index
              -> Pixel cs e


-- | Nearest Neighbor interpolation method.
data Nearest = Nearest


-- | Bilinear interpolation method.
data Bilinear = Bilinear


instance Interpolation Nearest where
  interpolate _ !(m, n) !getPx !defPx !(floor -> i, floor -> j) =
    if i >= 0 && j >= 0 && i < m && j < n then getPx (i, j) else defPx


instance Interpolation Bilinear where
  interpolate _ !(m, n) !getPx !defPx !(i, j) =
    if i0 >= 0 && j0 >= 0 && i0 < m && j0 < n then
      if i0 == (m-1) || j0 == (n-1) then getPx (i0, j0) else inter
    else defPx
    where
      !(i0, j0) = (floor i, floor j)
      !(i1, j1) = (i0 + 1, j0 + 1)
      inter = fi0 + jPx*(fi1-fi0) where
        !iPx = fromDouble $ fromChannel (i - fromIntegral i0)
        !jPx = fromDouble $ fromChannel (j - fromIntegral j0)
        !f00 = getPx (i0, j0)
        !f10 = getPx (i1, j0)
        !f01 = getPx (i0, j1) 
        !f11 = getPx (i1, j1) 
        !fi0 = f00 + iPx*(f10-f00)
        !fi1 = f01 + iPx*(f11-f01)
  {-# INLINE interpolate #-}
