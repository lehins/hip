{-# LANGUAGE BangPatterns, GADTs, FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, ViewPatterns #-}
module HIP.Algorithms.Interpolation (
  Interpolation(..), Method(..)
  ) where

import HIP.Pixel.Base (Pixel(..))


class Pixel px => Interpolation method px | px -> method where
  interpolate :: (RealFrac (Inner px), Pixel px) =>
                 method             -- ^ Interpolation method
              -> Int -> Int         -- ^ Image dimensions @m@ rows and @n@ columns.
              -> (Int -> Int -> px) -- ^ Lookup function that returns a pixel at @i@th
                                    -- and @j@th location.
              -> (Inner px) -> (Inner px)   -- ^ real values of @i@ and @j@ index
              -> px


-- | Interpolation Algorithms. All of them hold a default pixel to be used for
-- out of boundary locations.
data Method px where
  Nearest  :: Pixel px => px -> Method px
  Bilinear :: Pixel px => px -> Method px


instance Pixel px => Interpolation (Method px) px where
  interpolate (Nearest  defPx) !m !n !getPx (round -> !i) (round -> !j) =
    if i >= 0 && j >= 0 && i < m && j < n then getPx i j else defPx
          
  interpolate (Bilinear defPx) !m !n !getPx !i !j =
    if i0 >= 0 && j0 >= 0 && i0 < m && j0 < n then
      if i0 == (m-1) || j0 == (n-1) then getPx i0 j0 else inter
    else defPx
    where
      !(i0, j0) = (floor i, floor j)
      !(i1, j1) = (i0 + 1, j0 + 1)
      inter = fi0 + jPx*(fi1-fi0) where
        !iPx = pixel (i - (fromIntegral i0))
        !jPx = pixel (j - (fromIntegral j0))
        !f00 = getPx i0 j0
        !f10 = getPx i1 j0
        !f01 = getPx i0 j1 
        !f11 = getPx i1 j1 
        !fi0 = f00 + iPx*(f10-f00)
        !fi1 = f01 + iPx*(f11-f01)
  {-# INLINE interpolate #-}

