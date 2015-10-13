{-# LANGUAGE BangPatterns #-}
module Graphics.Image.Interpolation (
  Interpolation(..)
  ) where
import Graphics.Image.Interface hiding (Interpolation)
import qualified Graphics.Image.Interface as I (Interpolation)


data Interpolation = None     -- ^ Linear or zero order interpolation.
                   | Bilinear -- ^ Bilinear or first order interpolation at
                              -- given @i@ @j@ location.


instance I.Interpolation Interpolation where
  interpolate None          _  _  _ !getPx !i !j = getPx (round i) (round j)
  interpolate Bilinear !defPx !m !n !getPx !i !j = fi0 + jPx*(fi1-fi0) where
    !(!i0, !j0) = (floor i, floor j)
    !(!i1, !j1) = (i0 + 1, j0 + 1)
    !iPx = pixel (i - (fromIntegral i0))
    !jPx = pixel (j - (fromIntegral j0))
    !f00 = getPxDefault i0 j0
    !f10 = getPxDefault i1 j0
    !f01 = getPxDefault i0 j1 
    !f11 = getPxDefault i1 j1 
    !fi0 = f00 + iPx*(f10-f00)
    !fi1 = f01 + iPx*(f11-f01)
    getPxDefault !i' !j' = if i' >= 0 && j' >= 0 && i' < m && j' < n
                           then getPx i' j'
                           else defPx
    {-# INLINE getPxDefault #-}
  {-# INLINE interpolate #-}
