module Graphics.Image (
  ) where

import Prelude hiding (map)
import Graphics.Image.Interface (Array, Image, ColorSpace(..))
import qualified Graphics.Image.Interface as I
{-
-- | Map a function over each pixel's channel in source image.
map :: Array arr cs e =>
       (e -> e) -- ^ A function that takes a pixel's channel value from a source
       -- image and returns pixel's new channel value at the same location for
       -- the result image.
    -> Image arr cs e -- ^ Source image.
    -> Image arr cs e -- ^ Result image.
map = I.map
{-# INLINE map #-}


-- | Map a function over each pixel in source image.
mapPx :: (Array arr cs' e', Array arr cs e) =>
         (Pixel cs' e' -> Pixel cs e) -- ^ A function that takes a pixel from a
         -- source image and returns a new pixel at the same location for the
         -- result image.
      -> Image arr cs' e' -- ^ Source image.
      -> Image arr cs e -- ^ Result image.
mapPx = I.mapPx
{-# INLINE mapPx #-}


-- | Map an index aware function over each pixel in source image.
imapPx :: (Array arr cs' e', Array arr cs e) =>
          ((Int, Int) -> Pixel cs' e' -> Pixel cs e)  -- ^ A function that takes
         -- an index @(i, j)@, pixel at that location and returns a new pixel at
         -- the same location for the result image.
       -> Image arr cs' e' -- ^ Source image.
       -> Image arr cs e -- ^ Result image.
imapPx = I.imapPx
{-# INLINE imapPx #-}
-}
