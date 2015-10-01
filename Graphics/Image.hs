module Graphics.Image (
  Image, Abstract(..), Pixel(..), Convertable(..),
  module Graphics.Image.Gray,
  module Graphics.Image.Color,
  module Graphics.Image.Complex,
  hsiToGrays, rgbToHsi, hsiToRgb
  ) where

import Graphics.Image.Definition
import Graphics.Image.Gray
import Graphics.Image.Color
import Graphics.Image.Complex
--import Graphics.Image.IO
--import Graphics.Image.Interactive

hsiToGrays :: Image HSI -> (Image Gray, Image Gray, Image Gray)
{-# INLINE hsiToGrays #-}
hsiToGrays = convert

{-
graysToHsi :: (Image Gray, Image Gray, Image Gray) -> Image HSI
{-# INLINE graysToHsi #-}
graysToHsi = convert
-}

rgbToHsi :: Image RGB -> Image HSI
{-# INLINE rgbToHsi #-}
rgbToHsi = convert

hsiToRgb :: Image HSI -> Image RGB
{-# INLINE hsiToRgb #-}
hsiToRgb = convert

