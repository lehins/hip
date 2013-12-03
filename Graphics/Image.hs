{-# LANGUAGE ViewPatterns #-}
module Graphics.Image (
  Image, Processable(..), Pixel(..), Convertable(..),
  module Graphics.Image.Gray,
  module Graphics.Image.Color,
  module Graphics.Image.Complex,
  module Graphics.Image.IO,
  module Graphics.Image.Interactive,
  hsiToGrays, graysToHsi, rgbToHsi, hsiToRgb,
  ) where

import Prelude hiding (map, zipWith)
import Graphics.Image.Base
import Graphics.Image.Gray
import Graphics.Image.Color
import Graphics.Image.Complex
import Graphics.Image.IO
import Graphics.Image.Interactive
import qualified Data.Vector.Unboxed as V

hsiToGrays :: Image HSI -> (Image Gray, Image Gray, Image Gray)
{-# INLINE hsiToGrays #-}
hsiToGrays = convert

graysToHsi :: (Image Gray, Image Gray, Image Gray) -> Image HSI
{-# INLINE graysToHsi #-}
graysToHsi = convert

rgbToHsi :: Image RGB -> Image HSI
{-# INLINE rgbToHsi #-}
rgbToHsi = convert

hsiToRgb :: Image HSI -> Image RGB
{-# INLINE hsiToRgb #-}
hsiToRgb = convert

