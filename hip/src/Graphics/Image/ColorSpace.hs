{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.Image.ColorSpace (
  module Graphics.Image.ColorSpace.Gray,
  module Graphics.Image.ColorSpace.RGB
  ) where

import Graphics.Image.Interface
import Graphics.Image.ColorSpace.Gray
import Graphics.Image.ColorSpace.RGB


instance Convertible PixelRGB PixelGray where
  convert (PixelRGB r g b) = PixelGray ((r + g + b)/3)

instance Convertible PixelGray PixelRGB where
  convert (PixelGray g) = fromChannel g


instance Convertible PixelGrayA PixelRGBA where
  convert (PixelGrayA g a) = addAlpha a $ fromChannel g


instance ToGray RGB where
  toPixelGray (PixelRGB r g b) = PixelGray ((r + g + b)/3)

instance ToGrayA RGBA where

  
instance ToRGB Gray where
  toPixelRGB (PixelGray g) = fromChannel g

instance ToRGBA GrayA where

