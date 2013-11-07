
module Data.Image where


import Data.Image.Gray
import Data.Image.Color
import Data.Image.Internal

data DynamicImage = GrayImage (Image Gray)
                  | ColorImage (Image Color)

isColorImage (ColorImage _) = True
isColorImage _ = False

isGrayImage (GrayImage _) = True
isGrayImage _ = False

toColorImage (ColorImage i) = i

toGrayImage (GrayImage i) = i
