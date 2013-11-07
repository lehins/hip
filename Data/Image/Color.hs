{-# LANGUAGE TypeFamilies #-}

module Data.Image.Color (
  Color (..)
  ) where

import Data.Image.Internal

data Color = RGB Double Double Double
           | RGBA Double Double Double Double deriving (Show)

instance Pixel Color where
  data Image Color = ColorImage (VectorImage Color)
  
  width (ColorImage i) = vWidth i
  
  height (ColorImage i) = vHeight i

  ref (ColorImage i) = vRef i

  makeImage w h op = ColorImage $ vMake w h op

  fromVector w h v = ColorImage $ vFromVector w h v

  toVector (ColorImage img) = vPixels img

