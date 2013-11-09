{-# LANGUAGE TypeFamilies #-}

module Data.Image.Gray (
  Gray (..)
  ) where

import Data.Image.Pixel (Gray(..))
import Data.Image.Internal


instance Pixel Gray where
  data Image Gray = GrayImage (VectorImage Gray)
  
  width (GrayImage i) = vWidth i
  
  height (GrayImage i) = vHeight i

  ref (GrayImage i) = vRef i

  makeImage w h op = GrayImage $ vMake w h op

  fromVector w h v = GrayImage $ vFromVector w h v

  toVector (GrayImage img) = vPixels img
