{-# LANGUAGE TypeFamilies #-}

module Data.Image.Gray (
  Gray (..)
  ) where

import Data.Image.Internal

data Gray = Gray Double
          | GrayA Double Double deriving (Show)

instance Pixel Gray where
  data Image Gray = GrayImage (VectorImage Gray)
  
  width (GrayImage i) = vWidth i
  
  height (GrayImage i) = vHeight i

  ref (GrayImage i) = vRef i

  fromVector w h v = GrayImage $ vFromVector w h v

  makeImage w h op = GrayImage $ vMake w h op
