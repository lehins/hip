{-# LANGUAGE TypeFamilies #-}

module Data.Image.Internal (
  Pixel (..), PixelOp,
  VectorImage(..),
  vRef, vMake, vFromVector
  ) where

import qualified Data.Vector as V
import Data.Functor

type PixelOp px = Int -> Int -> px

class Pixel px where
  data Image px :: *
       
  width :: Image px -> Int
  
  height :: Image px -> Int

  ref :: Image px -> Int -> Int -> px

  makeImage :: Int -> Int -> PixelOp px -> Image px

  fromVector :: Int -> Int -> V.Vector px -> Image px


instance Pixel img => Show (Image img) where
  show img = "<Image: "++show (width img)++"x"++show (height img)++">"

instance Functor Image where
  fmap = fmap


class Viewable img where
  toString :: img -> String




data VectorImage px = VectorImage { vWidth :: Int,
                                    vHeight :: Int,
                                    vPixels :: V.Vector px }

vRef i x y = vPixels i V.! (vWidth i * y + x)

vMake w h f = VectorImage w h (V.generate (w*h) vOp) where
  vOp n = if h < 1 || w < 1
          then error $ "Invalid dimensions: "++show w++"x"++show h
          else f x y where x = n `mod` w
                           y = (n - x) `div` w

vFromVector = VectorImage 

instance Show (VectorImage px) where
  show img = "<Image : "++show (vWidth img)++"x"++show (vHeight img)++">"

instance Functor VectorImage where
  fmap f (VectorImage width height pixels) = VectorImage width height (fmap f pixels)
