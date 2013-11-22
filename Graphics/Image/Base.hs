{-# LANGUAGE FunctionalDependencies, TypeFamilies, ViewPatterns, TemplateHaskell, MultiParamTypeClasses, FlexibleContexts #-}

module Graphics.Image.Base where

import Prelude hiding ((++))
import qualified Prelude as P (floor)
import Data.Array.Repa.Eval
import qualified Data.Vector.Unboxed as V


class (Elt px, V.Unbox px, Floating px, Fractional px, Num px, Eq px, Show px) =>
      Pixel px where
  pixel :: Double -> px
       
  pxOp :: (Double -> Double) -> px -> px

  pxOp2 :: (Double -> Double -> Double) -> px -> px -> px

  strongest :: px -> px

  weakest :: px -> px


class Pixel px => Processable img px | px -> img where

  {-| Get the number of rows in the image -}
  rows :: Pixel px => img px -> Int

  {-| Get the number of columns in the image -}
  cols :: Pixel px => img px -> Int

  {-| Get a pixel at m-th row and n-th column -}
  ref :: Pixel px => img px -> Int -> Int -> px

  -- | Make an Image by supplying number of rows, number of columns and a
  -- function that returns a pixel value at the m n location which are provided
  -- as arguments.
  make :: Pixel px => Int -> Int -> (Int -> Int -> px) -> img px

  {-| Map a function over an image with a function. -}
  map :: (Pixel px, Pixel px1) => (px -> px1) -> img px -> img px1

  -- | Zip two Images with a function. Images do not have to hold the same type
  -- of pixels.
  zipWith :: (Pixel px, Pixel px2, Pixel px3) =>
                  (px -> px2 -> px3) -> img px -> img px2 -> img px3

  {-| Fold over an Image. -}
  fold :: Pixel px => (px -> px -> px) -> px -> img px -> px

  -- | Traverse the image.
  traverse :: Pixel px =>
              img px ->
              (Int -> Int -> (Int, Int)) ->
              ((Int -> Int -> px) -> Int -> Int -> px1) ->
              img px1

  {-| O(1) Convert a Vector to an Image by supplying width, height and a vector -}
  fromVector :: Pixel px => Int -> Int -> V.Vector px -> img px

  {-| O(1) Convert an Image to a Vector of length: width*height -}
  toVector :: Pixel px => img px -> V.Vector px

  dims :: Pixel px => img px -> (Int, Int)

  {-| Get a pixel at m n location with default pixel. If m or n are out of
      bounds it will return the default pixel.
  -}
  refd :: Pixel px => img px -> px -> Int -> Int -> px
  refd img@(dims -> (w, h)) def x y
     | x >= 0 && y >= 0 && x < w && y < h = ref img x y
     | otherwise = def

