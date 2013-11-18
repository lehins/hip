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

  {-| Get the width of an image -}
  width :: Pixel px => img px -> Int

  {-| Get the width of an image -}
  height :: Pixel px => img px -> Int

  dims :: Pixel px => img px -> (Int, Int)
  dims img = (width img, height img)

  {-| Get a pixel at x y coordinates -}
  ref :: Pixel px => img px -> Int -> Int -> px

  {-| Get a pixel at x y coordinates with default pixel. If x or y are out of
      bounds use the default pixel value.
  -}
  refd :: Pixel px => px -> img px -> Int -> Int -> px

  {-| Make an Image by supplying width, height and a function that takes x and y
      coordinates as arguments.
   -}
  make :: Pixel px => Int -> Int -> (Int -> Int -> px) -> img px

  {-| Map a function over an image with a function in Parallel -}
  map :: (Pixel px, Pixel px1) => (px -> px1) -> img px -> img px1

  {-| Zip two Images with a function in Parallel. Images don't have to hold the
      same type of pixels.
   -}
  zipWith :: (Pixel px, Pixel px2, Pixel px3) =>
                  (px -> px2 -> px3) -> img px -> img px2 -> img px3

  {-| Fold over an Image in Parallel. -}
  fold :: Pixel px => (px -> px -> px) -> px -> img px -> px

  traverse :: Pixel px => img px -> (Int -> Int -> (Int, Int)) ->
              ((Int -> Int -> px) -> Int -> Int -> px1) -> img px1

  {-| O(1) Convert a Vector to an Image by supplying width, height and a vector -}
  fromVector :: Pixel px => Int -> Int -> V.Vector px -> img px

  {-| O(1) Convert an Image to a Vector of length: width*height -}
  toVector :: Pixel px => img px -> V.Vector px

