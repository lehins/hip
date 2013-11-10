{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Data.Image.Generic where

import Data.Image.Pixel

type PixelOp px = Int -> Int -> px

type family Processable (img :: * -> *) :: * -> *

class (V.Unbox px, Num px) => Pixel px

class Image img px where
  bWidth :: img px -> Int

  bHeight :: img px -> Int

  bRef :: img px -> Int -> Int -> px

  bMakeImage :: Int -> Int -> PixelOp px -> img px

  bFromVector :: Int -> Int -> V.Vector px -> img px

  bToVector :: img px -> V.Vector px
