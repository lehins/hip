{-# LANGUAGE FunctionalDependencies, TypeFamilies, ViewPatterns, TemplateHaskell, MultiParamTypeClasses, FlexibleContexts #-}

module Data.Image.Base where

import Prelude hiding ((++))
import Data.Maybe
import Data.Default
import Data.Vector.Unboxed.Deriving
import Data.Array.Repa.Eval
import qualified Data.List as L ((++))
import qualified Data.Vector.Unboxed as V

type PixelOp px = Int -> Int -> px



class (Elt px, V.Unbox px, Floating px, Fractional px, Num px, Eq px, Show px) =>
      Pixel px where
  pxOp :: (Double -> Double) -> px -> px

  pxOp2 :: (Double -> Double -> Double) -> px -> px -> px

  strongest :: px -> px

  weakest :: px -> px

class Pixel px => Processable img px | px -> img where

  width :: Pixel px => img px -> Int

  height :: Pixel px => img px -> Int

  ref :: Pixel px => img px -> Int -> Int -> px

  makeImage :: Pixel px => Int -> Int -> PixelOp px -> img px

  imageMap :: (Pixel px, Pixel px1) => (px -> px1) -> img px -> img px1

  imageZipWith :: (Pixel px, Pixel px2, Pixel px3) =>
                  (px -> px2 -> px3) -> img px -> img px2 -> img px3

  imageFold :: Pixel px => (px -> px -> px) -> px -> img px -> px

  fromVector :: Pixel px => Int -> Int -> V.Vector px -> img px

  toVector :: Pixel px => img px -> V.Vector px

  compute :: Pixel px => img px -> img px



derivingUnbox "Maybe"
    [t| (Default a, V.Unbox a) => Maybe a -> (Bool, a) |]
    [| maybe (False, def) (\ x -> (True, x)) |]
    [| \ (b, x) -> if b then Just x else Nothing |]
