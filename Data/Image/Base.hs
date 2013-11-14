{-# LANGUAGE TypeFamilies, ViewPatterns, TemplateHaskell, MultiParamTypeClasses, InstanceSigs #-}

module Data.Image.Base where

import Prelude hiding ((++))
import Data.Maybe
import Data.Default
import Data.Vector.Unboxed.Deriving
import Data.Array.Repa.Eval
--import qualified Data.Image.Internal as I
import qualified Data.List as L ((++))
import qualified Data.Vector.Unboxed as V

type PixelOp px = Int -> Int -> px

class (V.Unbox px, Floating px, Fractional px, Num px, Eq px, Show px) =>
      Pixel px where
  pxOp :: (Double -> Double) -> px -> px

  pxOp2 :: (Double -> Double -> Double) -> px -> px -> px
  

class Processable img where

  width :: Pixel px => img px -> Int

  height :: Pixel px => img px -> Int

  ref :: Pixel px => img px -> Int -> Int -> px

  makeImage :: Pixel px => Int -> Int -> PixelOp px -> img px

  imageMap :: (Pixel px1, Pixel px2) => (px1 -> px2) -> img px1 -> img px2

  imageZipWith :: (Pixel px1, Pixel px2, Pixel px3) =>
                  (px1 -> px2 -> px3) -> img px1 -> img px2 -> img px3

  fromVector :: Pixel px => Int -> Int -> V.Vector px -> img px

  toVector :: Pixel px => img px -> V.Vector px

  compute :: Pixel px => img px -> img px


{-
instance Pixel px => Num (Image px) where
  (+) = imageZipWith (+)
  (-) = imageZipWith (-)
  (*) = imageZipWith (*)
  abs = imageMap abs
  signum = imageMap signum
  fromInteger = singleton . fromInteger
  
instance Pixel px => Fractional (Image px) where
  (/) = imageZipWith (/)
  fromRational = singleton . fromRational

instance Pixel px => Floating (Image px) where
  pi      = singleton pi
  exp     = imageMap exp
  log     = imageMap log
  sin     = imageMap sin
  cos     = imageMap cos
  asin    = imageMap asin
  atan    = imageMap atan
  acos    = imageMap acos
  sinh    = imageMap sinh
  cosh    = imageMap cosh
  asinh   = imageMap asinh
  atanh   = imageMap atanh
  acosh   = imageMap acosh
-}

derivingUnbox "Maybe"
    [t| (Default a, V.Unbox a) => Maybe a -> (Bool, a) |]
    [| maybe (False, def) (\ x -> (True, x)) |]
    [| \ (b, x) -> if b then Just x else Nothing |]
