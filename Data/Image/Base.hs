{-# LANGUAGE TypeFamilies, ViewPatterns, TemplateHaskell, MultiParamTypeClasses, InstanceSigs #-}

module Data.Image.Base (
  Pixel (..), PixelOp,
  ) where

import Prelude hiding ((++))
import Data.Maybe
import Data.Default
import Data.Vector.Unboxed.Deriving
import Data.Array.Repa.Eval
import qualified Data.List as L ((++))
import qualified Data.Vector.Unboxed as V

type PixelOp px = Int -> Int -> px



class (V.Unbox px, Floating px, Fractional px, Num px, Eq px, Show px) =>
      Pixel px where
  data Image px :: *

  liftPx :: (Double -> Double) -> px -> px

  liftPx2 :: (Double -> Double -> Double) -> px -> px -> px

  width :: Image px -> Int

  height :: Image px -> Int

  ref :: Image px -> Int -> Int -> px

  makeImage :: Int -> Int -> PixelOp px -> Image px

  singleton :: px -> Image px
  singleton px = makeImage 1 1 (\_ _ -> px)

  imageMap :: (px -> px) -> Image px -> Image px

  imageZipWith :: (px -> px -> px) -> Image px -> Image px -> Image px

  --imageFold :: (px -> px -> px) -> px -> Image px -> px

  fromVector :: Int -> Int -> V.Vector px -> Image px

  toVector :: Image px -> V.Vector px

  compute :: Image px -> Image px


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


derivingUnbox "Maybe"
    [t| (Default a, V.Unbox a) => Maybe a -> (Bool, a) |]
    [| maybe (False, def) (\ x -> (True, x)) |]
    [| \ (b, x) -> if b then Just x else Nothing |]
