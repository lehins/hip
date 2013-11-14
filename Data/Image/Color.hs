{-# LANGUAGE TypeFamilies, TemplateHaskell, ViewPatterns, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses #-}

module Data.Image.Color (
  Color (..),
  ColorImage
  ) where

import Data.Image.Internal
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V (Unbox)

data Color = RGB Double Double Double
           | RGBA Double Double Double Double deriving Eq

type ColorImage = Image Color

instance Pixel Color where

  pxOp f (RGB r g b) = RGB (f r) (f g) (f b)
  pxOp f (RGBA r g b a) = RGBA (f r) (f g) (f b) (f a)

  pxOp2 f (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (f r1 r2) (f g1 g2) (f b1 b2)
  pxOp2 f (RGBA r1 g1 b1 a) (RGB r2 g2 b2) = RGBA (f r1 r2) (f g1 g2) (f b1 b2) a
  pxOp2 f (RGB r1 g1 b1) (RGBA r2 g2 b2 a) = RGBA (f r1 r2) (f g1 g2) (f b1 b2) a
  pxOp2 f (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) =
    RGBA (f r1 r2) (f g1 g2) (f b1 b2) (f a1 a2)


instance Num Color where
  (+)           = pxOp2 (+)
  (-)           = pxOp2 (-)
  (*)           = pxOp2 (*)
  abs           = pxOp abs
  signum        = pxOp signum
  fromInteger n = RGB nd nd nd where nd = fromIntegral n

instance Fractional Color where
  (/)            = pxOp2 (/)
  recip          = pxOp recip
  fromRational n = RGB nd nd nd where nd = fromRational n

instance Floating Color where
  pi      = RGB pi pi pi
  exp     = pxOp exp
  log     = pxOp log
  sin     = pxOp sin
  cos     = pxOp cos
  asin    = pxOp asin
  atan    = pxOp atan
  acos    = pxOp acos
  sinh    = pxOp sinh
  cosh    = pxOp cosh
  asinh   = pxOp asinh
  atanh   = pxOp atanh
  acosh   = pxOp acosh


instance Show Color where
  show (RGB r g b) = "<RGB:("++show r++"|"++show g++"|"++show b++")>"
  show (RGBA r g b a) =
    "<RGBA:("++show r++"|"++show g++"|"++show b++"|"++show a++")>"

unboxColor (RGB r g b) = (r, g, b, Nothing)
unboxColor (RGBA r g b a) = (r, g, b, Just a)
boxColor (r, g, b, Nothing) = RGB r g b
boxColor (r, g, b, Just a) = RGBA r g b a

derivingUnbox "ColorPixel"
    [t| (V.Unbox Double) => Color -> (Double, Double, Double, Maybe Double) |]
    [| unboxColor |]
    [| boxColor |]


