{-# LANGUAGE TypeFamilies, TemplateHaskell, ViewPatterns, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses #-}

module Data.Image.Color (
  Color (..)
  ) where

import Data.Image.Internal
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V (Unbox)

data Color = RGB Double Double Double
           | RGBA Double Double Double Double deriving Eq

--type instance Internal Color = Double

instance Pixel Color where
  data Image Color = ColorImage (RepaImage Color)

  liftPx f (RGB r g b) = RGB (f r) (f g) (f b)
  liftPx f (RGBA r g b a) = RGBA (f r) (f g) (f b) (f a)

  liftPx2 f (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (f r1 r2) (f g1 g2) (f b1 b2)
  liftPx2 f (RGBA r1 g1 b1 a) (RGB r2 g2 b2) = RGBA (f r1 r2) (f g1 g2) (f b1 b2) a
  liftPx2 f (RGB r1 g1 b1) (RGBA r2 g2 b2 a) = RGBA (f r1 r2) (f g1 g2) (f b1 b2) a
  liftPx2 f (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) =
    RGBA (f r1 r2) (f g1 g2) (f b1 b2) (f a1 a2)

  width (ColorImage img) = rWidth img

  height (ColorImage img) = rHeight img

  ref (ColorImage img) x y = rRef img x y

  makeImage w h op = ColorImage $ rMakeImage w h op

  fromVector w h v = ColorImage $ rFromVector w h v

  toVector (ColorImage img) = rToVector img

  compute (ColorImage img) = ColorImage . rCompute $ img

instance Num Color where
  (+)           = liftPx2 (+)
  (-)           = liftPx2 (-)
  (*)           = liftPx2 (*)
  abs           = liftPx abs
  signum        = liftPx signum
  fromInteger n = RGB nd nd nd where nd = fromIntegral n

instance Fractional Color where
  (/)            = liftPx2 (/)
  recip          = liftPx recip
  fromRational n = RGB nd nd nd where nd = fromRational n

instance Floating Color where
  pi      = RGB pi pi pi
  exp     = liftPx exp
  log     = liftPx log
  sin     = liftPx sin
  cos     = liftPx cos
  asin    = liftPx asin
  atan    = liftPx atan
  acos    = liftPx acos
  sinh    = liftPx sinh
  cosh    = liftPx cosh
  asinh   = liftPx asinh
  atanh   = liftPx atanh
  acosh   = liftPx acosh

instance RealPixel Color where
  safeDiv = liftPx2 op where op x y = if y == 0 then 0 else x / y
  fromDouble d = RGB d d d

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


