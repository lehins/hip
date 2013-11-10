{-# LANGUAGE TypeFamilies, TemplateHaskell, ViewPatterns, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses #-}

module Data.Image.Color (
  Color (..)
  ) where

import Data.Image.Internal
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V (Unbox)

data Color = RGB Double Double Double
           | RGBA Double Double Double Double deriving (Show)

instance Pixel Color where
  data Image Color = ColorImage (RepaImage Color)

  width (ColorImage img) = rWidth img

  height (ColorImage img) = rHeight img

  ref (ColorImage img) x y = rRef img x y

  makeImage w h op = ColorImage $ rMakeImage w h op

  fromVector w h v = ColorImage $ rFromVector w h v

  toVector (ColorImage img) = rToVector img

  compute (ColorImage img) = ColorImage . rCompute $ img


cPxOp1 op (RGB r g b) = RGB (op r) (op g) (op b)
cPxOp1 op (RGBA r g b a) = RGBA (op r) (op g) (op b) (op a)

cPxOp op (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (op r1 r2) (op g1 g2) (op b1 b2)
cPxOp op (RGBA r1 g1 b1 a) (RGB r2 g2 b2) = RGBA (op r1 r2) (op g1 g2) (op b1 b2) a
cPxOp op (RGB r1 g1 b1) (RGBA r2 g2 b2 a) = RGBA (op r1 r2) (op g1 g2) (op b1 b2) a
cPxOp op (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) =
  RGBA (op r1 r2) (op g1 g2) (op b1 b2) (op a1 a2)

instance Num Color where
  (+) = cPxOp (+)
  (-) = cPxOp (-)
  (*) = cPxOp (*)
  abs = cPxOp1 abs
  signum = cPxOp1 signum
  fromInteger (fromIntegral -> n) = RGB n n n


unboxColor (RGB r g b) = (r, g, b, Nothing)
unboxColor (RGBA r g b a) = (r, g, b, Just a)
boxColor (r, g, b, Nothing) = RGB r g b
boxColor (r, g, b, Just a) = RGBA r g b a

derivingUnbox "Color"
    [t| (V.Unbox Double) => Color -> (Double, Double, Double, Maybe Double) |]
    [| unboxColor |]
    [| boxColor |]


