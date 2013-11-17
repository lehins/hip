{-# LANGUAGE TypeFamilies, TemplateHaskell, ViewPatterns, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses #-}

module Data.Image.Color (
  Color (..)
  ) where

import Data.Image.Internal
import Data.Array.Repa.Eval
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V (Unbox)

data Color = RGB Double Double Double deriving Eq

instance Pixel Color where

  pxOp f (RGB r g b) = RGB (f r) (f g) (f b)

  pxOp2 f (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (f r1 r2) (f g1 g2) (f b1 b2)

  strongest (RGB r g b) = RGB m m m where m = maximum [r, g, b]

  weakest (RGB r g b) = RGB m m m where m = minimum [r, g, b]

zipRGB (RGB r g b) = (r,g,b)
unzipRGB (r,g,b) = (RGB r g b)

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

instance Ord Color where
  (strongest -> RGB m1 _ _) <= (strongest -> RGB m2 _ _) =
    m1 <= m2

instance Elt Color where
  {-# INLINE touch #-}
  touch (RGB r g b) = touch r >> touch g >> touch b
  
  {-# INLINE zero #-}
  zero = 0

  {-# INLINE one #-}
  one = 1

instance Show Color where
  show (RGB r g b) = "<RGB:("++show r++"|"++show g++"|"++show b++")>"

derivingUnbox "ColorPixel"
    [t| (V.Unbox Double) => Color -> (Double, Double, Double) |]
    [| zipRGB |]
    [| unzipRGB |]


