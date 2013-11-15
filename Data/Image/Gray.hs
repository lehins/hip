{-# LANGUAGE TemplateHaskell, ViewPatterns, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

module Data.Image.Gray (
  Gray (..)
  ) where

import Data.Image.Base
import Data.Image.Internal
import Data.Array.Repa.Eval
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V

data Gray = Gray Double
          | GrayA Double Double deriving Eq



instance Pixel Gray where
  pxOp f (Gray y) = Gray (f y)
  pxOp f (GrayA y a) = GrayA (f y) (f a)
  
  pxOp2 f (Gray y1) (Gray y2) = Gray (f y1 y2)
  pxOp2 f (GrayA y1 a) (Gray y2) = GrayA (f y1 y2) a
  pxOp2 f (Gray y1) (GrayA y2 a) = GrayA (f y1 y2) a
  pxOp2 f (GrayA y1 a1) (GrayA y2 a2) = GrayA (f y1 y2) (f a1 a2)

  strongest (Gray y) = Gray y
  strongest (GrayA y a) = GrayA m m where m = max y a

  weakest (Gray y) = Gray y
  weakest (GrayA y a) = GrayA m m where m = min y a

getY (Gray y) = y
getY (GrayA y _) = y

getA (GrayA _ a) = a
getA _ = 1

instance Num Gray where
  (+)           = pxOp2 (+)
  (-)           = pxOp2 (-)
  (*)           = pxOp2 (*)
  abs           = pxOp abs
  signum        = pxOp signum
  fromInteger n = Gray . fromIntegral $ n 

instance Fractional Gray where
  (/)          = pxOp2 (/)
  recip        = pxOp recip
  fromRational = Gray . fromRational

instance Floating Gray where
  pi      = Gray pi
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

instance Ord Gray where
  ((getY . strongest) -> m1) <= ((getY . strongest) -> m2) = m1 <= m2


instance Show Gray where
  show (Gray y) = "<Gray:("++show y++")>"
  show (GrayA y a) = "<GrayA:("++show y++"|"++show a++")>"

instance Elt Gray where
  {-# INLINE touch #-}
  touch (Gray y) = touch y
  touch (GrayA y a) = touch y >> touch a
  
  {-# INLINE zero #-}
  zero = 0

  {-# INLINE one #-}
  one = 1


unboxGray (Gray y) = (y, Nothing)
unboxGray (GrayA y a) = (y, Just a)
boxGray (y, Nothing) = Gray y
boxGray (y, Just a) = GrayA y a

derivingUnbox "GrayPixel"
    [t| (V.Unbox Double) => Gray -> (Double, Maybe Double) |]
    [| unboxGray |]
    [| boxGray |]

