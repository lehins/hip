{-# LANGUAGE TemplateHaskell, ViewPatterns, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

module Data.Image.Gray (
  Gray (..)
  ) where

import Data.Image.Base
import Data.Image.Internal
import Data.Array.Repa.Eval
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V

data Gray = Gray Double deriving Eq


instance Pixel Gray where
  pxOp f (Gray y) = Gray (f y)
  
  pxOp2 f (Gray y1) (Gray y2) = Gray (f y1 y2)

  strongest (Gray y) = Gray y

  weakest (Gray y) = Gray y

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
  (Gray y1) <= (Gray y2) = y1 <= y2


instance Show Gray where
  show (Gray y) = "<Gray:("++show y++")>"

instance Elt Gray where
  {-# INLINE touch #-}
  touch (Gray y) = touch y
  
  {-# INLINE zero #-}
  zero = 0

  {-# INLINE one #-}
  one = 1


derivingUnbox "GrayPixel"
    [t| (V.Unbox Double) => Gray -> Double |]
    [| \(Gray y) -> y |]
    [| \y -> Gray y |]

