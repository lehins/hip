{-# LANGUAGE TemplateHaskell, ViewPatterns, MultiParamTypeClasses, TypeFamilies,
UndecidableInstances, BangPatterns #-}

module Graphics.Image.Gray (
  Gray (..)
  ) where

import Graphics.Image.Base
import Data.Array.Repa.Eval
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V

data Gray = Gray !Double deriving Eq


instance Pixel Gray where
  {-# INLINE pixel #-}
  pixel d = Gray d
  
  {-# INLINE pxOp #-}
  pxOp f (Gray y) = Gray (f y)
  
  {-# INLINE pxOp2 #-}
  pxOp2 f (Gray y1) (Gray y2) = Gray (f y1 y2)

  {-# INLINE strongest #-}
  strongest = id

  {-# INLINE weakest #-}
  weakest = id

instance Num Gray where
  {-# INLINE (+) #-}
  (+)           = pxOp2 (+)
  {-# INLINE (-) #-}
  (-)           = pxOp2 (-)
  {-# INLINE (*) #-}
  (*)           = pxOp2 (*)
  {-# INLINE abs #-}
  abs           = pxOp abs
  {-# INLINE signum #-}
  signum        = pxOp signum
  {-# INLINE fromInteger #-}
  fromInteger n = Gray . fromIntegral $ n

instance Fractional Gray where
  (/)          = pxOp2 (/)
  {-# INLINE (/) #-}
  recip        = pxOp recip
  {-# INLINE recip #-}
  fromRational = Gray . fromRational
  {-# INLINE fromRational #-}

instance Floating Gray where
  {-# INLINE pi #-}
  pi      = Gray pi
  {-# INLINE exp #-}
  exp     = pxOp exp
  {-# INLINE log #-}
  log     = pxOp log
  {-# INLINE sin #-}
  sin     = pxOp sin
  {-# INLINE cos #-}
  cos     = pxOp cos
  {-# INLINE asin #-}
  asin    = pxOp asin
  {-# INLINE atan #-}
  atan    = pxOp atan
  {-# INLINE acos #-}
  acos    = pxOp acos
  {-# INLINE sinh #-}
  sinh    = pxOp sinh
  {-# INLINE cosh #-}
  cosh    = pxOp cosh
  {-# INLINE asinh #-}
  asinh   = pxOp asinh
  {-# INLINE atanh #-}
  atanh   = pxOp atanh
  {-# INLINE acosh #-}
  acosh   = pxOp acosh

instance Ord Gray where
  {-# INLINE (<=) #-}
  (Gray y1) <= (Gray y2) = y1 <= y2


instance Show Gray where
  {-# INLINE show #-}
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

