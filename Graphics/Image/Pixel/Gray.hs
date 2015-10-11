{-# LANGUAGE TemplateHaskell, ViewPatterns, MultiParamTypeClasses, TypeFamilies,
UndecidableInstances, BangPatterns #-}

module Graphics.Image.Pixel.Gray (
  Gray (..)
  ) where

import Graphics.Image.Interface (Pixel(..))
import Data.Array.Repa.Eval
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V

data Gray = Gray !Double deriving Eq


instance Pixel Gray where
  pixel d                     = Gray d
  {-# INLINE pixel #-}
  
  pxOp f (Gray y)             = Gray (f y)
  {-# INLINE pxOp #-}
  
  pxOp2 f (Gray y1) (Gray y2) = Gray (f y1 y2)
  {-# INLINE pxOp2 #-}

  strongest                   = id
  {-# INLINE strongest #-}

  weakest                     = id
  {-# INLINE weakest #-}

  showType _                  = "Gray"


instance Num Gray where
  (+)           = pxOp2 (+)
  {-# INLINE (+) #-}

  (-)           = pxOp2 (-)
  {-# INLINE (-) #-}

  (*)           = pxOp2 (*)
  {-# INLINE (*) #-}

  abs           = pxOp abs
  {-# INLINE abs #-}
  signum        = pxOp signum
  {-# INLINE signum #-}

  fromInteger n = Gray . fromIntegral $ n
  {-# INLINE fromInteger #-}


instance Fractional Gray where
  (/)          = pxOp2 (/)
  {-# INLINE (/) #-}

  recip        = pxOp recip
  {-# INLINE recip #-}

  fromRational = Gray . fromRational
  {-# INLINE fromRational #-}


instance Floating Gray where
  pi      = Gray pi
  {-# INLINE pi #-}

  exp     = pxOp exp
  {-# INLINE exp #-}

  log     = pxOp log
  {-# INLINE log #-}

  sin     = pxOp sin
  {-# INLINE sin #-}

  cos     = pxOp cos
  {-# INLINE cos #-}

  asin    = pxOp asin
  {-# INLINE asin #-}

  atan    = pxOp atan
  {-# INLINE atan #-}

  acos    = pxOp acos
  {-# INLINE acos #-}

  sinh    = pxOp sinh
  {-# INLINE sinh #-}

  cosh    = pxOp cosh
  {-# INLINE cosh #-}

  asinh   = pxOp asinh
  {-# INLINE asinh #-}

  atanh   = pxOp atanh
  {-# INLINE atanh #-}

  acosh   = pxOp acosh
  {-# INLINE acosh #-}


instance Ord Gray where
  (Gray y1) <= (Gray y2) = y1 <= y2
  {-# INLINE (<=) #-}


instance Show Gray where
  show (Gray y) = "<Gray:("++show y++")>"
  {-# INLINE show #-}


instance Elt Gray where
  touch (Gray y) = touch y
  {-# INLINE touch #-}
  
  zero = 0
  {-# INLINE zero #-}

  one = 1
  {-# INLINE one #-}


derivingUnbox "GrayPixel"
    [t| (V.Unbox Double) => Gray -> Double |]
    [| \(Gray y) -> y |]
    [| \y -> Gray y |]

