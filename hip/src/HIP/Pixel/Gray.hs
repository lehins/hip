{-# LANGUAGE BangPatterns, DeriveDataTypeable, MultiParamTypeClasses, TypeFamilies,
UndecidableInstances, ViewPatterns, MagicHash #-}

module HIP.Pixel.Gray (
  Gray (..)
  ) where

import Data.Data
import HIP.Pixel.Base (Pixel(..))

-- | A Gray pixel with 'Double' precision.
data Gray = Gray {-# UNPACK #-} !Double deriving (Typeable, Data, Eq)


pxOp :: (Double -> Double) -> Gray -> Gray
pxOp !f !(Gray y)              = Gray (f y)
{-# INLINE pxOp #-}

pxOp2 :: (Double -> Double -> Double) -> Gray -> Gray -> Gray
pxOp2 !f !(Gray y1) !(Gray y2) = Gray (f y1 y2)
{-# INLINE pxOp2 #-}


instance Pixel Gray where
  type Channel Gray = Double
  fromDouble = Gray
  {-# INLINE fromDouble #-}
  
  arity _ = 1
  {-# INLINE arity #-}

  ref !(Gray y) 0 = y
  ref !px      !n = error ("Referencing "++show n++"is out of bounds for "++show (typeOf px))
  {-# INLINE ref #-}

  update _  0 y = Gray y
  update px n _ = error ("Updating "++show n++"is out of bounds for "++show (typeOf px))
  {-# INLINE update #-}

  apply !(f:_) !(Gray y) = Gray $ f y
  apply _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply #-}

  apply2 !(f:_) !(Gray y1) !(Gray y2) = Gray $ f y1 y2
  apply2 _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply2 #-}

  maxChannel !(Gray y) = y
  {-# INLINE maxChannel #-}

  minChannel !(Gray y) = y
  {-# INLINE minChannel #-}


instance Num Gray where
  (+)         = pxOp2 (+)
  {-# INLINE (+) #-}

  (-)         = pxOp2 (-)
  {-# INLINE (-) #-}

  (*)         = pxOp2 (*)
  {-# INLINE (*) #-}

  abs         = pxOp abs
  {-# INLINE abs #-}
  
  signum      = pxOp signum
  {-# INLINE signum #-}

  fromInteger = Gray . fromIntegral
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
  (<=) !(Gray y1) !(Gray y2) = y1 <= y2
  {-# INLINE (<=) #-}


instance Show Gray where
  show (Gray y) = "<Gray:("++show y++")>"
