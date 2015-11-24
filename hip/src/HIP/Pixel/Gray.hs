{-# LANGUAGE BangPatterns, MultiParamTypeClasses, TypeFamilies,
UndecidableInstances, ViewPatterns #-}

module HIP.Pixel.Gray (
  Gray (..)
  ) where

import HIP.Interface (Pixel(..))

-- | A Gray pixel with 'Double' precision.
newtype Gray = Gray Double deriving Eq


instance Pixel Gray where
  type Inner Gray = Double
  pixel                          = Gray 
  {-# INLINE pixel #-}
  
  pxOp !f !(Gray y)              = Gray (f y)
  {-# INLINE pxOp #-}
  
  pxOp2 !f !(Gray y1) !(Gray y2) = Gray (f y1 y2)
  {-# INLINE pxOp2 #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref 0 !(Gray y) = y
  ref n px = error ("Referencing "++show n++"is out of bounds for "++showType px)
  {-# INLINE ref #-}

  apply !(f:_) !(Gray y) = Gray $ f y
  apply _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply #-}

  apply2 !(f:_) !(Gray y1) !(Gray y2) = Gray $ f y1 y2
  apply2 _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply2 #-}

  apply2t !(f1:_) !(Gray y1) !(Gray y2) = (Gray y1', Gray y2') where (y1', y2') = (f1 y1 y2)
  apply2t _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply2t #-}

  strongest                      = id
  {-# INLINE strongest #-}

  weakest                        = id
  {-# INLINE weakest #-}

  showType _                     = "Gray"


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
  {-# INLINE show #-}
