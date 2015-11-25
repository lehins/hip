{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TypeFamilies, ViewPatterns, MultiParamTypeClasses, UndecidableInstances #-}

module HIP.Pixel.Tuple where

import HIP.Interface (Pixel(..))


pxOp :: (Double -> Double) -> (Double, Double, Double) -> (Double, Double, Double)
pxOp f (r, g, b) = (f r, f g, f b)
{-# INLINE pxOp #-}

pxOp2 :: (Double -> Double -> Double)
      -> (Double, Double, Double)
      -> (Double, Double, Double)
      -> (Double, Double, Double)
pxOp2 f (r1, g1, b1) (r2, g2, b2) = (f r1 r2, f g1 g2, f b1 b2)
{-# INLINE pxOp2 #-}


instance Pixel (Double, Double, Double) where
  type Inner (Double, Double, Double) = Double
  pixel d = (d, d, d)
  {-# INLINE pixel #-}

  arity _ = 3
  {-# INLINE arity #-}

  ref 0 (r, _, _) = r
  ref 1 (_, g, _) = g
  ref 2 (_, _, b) = b
  ref n px = error ("Referencing "++show n++"is out of bounds for "++showType px)
  {-# INLINE ref #-}

  apply !(f1:f2:f3:_) !(r, g, b) = (f1 r, f2 g, f3 b)
  apply _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply #-}

  apply2 !(f1:f2:f3:_) !(r1, g1, b1) !(r2, g2, b2) = (f1 r1 r2, f2 g1 g2, f3 b1 b2)
  apply2 _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply2 #-}

  showType _ = "RGB"
  

instance Num (Double, Double, Double) where
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
  
  fromInteger n = pixel . fromIntegral $ n
  {-# INLINE fromInteger #-}


instance Fractional (Double, Double, Double) where
  (/)            = pxOp2 (/)
  recip          = pxOp recip
  fromRational n = pixel . fromRational $ n


instance Floating (Double, Double, Double) where
  {-# INLINE pi #-}
  pi      = pixel pi
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

