{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies, ViewPatterns, MultiParamTypeClasses, UndecidableInstances #-}

module HIP.Pixel.RGB (
  RGB (..)
  ) where

import HIP.Interface (Pixel(..))

data RGB = RGB !Double !Double !Double deriving Eq


instance Pixel RGB where
  type Inner RGB = Double
  pixel d = RGB d d d
  {-# INLINE pixel #-}

  pxOp f (RGB r g b) = RGB (f r) (f g) (f b)
  {-# INLINE pxOp #-}

  pxOp2 f (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (f r1 r2) (f g1 g2) (f b1 b2)
  {-# INLINE pxOp2 #-}

  size _ = 3
  {-# INLINE size #-}

  ref 0 (RGB r _ _) = r
  ref 1 (RGB _ g _) = g
  ref 2 (RGB _ _ b) = b
  ref n px = error ("Referencing "++show n++"is out of bounds for "++showType px)
  {-# INLINE ref #-}

  strongest (RGB r g b) = pixel . maximum $ [r, g, b]
  {-# INLINE strongest #-}

  weakest (RGB r g b) = pixel . minimum $ [r, g, b]
  {-# INLINE weakest #-}

  showType _ = "RGB"
  

instance Num RGB where
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


instance Fractional RGB where
  (/)            = pxOp2 (/)
  recip          = pxOp recip
  fromRational n = pixel . fromRational $ n


instance Floating RGB where
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


instance Ord RGB where
  compare !(RGB r1 g1 b1) !(RGB r2 g2 b2) = compare (r1, g1, b1) (r2, g2, b2)
  {-# INLINE compare #-}

instance Show RGB where
  show (RGB r g b) = "<RGB:("++show r++"|"++show g++"|"++show b++")>"
  {-# INLINE show #-}
