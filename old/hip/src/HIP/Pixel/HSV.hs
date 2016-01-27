--{-# LANGUAGE BangPatterns, FlexibleInstances, MultiParamTypeClasses, 
--             TypeFamilies, UndecidableInstances, ViewPatterns #-}

module HIP.Pixel.HSV (
  --HSV (..)
  ) where

import HIP.Pixel.Base (Pixel(..))
{-
-- | HSV colorspace model consists of Hue, Saturation and Value. When read
-- from file or converted from a normalized image in other colorspace values
-- will be in ranges:
-- * Hue: @H = [0, 2*pi)@
-- * Saturation: @S = [0, 1]@
-- * Value: @I = [0, 1]@
data HSV c = HSV !c !c !c


pxOp :: (c -> c) -> HSV c -> HSV c
pxOp !f !(HSV h s i) = HSV (f h) (f s) (f i)
{-# INLINE pxOp #-}

pxOp2 :: (c -> c -> c) -> HSV c -> HSV c -> HSV c
pxOp2 !f !(HSV h1 s1 i1) !(HSV h2 s2 i2) = HSV (f h1 h2) (f s1 s2) (f i1 i2)
{-# INLINE pxOp2 #-}


instance (Eq c, Num c, Show c, Ord c) => Pixel (HSV c) where
  type Channel (HSV c) = c

  pixel !d = HSV d d d
  {-# INLINE pixel #-}

  arity _ = 3
  {-# INLINE arity #-}

  ref 0 (HSV h _ _) = h
  ref 1 (HSV _ s _) = s
  ref 2 (HSV _ _ i) = i
  ref n px = error ("Referencing "++show n++"is out of bounds for "++showType px)
  {-# INLINE ref #-}

  apply !(f1:f2:f3:_) !(HSV h s i) = HSV (f1 h) (f2 s) (f3 i)
  apply _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply #-}

  apply2 !(f1:f2:f3:_) !(HSV h1 s1 i1) !(HSV h2 s2 i2) = HSV (f1 h1 h2) (f2 s1 s2) (f3 i1 i2)
  apply2 _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply2 #-}

  showType _ = "HSV"


instance Eq c => Eq (HSV c) where
  (==) (HSV h1 s1 v1) (HSV h2 s2 v2) = h1 == h2 && s1 == s2 && v1 == v2
  {-# INLINE (==) #-}


instance (Eq c, Num c, Show c, Ord c) => Num (HSV c) where
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
  
  fromInteger = pixel . fromIntegral 
  {-# INLINE fromInteger #-}


instance (Eq c, Num c, Show c, Ord c, Fractional c) => Fractional (HSV c) where
  (/)          = pxOp2 (/)
  {-# INLINE (/) #-}

  recip        = pxOp recip
  {-# INLINE recip #-}
  
  fromRational = pixel . fromRational 
  {-# INLINE fromRational #-}


instance (Eq c, Num c, Show c, Ord c, Floating c) => Floating (HSV c) where
  pi    = pixel pi
  {-# INLINE pi #-}
  
  exp   = pxOp exp
  {-# INLINE exp #-}
  
  log   = pxOp log
  {-# INLINE log #-}
  
  sin   = pxOp sin
  {-# INLINE sin #-}
  
  cos   = pxOp cos
  {-# INLINE cos #-}
  
  asin  = pxOp asin
  {-# INLINE asin #-}
  
  atan  = pxOp atan
  {-# INLINE atan #-}
  
  acos  = pxOp acos
  {-# INLINE acos #-}
  
  sinh  = pxOp sinh
  {-# INLINE sinh #-}
  
  cosh  = pxOp cosh
  {-# INLINE cosh #-}
  
  asinh = pxOp asinh
  {-# INLINE asinh #-}
  
  atanh = pxOp atanh
  {-# INLINE atanh #-}
  
  acosh = pxOp acosh
  {-# INLINE acosh #-}


instance Ord c => Ord (HSV c) where
  compare !(HSV h1 s1 i1) !(HSV h2 s2 i2) = compare (h1, s1, i1) (h2, s2, i2)
  {-# INLINE compare #-}


instance Show c => Show (HSV c) where
  show (HSV h s v) = "<HSV:("++show h++"|"++show s++"|"++show v++")>"
  
-}
