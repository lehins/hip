{-# LANGUAGE BangPatterns, FlexibleInstances, MultiParamTypeClasses, 
TypeFamilies, UndecidableInstances, ViewPatterns #-}

module HIP.Pixel.HSI (
  HSI (..)
  ) where

import HIP.Interface (Pixel(..))

data HSI = HSI !Double !Double !Double deriving Eq


instance Pixel HSI where
  type Inner HSI = Double
  pixel !d                                = HSI d d d
  {-# INLINE pixel #-}

  pxOp !f !(HSI h s i)                    = HSI (f h) (f s) (f i)
  {-# INLINE pxOp #-}

  pxOp2 !f !(HSI h1 s1 i1) (HSI h2 s2 i2) = HSI (f h1 h2) (f s1 s2) (f i1 i2)
  {-# INLINE pxOp2 #-}

  arity _ = 3
  {-# INLINE arity #-}

  ref 0 (HSI h _ _) = h
  ref 1 (HSI _ s _) = s
  ref 2 (HSI _ _ i) = i
  ref n px = error ("Referencing "++show n++"is out of bounds for "++showType px)
  {-# INLINE ref #-}

  apply !(f1:f2:f3:_) !(HSI h s i) = HSI (f1 h) (f2 s) (f3 i)
  apply _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply #-}

  apply2 !(f1:f2:f3:_) !(HSI h1 s1 i1) !(HSI h2 s2 i2) = HSI (f1 h1 h2) (f2 s1 s2) (f3 i1 i2)
  apply2 _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply2 #-}

  strongest !(HSI h s i)                  = pixel . maximum $ [h, s, i]
  {-# INLINE strongest #-}

  weakest !(HSI h s i)                    = pixel . minimum $ [h, s, i]
  {-# INLINE weakest #-}

  showType _ = "HSI"
  

instance Num HSI where
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


instance Fractional HSI where
  (/)          = pxOp2 (/)
  recip        = pxOp recip
  fromRational = pixel . fromRational 


instance Floating HSI where
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


instance Ord HSI where
  compare !(HSI h1 s1 i1) !(HSI h2 s2 i2) = compare (h1, s1, i1) (h2, s2, i2)
  {-# INLINE compare #-}

instance Show HSI where
  {-# INLINE show #-}
  show (HSI h s i) = "<HSI:("++show h++"|"++show s++"|"++show i++")>"
  
