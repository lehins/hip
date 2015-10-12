{-# LANGUAGE TemplateHaskell, ViewPatterns, MultiParamTypeClasses,
UndecidableInstances, BangPatterns, FlexibleInstances, TypeFamilies #-}

module Graphics.Image.Pixel.HSI (
  HSI (..)
  ) where

import Graphics.Image.Interface (Pixel(..))
import Data.Array.Repa.Eval
import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed (Unbox)


data HSI = HSI !Double !Double !Double deriving Eq


instance Pixel HSI Double where
  pixel d = HSI d d d
  {-# INLINE pixel #-}

  pxOp f (HSI h s i) = HSI (f h) (f s) (f i)
  {-# INLINE pxOp #-}

  pxOp2 f (HSI h1 s1 i1) (HSI h2 s2 i2) = HSI (f h1 h2) (f s1 s2) (f i1 i2)
  {-# INLINE pxOp2 #-}

  strongest (HSI h s i) = pixel . maximum $ [h, s, i]
  {-# INLINE strongest #-}

  weakest (HSI h s i) = pixel . minimum $ [h, s, i]
  {-# INLINE weakest #-}

  showType _ = "HSI"
  

instance Num HSI where
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


instance Fractional HSI where
  (/)            = pxOp2 (/)
  recip          = pxOp recip
  fromRational n = pixel . fromRational $ n


instance Floating HSI where
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


instance Ord HSI where
  (strongest -> HSI m1 _ _) <= (strongest -> HSI m2 _ _) = m1 <= m2
  {-# INLINE (<=) #-}

instance Show HSI where
  {-# INLINE show #-}
  show (HSI h s i) = "<HSI:("++show h++"|"++show s++"|"++show i++")>"


instance Elt HSI where
  {-# INLINE touch #-}
  touch (HSI h s i) = touch h >> touch s >> touch i
  
  {-# INLINE zero #-}
  zero             = 0

  {-# INLINE one #-}
  one              = 1


unboxHSI :: HSI -> (Double, Double, Double)
{-# INLINE unboxHSI #-}  
unboxHSI (HSI h s i) = (h, s, i)


boxHSI :: (Double, Double, Double) -> HSI
{-# INLINE boxHSI #-}
boxHSI (h, s, i) = HSI h s i


derivingUnbox "HSIPixel"
    [t| (Unbox Double) => HSI -> (Double, Double, Double) |]
    [| unboxHSI |]
    [| boxHSI |]
