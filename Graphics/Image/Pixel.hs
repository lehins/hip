{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.Image.Pixel (
  module Graphics.Image.Pixel.Gray,
  module Graphics.Image.Pixel.RGB,
  module Graphics.Image.Pixel.HSI,
  module Graphics.Image.Pixel.Complex
  ) where

import Graphics.Image.Interface (Convertable(..), Pixel(..))
import Graphics.Image.Pixel.Gray
import Graphics.Image.Pixel.RGB
import Graphics.Image.Pixel.HSI
import Graphics.Image.Pixel.Complex


instance Convertable RGB HSI where
  convert !(RGB r g b) = HSI h s i where
    !h = if (v1 /= 0.0) then atan2 v2 v1 else 0
    !s = sqrt((v1 * v1) + (v2 * v2))
    !i = (r + g + b)/3
    !v1 = (2.0*r - g - b) / c
    !v2 = (g - b) / c
    !c = 2.44948974278318
  {-# INLINE convert #-}
  

instance Convertable HSI RGB where
  convert !(HSI h s i) = RGB r g b where
    !r  = i + v1
    !g  = i - (v1/2) + v2
    !b  = i - (v1/2) - v2
    !v1 = c * s * (cos h)/3
    !v2 = c * s * (sin h)/2
    !c  = 2.44948974278318
  {-# INLINE convert #-}


instance Convertable Gray RGB where
  convert !(Gray g) = pixel g
  {-# INLINE convert #-}


instance Convertable RGB Gray where
  convert !(RGB r g b) = Gray ((r + g + b)/3)
  {-# INLINE convert #-}

  
instance Convertable HSI Gray where
  convert !(HSI _ _ i) = Gray i
  {-# INLINE convert #-}

instance Convertable Gray HSI where
  convert !(Gray y) = HSI 0 0 y
  {-# INLINE convert #-}


instance Pixel px => Convertable px (Complex px) where
  convert px = px :+: pixel 0
  {-# INLINE convert #-}


instance Pixel px => Convertable (Complex px) px where
  convert (px :+: _) = px
  {-# INLINE convert #-}
