{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
module HIP.Pixel (
  Pixel(..),
  module HIP.Pixel.Gray,
  grayToRGB, grayToHSI,
  module HIP.Pixel.RGB,
  rgbToHSI, rgbToGray,
  module HIP.Pixel.HSI,
  hsiToRGB, hsiToGray,
  module HIP.Pixel.Alpha,
  module HIP.Binary.Pixel,
  module HIP.Complex.Pixel
  ) where

import Prelude hiding (map)

import HIP.Pixel.Base()
import HIP.Pixel.Gray
import HIP.Pixel.RGB
import HIP.Pixel.HSI
import HIP.Pixel.Alpha
import HIP.Binary.Pixel
import HIP.Complex.Pixel
import HIP.Interface (Pixel(..))


instance ComplexInner Float where
  apply2c !(f1:_) !v1 !v2 = (uncurry (:+:)) $ f1 v1 v2
  apply2c _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply2c #-}

  
instance ComplexInner Double where
  apply2c !(f1:_) !v1 !v2 = (uncurry (:+:)) $ f1 v1 v2
  apply2c _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply2c #-}
  
  
instance ComplexInner Gray where
  apply2c !(f1:_) !(Gray y1) !(Gray y2) = Gray y1' :+: Gray y2' where (y1', y2') = (f1 y1 y2)
  apply2c _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply2c #-}

  
instance ComplexInner RGB where
  apply2c !(f1:f2:f3:_) !(RGB r1 g1 b1) !(RGB r2 g2 b2) =
    RGB r1' g1' b1' :+: RGB r2' g2' b2' where
      (r1', r2') = (f1 r1 r2)
      (g1', g2') = (f2 g1 g2)
      (b1', b2') = (f3 b1 b2)
  apply2c _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply2c #-}

  

instance ComplexInner HSI where
  apply2c !(f1:f2:f3:_) !(HSI h1 s1 i1) !(HSI h2 s2 i2) =
    HSI h1' s1' i1' :+: HSI h2' s2' i2' where
      (h1', h2') = (f1 h1 h2)
      (s1', s2') = (f2 s1 s2)
      (i1', i2') = (f3 i1 i2)
  apply2c _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply2c #-}


instance ComplexInner px => ComplexInner (Alpha px) where
  apply2c !(f0:rest) !(Alpha a1 px1) !(Alpha a2 px2) =
    Alpha a1' px1' :+: Alpha a2' px2' where
      (a1', a2') = (f0 a1 a2)
      px1' :+: px2' = apply2c rest px1 px2
  apply2c _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply2c #-}


  
-- | Convert an 'Gray' pixel to 'HSI' pixel.
grayToHSI :: Gray -> HSI
grayToHSI !(Gray y) = HSI y y y
{-# INLINE grayToHSI #-}


-- | Convert an 'Gray' pixel to 'RGB' pixel.
grayToRGB :: Gray -> RGB
grayToRGB !(Gray y) = RGB y y y
{-# INLINE grayToRGB #-}

                     
-- | Convert an 'RGB' pixel to 'HSI' pixel.
rgbToHSI :: RGB -> HSI
rgbToHSI !(RGB r g b) = HSI h s i where
  !h = if (v1 /= 0.0) then atan2 v2 v1 else 0
  !s = sqrt((v1 * v1) + (v2 * v2))
  !i = (r + g + b)/3
  !v1 = (2.0*r - g - b) / c
  !v2 = (g - b) / c
  !c = 2.44948974278318
{-# INLINE rgbToHSI #-}


-- | Convert an 'RGB' pixel to 'Gray' pixel.
rgbToGray :: RGB -> Gray
rgbToGray !(RGB r g b) = Gray ((r + g + b)/3)
{-# INLINE rgbToGray #-}

-- | Convert an 'HSI' pixel to 'RGB' pixel.
hsiToRGB :: HSI -> RGB
hsiToRGB !(HSI h s i) = RGB r g b where
  !r  = i + v1
  !g  = i - (v1/2) + v2
  !b  = i - (v1/2) - v2
  !v1 = c * s * (cos h)/3
  !v2 = c * s * (sin h)/2
  !c  = 2.44948974278318
{-# INLINE hsiToRGB #-}


-- | Convert an 'HSI' pixel to 'Gray' pixel.
hsiToGray :: HSI -> Gray
hsiToGray (HSI _ _ i) = Gray i
{-# INLINE hsiToGray #-}


