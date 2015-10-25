{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
module Graphics.Image.Interface.Pixel (
  Pixel(..),
  module Graphics.Image.Interface.Pixel.Binary,
  module Graphics.Image.Interface.Pixel.Gray,
  grayToRGB, grayToHSI,
  module Graphics.Image.Interface.Pixel.RGB,
  rgbToHSI, rgbToGray,
  module Graphics.Image.Interface.Pixel.HSI,
  hsiToRGB, hsiToGray,
  module Graphics.Image.Interface.Pixel.Complex,
  module Graphics.Image.Interface.Pixel.Alpha
  ) where

import Prelude hiding (map)

import Graphics.Image.Interface (Pixel(..))
import Graphics.Image.Interface.Pixel.Binary
import Graphics.Image.Interface.Pixel.Gray
import Graphics.Image.Interface.Pixel.RGB
import Graphics.Image.Interface.Pixel.HSI
import Graphics.Image.Interface.Pixel.Complex
import Graphics.Image.Interface.Pixel.Alpha



instance AlphaInner Gray where

  
instance AlphaInner RGB where
  

instance AlphaInner HSI where

  
instance ComplexInner Gray where

  
instance ComplexInner RGB where
  

instance ComplexInner HSI where


instance (ComplexInner px, AlphaInner px) => ComplexInner (Alpha px) where

  
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

