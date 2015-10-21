{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, ConstraintKinds, FlexibleContexts, FlexibleInstances,
MultiParamTypeClasses, UndecidableInstances, ViewPatterns #-}
module Graphics.Image.Interface.Pixel (
  Pixel(..),
  module Graphics.Image.Interface.Pixel.Binary,
  module Graphics.Image.Interface.Pixel.Gray,
  module Graphics.Image.Interface.Pixel.RGB,
  module Graphics.Image.Interface.Pixel.HSI,
  module Graphics.Image.Interface.Pixel.Complex,
  module Graphics.Image.Interface.Pixel.Alpha,
  graysToHsi, hsiToGrays
  ) where

import Prelude hiding (map)
import Graphics.Image.Interface (Pixel(..), Convertible(..), Image(..))

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
  

instance Convertible RGB HSI where
  convert !(RGB r g b) = HSI h s i where
    !h = if (v1 /= 0.0) then atan2 v2 v1 else 0
    !s = sqrt((v1 * v1) + (v2 * v2))
    !i = (r + g + b)/3
    !v1 = (2.0*r - g - b) / c
    !v2 = (g - b) / c
    !c = 2.44948974278318
  {-# INLINE convert #-}
  

instance Convertible HSI RGB where
  convert !(HSI h s i) = RGB r g b where
    !r  = i + v1
    !g  = i - (v1/2) + v2
    !b  = i - (v1/2) - v2
    !v1 = c * s * (cos h)/3
    !v2 = c * s * (sin h)/2
    !c  = 2.44948974278318
  {-# INLINE convert #-}


instance Convertible Gray RGB where
  convert !(Gray g) = pixel g
  {-# INLINE convert #-}


instance Convertible RGB Gray where
  convert !(RGB r g b) = Gray ((r + g + b)/3)
  {-# INLINE convert #-}

  
instance Convertible HSI Gray where
  convert !(HSI _ _ i) = Gray i
  {-# INLINE convert #-}


instance Convertible Gray HSI where
  convert !(Gray y) = HSI 0 0 y
  {-# INLINE convert #-}


instance Pixel px => Convertible Binary px where
  convert !b = if isOn b then pixel 1 else pixel 0
  {-# INLINE convert #-}
  

instance Convertible Binary Bool where
  convert = isOn
  {-# INLINE convert #-}


instance Convertible Bool Binary where
  convert !b = if b then on else off
  {-# INLINE convert #-}


instance (Image img px1, Image img px2, Convertible px1 px2) =>
         Convertible (img px1) (img px2) where
  convert img = map convert img
  {-# INLINE convert #-}


instance (ComplexInner px) => Convertible px (Complex px) where
  convert px = px :+: pixel 0
  {-# INLINE convert #-}


instance (ComplexInner px) => Convertible (Complex px) px where
  convert (px :+: _) = px
  {-# INLINE convert #-}


hsiToGrays :: (Image img HSI, Image img Gray) => img HSI -> (img Gray, img Gray, img Gray)
hsiToGrays img = (map (\(HSI h _ _) -> Gray h) img,
                  map (\(HSI _ s _) -> Gray s) img,
                  map (\(HSI _ _ i) -> Gray i) img)

graysToHsi :: (Image img HSI, Image img Gray) => (img Gray, img Gray, img Gray) -> img HSI
graysToHsi (imgH@(dims -> (hM, hN)), imgS@(dims -> (sM, sN)), imgI@(dims -> (iM, iN)))
  | hM == sM && hM == iM && hN == sN && hN == iN =
    let newDims _ _ _ _ _ _ = (hM, hN)
        getValue (Gray v) = v
        newPx getPx1 getPx2 getPx3 i j =
          HSI (getValue $ getPx1 i j) (getValue $ getPx2 i j) (getValue $ getPx3 i j)
          in traverse3 imgH imgS imgI newDims newPx
  | otherwise = error ("Recieved images with inconsistent dimensions: "++
                       (show imgH)++", "++(show imgS)++", "++(show imgI))
