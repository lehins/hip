{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TemplateHaskell,
TypeFamilies, UndecidableInstances #-}
module Graphics.Image.Unboxed.Pixel (
  Pixel,
  Binary, on, off, isOn, isOff,
  Gray(..), RGB(..), HSI(..),
  Complex(..), ComplexInner, Alpha(..), AlphaInner, fmapAlpha, combineAlpha
  ) where

import Data.Vector.Unboxed (Unbox)
import Data.Vector.Unboxed.Deriving
import Graphics.Image.Interface.Pixel hiding (Pixel, ComplexInner, AlphaInner)
import qualified Graphics.Image.Interface.Pixel as P (ComplexInner, AlphaInner)
import qualified Graphics.Image.Interface as I (Pixel(..))
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable


-- | Unboxed Vector can only work with 'I.Pixel's that implement 'Unbox'
class (Unbox px, I.Pixel px) => Pixel px where


-- | Unboxed Vector can only work with 'I.Pixel's that implement 'Unbox'
class (Unbox px, Unbox (I.Inner px), P.ComplexInner px) =>
      ComplexInner px where

-- | Unboxed Vector can only work with 'I.Pixel's that implement 'Unbox'
class (Unbox px, Unbox (I.Inner px), P.AlphaInner px) =>
      AlphaInner px where
        

-- | Unboxed Pixel  
instance Pixel Binary where

-- | Unboxed Pixel  
instance Pixel Gray where

-- | Unboxed Pixel  
instance Pixel RGB where

-- | Unboxed Pixel  
instance Pixel HSI where
 
-- | Unboxed Pixel  
instance ComplexInner px => Pixel (Complex px) where


-- | Unboxed Pixel  
instance ComplexInner Gray where

-- | Unboxed Pixel  
instance ComplexInner RGB where

-- | Unboxed Pixel  
instance ComplexInner HSI where

  
-- | Unboxed Pixel  
instance (Pixel (Alpha px), ComplexInner px, AlphaInner px) =>
         ComplexInner (Alpha px) where
  

-- | Unboxed Pixel  
instance AlphaInner Gray where

-- | Unboxed Pixel  
instance AlphaInner RGB where

-- | Unboxed Pixel  
instance AlphaInner HSI where
  

unboxRGB :: RGB -> (Double, Double, Double)
unboxRGB (RGB r g b) = (r, g, b)
{-# INLINE unboxRGB #-}  


boxRGB :: (Double, Double, Double) -> RGB
boxRGB (r, g, b) = RGB r g b
{-# INLINE boxRGB #-}


unboxHSI :: HSI -> (Double, Double, Double)
{-# INLINE unboxHSI #-}  
unboxHSI (HSI h s i) = (h, s, i)


boxHSI :: (Double, Double, Double) -> HSI
{-# INLINE boxHSI #-}
boxHSI (h, s, i) = HSI h s i


derivingUnbox "BinaryPixel"
    [t| Binary -> Bool |]
    [| isOn |]
    [| \v -> if v then on else off |]


derivingUnbox "GrayPixel"
    [t| Gray -> Double |]
    [| \(Gray y) -> y |]
    [| \y -> Gray y |]


derivingUnbox "RGBPixel"
    [t| RGB -> (Double, Double, Double) |]
    [| unboxRGB |]
    [| boxRGB |]


derivingUnbox "HSIPixel"
    [t| HSI -> (Double, Double, Double) |]
    [| unboxHSI |]
    [| boxHSI |]


derivingUnbox "ComplexPixel"
    [t| (Unbox px, Unbox (Inner px), ComplexInner px) => (Complex px) -> (px, px) |]
    [| \(px1 :+: px2) -> (px1, px2) |]
    [| \(px1, px2) -> px1 :+: px2 |]

