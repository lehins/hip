{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Image.Interface.Conversion (
  graysToHsi, hsiToGrays  
  ) where

import Prelude hiding (map)
import Graphics.Image.Interface (Convertible(..), AImage(..))
import Graphics.Image.Interface.Pixel 

instance Convertible RGB HSI where
  convert = rgbToHSI
  {-# INLINE convert #-}
  

instance Convertible HSI RGB where
  convert = hsiToRGB
  {-# INLINE convert #-}


instance Convertible Gray RGB where
  convert = grayToRGB
  {-# INLINE convert #-}


instance Convertible RGB Gray where
  convert = rgbToGray
  {-# INLINE convert #-}

  
instance Convertible HSI Gray where
  convert = hsiToGray
  {-# INLINE convert #-}


instance Convertible Gray HSI where
  convert = grayToHSI
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


instance (AImage img px1, AImage img px2, Convertible px1 px2) =>
         Convertible (img px1) (img px2) where
  convert !img = map convert img
  {-# INLINE convert #-}


instance (ComplexInner px) => Convertible px (Complex px) where
  convert !px = px :+: pixel 0
  {-# INLINE convert #-}


instance (ComplexInner px) => Convertible (Complex px) px where
  convert !(px :+: _) = px
  {-# INLINE convert #-}


hsiToGrays :: (AImage img HSI, AImage img Gray) => img HSI -> (img Gray, img Gray, img Gray)
hsiToGrays img = (map (\(HSI h _ _) -> Gray h) img,
                  map (\(HSI _ s _) -> Gray s) img,
                  map (\(HSI _ _ i) -> Gray i) img)

graysToHsi :: (AImage img HSI, AImage img Gray) => (img Gray, img Gray, img Gray) -> img HSI
graysToHsi (imgH@(dims -> (hM, hN)), imgS@(dims -> (sM, sN)), imgI@(dims -> (iM, iN)))
  | hM == sM && hM == iM && hN == sN && hN == iN =
    let newDims _ _ _ _ _ _ = (hM, hN)
        getValue (Gray v) = v
        newPx getPx1 getPx2 getPx3 i j =
          HSI (getValue $ getPx1 i j) (getValue $ getPx2 i j) (getValue $ getPx3 i j)
          in traverse3 imgH imgS imgI newDims newPx
  | otherwise = error ("Received images with inconsistent dimensions: "++
                       (show imgH)++", "++(show imgS)++", "++(show imgI))
