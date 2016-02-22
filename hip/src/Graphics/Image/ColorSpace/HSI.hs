{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TypeFamilies #-}
module Graphics.Image.ColorSpace.HSI (
  HSI(..), HSIA(..), Pixel(..), 
  ToHSI(..), ToHSIA(..)
  ) where

import Prelude hiding (map)
import Graphics.Image.Interface


data HSI = HueHSI
         | SatHSI
         | IntHSI deriving (Eq, Enum)

data HSIA = HueHSIA
          | SatHSIA
          | IntHSIA
          | AlphaHSIA deriving (Eq, Enum)


class ColorSpace cs => ToHSI cs where

  toPixelHSI :: Pixel cs Double -> Pixel HSI Double

  toImageHSI :: (Array arr cs Double, Array arr HSI Double) =>
                Image arr cs Double
             -> Image arr HSI Double
  toImageHSI = map toPixelHSI
  {-# INLINE toImageHSI #-}


class (ToHSI (Opaque cs), Alpha cs) => ToHSIA cs where

  toPixelHSIA :: Pixel cs Double -> Pixel HSIA Double
  toPixelHSIA px = addAlpha (getAlpha px) (toPixelHSI (dropAlpha px))

  toImageHSIA :: (Array arr cs Double, Array arr HSIA Double) =>
                 Image arr cs Double
              -> Image arr HSIA Double
  toImageHSIA = map toPixelHSIA
  {-# INLINE toImageHSIA #-}

  
instance ColorSpace HSI where
  type PixelElt HSI e = (e, e, e)
  data Pixel HSI e = PixelHSI !e !e !e deriving Eq

  fromChannel !e = PixelHSI e e e
  {-# INLINE fromChannel #-}

  fromElt !(h, s, i) = PixelHSI h s i
  {-# INLINE fromElt #-}

  toElt (PixelHSI h s i) = (h, s, i)
  {-# INLINE toElt #-}

  getPxCh (PixelHSI h _ _) HueHSI   = h
  getPxCh (PixelHSI _ s _) SatHSI = s
  getPxCh (PixelHSI _ _ i) IntHSI  = i
  {-# INLINE getPxCh #-}
  
  chOp !f (PixelHSI h s i) = PixelHSI (f HueHSI h) (f SatHSI s) (f IntHSI i)
  {-# INLINE chOp #-}

  chOp2 !f (PixelHSI h1 s1 i1) (PixelHSI h2 s2 i2) =
    PixelHSI (f HueHSI h1 h2) (f SatHSI s1 s2) (f IntHSI i1 i2)
  {-# INLINE chOp2 #-}
  
  pxOp !f (PixelHSI h s i) = PixelHSI (f h) (f s) (f i)
  {-# INLINE pxOp #-}

  pxOp2 !f (PixelHSI h1 s1 i1) (PixelHSI h2 s2 i2) = PixelHSI (f h1 h2) (f s1 s2) (f i1 i2)
  {-# INLINE pxOp2 #-}



instance ColorSpace HSIA where
  type PixelElt HSIA e = (e, e, e, e)
  data Pixel HSIA e = PixelHSIA !e !e !e !e deriving Eq

  fromChannel !e = PixelHSIA e e e e
  {-# INLINE fromChannel #-}

  fromElt (h, s, i, a) = PixelHSIA h s i a
  {-# INLINE fromElt #-}

  toElt (PixelHSIA h s i a) = (h, s, i, a)
  {-# INLINE toElt #-}

  getPxCh (PixelHSIA r _ _ _) HueHSIA   = r
  getPxCh (PixelHSIA _ g _ _) SatHSIA = g
  getPxCh (PixelHSIA _ _ b _) IntHSIA  = b
  getPxCh (PixelHSIA _ _ _ a) AlphaHSIA = a
  {-# INLINE getPxCh #-}
  
  chOp !f (PixelHSIA h s i a) =
    PixelHSIA (f HueHSIA h) (f SatHSIA s) (f IntHSIA i) (f AlphaHSIA a)
  {-# INLINE chOp #-}

  chOp2 !f (PixelHSIA h1 s1 i1 a1) (PixelHSIA h2 s2 i2 a2) =
    PixelHSIA (f HueHSIA h1 h2) (f SatHSIA s1 s2) (f IntHSIA i1 i2) (f AlphaHSIA a1 a2)
  {-# INLINE chOp2 #-}
  
  pxOp !f (PixelHSIA h s i a) = PixelHSIA (f h) (f s) (f i) (f a)
  {-# INLINE pxOp #-}

  pxOp2 !f (PixelHSIA h1 s1 i1 a1) (PixelHSIA h2 s2 i2 a2) =
    PixelHSIA (f h1 h2) (f s1 s2) (f i1 i2) (f a1 a2)
  {-# INLINE pxOp2 #-}


instance Alpha HSIA where
  type Opaque HSIA = HSI

  getAlpha (PixelHSIA _ _ _ a) = a
  {-# INLINE getAlpha #-}
  
  addAlpha !a (PixelHSI h s i) = PixelHSIA h s i a
  {-# INLINE addAlpha #-}

  dropAlpha (PixelHSIA h s i _) = PixelHSI h s i
  {-# INLINE dropAlpha #-}


instance Show HSI where
  show _ = "HSI"

instance Show HSIA where
  show _ = "HSIA"

 
instance Show e => Show (Pixel HSI e) where
  show (PixelHSI h s i) = "<HSI:("++show h++"|"++show s++"|"++show i++")>"


instance Show e => Show (Pixel HSIA e) where
  show (PixelHSIA h s i a) = "<HSIA:("++show h++"|"++show s++"|"++show i++"|"++show a++")>"



