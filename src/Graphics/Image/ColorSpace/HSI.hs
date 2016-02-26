{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             TypeFamilies #-}
module Graphics.Image.ColorSpace.HSI (
  HSI(..), HSIA(..), Pixel(..), 
  ToHSI(..), ToHSIA(..)
  ) where

import Prelude hiding (map)
import Graphics.Image.Interface
import Data.Typeable (Typeable)

data HSI = HueHSI
         | SatHSI
         | IntHSI deriving (Eq, Enum, Typeable)

data HSIA = HueHSIA
          | SatHSIA
          | IntHSIA
          | AlphaHSIA deriving (Eq, Enum, Typeable)


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

  pxOp !f (PixelHSI h s i) = PixelHSI (f h) (f s) (f i)
  {-# INLINE pxOp #-}

  chApp (PixelHSI fh fs fi) (PixelHSI h s i) = PixelHSI (fh h) (fs s) (fi i)
  {-# INLINE chApp #-}



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

  pxOp !f (PixelHSIA h s i a) = PixelHSIA (f h) (f s) (f i) (f a)
  {-# INLINE pxOp #-}

  chApp (PixelHSIA fh fs fi fa) (PixelHSIA h s i a) = PixelHSIA (fh h) (fs s) (fi i) (fa a)
  {-# INLINE chApp #-}


instance Alpha HSIA where
  type Opaque HSIA = HSI

  getAlpha (PixelHSIA _ _ _ a) = a
  {-# INLINE getAlpha #-}
  
  addAlpha !a (PixelHSI h s i) = PixelHSIA h s i a
  {-# INLINE addAlpha #-}

  dropAlpha (PixelHSIA h s i _) = PixelHSI h s i
  {-# INLINE dropAlpha #-}


instance Show HSI where
  show HueHSI = "Hue"
  show SatHSI = "Saturation"
  show IntHSI = "Intensity"

instance Show HSIA where
  show HueHSIA   = "Hue"
  show SatHSIA   = "Saturation"
  show IntHSIA   = "Intensity"
  show AlphaHSIA = "Alpha"
 
instance Show e => Show (Pixel HSI e) where
  show (PixelHSI h s i) = "<HSI:("++show h++"|"++show s++"|"++show i++")>"


instance Show e => Show (Pixel HSIA e) where
  show (PixelHSIA h s i a) = "<HSIA:("++show h++"|"++show s++"|"++show i++"|"++show a++")>"



