{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Image.ColorSpace.HSI
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.ColorSpace.HSI (
  HSI(..), HSIA(..), Pixel(..), 
  ToHSI(..), ToHSIA(..)
  ) where

import Prelude hiding (map)
import Graphics.Image.Interface
import Data.Typeable (Typeable)
import qualified Data.Monoid as M (mappend)
import qualified Data.Colour as C
import qualified Data.Colour.Names as C

-- | Hue, Saturation and Intensity color space.
data HSI = HueHSI -- ^ Hue
         | SatHSI -- ^ Saturation 
         | IntHSI -- ^ Intensity
         deriving (Eq, Enum, Typeable)

-- | Hue, Saturation and Intensity color space with Alpha channel.
data HSIA = HueHSIA   -- ^ Hue
          | SatHSIA   -- ^ Saturation
          | IntHSIA   -- ^ Intensity
          | AlphaHSIA -- ^ Alpha
          deriving (Eq, Enum, Typeable)


-- | Conversion to `HSI` color space.
class ColorSpace cs => ToHSI cs where

  -- | Convert to an `HSI` pixel.
  toPixelHSI :: Pixel cs Double -> Pixel HSI Double

  -- | Convert to an `HSI` image.
  toImageHSI :: (Array arr cs Double, Array arr HSI Double) =>
                Image arr cs Double
             -> Image arr HSI Double
  toImageHSI = map toPixelHSI
  {-# INLINE toImageHSI #-}
  

-- | Conversion to `HSIA` from another color space with Alpha channel.
class (ToHSI (Opaque cs), Alpha cs) => ToHSIA cs where

  -- | Convert to an `HSIA` pixel.
  toPixelHSIA :: Pixel cs Double -> Pixel HSIA Double
  toPixelHSIA px = addAlpha (getAlpha px) (toPixelHSI (dropAlpha px))
  {-# INLINE toPixelHSIA #-}

  -- | Convert to an `HSIA` image.
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

  pxFoldMap f (PixelHSI h s i) = f h `M.mappend` f s `M.mappend` f i 
  {-# INLINE pxFoldMap #-}

  csColour HueHSI = C.opaque C.purple
  csColour SatHSI = C.opaque C.orange
  csColour IntHSI = C.opaque C.darkblue



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

  pxFoldMap f (PixelHSIA h s i a) = f h `M.mappend` f s `M.mappend` f i `M.mappend` f a
  {-# INLINE pxFoldMap #-}

  csColour AlphaHSIA = C.opaque C.gray
  csColour ch        = csColour $ opaque ch
  

instance Alpha HSIA where
  type Opaque HSIA = HSI

  getAlpha (PixelHSIA _ _ _ a) = a
  {-# INLINE getAlpha #-}
  
  addAlpha !a (PixelHSI h s i) = PixelHSIA h s i a
  {-# INLINE addAlpha #-}

  dropAlpha (PixelHSIA h s i _) = PixelHSI h s i
  {-# INLINE dropAlpha #-}

  opaque HueHSIA = HueHSI
  opaque SatHSIA = SatHSI
  opaque IntHSIA = IntHSI
  opaque _       = error "Data.Image.ColorSpace.HSI (Alpha.opaque)"


instance Show HSI where
  show HueHSI = "Hue"
  show SatHSI = "Saturation"
  show IntHSI = "Intensity"
  

instance Show HSIA where
  show AlphaHSIA = "Alpha"
  show ch        = show $ opaque ch

  
instance Show e => Show (Pixel HSI e) where
  show (PixelHSI h s i) = "<HSI:("++show h++"|"++show s++"|"++show i++")>"


instance Show e => Show (Pixel HSIA e) where
  show (PixelHSIA h s i a) = "<HSIA:("++show h++"|"++show s++"|"++show i++"|"++show a++")>"



