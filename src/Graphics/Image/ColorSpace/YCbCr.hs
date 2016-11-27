{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Image.ColorSpace.YCbCr
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.ColorSpace.YCbCr (
  YCbCr(..), YCbCrA(..), Pixel(..), 
  ToYCbCr(..), ToYCbCrA(..)
  ) where

import Prelude hiding (map)
import Graphics.Image.Interface
import Data.Typeable (Typeable)
import qualified Data.Monoid as M (mappend)
import qualified Data.Colour as C
import qualified Data.Colour.Names as C


-- | Color space is used to encode RGB information and is used in JPEG compression.
data YCbCr = LumaYCbCr  -- ^ Luma component (commonly denoted as __Y'__)
           | CBlueYCbCr -- ^ Blue difference chroma component
           | CRedYCbCr  -- ^ Red difference chroma component
           deriving (Eq, Enum, Typeable)


-- | YCbCr color space with Alpha channel.
data YCbCrA = LumaYCbCrA  -- ^ Luma component (commonly denoted as __Y'__)
            | CBlueYCbCrA -- ^ Blue difference chroma component
            | CRedYCbCrA  -- ^ Red difference chroma component
            | AlphaYCbCrA -- ^ Alpha component.
            deriving (Eq, Enum, Typeable)


-- | Conversion to `YCbCr` color space.
class ColorSpace cs => ToYCbCr cs where

  -- | Convert to an `YCbCr` pixel.
  toPixelYCbCr :: Pixel cs Double -> Pixel YCbCr Double

  -- | Convert to an `YCbCr` image.
  toImageYCbCr :: (Array arr cs Double, Array arr YCbCr Double) =>
                  Image arr cs Double
               -> Image arr YCbCr Double
  toImageYCbCr = map toPixelYCbCr
  {-# INLINE toImageYCbCr #-}


-- | Conversion to `YCbCrA` from another color space with Alpha channel.
class (ToYCbCr (Opaque cs), Alpha cs) => ToYCbCrA cs where

  -- | Convert to an `YCbCrA` pixel.
  toPixelYCbCrA :: Pixel cs Double -> Pixel YCbCrA Double
  toPixelYCbCrA px = addAlpha (getAlpha px) (toPixelYCbCr (dropAlpha px))
  {-# INLINE toPixelYCbCrA #-}

  -- | Convert to an `YCbCrA` image.
  toImageYCbCrA :: (Array arr cs Double, Array arr YCbCrA Double) =>
                   Image arr cs Double
                -> Image arr YCbCrA Double
  toImageYCbCrA = map toPixelYCbCrA
  {-# INLINE toImageYCbCrA #-}

  
instance ColorSpace YCbCr where
  type PixelElt YCbCr e = (e, e, e)
  data Pixel YCbCr e = PixelYCbCr !e !e !e deriving Eq

  fromChannel !e = PixelYCbCr e e e
  {-# INLINE fromChannel #-}

  fromElt !(y, b, r) = PixelYCbCr y b r
  {-# INLINE fromElt #-}

  toElt (PixelYCbCr y b r) = (y, b, r)
  {-# INLINE toElt #-}

  getPxCh (PixelYCbCr y _ _) LumaYCbCr  = y
  getPxCh (PixelYCbCr _ b _) CBlueYCbCr = b
  getPxCh (PixelYCbCr _ _ r) CRedYCbCr  = r
  {-# INLINE getPxCh #-}
  
  chOp !f (PixelYCbCr y b r) = PixelYCbCr (f LumaYCbCr y) (f CBlueYCbCr b) (f CRedYCbCr r)
  {-# INLINE chOp #-}

  pxOp !f (PixelYCbCr y b r) = PixelYCbCr (f y) (f b) (f r)
  {-# INLINE pxOp #-}

  chApp (PixelYCbCr fy fb fr) (PixelYCbCr y b r) = PixelYCbCr (fy y) (fb b) (fr r)
  {-# INLINE chApp #-}

  pxFoldMap f (PixelYCbCr y b r) = f y `M.mappend` f b `M.mappend` f r
  {-# INLINE pxFoldMap #-}

  csColour LumaYCbCr  = C.opaque C.darkgray
  csColour CBlueYCbCr = C.opaque C.darkblue
  csColour CRedYCbCr  = C.opaque C.darkred
  

instance ColorSpace YCbCrA where
  type PixelElt YCbCrA e = (e, e, e, e)
  data Pixel YCbCrA e = PixelYCbCrA !e !e !e !e deriving Eq

  fromChannel !e = PixelYCbCrA e e e e
  {-# INLINE fromChannel #-}

  fromElt (y, b, r, a) = PixelYCbCrA y b r a
  {-# INLINE fromElt #-}

  toElt (PixelYCbCrA y b r a) = (y, b, r, a)
  {-# INLINE toElt #-}

  getPxCh (PixelYCbCrA y _ _ _) LumaYCbCrA  = y
  getPxCh (PixelYCbCrA _ b _ _) CBlueYCbCrA = b
  getPxCh (PixelYCbCrA _ _ r _) CRedYCbCrA  = r
  getPxCh (PixelYCbCrA _ _ _ a) AlphaYCbCrA = a
  {-# INLINE getPxCh #-}
  
  chOp !f (PixelYCbCrA y b r a) =
    PixelYCbCrA (f LumaYCbCrA y) (f CBlueYCbCrA b) (f CRedYCbCrA r) (f AlphaYCbCrA a)
  {-# INLINE chOp #-}

  pxOp !f (PixelYCbCrA y b r a) = PixelYCbCrA (f y) (f b) (f r) (f a)
  {-# INLINE pxOp #-}

  chApp (PixelYCbCrA fy fb fr fa) (PixelYCbCrA y b r a) = PixelYCbCrA (fy y) (fb b) (fr r) (fa a)
  {-# INLINE chApp #-}

  pxFoldMap f (PixelYCbCrA y b r a) = f y `M.mappend` f b `M.mappend` f r `M.mappend` f a
  {-# INLINE pxFoldMap #-}

  csColour AlphaYCbCrA = C.opaque C.gray
  csColour ch          = csColour $ opaque ch


instance Alpha YCbCrA where
  type Opaque YCbCrA = YCbCr

  getAlpha (PixelYCbCrA _ _ _ a) = a
  {-# INLINE getAlpha #-}
  
  addAlpha !a (PixelYCbCr y b r) = PixelYCbCrA y b r a
  {-# INLINE addAlpha #-}

  dropAlpha (PixelYCbCrA y b r _) = PixelYCbCr y b r
  {-# INLINE dropAlpha #-}

  opaque LumaYCbCrA  = LumaYCbCr
  opaque CBlueYCbCrA = CBlueYCbCr
  opaque CRedYCbCrA  = CRedYCbCr
  opaque AlphaYCbCrA = error "Data.Image.ColorSpace.YCbCr (Alpha.opaque)"


instance Show YCbCr where
  show LumaYCbCr  = "Luma"
  show CBlueYCbCr = "Blue Chroma"
  show CRedYCbCr  = "Red Chroma"


instance Show YCbCrA where
  show AlphaYCbCrA = "Alpha"
  show ch          = show $ opaque ch

 
instance Show e => Show (Pixel YCbCr e) where
  show (PixelYCbCr y b r) = "<YCbCr:("++show y++"|"++show b++"|"++show r++")>"


instance Show e => Show (Pixel YCbCrA e) where
  show (PixelYCbCrA y b r a) = "<YCbCrA:("++show y++"|"++show b++"|"++show r++"|"++show a++")>"



