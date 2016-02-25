{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             TypeFamilies #-}
module Graphics.Image.ColorSpace.RGB (
  RGB(..), RGBA(..), Pixel(..), 
  ToRGB(..), ToRGBA(..)
  ) where

import Prelude hiding (map)
import Graphics.Image.Interface
import Data.Typeable (Typeable)

data RGB = RedRGB
         | GreenRGB
         | BlueRGB deriving (Eq, Enum, Typeable)

data RGBA = RedRGBA
          | GreenRGBA
          | BlueRGBA
          | AlphaRGBA deriving (Eq, Enum, Typeable)


class ColorSpace cs => ToRGB cs where

  toPixelRGB :: Pixel cs Double -> Pixel RGB Double

  toImageRGB :: (Array arr cs Double, Array arr RGB Double) =>
                Image arr cs Double
             -> Image arr RGB Double
  toImageRGB = map toPixelRGB
  {-# INLINE toImageRGB #-}


class (ToRGB (Opaque cs), Alpha cs) => ToRGBA cs where

  toPixelRGBA :: Pixel cs Double -> Pixel RGBA Double
  toPixelRGBA px = addAlpha (getAlpha px) (toPixelRGB (dropAlpha px))

  toImageRGBA :: (Array arr cs Double, Array arr RGBA Double) =>
                Image arr cs Double
             -> Image arr RGBA Double
  toImageRGBA = map toPixelRGBA
  {-# INLINE toImageRGBA #-}

  
instance ColorSpace RGB where
  type PixelElt RGB e = (e, e, e)
  data Pixel RGB e = PixelRGB !e !e !e deriving Eq

  fromChannel !e = PixelRGB e e e
  {-# INLINE fromChannel #-}

  fromElt !(r, g, b) = PixelRGB r g b
  {-# INLINE fromElt #-}

  toElt (PixelRGB r g b) = (r, g, b)
  {-# INLINE toElt #-}

  getPxCh (PixelRGB r _ _) RedRGB   = r
  getPxCh (PixelRGB _ g _) GreenRGB = g
  getPxCh (PixelRGB _ _ b) BlueRGB  = b
  {-# INLINE getPxCh #-}
  
  chOp !f (PixelRGB r g b) = PixelRGB (f RedRGB r) (f GreenRGB g) (f BlueRGB b)
  {-# INLINE chOp #-}

  pxOp !f (PixelRGB r g b) = PixelRGB (f r) (f g) (f b)
  {-# INLINE pxOp #-}

  chApp (PixelRGB fr fg fb) (PixelRGB r g b) = PixelRGB (fr r) (fg g) (fb b)
  {-# INLINE chApp #-}



instance ColorSpace RGBA where
  type PixelElt RGBA e = (e, e, e, e)
  data Pixel RGBA e = PixelRGBA !e !e !e !e deriving Eq

  fromChannel !e = PixelRGBA e e e e
  {-# INLINE fromChannel #-}

  fromElt (r, g, b, a) = PixelRGBA r g b a
  {-# INLINE fromElt #-}

  toElt (PixelRGBA r g b a) = (r, g, b, a)
  {-# INLINE toElt #-}

  getPxCh (PixelRGBA r _ _ _) RedRGBA   = r
  getPxCh (PixelRGBA _ g _ _) GreenRGBA = g
  getPxCh (PixelRGBA _ _ b _) BlueRGBA  = b
  getPxCh (PixelRGBA _ _ _ a) AlphaRGBA = a
  {-# INLINE getPxCh #-}
  
  chOp !f (PixelRGBA r g b a) =
    PixelRGBA (f RedRGBA r) (f GreenRGBA g) (f BlueRGBA b) (f AlphaRGBA a)
  {-# INLINE chOp #-}

  pxOp !f (PixelRGBA r g b a) = PixelRGBA (f r) (f g) (f b) (f a)
  {-# INLINE pxOp #-}

  chApp (PixelRGBA fr fg fb fa) (PixelRGBA r g b a) = PixelRGBA (fr r) (fg g) (fb b) (fa a)
  {-# INLINE chApp #-}


instance Alpha RGBA where
  type Opaque RGBA = RGB

  getAlpha (PixelRGBA _ _ _ a) = a
  {-# INLINE getAlpha #-}
  
  addAlpha !a (PixelRGB r g b) = PixelRGBA r g b a
  {-# INLINE addAlpha #-}

  dropAlpha (PixelRGBA r g b _) = PixelRGB r g b
  {-# INLINE dropAlpha #-}


instance Show RGB where
  show _ = "RGB"

instance Show RGBA where
  show _ = "RGBA"

 
instance Show e => Show (Pixel RGB e) where
  show (PixelRGB r g b) = "<RGB:("++show r++"|"++show g++"|"++show b++")>"


instance Show e => Show (Pixel RGBA e) where
  show (PixelRGBA r g b a) = "<RGBA:("++show r++"|"++show g++"|"++show b++"|"++show a++")>"



