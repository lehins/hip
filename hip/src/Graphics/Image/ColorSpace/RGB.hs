{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TypeFamilies #-}
module Graphics.Image.ColorSpace.RGB (
  RGB(..), RGBA(..), Pixel(..), PixelRGB, PixelRGBA, 
  ToRGB(..), ToRGBA(..)
  ) where

import Prelude hiding (map)
import Graphics.Image.Interface


data RGB = RedRGB
         | GreenRGB
         | BlueRGB deriving (Eq, Enum)

data RGBA = RedRGBA
          | GreenRGBA
          | BlueRGBA
          | AlphaRGBA deriving (Eq, Enum)


type PixelRGB = Pixel RGB Double  

type PixelRGBA = Pixel RGBA Double


class ColorSpace cs => ToRGB cs where

  toPixelRGB :: Pixel cs Double -> PixelRGB

  toImageRGB :: (Array arr cs Double, Array arr RGB Double) =>
                Image arr cs Double
             -> Image arr RGB Double
  toImageRGB = map toPixelRGB
  {-# INLINE toImageRGB #-}


class (ToRGB (Opaque cs), Alpha cs) => ToRGBA cs where

  toPixelRGBA :: Pixel cs Double -> PixelRGBA
  toPixelRGBA px = addAlpha (getAlpha px) (toPixelRGB (dropAlpha px))

  toImageRGBA :: (Array arr cs Double, Array arr RGBA Double) =>
                Image arr cs Double
             -> Image arr RGBA Double
  toImageRGBA = map toPixelRGBA
  {-# INLINE toImageRGBA #-}

  
instance ColorSpace RGB where
  type PixelElt RGB e = (e, e, e)
  data Pixel RGB e = PixelRGB !e !e !e

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

  chOp2 !f (PixelRGB r1 g1 b1) (PixelRGB r2 g2 b2) =
    PixelRGB (f RedRGB r1 r2) (f GreenRGB g1 g2) (f BlueRGB b1 b2)
  {-# INLINE chOp2 #-}
  
  pxOp !f (PixelRGB r g b) = PixelRGB (f r) (f g) (f b)
  {-# INLINE pxOp #-}

  pxOp2 !f (PixelRGB r1 g1 b1) (PixelRGB r2 g2 b2) = PixelRGB (f r1 r2) (f g1 g2) (f b1 b2)
  {-# INLINE pxOp2 #-}



instance ColorSpace RGBA where
  type PixelElt RGBA e = (e, e, e, e)
  data Pixel RGBA e = PixelRGBA !e !e !e !e

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

  chOp2 !f (PixelRGBA r1 g1 b1 a1) (PixelRGBA r2 g2 b2 a2) =
    PixelRGBA (f RedRGBA r1 r2) (f GreenRGBA g1 g2) (f BlueRGBA b1 b2) (f AlphaRGBA a1 a2)
  {-# INLINE chOp2 #-}
  
  pxOp !f (PixelRGBA r g b a) = PixelRGBA (f r) (f g) (f b) (f a)
  {-# INLINE pxOp #-}

  pxOp2 !f (PixelRGBA r1 g1 b1 a1) (PixelRGBA r2 g2 b2 a2) =
    PixelRGBA (f r1 r2) (f g1 g2) (f b1 b2) (f a1 a2)
  {-# INLINE pxOp2 #-}


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



