{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TypeFamilies #-}
module Graphics.Image.ColorSpace.YCbCr (
  YCbCr(..), YCbCrA(..), Pixel(..), PixelYCbCr, PixelYCbCrA, 
  ToYCbCr(..), ToYCbCrA(..)
  ) where

import Prelude hiding (map)
import Graphics.Image.Interface


data YCbCr = LumaYCbCr
           | CBlueYCbCr
           | CRedYCbCr deriving (Eq, Enum)

data YCbCrA = LumaYCbCrA
            | CBlueYCbCrA
            | CRedYCbCrA
            | AlphaYCbCrA deriving (Eq, Enum)


type PixelYCbCr = Pixel YCbCr Double  

type PixelYCbCrA = Pixel YCbCrA Double


class ColorSpace cs => ToYCbCr cs where

  toPixelYCbCr :: Pixel cs Double -> PixelYCbCr

  toImageYCbCr :: (Array arr cs Double, Array arr YCbCr Double) =>
                  Image arr cs Double
               -> Image arr YCbCr Double
  toImageYCbCr = map toPixelYCbCr
  {-# INLINE toImageYCbCr #-}


class (ToYCbCr (Opaque cs), Alpha cs) => ToYCbCrA cs where

  toPixelYCbCrA :: Pixel cs Double -> PixelYCbCrA
  toPixelYCbCrA px = addAlpha (getAlpha px) (toPixelYCbCr (dropAlpha px))

  toImageYCbCrA :: (Array arr cs Double, Array arr YCbCrA Double) =>
                   Image arr cs Double
                -> Image arr YCbCrA Double
  toImageYCbCrA = map toPixelYCbCrA
  {-# INLINE toImageYCbCrA #-}

  
instance ColorSpace YCbCr where
  type PixelElt YCbCr e = (e, e, e)
  data Pixel YCbCr e = PixelYCbCr !e !e !e

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

  chOp2 !f (PixelYCbCr y1 b1 r1) (PixelYCbCr y2 b2 r2) =
    PixelYCbCr (f LumaYCbCr y1 y2) (f CBlueYCbCr b1 b2) (f CRedYCbCr r1 r2)
  {-# INLINE chOp2 #-}
  
  pxOp !f (PixelYCbCr y b r) = PixelYCbCr (f y) (f b) (f r)
  {-# INLINE pxOp #-}

  pxOp2 !f (PixelYCbCr y1 b1 r1) (PixelYCbCr y2 b2 r2) = PixelYCbCr (f y1 y2) (f b1 b2) (f r1 r2)
  {-# INLINE pxOp2 #-}



instance ColorSpace YCbCrA where
  type PixelElt YCbCrA e = (e, e, e, e)
  data Pixel YCbCrA e = PixelYCbCrA !e !e !e !e

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

  chOp2 !f (PixelYCbCrA y1 b1 r1 a1) (PixelYCbCrA y2 b2 r2 a2) =
    PixelYCbCrA (f LumaYCbCrA y1 y2) (f CBlueYCbCrA b1 b2) (f CRedYCbCrA r1 r2)
                (f AlphaYCbCrA a1 a2)
  {-# INLINE chOp2 #-}
  
  pxOp !f (PixelYCbCrA y b r a) = PixelYCbCrA (f y) (f b) (f r) (f a)
  {-# INLINE pxOp #-}

  pxOp2 !f (PixelYCbCrA y1 b1 r1 a1) (PixelYCbCrA y2 b2 r2 a2) =
    PixelYCbCrA (f y1 y2) (f b1 b2) (f r1 r2) (f a1 a2)
  {-# INLINE pxOp2 #-}


instance Alpha YCbCrA where
  type Opaque YCbCrA = YCbCr

  getAlpha (PixelYCbCrA _ _ _ a) = a
  {-# INLINE getAlpha #-}
  
  addAlpha !a (PixelYCbCr y b r) = PixelYCbCrA y b r a
  {-# INLINE addAlpha #-}

  dropAlpha (PixelYCbCrA y b r _) = PixelYCbCr y b r
  {-# INLINE dropAlpha #-}


instance Show YCbCr where
  show _ = "YCbCr"

instance Show YCbCrA where
  show _ = "YCbCrA"

 
instance Show e => Show (Pixel YCbCr e) where
  show (PixelYCbCr y b r) = "<YCbCr:("++show y++"|"++show b++"|"++show r++")>"


instance Show e => Show (Pixel YCbCrA e) where
  show (PixelYCbCrA y b r a) = "<YCbCrA:("++show y++"|"++show b++"|"++show r++"|"++show a++")>"



