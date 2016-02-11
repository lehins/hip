{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TypeFamilies #-}
module Graphics.Image.ColorSpace.Luma (
  Y(..), YA(..), Pixel(..), PixelY, PixelYA, 
  ToY(..), ToYA(..)
  ) where

import Prelude hiding (map)
import Graphics.Image.Interface

-- ^ Luma, that is usually denoted as Y'.
data Y = Y deriving (Eq, Enum)

-- ^ Luma with Alpha channel.
data YA = YA
        | AlphaYA deriving (Eq, Enum)

type PixelY = Pixel Y Double  

type PixelYA = Pixel YA Double


class ColorSpace cs => ToY cs where

  toPixelY :: Pixel cs Double -> PixelY

  toImageY :: (Array arr cs Double, Array arr Y Double) =>
              Image arr cs Double
           -> Image arr Y Double
  toImageY = map toPixelY
  {-# INLINE toImageY #-}
  

class (ToY (Opaque cs), Alpha cs) => ToYA cs where

  toPixelYA :: Pixel cs Double -> PixelYA
  toPixelYA px = addAlpha (getAlpha px) (toPixelY (dropAlpha px))

  toImageYA :: (Array arr cs Double, Array arr YA Double) =>
                  Image arr cs Double
               -> Image arr YA Double
  toImageYA = map toPixelYA
  {-# INLINE toImageYA #-}


instance ColorSpace Y where
  type PixelElt Y e = e
  data Pixel Y e = PixelY !e 

  fromChannel = PixelY
  {-# INLINE fromChannel #-}

  fromElt = PixelY
  {-# INLINE fromElt #-}

  toElt (PixelY g) = g
  {-# INLINE toElt #-}

  getPxCh (PixelY g) _ = g
  {-# INLINE getPxCh #-}
  
  chOp !f (PixelY g) = PixelY (f Y g)
  {-# INLINE chOp #-}

  chOp2 !f (PixelY g1) (PixelY g2) = PixelY (f Y g1 g2)
  {-# INLINE chOp2 #-}
  
  pxOp !f (PixelY g) = PixelY (f g)
  {-# INLINE pxOp #-}

  pxOp2 !f (PixelY g1) (PixelY g2) = PixelY (f g1 g2)
  {-# INLINE pxOp2 #-}


instance ColorSpace YA where
  type PixelElt YA e = (e, e)
  data Pixel YA e = PixelYA !e !e

  fromChannel !e = PixelYA e e 
  {-# INLINE fromChannel #-}

  fromElt !(g, a) = PixelYA g a
  {-# INLINE fromElt #-}

  toElt (PixelYA g a) = (g, a)
  {-# INLINE toElt  #-}

  getPxCh (PixelYA g _) YA      = g
  getPxCh (PixelYA _ a) AlphaYA = a
  {-# INLINE getPxCh  #-}
  
  chOp !f (PixelYA g a) = PixelYA (f YA g) (f AlphaYA a)
  {-# INLINE chOp #-}
  
  chOp2 !f (PixelYA g1 a1) (PixelYA g2 a2) =
    PixelYA (f YA g1 g2) (f AlphaYA a1 a2)
  {-# INLINE chOp2 #-}
    
  pxOp !f (PixelYA g a) = PixelYA (f g) (f a)
  {-# INLINE pxOp #-}

  pxOp2 !f (PixelYA g1 a1) (PixelYA g2 a2) =  PixelYA (f g1 g2) (f a1 a2)
  {-# INLINE pxOp2 #-}

  
instance Alpha YA where
  type Opaque YA = Y

  getAlpha (PixelYA _ a) = a
  {-# INLINE getAlpha  #-}
  
  addAlpha !a (PixelY g) = PixelYA g a
  {-# INLINE addAlpha #-}

  dropAlpha (PixelYA g _) = PixelY g
  {-# INLINE dropAlpha #-}


instance Show Y where
  show _ = "Luma"

instance Show YA where
  show _ = "LumaAlpha"


instance Show e => Show (Pixel Y e) where
  show (PixelY g) = "<Luma:("++show g++")>"


instance Show e => Show (Pixel YA e) where
  show (PixelYA g a) = "<LumaA:("++show g++"|"++show a++")>"


