{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             TypeFamilies #-}
module Graphics.Image.ColorSpace.Luma (
  Y(..), YA(..), Pixel(..), 
  ToY(..), ToYA(..)
  ) where

import Prelude hiding (map)
import Graphics.Image.Interface
import Data.Typeable (Typeable)

-- | Luma or brightness, that is usually denoted as @Y'@.
data Y = Y deriving (Eq, Enum, Show, Typeable)

-- | Luma with Alpha channel.
data YA = YA
        | AlphaYA deriving (Eq, Enum, Show, Typeable)


class ColorSpace cs => ToY cs where

  toPixelY :: Pixel cs Double -> Pixel Y Double

  toImageY :: (Array arr cs Double, Array arr Y Double) =>
              Image arr cs Double
           -> Image arr Y Double
  toImageY = map toPixelY
  {-# INLINE toImageY #-}
  

class (ToY (Opaque cs), Alpha cs) => ToYA cs where

  toPixelYA :: Pixel cs Double -> Pixel YA Double
  toPixelYA px = addAlpha (getAlpha px) (toPixelY (dropAlpha px))

  toImageYA :: (Array arr cs Double, Array arr YA Double) =>
               Image arr cs Double
            -> Image arr YA Double
  toImageYA = map toPixelYA
  {-# INLINE toImageYA #-}


instance ColorSpace Y where
  type PixelElt Y e = e
  data Pixel Y e = PixelY !e deriving (Ord, Eq)

  fromChannel = PixelY
  {-# INLINE fromChannel #-}

  fromElt = PixelY
  {-# INLINE fromElt #-}

  toElt (PixelY y) = y
  {-# INLINE toElt #-}

  getPxCh (PixelY y) _ = y
  {-# INLINE getPxCh #-}
  
  chOp !f (PixelY y) = PixelY (f Y y)
  {-# INLINE chOp #-}
 
  pxOp !f (PixelY y) = PixelY (f y)
  {-# INLINE pxOp #-}

  chApp (PixelY fy) (PixelY y) = PixelY (fy y)
  {-# INLINE chApp #-}


instance ColorSpace YA where
  type PixelElt YA e = (e, e)
  data Pixel YA e = PixelYA !e !e deriving Eq

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
  
  pxOp !f (PixelYA g a) = PixelYA (f g) (f a)
  {-# INLINE pxOp #-}

  chApp (PixelYA fy fa) (PixelYA y a) = PixelYA (fy y) (fa a)
  {-# INLINE chApp #-}

  
instance Alpha YA where
  type Opaque YA = Y

  getAlpha (PixelYA _ a) = a
  {-# INLINE getAlpha  #-}
  
  addAlpha !a (PixelY g) = PixelYA g a
  {-# INLINE addAlpha #-}

  dropAlpha (PixelYA g _) = PixelY g
  {-# INLINE dropAlpha #-}


instance Show e => Show (Pixel Y e) where
  show (PixelY g) = "<Luma:("++show g++")>"


instance Show e => Show (Pixel YA e) where
  show (PixelYA g a) = "<LumaA:("++show g++"|"++show a++")>"


