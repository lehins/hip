{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TypeFamilies #-}
module Graphics.Image.ColorSpace.Gray (
  Gray(..), GrayA(..), Pixel(..), PixelGray, PixelGrayA, 
  ToGray(..), ToGrayA(..)
  ) where

import Prelude hiding (map)
import Graphics.Image.Interface


data Gray = Gray deriving (Eq, Enum)

data GrayA = GrayA
           | AlphaGrayA deriving (Eq, Enum)

type PixelGray = Pixel Gray Double  

type PixelGrayA = Pixel GrayA Double


class ColorSpace cs => ToGray cs where

  toPixelGray :: Pixel cs Double -> PixelGray

  toImageGray :: (Array arr cs Double, Array arr Gray Double) =>
                 Image arr cs Double
              -> Image arr Gray Double
  toImageGray = map toPixelGray
  {-# INLINE toImageGray #-}
  

class (ToGray (Opaque cs), Alpha cs) => ToGrayA cs where

  toPixelGrayA :: Pixel cs Double -> PixelGrayA
  toPixelGrayA px = addAlpha (getAlpha px) (toPixelGray (dropAlpha px))

  toImageGrayA :: (Array arr cs Double, Array arr GrayA Double) =>
                  Image arr cs Double
               -> Image arr GrayA Double
  toImageGrayA = map toPixelGrayA
  {-# INLINE toImageGrayA #-}


instance ColorSpace Gray where
  type PixelElt Gray e = e
  data Pixel Gray e = PixelGray !e 

  fromChannel = PixelGray
  {-# INLINE fromChannel #-}

  fromElt = PixelGray
  {-# INLINE fromElt #-}

  toElt (PixelGray g) = g
  {-# INLINE toElt #-}

  getPxCh (PixelGray g) _ = g
  {-# INLINE getPxCh #-}
  
  chOp !f (PixelGray g) = PixelGray (f Gray g)
  {-# INLINE chOp #-}

  chOp2 !f (PixelGray g1) (PixelGray g2) = PixelGray (f Gray g1 g2)
  {-# INLINE chOp2 #-}
  
  pxOp !f (PixelGray g) = PixelGray (f g)
  {-# INLINE pxOp #-}

  pxOp2 !f (PixelGray g1) (PixelGray g2) = PixelGray (f g1 g2)
  {-# INLINE pxOp2 #-}


instance ColorSpace GrayA where
  type PixelElt GrayA e = (e, e)
  data Pixel GrayA e = PixelGrayA !e !e

  fromChannel !e = PixelGrayA e e 
  {-# INLINE fromChannel #-}

  fromElt !(g, a) = PixelGrayA g a
  {-# INLINE fromElt #-}

  toElt (PixelGrayA g a) = (g, a)
  {-# INLINE toElt  #-}

  getPxCh (PixelGrayA g _) GrayA      = g
  getPxCh (PixelGrayA _ a) AlphaGrayA = a
  {-# INLINE getPxCh  #-}
  
  chOp !f (PixelGrayA g a) = PixelGrayA (f GrayA g) (f AlphaGrayA a)
  {-# INLINE chOp #-}
  
  chOp2 !f (PixelGrayA g1 a1) (PixelGrayA g2 a2) =
    PixelGrayA (f GrayA g1 g2) (f AlphaGrayA a1 a2)
  {-# INLINE chOp2 #-}
    
  pxOp !f (PixelGrayA g a) = PixelGrayA (f g) (f a)
  {-# INLINE pxOp #-}

  pxOp2 !f (PixelGrayA g1 a1) (PixelGrayA g2 a2) =  PixelGrayA (f g1 g2) (f a1 a2)
  {-# INLINE pxOp2 #-}

  
instance Alpha GrayA where
  type Opaque GrayA = Gray

  getAlpha (PixelGrayA _ a) = a
  {-# INLINE getAlpha  #-}
  
  addAlpha !a (PixelGray g) = PixelGrayA g a
  {-# INLINE addAlpha #-}

  dropAlpha (PixelGrayA g _) = PixelGray g
  {-# INLINE dropAlpha #-}


instance Show Gray where
  show _ = "Gray"

instance Show GrayA where
  show _ = "GrayAlpha"


instance Show e => Show (Pixel Gray e) where
  show (PixelGray g) = "<Gray:("++show g++")>"


instance Show e => Show (Pixel GrayA e) where
  show (PixelGrayA g a) = "<GrayA:("++show g++"|"++show a++")>"


