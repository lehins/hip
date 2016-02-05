{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies #-}
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
  

class (ToGray (Opaque cs), Alpha cs) => ToGrayA cs where

  toPixelGrayA :: Pixel cs Double -> PixelGrayA
  toPixelGrayA px = addAlpha (getAlpha px) (toPixelGray (dropAlpha px))

  toImageGrayA :: (Array arr cs Double, Array arr GrayA Double) =>
                  Image arr cs Double
               -> Image arr GrayA Double
  toImageGrayA = map toPixelGrayA


instance Show Gray where
  show _ = "Gray"

instance Show GrayA where
  show _ = "GrayAlpha"


instance ColorSpace Gray where
  type PixelElt Gray e = e
  data Pixel Gray e = PixelGray e 

  fromChannel g = PixelGray g

  fromElt g = PixelGray g

  toElt (PixelGray g) = g

  getPxCh (PixelGray g) _ = g
  
  chOp f (PixelGray g) = PixelGray (f Gray g)

  chOp2 f (PixelGray g1) (PixelGray g2) =
    PixelGray (f Gray g1 g2)
  
  pxOp f (PixelGray g) = PixelGray (f g)

  pxOp2 f (PixelGray g1) (PixelGray g2) = PixelGray (f g1 g2)


instance ColorSpace GrayA where
  type PixelElt GrayA e = (e, e)
  data Pixel GrayA e = PixelGrayA e e

  fromChannel e = PixelGrayA e e 

  fromElt (g, a) = PixelGrayA g a

  toElt (PixelGrayA g a) = (g, a)

  getPxCh (PixelGrayA g _) GrayA      = g
  getPxCh (PixelGrayA _ a) AlphaGrayA = a
  
  chOp f (PixelGrayA g a) = PixelGrayA (f GrayA g) (f AlphaGrayA a)
  
  chOp2 f (PixelGrayA g1 a1) (PixelGrayA g2 a2) =
    PixelGrayA (f GrayA g1 g2) (f AlphaGrayA a1 a2)
    
  pxOp f (PixelGrayA g a) = PixelGrayA (f g) (f a)

  pxOp2 f (PixelGrayA g1 a1) (PixelGrayA g2 a2) =
    PixelGrayA (f g1 g2) (f a1 a2)

  
instance Show e => Show (Pixel Gray e) where
  show (PixelGray g) = "<Gray:("++show g++")>"


instance Show e => Show (Pixel GrayA e) where
  show (PixelGrayA g a) = "<GrayA:("++show g++"|"++show a++")>"


instance Alpha GrayA where
  type Opaque GrayA = Gray

  getAlpha (PixelGrayA _ a) = a
  
  addAlpha a (PixelGray g) = PixelGrayA g a

  dropAlpha (PixelGrayA g _) = PixelGray g

