{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Graphics.Image.ColorSpace.Gray (
  Gray(..), GrayA(..), Pixel(..), PixelGray, PixelGrayA, gray, grayAlpha
  ) where

import Graphics.Image.Interface


data Gray

data GrayA

type PixelGray = Pixel Gray Double  

type PixelGrayA = Pixel GrayA Double  

gray :: Int
gray = 0

grayAlpha :: Int
grayAlpha = 1


instance Show Gray where
  show _ = "Gray"

instance Show GrayA where
  show _ = "GrayAlpha"


instance ColorSpace Gray where
  type PixelElt Gray e = e
  data Pixel Gray e = PixelGray e 

  rank _ = 1

  fromChannel g = PixelGray g

  fromElt g = PixelGray g

  toElt (PixelGray g) = g

  getEltCh g _ n | n == gray = g
                 | otherwise = channelIndexError "getEltCh" (undefined :: Gray) n

  getPxCh (PixelGray g) n | n == gray = g
                          | otherwise = channelIndexError "getPxCh" (undefined :: Gray) n 
  
  chOp f (PixelGray g) = PixelGray (f gray g)

  chOp2 f (PixelGray g1) (PixelGray g2) =
    PixelGray (f gray g1 g2)
  
  pxOp f (PixelGray g) = PixelGray (f g)

  pxOp2 f (PixelGray g1) (PixelGray g2) = PixelGray (f g1 g2)

  indexElt' img _ ix = index img gray ix


instance ColorSpace GrayA where
  type PixelElt GrayA e = (e, e)
  data Pixel GrayA e = PixelGrayA e e

  rank _ = 2

  fromChannel e = PixelGrayA e e 

  fromElt (g, a) = PixelGrayA g a

  toElt (PixelGrayA g a) = (g, a)

  getEltCh (g, a) _ n | n == gray      = g
                      | n == grayAlpha = a
                      | otherwise  = channelIndexError "getEltCh" (undefined :: GrayA) n

  getPxCh (PixelGrayA g a) n | n == gray      = g
                             | n == grayAlpha = a
                             | otherwise      = channelIndexError "getPxCh" (undefined :: GrayA) n 
  
  chOp f (PixelGrayA g a) = PixelGrayA (f gray g) (f grayAlpha a)
  
  chOp2 f (PixelGrayA g1 a1) (PixelGrayA g2 a2) =
    PixelGrayA (f gray g1 g2) (f grayAlpha a1 a2)
    
  pxOp f (PixelGrayA g a) = PixelGrayA (f g) (f a)

  pxOp2 f (PixelGrayA g1 a1) (PixelGrayA g2 a2) =
    PixelGrayA (f g1 g2) (f a1 a2)

  indexElt' img _ ix =
    (index img gray ix, index img grayAlpha ix)

  
instance Show e => Show (Pixel Gray e) where
  show (PixelGray g) = "<Gray:("++show g++")>"


instance Show e => Show (Pixel GrayA e) where
  show (PixelGrayA g a) = "<GrayA:("++show g++"|"++show a++")>"


instance Alpha GrayA where
  type Opaque GrayA = Gray

  addAlpha a (PixelGray g) = PixelGrayA g a

  dropAlpha (PixelGrayA g _) = PixelGray g

