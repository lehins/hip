{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Graphics.Image.ColorSpace.RGB (
  RGB(..), RGBA(..), Pixel(..), red, green, blue, rgbAlpha
  ) where

import Graphics.Image.Interface

data RGB

data RGBA

red :: Num Ix => Ix
red = 0

green :: Num Ix => Ix
green = 1

blue :: Num Ix => Ix
blue = 2

rgbAlpha :: Num Ix => Ix
rgbAlpha = 3


instance Show RGB where
  show _ = "RGB"

instance Show RGBA where
  show _ = "RGBA"


--type PixelRGB e = Pixel RGB e
--type PixelRGBA e = Pixel RGBA e

instance ColorSpace RGB where
  type PixelElt RGB e = (e, e, e)
  data Pixel RGB e = PixelRGB e e e

  rank _ = 3

  fromChannel e = PixelRGB e e e

  fromElt (r, g, b) = PixelRGB r g b

  toElt (PixelRGB r g b) = (r, g, b)

  getEltCh cif eq (r, g, b) _ n =
    cif (n `eq` red) r (
      cif (n `eq` green) g (
         cif (n `eq` blue) b (error ((show (undefined :: RGB))++
                                     " Color Space does not have a channel with index: "++
                                     (show n)))))
  
  getPxCh cif eq (PixelRGB r g b) n = cif (n `eq` 0) r (cif (n `eq` 1) g b)
  
  chOp f (PixelRGB r g b) = PixelRGB (f red r) (f green g) (f blue b)

  chOp2 f (PixelRGB r1 g1 b1) (PixelRGB r2 g2 b2) =
    PixelRGB (f red r1 r2) (f green g1 g2) (f blue b1 b2)
  
  pxOp f (PixelRGB r g b) = PixelRGB (f r) (f g) (f b)

  pxOp2 f (PixelRGB r1 g1 b1) (PixelRGB r2 g2 b2) = PixelRGB (f r1 r2) (f g1 g2) (f b1 b2)

  indexElt' img _ ix = (index img red ix, index img green ix, index img blue ix)


instance ColorSpace RGBA where
  type PixelElt RGBA e = (e, e, e, e)
  data Pixel RGBA e = PixelRGBA e e e e

  rank _ = 4

  fromChannel e = PixelRGBA e e e e

  fromElt (r, g, b, a) = PixelRGBA r g b a

  toElt (PixelRGBA r g b a) = (r, g, b, a)

  getEltCh cif eq (r, g, b, a) _ n =
    cif (n `eq` 0) r (cif (n `eq` 1) g (cif (n `eq` 2) b a))

  getPxCh cif eq (PixelRGBA r g b a) n =
    cif (n `eq` 0) r (cif (n `eq` 1) g (cif (n `eq` 2) b a))

  chOp f (PixelRGBA r g b a) = PixelRGBA (f red r) (f green g) (f blue b) (f rgbAlpha a)
  
  chOp2 f (PixelRGBA r1 g1 b1 a1) (PixelRGBA r2 g2 b2 a2) =
    PixelRGBA (f red r1 r2) (f green g1 g2) (f blue b1 b2) (f rgbAlpha a1 a2)
    
  pxOp f (PixelRGBA r g b a) = PixelRGBA (f r) (f g) (f b) (f a)

  pxOp2 f (PixelRGBA r1 g1 b1 a1) (PixelRGBA r2 g2 b2 a2) =
    PixelRGBA (f r1 r2) (f g1 g2) (f b1 b2) (f a1 a2)

  indexElt' img _ ix =
    (index img red ix, index img green ix, index img blue ix, index img rgbAlpha ix)

  
instance Show e => Show (Pixel RGB e) where
  show (PixelRGB r g b) = "<RGB:("++show r++"|"++show g++"|"++show b++")>"


instance Show e => Show (Pixel RGBA e) where
  show (PixelRGBA r g b a) = "<RGB:("++show r++"|"++show g++"|"++show b++"|"++show a++")>"
