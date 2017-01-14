{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Image.ColorSpace.RGB
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.ColorSpace.RGB (
  RGB(..), RGBA(..), Pixel(..), 
  ToRGB(..), ToRGBA(..)
  ) where

import Prelude hiding (map)
import Graphics.Image.Interface
import Data.Typeable (Typeable)
import qualified Data.Monoid as M (mappend)
import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import Foreign.Ptr
import Foreign.Storable


-- | Red, Green and Blue color space.
data RGB = RedRGB
         | GreenRGB
         | BlueRGB deriving (Eq, Enum, Typeable)

-- | Red, Green and Blue color space with Alpha channel.
data RGBA = RedRGBA
          | GreenRGBA
          | BlueRGBA
          | AlphaRGBA deriving (Eq, Enum, Typeable)


data instance Pixel RGB e = PixelRGB !e !e !e deriving Eq

data instance Pixel RGBA e = PixelRGBA !e !e !e !e deriving Eq


-- | Conversion to `RGB` color space.
class ColorSpace cs => ToRGB cs where

  -- | Convert to an `RGB` pixel.
  toPixelRGB :: Pixel cs Double -> Pixel RGB Double

  -- | Convert to an `RGB` image.
  toImageRGB :: (Array arr cs Double, Array arr RGB Double) =>
                Image arr cs Double
             -> Image arr RGB Double
  toImageRGB = map toPixelRGB
  {-# INLINE toImageRGB #-}


-- | Conversion to `RGBA` from another color space with Alpha channel.
class (ToRGB (Opaque cs), Alpha cs) => ToRGBA cs where

  -- | Convert to an `RGBA` pixel.
  toPixelRGBA :: Pixel cs Double -> Pixel RGBA Double
  toPixelRGBA px = addAlpha (getAlpha px) (toPixelRGB (dropAlpha px))
  {-# INLINE toPixelRGBA #-}

  -- | Convert to an `RGBA` image.
  toImageRGBA :: (Array arr cs Double, Array arr RGBA Double) =>
                Image arr cs Double
             -> Image arr RGBA Double
  toImageRGBA = map toPixelRGBA
  {-# INLINE toImageRGBA #-}

  
instance ColorSpace RGB where
  type PixelElt RGB e = (e, e, e)

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
  
  chOp f (PixelRGB r g b) = PixelRGB (f RedRGB r) (f GreenRGB g) (f BlueRGB b)
  {-# INLINE chOp #-}

  pxOp f (PixelRGB r g b) = PixelRGB (f r) (f g) (f b)
  {-# INLINE pxOp #-}

  chApp (PixelRGB fr fg fb) (PixelRGB r g b) = PixelRGB (fr r) (fg g) (fb b)
  {-# INLINE chApp #-}

  pxFoldMap f (PixelRGB r g b) = f r `M.mappend` f g `M.mappend` f b
  {-# INLINE pxFoldMap #-}

  csColour RedRGB   = C.opaque C.red
  csColour GreenRGB = C.opaque C.green
  csColour BlueRGB  = C.opaque C.blue
  

instance ColorSpace RGBA where
  type PixelElt RGBA e = (e, e, e, e)

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

  pxFoldMap f (PixelRGBA r g b a) = f r `M.mappend` f g `M.mappend` f b `M.mappend` f a
  {-# INLINE pxFoldMap #-}

  csColour AlphaRGBA = C.opaque C.gray
  csColour ch        = csColour $ opaque ch


instance Alpha RGBA where
  type Opaque RGBA = RGB

  getAlpha (PixelRGBA _ _ _ a) = a
  {-# INLINE getAlpha #-}
  
  addAlpha !a (PixelRGB r g b) = PixelRGBA r g b a
  {-# INLINE addAlpha #-}

  dropAlpha (PixelRGBA r g b _) = PixelRGB r g b
  {-# INLINE dropAlpha #-}

  opaque RedRGBA   = RedRGB
  opaque GreenRGBA = GreenRGB
  opaque BlueRGBA  = BlueRGB
  opaque AlphaRGBA = error "Data.Image.ColorSpace.RGB (Alpha.opaque)"


instance Show RGB where
  show RedRGB   = "Red"
  show GreenRGB = "Green"
  show BlueRGB  = "Blue"


instance Show RGBA where
  show AlphaRGBA = "Alpha"
  show ch        = show $ opaque ch

 
instance Show e => Show (Pixel RGB e) where
  show (PixelRGB r g b) = "<RGB:("++show r++"|"++show g++"|"++show b++")>"


instance Show e => Show (Pixel RGBA e) where
  show (PixelRGBA r g b a) = "<RGBA:("++show r++"|"++show g++"|"++show b++"|"++show a++")>"



instance Storable e => Storable (Pixel RGB e) where

  sizeOf _ = 3 * sizeOf (undefined :: e)
  alignment _ = alignment (undefined :: e)
  peek p = do
    q <- return $ castPtr p
    r <- peek q
    g <- peekElemOff q 1
    b <- peekElemOff q 2
    return (PixelRGB r g b)
  poke p (PixelRGB r g b) = do
    q <- return $ castPtr p
    poke q r
    pokeElemOff q 1 g
    pokeElemOff q 2 b


instance Storable e => Storable (Pixel RGBA e) where

  sizeOf _ = 3 * sizeOf (undefined :: e)
  alignment _ = alignment (undefined :: e)
  peek p = do
    q <- return $ castPtr p
    r <- peek q
    g <- peekElemOff q 1
    b <- peekElemOff q 2
    a <- peekElemOff q 3
    return (PixelRGBA r g b a)
  poke p (PixelRGBA r g b a) = do
    q <- return $ castPtr p
    poke q r
    pokeElemOff q 1 g
    pokeElemOff q 2 b
    pokeElemOff q 3 a

------------------------------------------------

-- -- | Red, Green and Blue color space.
-- data RGBd = RedRGBd
--           | GreenRGBd
--           | BlueRGBd deriving (Eq, Enum, Typeable)


-- data instance Pixel RGBd Double = PixelRGBd
--                                   {-# UNPACK #-} !Double
--                                   {-# UNPACK #-} !Double
--                                   {-# UNPACK #-} !Double deriving Eq


-- instance ColorSpace RGBd where
--   type PixelElt RGBd Double = (Double, Double, Double)

--   fromChannel !e = PixelRGBd e e e
--   {-# INLINE fromChannel #-}

--   fromElt !(r, g, b) = PixelRGBd r g b
--   {-# INLINE fromElt #-}

--   toElt (PixelRGBd r g b) = (r, g, b)
--   {-# INLINE toElt #-}

--   getPxCh (PixelRGBd r _ _) RedRGBd   = r
--   getPxCh (PixelRGBd _ g _) GreenRGBd = g
--   getPxCh (PixelRGBd _ _ b) BlueRGBd  = b
--   {-# INLINE getPxCh #-}
  
--   chOp f (PixelRGBd r g b) = PixelRGBd (f RedRGBd r) (f GreenRGBd g) (f BlueRGBd b)
--   {-# INLINE chOp #-}

--   pxOp f (PixelRGBd r g b) = PixelRGBd (f r) (f g) (f b)
--   {-# INLINE pxOp #-}

--   chApp (PixelRGBd fr fg fb) (PixelRGBd r g b) = PixelRGBd (fr r) (fg g) (fb b)
--   {-# INLINE chApp #-}

--   pxFoldMap f (PixelRGBd r g b) = f r `M.mappend` f g `M.mappend` f b
--   {-# INLINE pxFoldMap #-}

--   csColour RedRGBd   = C.opaque C.red
--   csColour GreenRGBd = C.opaque C.green
--   csColour BlueRGBd  = C.opaque C.blue
  


-- instance Show RGBd where
--   show RedRGBd   = "Red"
--   show GreenRGBd = "Green"
--   show BlueRGBd  = "Blue"

-- instance Show (Pixel RGBd Double) where
--   show (PixelRGBd r g b) = "<RGB:("++show r++"|"++show g++"|"++show b++")>"


-- instance Storable (Pixel RGBd Double) where

--   sizeOf _ = 3 * sizeOf (undefined :: Double)
--   alignment _ = alignment (undefined :: Double)
--   peek p = do
--     q <- return $ castPtr p
--     r <- peek q
--     g <- peekElemOff q 1
--     b <- peekElemOff q 2
--     return (PixelRGBd r g b)
--   poke p (PixelRGBd r g b) = do
--     q <- return $ castPtr p
--     poke q r
--     pokeElemOff q 1 g
--     pokeElemOff q 2 b
