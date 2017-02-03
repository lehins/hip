{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Image.ColorSpace.Y
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.ColorSpace.Y (
  Y(..), YA(..), Pixel(..), 
  ToY(..), toImageY, ToYA(..), toImageYA
  ) where

import Prelude hiding (map)
import Control.Applicative
import Data.Foldable
import Data.Typeable (Typeable)
import Foreign.Ptr
import Foreign.Storable

import Graphics.Image.Interface
import Graphics.Image.Interface.Instances()

---------
--- Y ---
---------

-- | Luma or brightness, which is usually denoted as @Y'@.
data Y = LumaY deriving (Eq, Enum, Show, Bounded, Typeable)


newtype instance Pixel Y e = PixelY e deriving (Ord, Eq)

-- | Conversion to Luma color space.
class ColorSpace cs e => ToY cs e where

  -- | Convert a pixel to Luma pixel.
  toPixelY :: Pixel cs e -> Pixel Y Double

-- | Convert an image to Luma image.
toImageY :: (ToY cs e, Array arr cs e, Array arr Y Double) =>
            Image arr cs e
         -> Image arr Y Double
toImageY = map toPixelY
{-# INLINE toImageY #-}


instance Show e => Show (Pixel Y e) where
  show (PixelY g) = "<Luma:("++show g++")>"

instance Elevator e => ColorSpace Y e where
  type Components Y e = e
  promote = PixelY
  {-# INLINE promote #-}
  fromComponents = PixelY
  {-# INLINE fromComponents #-}
  toComponents (PixelY y) = y
  {-# INLINE toComponents #-}
  getPxC (PixelY y) LumaY = y
  {-# INLINE getPxC #-}
  setPxC _ LumaY y = PixelY y
  {-# INLINE setPxC #-}
  mapPxC f (PixelY y) = PixelY (f LumaY y)
  {-# INLINE mapPxC #-}
  liftPx = fmap
  {-# INLINE liftPx #-}
  liftPx2 = liftA2
  {-# INLINE liftPx2 #-}
  foldlPx = foldl'
  {-# INLINE foldlPx #-}
  foldlPx2 f !z (PixelY y1) (PixelY y2) = f z y1 y2
  {-# INLINE foldlPx2 #-}


instance Functor (Pixel Y) where
  fmap f (PixelY y) = PixelY (f y)
  {-# INLINE fmap #-}


instance Applicative (Pixel Y) where
  pure = PixelY
  {-# INLINE pure #-}
  (PixelY fy) <*> (PixelY y) = PixelY (fy y)
  {-# INLINE (<*>) #-}


instance Foldable (Pixel Y) where
  foldr f !z (PixelY y) = f y z
  {-# INLINE foldr #-}


instance Monad (Pixel Y) where

  return = PixelY
  {-# INLINE return #-}

  (>>=) (PixelY y) f = f y
  {-# INLINE (>>=) #-}



instance Storable e => Storable (Pixel Y e) where

  sizeOf _ = sizeOf (undefined :: e)
  alignment _ = alignment (undefined :: e)
  peek p = do
    q <- return $ castPtr p
    y <- peek q
    return (PixelY y)
  poke p (PixelY y) = do
    q <- return $ castPtr p
    poke q y




----------
--- YA ---
----------

-- | Luma with Alpha channel.
data YA = LumaYA  -- ^ Luma
        | AlphaYA -- ^ Alpha channel
        deriving (Eq, Enum, Show, Bounded, Typeable)

data instance Pixel YA e = PixelYA !e !e deriving Eq

-- | Conversion to Luma from another color space.
class ToY cs e => ToYA cs e where

  -- | Convert a pixel to Luma pixel with Alpha.
  toPixelYA :: Pixel cs e -> Pixel YA Double
  toPixelYA = addAlpha 1 . toPixelY
  {-# INLINE toPixelYA #-}

-- | Convert an image to Luma image with Alpha.
toImageYA :: (ToYA cs e, Array arr cs e, Array arr YA Double) =>
             Image arr cs e
          -> Image arr YA Double
toImageYA = map toPixelYA
{-# INLINE toImageYA #-}

instance Show e => Show (Pixel YA e) where
  show (PixelYA y a) = "<LumaAlpha:("++show y++"|"++show a++")>"


instance Elevator e => ColorSpace YA e where
  type Components YA e = (e, e)
  promote e = PixelYA e e
  {-# INLINE promote #-}
  fromComponents (y, a) = PixelYA y a
  {-# INLINE fromComponents #-}
  toComponents (PixelYA y a) = (y, a)
  {-# INLINE toComponents #-}
  getPxC (PixelYA y _)  LumaYA = y
  getPxC (PixelYA _ a) AlphaYA = a
  {-# INLINE getPxC #-}
  setPxC (PixelYA _ a) LumaYA  y = PixelYA y a
  setPxC (PixelYA y _) AlphaYA a = PixelYA y a
  {-# INLINE setPxC #-}
  mapPxC f (PixelYA y a) = PixelYA (f LumaYA y) (f AlphaYA a)
  {-# INLINE mapPxC #-}
  liftPx = fmap
  {-# INLINE liftPx #-}
  liftPx2 = liftA2
  {-# INLINE liftPx2 #-}
  foldlPx = foldl'
  {-# INLINE foldlPx #-}
  foldlPx2 f !z (PixelYA y1 a1) (PixelYA y2 a2) = f (f z y1 y2) a1 a2
  {-# INLINE foldlPx2 #-}

  
instance Elevator e => AlphaSpace YA e where
  type Opaque YA = Y

  getAlpha (PixelYA _ a) = a
  {-# INLINE getAlpha  #-}
  addAlpha !a (PixelY y) = PixelYA y a
  {-# INLINE addAlpha #-}
  dropAlpha (PixelYA y _) = PixelY y
  {-# INLINE dropAlpha #-}

  
instance Functor (Pixel YA) where
  fmap f (PixelYA y a) = PixelYA (f y) (f a)
  {-# INLINE fmap #-}


instance Applicative (Pixel YA) where
  pure !e = PixelYA e e
  {-# INLINE pure #-}
  (PixelYA fy fa) <*> (PixelYA y a) = PixelYA (fy y) (fa a)
  {-# INLINE (<*>) #-}


instance Foldable (Pixel YA) where
  foldr f !z (PixelYA y a) = f y (f a z)
  {-# INLINE foldr #-}


instance Storable e => Storable (Pixel YA e) where

  sizeOf _ = 2 * sizeOf (undefined :: e)
  alignment _ = alignment (undefined :: e)
  peek p = do
    q <- return $ castPtr p
    y <- peekElemOff q 0
    a <- peekElemOff q 1
    return (PixelYA y a)
  poke p (PixelYA y a) = do
    q <- return $ castPtr p
    pokeElemOff q 0 y
    pokeElemOff q 1 a

