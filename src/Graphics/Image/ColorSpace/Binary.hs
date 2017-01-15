{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Image.ColorSpace.Binary
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.ColorSpace.Binary (
  Binary(..), Bit(..), on, off, isOn, isOff, fromBool, complement
  ) where

import Prelude hiding (map)
import Data.Word (Word8)
import Graphics.Image.Interface
import Data.Typeable (Typeable)
import Foreign.Ptr
import Foreign.Storable

-- | This is a Binary colorspace, pixel's of which can be created using
-- these __/constructors/__:
--
--   [@'on'@] Represents value @1@ or 'True'. It's a foreground pixel and is
--   displayed in black.
--
--   [@'off'@] Represents value @0@ or 'False'. It's a background pixel and is
--   displayed in white.
--
-- Note, that values are inverted before writing to or reading from file, since
-- grayscale images represent black as a @0@ value and white as @1@ on a
-- @[0,1]@ scale.
--
-- Binary pixels also behave as binary numbers with a size of 1-bit, for instance:
--
-- >>> on + on -- equivalent to: 1 .|. 1
-- <Binary:(1)>
-- >>> (on + on) * off -- equivalent to: (1 .|. 1) .&. 0
-- <Binary:(0)>
-- >>> (on + on) - on
-- <Binary:(0)>
--
data Binary = Binary deriving (Eq, Enum, Show, Typeable)


-- | Under the hood, Binary pixels are represented as 'Word8' that can only take
-- values of @0@ or @1@.
newtype Bit = Bit Word8 deriving (Ord, Eq, Typeable)

data instance Pixel Binary e = PixelBinary !e deriving (Ord, Eq)

instance Show (Pixel Binary Bit) where
  show (PixelBinary (Bit 0)) = "<Binary:(0)>"
  show _                     = "<Binary:(1)>"


-- | Represents value 'True' or @1@ in binary. Often also called a foreground
-- pixel of an object.
on :: Pixel Binary Bit
on = PixelBinary (Bit 1)
{-# INLINE on #-}


-- | Represents value 'False' or @0@ in binary. Often also called a background
-- pixel.
off :: Pixel Binary Bit
off = PixelBinary (Bit 0)
{-# INLINE off #-}


-- | Convert a 'Bool' to a 'PixelBin' pixel.
--
-- >>> isOn (fromBool True)
-- True
--
fromBool :: Bool -> Pixel Binary Bit
fromBool False = off
fromBool True  = on
{-# INLINE fromBool #-}


-- | Test if Pixel's value is 'on'.
isOn :: Pixel Binary Bit -> Bool
isOn (PixelBinary (Bit 0)) = False
isOn _                     = True
{-# INLINE isOn #-}


-- | Test if Pixel's value is 'off'.
isOff :: Pixel Binary Bit -> Bool
isOff = not . isOn
{-# INLINE isOff #-}


-- | Invert value of a pixel. Equivalent of 'not' for Bool's.
complement :: Pixel Binary Bit -> Pixel Binary Bit
complement = fromBool . isOff
{-# INLINE complement #-}


instance ColorSpace Binary Bit where
  type Components Binary Bit = Bit

  broadcastC = PixelBinary
  {-# INLINE broadcastC #-}
  fromComponents = PixelBinary
  {-# INLINE fromComponents #-}
  toComponents (PixelBinary b) = b
  {-# INLINE toComponents #-}
  getPxC (PixelBinary b) _ = b
  {-# INLINE getPxC #-}
  setPxC (PixelBinary _) _ b = PixelBinary b
  {-# INLINE setPxC #-}  
  mapPxC f (PixelBinary b) = PixelBinary (f Binary b)
  {-# INLINE mapPxC #-}
  mapPx f (PixelBinary b) = PixelBinary (f b)
  {-# INLINE mapPx #-}
  zipWithPx f (PixelBinary b1) (PixelBinary b2) = PixelBinary (f b1 b2)
  {-# INLINE zipWithPx #-}
  foldrPx f z (PixelBinary b) = f b z
  {-# INLINE foldrPx #-}


instance Elevator Bit where
  toWord8 (Bit 0) = 0
  toWord8 _       = maxBound
  {-# INLINE toWord8 #-}
  toWord16 (Bit 0) = 0
  toWord16 _       = maxBound
  {-# INLINE toWord16 #-}
  toWord32 (Bit 0) = 0
  toWord32 _       = maxBound
  {-# INLINE toWord32 #-}
  toWord64 (Bit 0) = 0
  toWord64 _       = maxBound
  {-# INLINE toWord64 #-}
  toFloat (Bit 0) = 0
  toFloat _       = 1
  {-# INLINE toFloat #-}
  toDouble (Bit 0) = 0
  toDouble _       = 1
  {-# INLINE toDouble #-}
  fromDouble 0 = Bit 0
  fromDouble _ = Bit 1
  {-# INLINE fromDouble #-}

  

instance Num Bit where
  (Bit 0) + (Bit 0) = Bit 0
  _       + _       = Bit 1
  {-# INLINE (+) #-}
  _ - (Bit 1) = Bit 0
  _ - _       = Bit 1
  {-# INLINE (-) #-}
  _       * (Bit 0) = Bit 0
  (Bit 0) * _       = Bit 0
  _       * _       = Bit 1
  {-# INLINE (*) #-}
  abs         = id
  {-# INLINE abs #-}
  signum      = id
  {-# INLINE signum #-}
  fromInteger 0 = Bit 0
  fromInteger _ = Bit 1
  {-# INLINE fromInteger #-}


instance Num (Pixel Binary Bit) where
  (+)         = zipWithPx (+)
  {-# INLINE (+) #-}
  (-)         = zipWithPx (-)
  {-# INLINE (-) #-}
  (*)         = zipWithPx (*)
  {-# INLINE (*) #-}
  abs         = mapPx abs
  {-# INLINE abs #-}
  signum      = mapPx signum
  {-# INLINE signum #-}
  fromInteger = broadcastC . fromInteger
  {-# INLINE fromInteger #-}


instance Storable Bit where

  sizeOf _ = sizeOf (undefined :: Word8)
  alignment _ = alignment (undefined :: Word8)
  peek p = do
    q <- return $ castPtr p
    b <- peek q
    return (Bit b)
  poke p (Bit b) = do
    q <- return $ castPtr p
    poke q b


instance Storable (Pixel Binary Bit) where

  sizeOf _ = sizeOf (undefined :: Bit)
  alignment _ = alignment (undefined :: Bit)
  peek p = do
    q <- return $ castPtr p
    b <- peek q
    return (PixelBinary b)
  poke p (PixelBinary b) = do
    q <- return $ castPtr p
    poke q b
