{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
  Binary(..), Bit(..), Pixel(..), on, off, isOn, isOff, fromBool, zero, one,
  toPixelBinary,
  module Data.Bits
  ) where

import Prelude hiding (map)
import Control.Monad
import Data.Bits
import Data.Word (Word8)
import Data.Typeable (Typeable)
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Vector.Generic            as V
import qualified Data.Vector.Generic.Mutable    as M
import qualified Data.Vector.Unboxed            as U
import Graphics.Image.Interface


-- | Binary Color Space, also known as bi-tonal.
data Binary = Binary deriving (Eq, Enum, Bounded, Show, Typeable)


-- | Under the hood, Binary pixels are represented as 'Word8', but can only take
-- values of @0@ or @1@.
newtype Bit = Bit Word8 deriving (Ord, Eq, Typeable)

newtype instance Pixel Binary Bit = PixelBinary Bit deriving (Ord, Eq)


-- | Convert to a `Binary` pixel.
toPixelBinary :: ColorSpace cs e => Pixel cs e -> Pixel Binary Bit
toPixelBinary px = if px == 0 then on else off


instance Show (Pixel Binary Bit) where
  show (PixelBinary (Bit 0)) = "<Binary:(0)>"
  show _                     = "<Binary:(1)>"


instance Bits Bit where
  (.&.) = (*)
  {-# INLINE (.&.) #-}

  (.|.) = (+)
  {-# INLINE (.|.) #-}

  (Bit 0) `xor` (Bit 0) = Bit 0
  (Bit 1) `xor` (Bit 1) = Bit 0
  _       `xor` _       = Bit 1
  {-# INLINE xor #-}

  complement (Bit 0) = Bit 1
  complement       _ = Bit 0

  shift !b 0 = b
  shift  _ _ = Bit 0
  
  rotate !b _ = b

  zeroBits = Bit 0

  bit 0 = Bit 1
  bit _ = Bit 0

  testBit (Bit 1) 0 = True
  testBit _       _ = False

  bitSizeMaybe _ = Just 1

  bitSize _ = 1

  isSigned _ = False

  popCount (Bit 0) = 0
  popCount _       = 1



instance Bits (Pixel Binary Bit) where
  (.&.) = liftPx2 (.&.)
  {-# INLINE (.&.) #-}

  (.|.) = liftPx2 (.|.)
  {-# INLINE (.|.) #-}

  xor = liftPx2 xor
  {-# INLINE xor #-}

  complement = liftPx complement

  shift !b !n = liftPx (`shift` n) b
  
  rotate !b !n = liftPx (`rotate` n) b

  zeroBits = promote zeroBits

  bit = promote . bit

  testBit (PixelBinary (Bit 1)) 0 = True
  testBit _                     _ = False

  bitSizeMaybe _ = Just 1

  bitSize _ = 1

  isSigned _ = False

  popCount (PixelBinary (Bit 0)) = 0
  popCount _                     = 1


zero :: Bit
zero = Bit 0
{-# INLINE zero #-}

one :: Bit
one = Bit 1
{-# INLINE one #-}

-- | Represents value 'True' or @1@ in binary. Often also called a foreground
-- pixel of an object.
on :: Pixel Binary Bit
on = PixelBinary one
{-# INLINE on #-}


-- | Represents value 'False' or @0@ in binary. Often also called a background
-- pixel.
off :: Pixel Binary Bit
off = PixelBinary zero
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



instance ColorSpace Binary Bit where
  type Components Binary Bit = Bit

  promote = PixelBinary
  {-# INLINE promote #-}
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
  liftPx f (PixelBinary b) = PixelBinary (f b)
  {-# INLINE liftPx #-}
  liftPx2 f (PixelBinary b1) (PixelBinary b2) = PixelBinary (f b1 b2)
  {-# INLINE liftPx2 #-}
  foldrPx f z (PixelBinary b) = f b z
  {-# INLINE foldrPx #-}
  foldlPx2 f !z (PixelBinary b1) (PixelBinary b2) = f z b1 b2
  {-# INLINE foldlPx2 #-}

-- | Values: @0@ and @1@
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
  -- 0 - 0 = 0
  -- 0 - 1 = 0
  -- 1 - 0 = 1
  -- 1 - 1 = 0
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



-- | Unboxing of a `Bit`.
instance U.Unbox Bit

newtype instance U.MVector s Bit = MV_Bit (U.MVector s Word8)

instance M.MVector U.MVector Bit where
  basicLength (MV_Bit mvec) = M.basicLength mvec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (MV_Bit mvec) = MV_Bit (M.basicUnsafeSlice idx len mvec)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Bit mvec) (MV_Bit mvec') = M.basicOverlaps mvec mvec'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew len = MV_Bit `liftM` M.basicUnsafeNew len
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate len (Bit w) = MV_Bit `liftM` M.basicUnsafeReplicate len w
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Bit mvec) idx = Bit `liftM` M.basicUnsafeRead mvec idx
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Bit mvec) idx (Bit w) = M.basicUnsafeWrite mvec idx w
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Bit mvec) = M.basicClear mvec
  {-# INLINE basicClear #-}
  basicSet (MV_Bit mvec) (Bit w) =  M.basicSet mvec w
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Bit mvec) (MV_Bit mvec') = M.basicUnsafeCopy mvec mvec'
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Bit mvec) (MV_Bit mvec') = M.basicUnsafeMove mvec mvec'
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Bit mvec) len = MV_Bit `liftM` M.basicUnsafeGrow mvec len
  {-# INLINE basicUnsafeGrow #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Bit mvec) = M.basicInitialize mvec
  {-# INLINE basicInitialize #-}
#endif


newtype instance U.Vector Bit = V_Bit (U.Vector Word8)

instance V.Vector U.Vector Bit where
  basicUnsafeFreeze (MV_Bit mvec) = V_Bit `liftM` V.basicUnsafeFreeze mvec
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Bit vec) = MV_Bit `liftM` V.basicUnsafeThaw vec
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Bit vec) = V.basicLength vec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (V_Bit vec) = V_Bit (V.basicUnsafeSlice idx len vec)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Bit vec) idx = Bit `liftM` V.basicUnsafeIndexM vec idx
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Bit mvec) (V_Bit vec) = V.basicUnsafeCopy mvec vec
  {-# INLINE basicUnsafeCopy #-}
  elemseq (V_Bit vec) (Bit w) = V.elemseq vec w
  {-# INLINE elemseq #-}
