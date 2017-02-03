{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 800
  {-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
-- |
-- Module      : Graphics.Image.Interface.Instance
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Instances where

import qualified Data.Complex as C
import Data.Int
import Data.Word
import GHC.Float

import Graphics.Image.Interface

dropDown :: forall a b. (Integral a, Bounded a, Integral b, Bounded b) => a -> b
dropDown !e = fromIntegral $ fromIntegral e `div` ((maxBound :: a) `div`
                                                   fromIntegral (maxBound :: b)) 
{-# INLINE dropDown #-}

raiseUp :: forall a b. (Integral a, Bounded a, Integral b, Bounded b) => a -> b
raiseUp !e = fromIntegral e * ((maxBound :: b) `div` fromIntegral (maxBound :: a))
{-# INLINE raiseUp #-}


squashTo1 :: forall a b. (Fractional b, Integral a, Bounded a) => a -> b
squashTo1 !e = fromIntegral e / fromIntegral (maxBound :: a)
{-# INLINE squashTo1 #-}

stretch :: forall a b. (RealFrac a, Floating a, Integral b, Bounded b) => a -> b
stretch !e = round (fromIntegral (maxBound :: b) * clamp01 e)


-- | Clamp a value to @[0, 1]@ range.
clamp01 :: (Ord a, Floating a) => a -> a
clamp01 !x = min (max 0 x) 1
{-# INLINE clamp01 #-}


instance Elevator Word8 where

  toWord8 = id
  {-# INLINE toWord8 #-}
  toWord16 = raiseUp
  {-# INLINE toWord16 #-}
  toWord32 = raiseUp
  {-# INLINE toWord32 #-}
  toWord64 = raiseUp
  {-# INLINE toWord64 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  fromDouble = toWord8
  {-# INLINE fromDouble #-}


instance Elevator Word16 where

  toWord8 = dropDown
  {-# INLINE toWord8 #-}
  toWord16 = id
  {-# INLINE toWord16 #-}
  toWord32 = raiseUp
  {-# INLINE toWord32 #-}
  toWord64 = raiseUp
  {-# INLINE toWord64 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  fromDouble = toWord16
  {-# INLINE fromDouble #-}


instance Elevator Word32 where

  toWord8 = dropDown
  {-# INLINE toWord8 #-}
  toWord16 = dropDown
  {-# INLINE toWord16 #-}
  toWord32 = id
  {-# INLINE toWord32 #-}
  toWord64 = raiseUp
  {-# INLINE toWord64 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  fromDouble = toWord32
  {-# INLINE fromDouble #-}


instance Elevator Word64 where

  toWord8 = dropDown
  {-# INLINE toWord8 #-}
  toWord16 = dropDown
  {-# INLINE toWord16 #-}
  toWord32 = dropDown
  {-# INLINE toWord32 #-}
  toWord64 = id
  {-# INLINE toWord64 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  fromDouble = toWord64
  {-# INLINE fromDouble #-}


instance Elevator Word where

  toWord8 = dropDown
  {-# INLINE toWord8 #-}
  toWord16 = dropDown
  {-# INLINE toWord16 #-}
  toWord32 = dropDown
  {-# INLINE toWord32 #-}
  toWord64 = fromIntegral
  {-# INLINE toWord64 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  fromDouble = stretch . clamp01
  {-# INLINE fromDouble #-}


instance Elevator Int8 where

  toWord8 = fromIntegral . (max 0)
  {-# INLINE toWord8 #-}
  toWord16 = raiseUp . (max 0)
  {-# INLINE toWord16 #-}
  toWord32 = raiseUp . (max 0)
  {-# INLINE toWord32 #-}
  toWord64 = raiseUp . (max 0)
  {-# INLINE toWord64 #-}
  toFloat = squashTo1 . (max 0)
  {-# INLINE toFloat #-}
  toDouble = squashTo1 . (max 0)
  {-# INLINE toDouble #-}
  fromDouble = stretch . clamp01
  {-# INLINE fromDouble #-}


instance Elevator Int16 where

  toWord8 = dropDown . (max 0)
  {-# INLINE toWord8 #-}
  toWord16 = fromIntegral . (max 0)
  {-# INLINE toWord16 #-}
  toWord32 = raiseUp . (max 0)
  {-# INLINE toWord32 #-}
  toWord64 = raiseUp . (max 0)
  {-# INLINE toWord64 #-}
  toFloat = squashTo1 . (max 0)
  {-# INLINE toFloat #-}
  toDouble = squashTo1 . (max 0)
  {-# INLINE toDouble #-}
  fromDouble = stretch . clamp01
  {-# INLINE fromDouble #-}


instance Elevator Int32 where

  toWord8 = dropDown . (max 0)
  {-# INLINE toWord8 #-}
  toWord16 = dropDown . (max 0)
  {-# INLINE toWord16 #-}
  toWord32 = fromIntegral . (max 0)
  {-# INLINE toWord32 #-}
  toWord64 = raiseUp . (max 0)
  {-# INLINE toWord64 #-}
  toFloat = squashTo1 . (max 0)
  {-# INLINE toFloat #-}
  toDouble = squashTo1 . (max 0)
  {-# INLINE toDouble #-}
  fromDouble = stretch . clamp01
  {-# INLINE fromDouble #-}


instance Elevator Int64 where

  toWord8 = dropDown . (max 0)
  {-# INLINE toWord8 #-}
  toWord16 = dropDown . (max 0)
  {-# INLINE toWord16 #-}
  toWord32 = dropDown . (max 0)
  {-# INLINE toWord32 #-}
  toWord64 = fromIntegral . (max 0)
  {-# INLINE toWord64 #-}
  toFloat = squashTo1 . (max 0)
  {-# INLINE toFloat #-}
  toDouble = squashTo1 . (max 0)
  {-# INLINE toDouble #-}
  fromDouble = stretch . clamp01
  {-# INLINE fromDouble #-}


instance Elevator Int where

  toWord8 = dropDown . (max 0)
  {-# INLINE toWord8 #-}
  toWord16 = dropDown . (max 0)
  {-# INLINE toWord16 #-}
  toWord32 = dropDown . (max 0)
  {-# INLINE toWord32 #-}
  toWord64 = fromIntegral . (max 0)
  {-# INLINE toWord64 #-}
  toFloat = squashTo1 . (max 0)
  {-# INLINE toFloat #-}
  toDouble = squashTo1 . (max 0)
  {-# INLINE toDouble #-}
  fromDouble = stretch . clamp01
  {-# INLINE fromDouble #-}


instance Elevator Float where
  toWord8 = stretch . clamp01
  {-# INLINE toWord8 #-}
  toWord16 = stretch . clamp01
  {-# INLINE toWord16 #-}
  toWord32 = stretch . clamp01
  {-# INLINE toWord32 #-}
  toWord64 = stretch . clamp01
  {-# INLINE toWord64 #-}
  toFloat = id
  {-# INLINE toFloat #-}
  toDouble = float2Double
  {-# INLINE toDouble #-}
  fromDouble = toFloat
  {-# INLINE fromDouble #-}

instance Elevator Double where
  toWord8 = stretch . clamp01
  {-# INLINE toWord8 #-}
  toWord16 = stretch . clamp01
  {-# INLINE toWord16 #-}
  toWord32 = stretch . clamp01
  {-# INLINE toWord32 #-}
  toWord64 = stretch . clamp01
  {-# INLINE toWord64 #-}
  toFloat = double2Float
  {-# INLINE toFloat #-}
  toDouble = id
  {-# INLINE toDouble #-}
  fromDouble = id
  {-# INLINE fromDouble #-}


instance (Num e, Elevator e, RealFloat e) => Elevator (C.Complex e) where
  toWord8 = toWord8 . C.realPart
  {-# INLINE toWord8 #-}
  toWord16 = toWord16 . C.realPart
  {-# INLINE toWord16 #-}
  toWord32 = toWord32 . C.realPart
  {-# INLINE toWord32 #-}
  toWord64 = toWord64 . C.realPart
  {-# INLINE toWord64 #-}
  toFloat = toFloat . C.realPart
  {-# INLINE toFloat #-}
  toDouble = toDouble . C.realPart
  {-# INLINE toDouble #-}
  fromDouble = (C.:+ 0) . fromDouble
  {-# INLINE fromDouble #-}
