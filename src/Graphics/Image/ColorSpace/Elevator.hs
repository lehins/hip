{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 800
  {-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
-- |
-- Module      : Graphics.Image.ColorSpace.Elevator
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.ColorSpace.Elevator (
  Elevator(..)
  , clamp01
  ) where

import qualified Data.Complex        as C
import           Data.Int
import           Data.Typeable
import           Data.Vector.Unboxed (Unbox)
import           Data.Word
import           GHC.Float


-- | A class with a set of convenient functions that allow for changing precision of
-- channels within pixels, while scaling the values to keep them in an appropriate range.
--
-- >>> let rgb = PixelRGB 0.0 0.5 1.0 :: Pixel RGB Double
-- >>> toWord8 <$> rgb
-- <RGB:(0|128|255)>
-- >>> toWord16 <$> rgb
-- <RGB:(0|32768|65535)>
--
class (Eq e, Num e, Typeable e, Unbox e) => Elevator e where

  -- | Values are scaled to @[0, 255]@ range.
  toWord8 :: e -> Word8

  -- | Values are scaled to @[0, 65535]@ range.
  toWord16 :: e -> Word16

  -- | Values are scaled to @[0, 4294967295]@ range.
  toWord32 :: e -> Word32

  -- | Values are scaled to @[0, 18446744073709551615]@ range.
  toWord64 :: e -> Word64

  -- | Values are scaled to @[0.0, 1.0]@ range.
  toFloat :: e -> Float

  -- | Values are scaled to @[0.0, 1.0]@ range.
  toDouble :: e -> Double

  -- | Values are scaled from @[0.0, 1.0]@ range.
  fromDouble :: Double -> e


-- | Lower the precision
dropDown :: forall a b. (Integral a, Bounded a, Integral b, Bounded b) => a -> b
dropDown !e = fromIntegral $ fromIntegral e `div` ((maxBound :: a) `div`
                                                   fromIntegral (maxBound :: b))
{-# INLINE dropDown #-}

-- | Increase the precision
raiseUp :: forall a b. (Integral a, Bounded a, Integral b, Bounded b) => a -> b
raiseUp !e = fromIntegral e * ((maxBound :: b) `div` fromIntegral (maxBound :: a))
{-# INLINE raiseUp #-}

-- | Convert to fractional with value less than or equal to 1.
squashTo1 :: forall a b. (Fractional b, Integral a, Bounded a) => a -> b
squashTo1 !e = fromIntegral e / fromIntegral (maxBound :: a)
{-# INLINE squashTo1 #-}

-- | Convert to integral streaching it's value up to a maximum value.
stretch :: forall a b. (RealFrac a, Floating a, Integral b, Bounded b) => a -> b
stretch !e = round (fromIntegral (maxBound :: b) * clamp01 e)
{-# INLINE stretch #-}


-- | Clamp a value to @[0, 1]@ range.
clamp01 :: (Ord a, Floating a) => a -> a
clamp01 !x = min (max 0 x) 1
{-# INLINE clamp01 #-}


-- | Values between @[0, 255]]@
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


-- | Values between @[0, 65535]]@
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


-- | Values between @[0, 4294967295]@
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


-- | Values between @[0, 18446744073709551615]@
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

-- | Values between @[0, 18446744073709551615]@ on 64bit
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

-- | Values between @[0, 127]@
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


-- | Values between @[0, 32767]@
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


-- | Values between @[0, 2147483647]@
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


-- | Values between @[0, 9223372036854775807]@
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


-- | Values between @[0, 9223372036854775807]@ on 64bit
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


-- | Values between @[0.0, 1.0]@
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


-- | Values between @[0.0, 1.0]@
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


-- | Discards imaginary part and changes precision of real part.
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
