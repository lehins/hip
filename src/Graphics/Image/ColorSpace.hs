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
-- Module      : Graphics.Image.ColorSpace
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.ColorSpace (
  -- * ColorSpace
  ColorSpace, Pixel(..), AlphaSpace(..), Elevator(..),
  -- * Operations on Pixels
  eqTolPx,
  -- * Luma
  module Graphics.Image.ColorSpace.Y,
  -- * RGB
  module Graphics.Image.ColorSpace.RGB,
  -- * HSI
  module Graphics.Image.ColorSpace.HSI,
  -- * CMYK
  module Graphics.Image.ColorSpace.CMYK,
  -- * YCbCr
  module Graphics.Image.ColorSpace.YCbCr,
  -- * Gray level
  module Graphics.Image.ColorSpace.X,
  -- * Binary
  Binary, Bit, on, off, isOn, isOff, fromBool, complement,
  toPixelBinary, fromPixelBinary, toImageBinary, fromImageBinary,
  -- * Complex
  module Graphics.Image.ColorSpace.Complex,
  -- * Re-exports
  Word8, Word16, Word32, Word64
  ) where

import qualified Data.Complex as C
import Data.Word
import Data.Int
import GHC.Float
import Graphics.Image.Interface hiding (map)
import Graphics.Image.ColorSpace.Binary
import Graphics.Image.ColorSpace.RGB
import Graphics.Image.ColorSpace.HSI
import Graphics.Image.ColorSpace.CMYK
import Graphics.Image.ColorSpace.X
import Graphics.Image.ColorSpace.Y
import Graphics.Image.ColorSpace.YCbCr
import Graphics.Image.ColorSpace.Complex
import qualified Graphics.Image.Interface as I (map)


-- Binary:

-- | Convert any pixel to binary pixel.
toPixelBinary :: (Eq (Pixel cs e), Num (Pixel cs e))
                 => Pixel cs e -> Pixel Binary Bit
toPixelBinary px = if px == 0 then on else off
{-# INLINE toPixelBinary #-}

-- | Convert a Binary pixel to Luma pixel
fromPixelBinary :: Pixel Binary Bit -> Pixel Y Word8
fromPixelBinary b = PixelY $ if isOn b then minBound else maxBound
{-# INLINE fromPixelBinary #-}


-- | Convert any image to binary image.
toImageBinary :: (Array arr cs e, Array arr Binary Bit, Eq (Pixel cs e)) =>
                 Image arr cs e
              -> Image arr Binary Bit
toImageBinary = I.map toPixelBinary
{-# INLINE toImageBinary #-}


-- | Convert a Binary image to Luma image
fromImageBinary :: (Array arr Binary Bit, Array arr Y Word8) =>
                   Image arr Binary Bit
                -> Image arr Y Word8
fromImageBinary = I.map fromPixelBinary
{-# INLINE fromImageBinary #-}


-- | Check weather two Pixels are equal within a tolerance. Useful for comparing
-- pixels with `Float` or `Double` precision.
eqTolPx :: (ColorSpace cs e, Num e, Ord e) =>
           e -> Pixel cs e -> Pixel cs e -> Bool
eqTolPx !tol = foldlPx2 comp True 
  where comp !acc !e1 !e2 = acc && max e1 e2 - min e1 e2 <= tol
        {-# INLINE comp #-}
{-# INLINE eqTolPx #-}


instance ToY X where
  toPixelY (PixelX y) = PixelY y
  {-# INLINE toPixelY #-}

-- | Computes Luma: @ Y' = 0.299 * R' + 0.587 * G' + 0.114 * B' @
instance ToY RGB where
  toPixelY (PixelRGB r g b) = PixelY (0.299*r + 0.587*g + 0.114*b)
  {-# INLINE toPixelY #-}

instance ToYA RGBA where

instance ToY HSI where
  toPixelY = toPixelY . toPixelRGB
  {-# INLINE toPixelY #-}

instance ToYA HSIA where

instance ToY CMYK where
  toPixelY = toPixelY . toPixelRGB
  {-# INLINE toPixelY #-}

  
instance ToY YCbCr where
  toPixelY (PixelYCbCr y _ _) = PixelY y
  {-# INLINE toPixelY #-}
  
instance ToYA YCbCrA where
  
instance ToRGB Y where
  toPixelRGB (PixelY g) = broadcastC g
  {-# INLINE toPixelRGB #-}

instance ToRGBA YA where

instance ToRGB HSI where
  toPixelRGB (PixelHSI h' s i) = getRGB (h'*2*pi) where
    !is = i*s
    !second = i - is
    getFirst !a !b = i + is*cos a/cos b
    {-# INLINE getFirst #-}
    getThird !v1 !v2 = i + 2*is + v1 - v2
    {-# INLINE getThird #-}
    getRGB h
      | h < 0      = error ("HSI pixel is not properly scaled, Hue: "++show h')
      | h < 2*pi/3 = let !r = getFirst h (pi/3 - h)
                         !b = second
                         !g = getThird b r
                     in PixelRGB r g b
      | h < 4*pi/3 = let !g = getFirst (h - 2*pi/3) (h + pi)
                         !r = second
                         !b = getThird r g
                     in PixelRGB r g b
      | h < 2*pi   = let !b = getFirst (h - 4*pi/3) (2*pi - pi/3 - h)
                         !g = second
                         !r = getThird g b
                     in PixelRGB r g b
      | otherwise  = error ("HSI pixel is not properly scaled, Hue: "++show h')
    {-# INLINE getRGB #-}
  {-# INLINE toPixelRGB #-}

instance ToRGBA HSIA where


instance ToRGB YCbCr where

  toPixelRGB (PixelYCbCr y cb cr) = PixelRGB r g b where
    !r = y                      +   1.402*(cr - 0.5)
    !g = y - 0.34414*(cb - 0.5) - 0.71414*(cr - 0.5)
    !b = y +   1.772*(cb - 0.5)
  {-# INLINE toPixelRGB #-}

instance ToRGBA YCbCrA where

instance ToRGB CMYK where

  toPixelRGB (PixelCMYK c m y k) = PixelRGB r g b where
    !r = (1-c)*(1-k)
    !g = (1-m)*(1-k)
    !b = (1-y)*(1-k)
  {-# INLINE toPixelRGB #-}
  
instance ToRGBA CMYKA where

  
instance ToHSI Y where
  toPixelHSI (PixelY g) = PixelHSI 0 0 g
  {-# INLINE toPixelHSI #-}

instance ToHSIA YA where
  
instance ToHSI RGB where
  toPixelHSI (PixelRGB r g b) = PixelHSI h s i where
    !h' = atan2 y x
    !h = (if h' < 0 then h' + 2*pi else h') / (2*pi)
    !s = if i == 0 then 0 else 1 - minimum [r, g, b] / i
    !i = (r + g + b) / 3
    !x = (2*r - g - b) / 2.449489742783178
    !y = (g - b) / 1.4142135623730951
  {-# INLINE toPixelHSI #-}
    
instance ToHSIA RGBA where


instance ToYCbCr RGB where

  toPixelYCbCr (PixelRGB r g b) = PixelYCbCr y cb cr where
    !y  =          0.299*r +    0.587*g +    0.114*b
    !cb = 0.5 - 0.168736*r - 0.331264*g +      0.5*b
    !cr = 0.5 +      0.5*r - 0.418688*g - 0.081312*b
  {-# INLINE toPixelYCbCr #-}

instance ToYCbCrA RGBA where
  

instance ToCMYK RGB where

  toPixelCMYK (PixelRGB r g b) = PixelCMYK c m y k where
    !c = (1 - r - k)/(1 - k)
    !m = (1 - g - k)/(1 - k)
    !y = (1 - b - k)/(1 - k)
    !k = 1 - max r (max g b)

instance ToCMYKA RGBA where


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
