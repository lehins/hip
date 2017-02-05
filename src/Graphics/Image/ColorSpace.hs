{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
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
  -- * Pixels
  -- ** Family of Pixels
  -- | Pixel is a type family for all available color spaces. Below will be
  -- listed all class instances pixels are installed in, as well as all pixel
  -- constructors, eg:
  --
  -- >>> :t (PixelY 0) -- Black pixel in Luma
  -- (PixelY 0) :: Num e => Pixel Y e
  -- >>> PixelRGB 255 0 0 :: Pixel RGB Word8 -- Red pixel in RGB
  -- <RGB:(255|0|0)>
  -- >>> PixelRGB 1 0 0 :: Pixel RGB Double -- Same red pixel in RGB with Double precision.
  -- <RGB:(1.0|0.0|0.0)>
  -- >>> (PixelRGB 255 0 0 :: Pixel RGB Word8) == (toWord8 <$> (PixelRGB 1 0 0 :: Pixel RGB Double))
  -- True
  --
  Pixel(..),
  -- ** Luma (gray scale)
  -- | Conversion to Luma from other color spaces.
  toPixelY, toImageY,
  toPixelYA, toImageYA,
  -- ** RGB
  -- | Conversion to RGB from other color spaces.
  toPixelRGB, toImageRGB,
  toPixelRGBA, toImageRGBA,
  -- ** HSI
  -- | Conversion to HSI from other color spaces.
  toPixelHSI, toImageHSI,
  toPixelHSIA, toImageHSIA,
  -- ** CMYK
  -- | Conversion to CMYK from other color spaces.
  toPixelCMYK, toImageCMYK,
  toPixelCMYKA, toImageCMYKA,
  -- ** YCbCr
  -- | Conversion to YCbCr from other color spaces.
  toPixelYCbCr, toImageYCbCr,
  toPixelYCbCrA, toImageYCbCrA,
  -- ** Binary
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
  on, off, isOn, isOff, one, zero, fromBool, complement,
  toPixelBinary, fromPixelBinary, toImageBinary, fromImageBinary,
  -- ** Complex
  module Graphics.Image.ColorSpace.Complex,
  -- ** X
  toPixelsX, fromPixelsX,
  toImagesX, fromImagesX,
  -- * ColorSpace
  -- ** Operations on Pixels
  eqTolPx,
  -- ** Luma
  Y(..), YA(..),
  ToY, ToYA,
  -- ** RGB
  RGB(..), RGBA(..),
  ToRGB, ToRGBA,
  -- ** HSI
  HSI(..), HSIA(..),
  ToHSI, ToHSIA,
  -- ** CMYK
  CMYK(..), CMYKA(..),
  ToCMYK, ToCMYKA,
  -- ** YCbCr
  YCbCr(..), YCbCrA(..),
  ToYCbCr, ToYCbCrA,
  -- ** Binary
  Binary(..), Bit, 
  -- ** X
  X(..),
  -- * Useful re-exports
  Word8, Word16, Word32, Word64
  ) where

import Data.Word
import Graphics.Image.Interface as I
import Graphics.Image.ColorSpace.Binary
import Graphics.Image.ColorSpace.RGB
import Graphics.Image.ColorSpace.HSI
import Graphics.Image.ColorSpace.CMYK
import Graphics.Image.ColorSpace.X
import Graphics.Image.ColorSpace.Y
import Graphics.Image.ColorSpace.YCbCr
import Graphics.Image.ColorSpace.Complex


-- Binary:

-- | Convert to a `Binary` image.
toImageBinary :: (Array arr cs e, Array arr Binary Bit) =>
                 Image arr cs e
              -> Image arr Binary Bit
toImageBinary = I.map toPixelBinary
{-# INLINE toImageBinary #-}

-- | Convert a Binary pixel to Luma pixel
fromPixelBinary :: Pixel Binary Bit -> Pixel Y Word8
fromPixelBinary b = PixelY $ if isOn b then minBound else maxBound
{-# INLINE fromPixelBinary #-}


-- | Convert a Binary image to Luma image
fromImageBinary :: (Array arr Binary Bit, Array arr Y Word8) =>
                   Image arr Binary Bit
                -> Image arr Y Word8
fromImageBinary = I.map fromPixelBinary
{-# INLINE fromImageBinary #-}


-- | Check weather two Pixels are equal within a tolerance. Useful for comparing
-- pixels with `Float` or `Double` precision.
eqTolPx :: (ColorSpace cs e, Ord e) =>
           e -> Pixel cs e -> Pixel cs e -> Bool
eqTolPx !tol = foldlPx2 comp True 
  where comp !acc !e1 !e2 = acc && max e1 e2 - min e1 e2 <= tol
        {-# INLINE comp #-}
{-# INLINE eqTolPx #-}

-- ToY

-- | Conversion to Luma color space.
class ColorSpace cs e => ToY cs e where

  -- | Convert a pixel to Luma pixel.
  toPixelY :: Pixel cs e -> Pixel Y Double

-- | Convert an image to Luma image.
toImageY :: (ToY cs e, Array arr cs e, Array arr Y Double) =>
            Image arr cs e
         -> Image arr Y Double
toImageY = I.map toPixelY
{-# INLINE toImageY #-}


instance Elevator e => ToY X e where
  toPixelY (PixelX y) = PixelY $ toDouble y
  {-# INLINE toPixelY #-}

instance ToY Binary Bit where
  toPixelY (PixelBinary b) = PixelY $ toDouble b
  {-# INLINE toPixelY #-}

instance Elevator e => ToY Y e where
  toPixelY (PixelY y) = PixelY $ toDouble y
  {-# INLINE toPixelY #-}

instance Elevator e => ToY YA e where
  toPixelY (PixelYA y _) = PixelY $ toDouble y
  {-# INLINE toPixelY #-}

-- | Computes Luma: @ Y' = 0.299 * R' + 0.587 * G' + 0.114 * B' @
instance Elevator e => ToY RGB e where
  toPixelY (fmap toDouble -> (PixelRGB r g b)) = PixelY (0.299*r + 0.587*g + 0.114*b)
  {-# INLINE toPixelY #-}

instance Elevator e => ToY RGBA e where
  toPixelY = toPixelY . dropAlpha
  {-# INLINE toPixelY #-}

instance Elevator e => ToY HSI e where
  toPixelY = toPixelY . toPixelRGB . fmap toDouble
  {-# INLINE toPixelY #-}

instance Elevator e => ToY HSIA e where
  toPixelY = toPixelY . dropAlpha
  {-# INLINE toPixelY #-}

instance Elevator e => ToY CMYK e where
  toPixelY = toPixelY . toPixelRGB . fmap toDouble
  {-# INLINE toPixelY #-}

instance Elevator e => ToY CMYKA e where
  toPixelY = toPixelY . toPixelRGB . fmap toDouble . dropAlpha
  {-# INLINE toPixelY #-}

instance Elevator e => ToY YCbCr e where
  toPixelY (PixelYCbCr y _ _) = PixelY $ toDouble y
  {-# INLINE toPixelY #-}

instance Elevator e => ToY YCbCrA e where
  toPixelY (PixelYCbCrA y _ _ _) = PixelY $ toDouble y
  {-# INLINE toPixelY #-}


-- ToYA

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
toImageYA = I.map toPixelYA
{-# INLINE toImageYA #-}

                                
instance ToY Y e => ToYA Y e

instance ToYA Binary Bit where
  toPixelYA(PixelBinary b) = PixelYA (toDouble b) 1
  {-# INLINE toPixelYA #-}

instance Elevator e => ToYA YA e where
  toPixelYA = fmap toDouble
  {-# INLINE toPixelYA #-}

instance ToY RGB e => ToYA RGB e

instance Elevator e => ToYA RGBA e where
  toPixelYA !px = addAlpha (toDouble $ getAlpha px) (toPixelY (dropAlpha px))
  {-# INLINE toPixelYA #-}

instance ToY HSI e => ToYA HSI e

instance Elevator e => ToYA HSIA e where
  toPixelYA !px = addAlpha (toDouble $ getAlpha px) (toPixelY (dropAlpha px))
  {-# INLINE toPixelYA #-}

instance ToY CMYK e => ToYA CMYK e

instance Elevator e => ToYA CMYKA e where
  toPixelYA !px = addAlpha (toDouble $ getAlpha px) (toPixelY (dropAlpha px))
  {-# INLINE toPixelYA #-}

instance ToY YCbCr e => ToYA YCbCr e

instance Elevator e => ToYA YCbCrA e where
  toPixelYA !px = addAlpha (toDouble $ getAlpha px) (toPixelY (dropAlpha px))
  {-# INLINE toPixelYA #-}

-- ToRGB


-- | Conversion to `RGB` color space.
class ColorSpace cs e => ToRGB cs e where

  -- | Convert to an `RGB` pixel.
  toPixelRGB :: Pixel cs e -> Pixel RGB Double

-- | Convert to an `RGB` image.
toImageRGB :: (ToRGB cs e, Array arr cs e, Array arr RGB Double) =>
              Image arr cs e
           -> Image arr RGB Double
toImageRGB = I.map toPixelRGB
{-# INLINE toImageRGB #-}


instance ToRGB Binary Bit where
  toPixelRGB (PixelBinary b) = pure $ toDouble b
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB Y e where
  toPixelRGB (PixelY g) = promote $ toDouble g
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB YA e where
  toPixelRGB = toPixelRGB . dropAlpha
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB RGB e where
  toPixelRGB = fmap toDouble
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB RGBA e where
  toPixelRGB = fmap toDouble . dropAlpha
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB HSI e where
  toPixelRGB (fmap toDouble -> PixelHSI h' s i) = getRGB (h'*2*pi) where
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

instance Elevator e => ToRGB HSIA e where
  toPixelRGB = toPixelRGB . dropAlpha
  {-# INLINE toPixelRGB #-}


instance Elevator e => ToRGB YCbCr e where
  toPixelRGB (fmap toDouble -> PixelYCbCr y cb cr) = PixelRGB r g b where
    !r = y                      +   1.402*(cr - 0.5)
    !g = y - 0.34414*(cb - 0.5) - 0.71414*(cr - 0.5)
    !b = y +   1.772*(cb - 0.5)
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB YCbCrA e where
  toPixelRGB = toPixelRGB . dropAlpha
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB CMYK e where
  toPixelRGB (fmap toDouble -> PixelCMYK c m y k) = PixelRGB r g b where
    !r = (1-c)*(1-k)
    !g = (1-m)*(1-k)
    !b = (1-y)*(1-k)
  {-# INLINE toPixelRGB #-}

instance Elevator e => ToRGB CMYKA e where
  toPixelRGB = toPixelRGB . dropAlpha
  {-# INLINE toPixelRGB #-}


-- ToRGBA

-- | Conversion to `RGBA` from another color space with Alpha channel.
class ToRGB cs e => ToRGBA cs e where

  -- | Convert to an `RGBA` pixel.
  toPixelRGBA :: Pixel cs e -> Pixel RGBA Double
  toPixelRGBA = addAlpha 1 . toPixelRGB
  {-# INLINE toPixelRGBA #-}

-- | Convert to an `RGBA` image.
toImageRGBA :: (ToRGBA cs e, Array arr cs e, Array arr RGBA Double) =>
              Image arr cs e
           -> Image arr RGBA Double
toImageRGBA = I.map toPixelRGBA
{-# INLINE toImageRGBA #-}


instance ToRGBA Binary Bit

instance ToRGB Y e => ToRGBA Y e

instance Elevator e => ToRGBA YA e where
  toPixelRGBA !px = addAlpha (toDouble $ getAlpha px) (toPixelRGB (dropAlpha px))
  {-# INLINE toPixelRGBA #-}

instance ToRGB RGB e => ToRGBA RGB e

instance Elevator e => ToRGBA RGBA e where
  toPixelRGBA = fmap toDouble
  {-# INLINE toPixelRGBA #-}

instance ToRGB HSI e => ToRGBA HSI e

instance Elevator e => ToRGBA HSIA e where
  toPixelRGBA !px = addAlpha (toDouble $ getAlpha px) (toPixelRGB (dropAlpha px))
  {-# INLINE toPixelRGBA #-}

instance ToRGB CMYK e => ToRGBA CMYK e

instance Elevator e => ToRGBA CMYKA e where
  toPixelRGBA !px = addAlpha (toDouble $ getAlpha px) (toPixelRGB (dropAlpha px))
  {-# INLINE toPixelRGBA #-}

instance ToRGB YCbCr e => ToRGBA YCbCr e

instance Elevator e => ToRGBA YCbCrA e where
  toPixelRGBA !px = addAlpha (toDouble $ getAlpha px) (toPixelRGB (dropAlpha px))
  {-# INLINE toPixelRGBA #-}


-- ToHSI

-- | Conversion to `HSI` color space.
class ColorSpace cs e => ToHSI cs e where

  -- | Convert to an `HSI` pixel.
  toPixelHSI :: Pixel cs e -> Pixel HSI Double

-- | Convert to an `HSI` image.
toImageHSI :: (ToHSI cs e, Array arr cs e, Array arr HSI Double) =>
              Image arr cs e
           -> Image arr HSI Double
toImageHSI = I.map toPixelHSI
{-# INLINE toImageHSI #-}

  
instance Elevator e => ToHSI Y e where
  toPixelHSI (PixelY y) = PixelHSI 0 0 $ toDouble y
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI YA e where
  toPixelHSI = toPixelHSI . dropAlpha
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI RGB e where
  toPixelHSI (fmap toDouble -> PixelRGB r g b) = PixelHSI h s i where
    !h' = atan2 y x
    !h = (if h' < 0 then h' + 2*pi else h') / (2*pi)
    !s = if i == 0 then 0 else 1 - minimum [r, g, b] / i
    !i = (r + g + b) / 3
    !x = (2*r - g - b) / 2.449489742783178
    !y = (g - b) / 1.4142135623730951
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI RGBA e where
  toPixelHSI = toPixelHSI . dropAlpha
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI HSI e where
  toPixelHSI = fmap toDouble
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI HSIA e where
  toPixelHSI = toPixelHSI . dropAlpha
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI YCbCr e where
  toPixelHSI = toPixelHSI . toPixelRGB
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI YCbCrA e where
  toPixelHSI = toPixelHSI . dropAlpha
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI CMYK e where
  toPixelHSI = toPixelHSI . toPixelRGB
  {-# INLINE toPixelHSI #-}

instance Elevator e => ToHSI CMYKA e where
  toPixelHSI = toPixelHSI . dropAlpha
  {-# INLINE toPixelHSI #-}


-- ToHSIA

-- | Conversion to `HSIA` from another color space with Alpha channel.
class ToHSI cs e => ToHSIA cs e where

  -- | Convert to an `HSIA` pixel.
  toPixelHSIA :: Pixel cs e -> Pixel HSIA Double
  toPixelHSIA = addAlpha 1 . toPixelHSI
  {-# INLINE toPixelHSIA #-}

-- | Convert to an `HSIA` image.
toImageHSIA :: (ToHSIA cs e, Array arr cs e, Array arr HSIA Double) =>
               Image arr cs e
            -> Image arr HSIA Double
toImageHSIA = I.map toPixelHSIA
{-# INLINE toImageHSIA #-}


instance ToHSI Y e => ToHSIA Y e

instance Elevator e => ToHSIA YA e where
  toPixelHSIA !px = addAlpha (toDouble $ getAlpha px) (toPixelHSI (dropAlpha px))
  {-# INLINE toPixelHSIA #-}

instance ToHSI RGB e => ToHSIA RGB e

instance Elevator e => ToHSIA RGBA e where
  toPixelHSIA !px = addAlpha (toDouble $ getAlpha px) (toPixelHSI (dropAlpha px))
  {-# INLINE toPixelHSIA #-}

instance ToHSI HSI e => ToHSIA HSI e

instance Elevator e => ToHSIA HSIA e where
  toPixelHSIA = fmap toDouble
  {-# INLINE toPixelHSIA #-}

instance ToHSI CMYK e => ToHSIA CMYK e

instance Elevator e => ToHSIA CMYKA e where
  toPixelHSIA !px = addAlpha (toDouble $ getAlpha px) (toPixelHSI (dropAlpha px))
  {-# INLINE toPixelHSIA #-}

instance ToHSI YCbCr e => ToHSIA YCbCr e

instance Elevator e => ToHSIA YCbCrA e where
  toPixelHSIA !px = addAlpha (toDouble $ getAlpha px) (toPixelHSI (dropAlpha px))
  {-# INLINE toPixelHSIA #-}



-- ToCMYK

-- | Conversion to `CMYK` color space.
class ColorSpace cs e => ToCMYK cs e where

  -- | Convert to a `CMYK` pixel.
  toPixelCMYK :: Pixel cs e -> Pixel CMYK Double

-- | Convert to a `CMYK` image.
toImageCMYK :: (ToCMYK cs e, Array arr cs e, Array arr CMYK Double) =>
               Image arr cs e
            -> Image arr CMYK Double
toImageCMYK = I.map toPixelCMYK
{-# INLINE toImageCMYK #-}


instance Elevator e => ToCMYK Y e where
  toPixelCMYK = toPixelCMYK . toPixelRGB
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK YA e where
  toPixelCMYK = toPixelCMYK . dropAlpha
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK RGB e where
  toPixelCMYK (fmap toDouble -> PixelRGB r g b) = PixelCMYK c m y k where
    !c = (1 - r - k)/(1 - k)
    !m = (1 - g - k)/(1 - k)
    !y = (1 - b - k)/(1 - k)
    !k = 1 - max r (max g b)
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK RGBA e where
  toPixelCMYK = toPixelCMYK . dropAlpha
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK HSI e where
  toPixelCMYK = toPixelCMYK . toPixelRGB
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK HSIA e where
  toPixelCMYK = toPixelCMYK . dropAlpha
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK CMYK e where
  toPixelCMYK = fmap toDouble
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK CMYKA e where
  toPixelCMYK = toPixelCMYK . dropAlpha
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK YCbCr e where
  toPixelCMYK = toPixelCMYK . toPixelRGB
  {-# INLINE toPixelCMYK #-}

instance Elevator e => ToCMYK YCbCrA e where
  toPixelCMYK = toPixelCMYK . dropAlpha
  {-# INLINE toPixelCMYK #-}


-- ToCMYKA

-- | Conversion to `CMYKA`.
class ToCMYK cs e => ToCMYKA cs e where

  -- | Convert to a `CMYKA` pixel.
  toPixelCMYKA :: Pixel cs e -> Pixel CMYKA Double
  toPixelCMYKA = addAlpha 1 . toPixelCMYK
  {-# INLINE toPixelCMYKA #-}

-- | Convert to a `CMYKA` image.
toImageCMYKA :: (ToCMYKA cs e, Array arr cs e, Array arr CMYKA Double) =>
                Image arr cs e
             -> Image arr CMYKA Double
toImageCMYKA = I.map toPixelCMYKA
{-# INLINE toImageCMYKA #-}


instance ToCMYK Y e => ToCMYKA Y e

instance Elevator e => ToCMYKA YA e where
  toPixelCMYKA !px = addAlpha (toDouble $ getAlpha px) (toPixelCMYK (dropAlpha px))
  {-# INLINE toPixelCMYKA #-}

instance ToCMYK RGB e => ToCMYKA RGB e

instance Elevator e => ToCMYKA RGBA e where
  toPixelCMYKA !px = addAlpha (toDouble $ getAlpha px) (toPixelCMYK (dropAlpha px))
  {-# INLINE toPixelCMYKA #-}

instance ToCMYK HSI e => ToCMYKA HSI e

instance Elevator e => ToCMYKA HSIA e where
  toPixelCMYKA !px = addAlpha (toDouble $ getAlpha px) (toPixelCMYK (dropAlpha px))
  {-# INLINE toPixelCMYKA #-}

instance ToCMYK CMYK e => ToCMYKA CMYK e

instance Elevator e => ToCMYKA CMYKA e where
  toPixelCMYKA = fmap toDouble
  {-# INLINE toPixelCMYKA #-}

instance ToCMYK YCbCr e => ToCMYKA YCbCr e

instance Elevator e => ToCMYKA YCbCrA e where
  toPixelCMYKA !px = addAlpha (toDouble $ getAlpha px) (toPixelCMYK (dropAlpha px))
  {-# INLINE toPixelCMYKA #-}




-- ToYCbCr

-- | Conversion to `YCbCr` color space.
class ColorSpace cs e => ToYCbCr cs e where

  -- | Convert to an `YCbCr` pixel.
  toPixelYCbCr :: Pixel cs e -> Pixel YCbCr Double

-- | Convert to an `YCbCr` image.
toImageYCbCr :: (ToYCbCr cs e, Array arr cs e, Array arr YCbCr Double) =>
                Image arr cs e
             -> Image arr YCbCr Double
toImageYCbCr = I.map toPixelYCbCr
{-# INLINE toImageYCbCr #-}



instance Elevator e => ToYCbCr Y e where
  toPixelYCbCr = toPixelYCbCr . toPixelRGB
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr YA e where
  toPixelYCbCr = toPixelYCbCr . dropAlpha
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr RGB e where
  toPixelYCbCr (fmap toDouble -> PixelRGB r g b) = PixelYCbCr y cb cr where
    !y  =          0.299*r +    0.587*g +    0.114*b
    !cb = 0.5 - 0.168736*r - 0.331264*g +      0.5*b
    !cr = 0.5 +      0.5*r - 0.418688*g - 0.081312*b
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr RGBA e where
  toPixelYCbCr = toPixelYCbCr . dropAlpha
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr HSI e where
  toPixelYCbCr = toPixelYCbCr . toPixelRGB
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr HSIA e where
  toPixelYCbCr = toPixelYCbCr . dropAlpha
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr YCbCr e where
  toPixelYCbCr = fmap toDouble
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr YCbCrA e where
  toPixelYCbCr = toPixelYCbCr . dropAlpha
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr CMYK e where
  toPixelYCbCr = toPixelYCbCr . toPixelRGB
  {-# INLINE toPixelYCbCr #-}

instance Elevator e => ToYCbCr CMYKA e where
  toPixelYCbCr = toPixelYCbCr . dropAlpha
  {-# INLINE toPixelYCbCr #-}


-- ToYCbCrA


-- | Conversion to `YCbCrA` from another color space with Alpha channel.
class ToYCbCr cs e => ToYCbCrA cs e where

  -- | Convert to an `YCbCrA` pixel.
  toPixelYCbCrA :: Pixel cs e -> Pixel YCbCrA Double
  toPixelYCbCrA = addAlpha 1 . toPixelYCbCr
  {-# INLINE toPixelYCbCrA #-}

-- | Convert to an `YCbCrA` image.
toImageYCbCrA :: (ToYCbCrA cs e, Array arr cs e, Array arr YCbCrA Double) =>
                 Image arr cs e
              -> Image arr YCbCrA Double
toImageYCbCrA = I.map toPixelYCbCrA
{-# INLINE toImageYCbCrA #-}

 

instance ToYCbCr Y e => ToYCbCrA Y e

instance Elevator e => ToYCbCrA YA e where
  toPixelYCbCrA !px = addAlpha (toDouble $ getAlpha px) (toPixelYCbCr (dropAlpha px))
  {-# INLINE toPixelYCbCrA #-}

instance ToYCbCr RGB e => ToYCbCrA RGB e


instance ToYCbCr HSI e => ToYCbCrA HSI e

instance Elevator e => ToYCbCrA HSIA e where
  toPixelYCbCrA !px = addAlpha (toDouble $ getAlpha px) (toPixelYCbCr (dropAlpha px))
  {-# INLINE toPixelYCbCrA #-}

instance Elevator e => ToYCbCrA RGBA e where
  toPixelYCbCrA !px = addAlpha (toDouble $ getAlpha px) (toPixelYCbCr (dropAlpha px))
  {-# INLINE toPixelYCbCrA #-}

instance ToYCbCr CMYK e => ToYCbCrA CMYK e

instance Elevator e => ToYCbCrA CMYKA e where
  toPixelYCbCrA !px = addAlpha (toDouble $ getAlpha px) (toPixelYCbCr (dropAlpha px))
  {-# INLINE toPixelYCbCrA #-}

instance ToYCbCr YCbCr e => ToYCbCrA YCbCr e

instance Elevator e => ToYCbCrA YCbCrA e where
  toPixelYCbCrA = fmap toDouble
  {-# INLINE toPixelYCbCrA #-}


