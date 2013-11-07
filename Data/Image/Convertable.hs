{-# LANGUAGE TypeFamilies, ViewPatterns, TypeSynonymInstances #-}
module Data.Image.Convertable (
  Convertable(..),
  decodeImage,
  DynamicImage(..),
  Image(..),
  Pixel(..),
  parsePPM,
  PPM(..),
  PPMHeader(..),
  PpmPixelData(..),
  PpmPixelRGB8(..),
  PpmPixelRGB16(..),
  PbmPixel(..),
  PgmPixel8(..),
  PgmPixel16(..)
  ) where

import Data.Image.Gray
import Data.Image.Color
import qualified Data.Image.Internal as I
import Data.Typeable
import Data.Word (Word8, Word16)
import Codec.Picture 
import Graphics.Netpbm

fromWord8 :: Word8 -> Double
fromWord8 px = fromIntegral px / 255
toWord8 :: Double -> Word8
toWord8 px = round (255*px)

fromWord16 :: Word16 -> Double
fromWord16 px = fromIntegral px / 65535
toWord16 :: Double -> Word16
toWord16 px = round (65535*px)

caster n = getDouble $ cast n where
  getDouble (Just d) = d
  getDouble Nothing = error $ "Was unable to cast "++show n

fromFloat :: Float -> Double
fromFloat = caster
toFloat :: Double -> Float
toFloat = caster

class Convertable px where

  toGray :: px -> Gray

  fromGray :: Gray -> px

  toColor :: px -> Color

  fromColor :: Color -> px


-- Internal ----------------------------------------------------------------------

instance Convertable Gray where
  toGray = id
  fromGray = id
  toColor (Gray g) = RGB g g g
  toColor (GrayA g a) = RGBA g g g a
  fromColor (RGB r g b) = Gray ((r+g+b)/3)
  fromColor (RGBA r g b a) = GrayA ((r+g+b)/3) a

instance Convertable Color where
  toGray = fromColor
  fromGray = toColor
  toColor = id
  fromColor = id

-- JuicyPixel ---------------------------------------------------------------------

instance Convertable Word8 where
  toGray = Gray . fromWord8
  fromGray (Gray px) = toWord8 px
  fromGray (GrayA px _) = toWord8 px
  toColor = fromGray . toGray
  fromColor = fromGray . toGray

instance Convertable Word16 where
  toGray = Gray . fromWord16
  fromGray (Gray px) = toWord16 px
  fromGray (GrayA px _) = toWord16 px
  toColor = fromGray . toGray
  fromColor = fromGray . toGray

instance Convertable Float where
  toGray = Gray . fromFloat
  fromGray (Gray px) = toFloat px
  fromGray (GrayA px _) = toFloat px
  toColor = fromGray . toGray
  fromColor = fromGray . toGray

instance Convertable PixelYA8 where
  toGray (PixelYA8 g a) = GrayA (fromWord8 g) (fromWord8 a)
  fromGray (Gray g) = PixelYA8 (toWord8 g) 1
  fromGray (GrayA g a) = PixelYA8 (toWord8 g) (toWord8 a)
  toColor = fromGray . toGray
  fromColor = fromGray . toGray

instance Convertable PixelYA16 where
  toGray (PixelYA16 g a) = GrayA (fromWord16 g) (fromWord16 a)
  fromGray (Gray g) = PixelYA16 (toWord16 g) 1
  fromGray (GrayA g a) = PixelYA16 (toWord16 g) (toWord16 a)
  toColor = fromGray . toGray
  fromColor = fromGray . toGray

instance Convertable PixelRGB8 where
  toGray = toGray . toColor
  fromGray = fromColor . toColor
  toColor (PixelRGB8 r g b) = RGB (fromWord8 r) (fromWord8 g) (fromWord8 b)
  fromColor (RGB r g b) = PixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)
  fromColor (RGBA r g b _) = fromColor (RGB r g b)

instance Convertable PixelRGB16 where
  toGray = toGray . toColor
  fromGray = fromColor . toColor
  toColor (PixelRGB16 r g b) = RGB (fromWord16 r) (fromWord16 g) (fromWord16 b)
  fromColor (RGB r g b) = PixelRGB16 (toWord16 r) (toWord16 g) (toWord16 b)
  fromColor (RGBA r g b _) = fromColor (RGB r g b)


instance Convertable PixelRGBF where
  toGray = toGray . toColor
  fromGray = fromColor . toColor
  toColor (PixelRGBF r g b) = RGB (fromFloat r) (fromFloat g) (fromFloat b)
  fromColor (RGB r g b) = PixelRGBF (toFloat r) (toFloat g) (toFloat b)
  fromColor (RGBA r g b _) = fromColor (RGB r g b)

instance Convertable PixelRGBA8 where
  toGray = toGray . toColor
  fromGray = fromColor . toColor
  toColor (PixelRGBA8 r g b a) =
    RGBA (fromWord8 r) (fromWord8 g) (fromWord8 b) (fromWord8 a)
  fromColor (RGB r g b) = fromColor (RGBA r g b 1)
  fromColor (RGBA r g b a) =
    PixelRGBA8 (toWord8 r) (toWord8 g) (toWord8 b) (toWord8 a)

instance Convertable PixelRGBA16 where
  toGray = toGray . toColor
  fromGray = fromColor . toColor
  toColor (PixelRGBA16 r g b a) =
    RGBA (fromWord16 r) (fromWord16 g) (fromWord16 b) (fromWord16 a)
  fromColor (RGB r g b) = fromColor (RGBA r g b 1)
  fromColor (RGBA r g b a) =
    PixelRGBA16 (toWord16 r) (toWord16 g) (toWord16 b) (toWord16 a)

instance Convertable PixelYCbCr8 where
  toGray = toGray . toColor
  fromGray = fromColor . toColor
  toColor (PixelYCbCr8
           (fromIntegral -> y) (fromIntegral -> cR) (fromIntegral -> cB)) =
    RGB ((y                       +    1.402*(cR - 128)) / 255)
        ((y - 0.344136*(cB - 128) - 0.714136*(cR - 128)) / 255)
        ((y +    1.772*(cB - 128)                      ) / 255)
  fromColor (RGB r g b) = PixelYCbCr8
                          (round (         0.299*r +    0.587*g +    0.114*b))
                          (round (128 - 0.168736*r - 0.331264*g +      0.5*b))
                          (round (128 +      0.5*r - 0.418688*g - 0.081312*b))
  fromColor (RGBA r g b _) = fromColor (RGB r g b)

---  TODO CMYK

-- Netpbm--------------------------------------------------------------------------

instance Convertable PbmPixel where
  toGray (PbmPixel g) = Gray $ if g then 0 else 1
  fromGray (Gray g) = PbmPixel (g<1)
  toColor = fromGray . toGray
  fromColor = fromGray . fromColor

instance Convertable PgmPixel8 where
  toGray (PgmPixel8 g) = toGray g
  fromGray = PgmPixel8 . fromGray
  toColor = fromGray . toGray
  fromColor = fromGray . fromColor

instance Convertable PgmPixel16 where
  toGray (PgmPixel16 g) = toGray g
  fromGray = PgmPixel16 . fromGray
  toColor = fromGray . toGray
  fromColor = fromGray . fromColor

instance Convertable PpmPixelRGB8 where
  toGray = toGray . toColor
  fromGray = fromColor . toColor
  toColor (PpmPixelRGB8 r g b) = RGB (fromWord8 r) (fromWord8 g) (fromWord8 b)
  fromColor (RGB r g b) = PpmPixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)
  fromColor (RGBA r g b _) = fromColor (RGB r g b)

instance Convertable PpmPixelRGB16 where
  toGray = toGray . toColor
  fromGray = fromColor . toColor
  toColor (PpmPixelRGB16 r g b) =
    RGB (fromWord16 r) (fromWord16 g) (fromWord16 b)
  fromColor (RGB r g b) = PpmPixelRGB16 (toWord16 r) (toWord16 g) (toWord16 b)
  fromColor (RGBA r g b _) = fromColor (RGB r g b)


