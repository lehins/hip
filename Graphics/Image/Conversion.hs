{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Graphics.Image.Conversion (
  Convertable(..)
  ) where

import Graphics.Image.Base
import Graphics.Image.Gray
import Graphics.Image.Color
import Data.Typeable
import Data.Word (Word8, Word16)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Storable as VS (map, convert)
import Codec.Picture hiding (Pixel, Image)
import Codec.Picture.Types hiding (Pixel, Image)
import qualified Codec.Picture as JP (Pixel(..), Image(..))
import Graphics.Netpbm
import GHC.Float

-- Pixels ========================================================================

-- Helper Functions --------------------------------------------------------------

fromWord8 :: Word8 -> Double
fromWord8 px = (fromIntegral px) / 255
toWord8 :: Double -> Word8
toWord8 px = round (255*px)

fromWord16 :: Word16 -> Double
fromWord16 px = fromIntegral px / 65535
toWord16 :: Double -> Word16
toWord16 px = round (65535*px)

-- Internal ----------------------------------------------------------------------

instance Convertable Gray RGB where
  convert (Gray g) = pixel g

instance Convertable Gray Gray where
  convert = id

instance Convertable RGB Gray where
  convert (RGB r g b) = Gray ((r + g + b)/3)
  
-- JuicyPixel ---------------------------------------------------------------------

---- to and from Gray -----

instance Convertable Word8 Gray where
  convert = Gray . fromWord8

instance Convertable Gray Word8 where
  convert (Gray g) = toWord8 g

instance Convertable Word16 Gray where
  convert = Gray . fromWord16

instance Convertable Gray Word16 where
  convert (Gray g) = toWord16 g

instance Convertable Float Gray  where
  convert = Gray . float2Double

instance Convertable Gray Float where
  convert (Gray d) = double2Float d

instance Convertable PixelYA8 Gray where
  convert = convert . dropTransparency

instance Convertable Gray PixelYA8 where
  convert = promotePixel . (convert :: Gray -> Word8)

instance Convertable PixelYA16 Gray where
  convert = convert . dropTransparency

instance Convertable Gray PixelYA16 where
  convert = promotePixel . (convert :: Gray -> Word16)

instance Convertable PixelRGB8 Gray where
  convert = convert . computeLuma

instance Convertable Gray PixelRGB8 where
  convert = convert . (convert :: Gray -> RGB)

instance Convertable PixelRGB16 Gray where
  convert = convert . computeLuma

instance Convertable Gray PixelRGB16 where
  convert = convert . (convert :: Gray -> RGB)

instance Convertable PixelRGBA8 Gray where
  convert = convert . computeLuma

instance Convertable Gray PixelRGBA8 where
  convert = convert . (convert :: Gray -> RGB)

instance Convertable PixelRGBA16 Gray where
  convert = convert . dropTransparency

instance Convertable Gray PixelRGBA16 where
  convert = convert . (convert :: Gray -> RGB)

instance Convertable PixelRGBF Gray where
  convert = convert . computeLuma

instance Convertable Gray PixelRGBF where
  convert = convert . (convert :: Gray -> RGB)

instance Convertable PixelYCbCr8 Gray where
  convert = convert . computeLuma

instance Convertable Gray PixelYCbCr8 where
  convert = convert . (convert :: Gray -> RGB)

instance Convertable PixelCMYK8 Gray where
  convert = convert . (convertPixel :: PixelCMYK8 -> PixelRGB8)

instance Convertable Gray PixelCMYK8 where
  convert = convert . (convert :: Gray -> RGB)

instance Convertable PixelCMYK16 Gray where
  convert = convert . (convertPixel :: PixelCMYK16 -> PixelRGB16)

instance Convertable Gray PixelCMYK16 where
  convert = convert . (convert :: Gray -> RGB)

---- to and from RGB -----

instance Convertable Word8 RGB where
  convert = convert . (convert :: Word8 -> Gray)

instance Convertable RGB Word8 where
  convert = convert . (convert :: RGB -> Gray)

instance Convertable Word16 RGB where
  convert = convert . (convert :: Word16 -> Gray)

instance Convertable RGB Word16 where
  convert = convert . (convert :: RGB -> Gray)
  
instance Convertable Float RGB where
  convert = convert . (convert :: Float -> Gray)

instance Convertable RGB Float where
  convert = convert . (convert :: RGB -> Gray)

instance Convertable PixelYA8 RGB where
  convert = convert . dropTransparency

instance Convertable RGB PixelYA8 where
  convert = convert . (convert :: RGB -> Gray)

instance Convertable PixelYA16 RGB where
  convert = convert . dropTransparency

instance Convertable RGB PixelYA16 where
  convert = convert . (convert :: RGB -> Gray)

instance Convertable PixelRGB8 RGB where
  convert (PixelRGB8 r g b) = RGB (fromWord8 r) (fromWord8 g) (fromWord8 b)

instance Convertable RGB PixelRGB8 where
  convert (RGB r g b) = PixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)

instance Convertable PixelRGB16 RGB where
  convert (PixelRGB16 r g b) = RGB (fromWord16 r) (fromWord16 g) (fromWord16 b)

instance Convertable RGB PixelRGB16 where
  convert (RGB r g b) = PixelRGB16 (toWord16 r) (toWord16 g) (toWord16 b)
  
instance Convertable PixelRGBA8 RGB where
  convert = convert . dropTransparency

instance Convertable RGB PixelRGBA8 where
  convert = promotePixel . (convert :: RGB -> PixelRGB8)

instance Convertable PixelRGBA16 RGB where
  convert = convert . dropTransparency

instance Convertable RGB PixelRGBA16 where
  convert = promotePixel . (convert :: RGB -> PixelRGB16)

instance Convertable PixelRGBF RGB where
  convert (PixelRGBF r g b) = RGB (float2Double r) (float2Double g) (float2Double b)

instance Convertable RGB PixelRGBF where
  convert (RGB r g b) = PixelRGBF (double2Float r) (double2Float g) (double2Float b)

instance Convertable PixelYCbCr8 RGB where
  convert = convert . (convertPixel :: PixelYCbCr8 -> PixelRGB8)

instance Convertable RGB PixelYCbCr8 where
  convert = (convertPixel :: PixelRGB8 -> PixelYCbCr8) . convert

instance Convertable PixelCMYK8 RGB where
  convert = convert . (convertPixel :: PixelCMYK8 -> PixelRGB8)

instance Convertable RGB PixelCMYK8 where
  convert = (convertPixel :: PixelRGB8 -> PixelCMYK8) . convert

instance Convertable PixelCMYK16 RGB where
  convert = convert . (convertPixel :: PixelCMYK16 -> PixelRGB16)

instance Convertable RGB PixelCMYK16 where
  convert = (convertPixel :: PixelRGB16 -> PixelCMYK16) . convert

----- JuicyPixels Images --------------------------------------------------------

jp2Image i = make (imageHeight i) (imageWidth i) pxOp
  where pxOp y x = convert $ pixelAt i x y

instance Convertable DynamicImage (Image Gray) where
  convert (ImageY8 i) = jp2Image i
  convert (ImageY16 i) = jp2Image i
  convert (ImageYF i) = jp2Image i
  convert (ImageRGB8 i) = jp2Image i
  convert (ImageRGB16 i) = jp2Image i
  convert (ImageRGBF i) = jp2Image i
  convert (ImageRGBA8 i) = jp2Image i
  convert (ImageRGBA16 i) = jp2Image i
  convert (ImageYCbCr8 i) = jp2Image i
  convert (ImageCMYK8 i) = jp2Image i
  convert (ImageCMYK16 i) = jp2Image i

instance Convertable DynamicImage (Image RGB) where
  convert (ImageY8 i) = jp2Image i
  convert (ImageY16 i) = jp2Image i
  convert (ImageYF i) = jp2Image i
  convert (ImageRGB8 i) = jp2Image i
  convert (ImageRGB16 i) = jp2Image i
  convert (ImageRGBF i) = jp2Image i
  convert (ImageRGBA8 i) = jp2Image i
  convert (ImageRGBA16 i) = jp2Image i
  convert (ImageYCbCr8 i) = jp2Image i
  convert (ImageCMYK8 i) = jp2Image i
  convert (ImageCMYK16 i) = jp2Image i


-- Netpbm--------------------------------------------------------------------------

---- to and from Gray -----
  
instance Convertable PgmPixel8 Gray where
  convert (PgmPixel8 w8) = convert w8

instance Convertable Gray PgmPixel8 where
  convert (Gray g) = PgmPixel8 $ toWord8 g

instance Convertable PgmPixel16 Gray where
  convert (PgmPixel16 w16) = convert w16

instance Convertable Gray PgmPixel16 where
  convert (Gray g) = PgmPixel16 $ toWord16 g

instance Convertable PpmPixelRGB8 Gray where
  convert = convert . (convert :: PpmPixelRGB8 -> RGB)

instance Convertable Gray PpmPixelRGB8 where
  convert = convert . (convert :: Gray -> RGB)

instance Convertable PpmPixelRGB16 Gray where
  convert = convert . (convert :: PpmPixelRGB16 -> RGB)

instance Convertable Gray PpmPixelRGB16 where
  convert = convert . (convert :: Gray -> RGB)

---- to and from RGB -----

instance Convertable PgmPixel8 RGB where
  convert = convert . (convert :: PgmPixel8 -> Gray)

instance Convertable RGB PgmPixel8 where
  convert = convert . (convert :: RGB -> Gray)

instance Convertable PgmPixel16 RGB where
  convert = convert . (convert :: PgmPixel16 -> Gray)

instance Convertable RGB PgmPixel16 where
  convert = convert . (convert :: RGB -> Gray)

instance Convertable PpmPixelRGB8 RGB where
  convert (PpmPixelRGB8 r g b) = RGB (fromWord8 r) (fromWord8 g) (fromWord8 b)

instance Convertable RGB PpmPixelRGB8 where
  convert (RGB r g b) = PpmPixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)

instance Convertable PpmPixelRGB16 RGB where
  convert (PpmPixelRGB16 r g b) = RGB (fromWord16 r) (fromWord16 g) (fromWord16 b)

instance Convertable RGB PpmPixelRGB16 where
  convert (RGB r g b) = PpmPixelRGB16 (toWord16 r) (toWord16 g) (toWord16 b)

{-
instance Convertable PbmPixel where
  toGray (PbmPixel g) = Gray $ if g then 1 else 0
  fromGray (Gray g) = PbmPixel (g<1)
  toRGB = fromGray . toGray
  fromRGB = fromGray . fromRGB

-}

ppm2Image (PPMHeader _ c r) v = fromVector r c $ V.map convert $ VS.convert v

instance Convertable PPM (Image Gray) where
  convert (PPM header (PpmPixelDataRGB8 v)) = ppm2Image header v
  convert (PPM header (PpmPixelDataRGB16 v)) = ppm2Image header v
  convert (PPM header (PgmPixelData8 v)) = ppm2Image header v
  convert (PPM header (PgmPixelData16 v)) = ppm2Image header v

instance Convertable PPM (Image RGB) where
  convert (PPM header (PpmPixelDataRGB8 v)) = ppm2Image header v
  convert (PPM header (PpmPixelDataRGB16 v)) = ppm2Image header v
  convert (PPM header (PgmPixelData8 v)) = ppm2Image header v
  convert (PPM header (PgmPixelData16 v)) = ppm2Image header v

