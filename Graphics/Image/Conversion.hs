{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, ViewPatterns #-}
module Graphics.Image.Conversion where

import Prelude hiding (map)
import Graphics.Image.Definition (Convertable(..), Pixel(..))
import Graphics.Image.Internal hiding (maximum, minimum)
import Graphics.Image.Gray
import Graphics.Image.Color
import Data.Array.Repa hiding ((++), map)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString as B (ByteString)
import Data.Word (Word8, Word16)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Storable as VS (convert, Vector, Storable)
import Codec.Picture hiding (Pixel, Image)
import Codec.Picture.Types hiding (Pixel, Image)
import qualified Codec.Picture as JP    -- JuicyPixels
import qualified Graphics.Netpbm as PNM -- Portable anymap format (PNM)
import GHC.Float


-- TODO: add links to wikipedia on formats
-- TODO: implement writing for PNM formats
-- | Format types that an image can be saved in.
data Format = BMP  -- ^ A BMP image with .bmp extension
            | JPG  -- ^ A JPG image with .jpg or .jpeg extension
            | PNG  -- ^ A PNG (Portable Network Graphics) image with .png extension
            | TIFF -- ^ A TIFF image with .tif or .tiff extension
            | HDR  -- ^ A HDR image with .hdr extension
            -- | PBM
            -- | PGM
            -- | PPM
            deriving Show


type Encoder px = Format -> Array U DIM2 px -> BL.ByteString


data SaveOption px = Format Format
                   | Encoder (Encoder px)
                   | Normalize Bool


class (Ord px, Pixel px) => Saveable px where
  inY8 :: Encoder px
  inY16 :: Encoder px
  inYA8 :: Encoder px
  inYA16 :: Encoder px
  inRGB8 :: Encoder px
  inRGB16 :: Encoder px
  inRGBF :: Encoder px
  inRGBA8 :: Encoder px
  inRGBA16 :: Encoder px
  inYCbCr8 :: Encoder px
  inCMYK8 :: Encoder px
  inCMYK16 :: Encoder px

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

instance Convertable Gray Color where
  convert (Gray g) = pixel g


instance Convertable Gray Gray where
  convert = id


instance Convertable Color Gray where
  convert (RGB r g b) = Gray ((r + g + b)/3)
  convert (HSI _ _ i) = Gray i


instance Convertable (Image Color) (Image Gray, Image Gray, Image Gray) where
  convert img = (map v1 img, map v2 img, map v3 img)
    where v1 (HSI v _ _) = Gray v
          v1 (RGB v _ _) = Gray v
          v2 (HSI _ v _) = Gray v
          v2 (RGB _ v _) = Gray v
          v3 (HSI _ _ v) = Gray v
          v3 (RGB _ _ v) = Gray v


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
  convert = convert . (convert :: Gray -> Color)

instance Convertable PixelRGB16 Gray where
  convert = convert . computeLuma

instance Convertable Gray PixelRGB16 where
  convert = convert . (convert :: Gray -> Color)

instance Convertable PixelRGBA8 Gray where
  convert = convert . computeLuma

instance Convertable Gray PixelRGBA8 where
  convert = convert . (convert :: Gray -> Color)

instance Convertable PixelRGBA16 Gray where
  convert = convert . dropTransparency

instance Convertable Gray PixelRGBA16 where
  convert = convert . (convert :: Gray -> Color)

instance Convertable PixelRGBF Gray where
  convert = convert . computeLuma

instance Convertable Gray PixelRGBF where
  convert = convert . (convert :: Gray -> Color)

instance Convertable PixelYCbCr8 Gray where
  convert = convert . computeLuma

instance Convertable Gray PixelYCbCr8 where
  convert = convert . (convert :: Gray -> Color)

instance Convertable PixelCMYK8 Gray where
  convert = convert . (convertPixel :: PixelCMYK8 -> PixelRGB8)

instance Convertable Gray PixelCMYK8 where
  convert = convert . (convert :: Gray -> Color)

instance Convertable PixelCMYK16 Gray where
  convert = convert . (convertPixel :: PixelCMYK16 -> PixelRGB16)

instance Convertable Gray PixelCMYK16 where
  convert = convert . (convert :: Gray -> Color)

---- to and from Color -----

instance Convertable Word8 Color where
  convert = convert . (convert :: Word8 -> Gray)

instance Convertable Color Word8 where
  convert = convert . (convert :: Color -> Gray)

instance Convertable Word16 Color where
  convert = convert . (convert :: Word16 -> Gray)

instance Convertable Color Word16 where
  convert = convert . (convert :: Color -> Gray)
  
instance Convertable Float Color where
  convert = convert . (convert :: Float -> Gray)

instance Convertable Color Float where
  convert = convert . (convert :: Color -> Gray)

instance Convertable PixelYA8 Color where
  convert = convert . dropTransparency

instance Convertable Color PixelYA8 where
  convert = convert . (convert :: Color -> Gray)

instance Convertable PixelYA16 Color where
  convert = convert . dropTransparency

instance Convertable Color PixelYA16 where
  convert = convert . (convert :: Color -> Gray)

instance Convertable PixelRGB8 Color where
  convert (PixelRGB8 r g b) = RGB (fromWord8 r) (fromWord8 g) (fromWord8 b)

instance Convertable Color PixelRGB8 where
  convert (RGB r g b) = PixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)
  convert px = convert $ inRGB px

instance Convertable PixelRGB16 Color where
  convert (PixelRGB16 r g b) = RGB (fromWord16 r) (fromWord16 g) (fromWord16 b)

instance Convertable Color PixelRGB16 where
  convert (RGB r g b) = PixelRGB16 (toWord16 r) (toWord16 g) (toWord16 b)
  convert px          = convert $ inRGB px
  
instance Convertable PixelRGBA8 Color where
  convert = convert . dropTransparency

instance Convertable Color PixelRGBA8 where
  convert = promotePixel . (convert :: Color -> PixelRGB8)

instance Convertable PixelRGBA16 Color where
  convert = convert . dropTransparency

instance Convertable Color PixelRGBA16 where
  convert = promotePixel . (convert :: Color -> PixelRGB16)

instance Convertable PixelRGBF Color where
  convert (PixelRGBF r g b) = RGB (float2Double r) (float2Double g) (float2Double b)

instance Convertable Color PixelRGBF where
  convert (RGB r g b) = PixelRGBF (double2Float r) (double2Float g) (double2Float b)
  convert px          = convert $ inRGB px

instance Convertable PixelYCbCr8 Color where
  convert = convert . (convertPixel :: PixelYCbCr8 -> PixelRGB8)

instance Convertable Color PixelYCbCr8 where
  convert = (convertPixel :: PixelRGB8 -> PixelYCbCr8) . convert

instance Convertable PixelCMYK8 Color where
  convert = convert . (convertPixel :: PixelCMYK8 -> PixelRGB8)

instance Convertable Color PixelCMYK8 where
  convert = (convertPixel :: PixelRGB8 -> PixelCMYK8) . convert

instance Convertable PixelCMYK16 Color where
  convert = convert . (convertPixel :: PixelCMYK16 -> PixelRGB16)

instance Convertable Color PixelCMYK16 where
  convert = (convertPixel :: PixelRGB16 -> PixelCMYK16) . convert

----- JuicyPixels Images --------------------------------------------------------

jp2Image :: (Pixel px1, Convertable px2 px1, JP.Pixel px2) =>
            JP.Image px2 -> Image px1
jp2Image i = make (imageHeight i) (imageWidth i) conv
  where conv y x = convert $ pixelAt i x y

instance Convertable DynamicImage (Image Gray) where
  convert (ImageY8 i) = jp2Image i
  convert (ImageY16 i) = jp2Image i
  convert (ImageYF i) = jp2Image i
  convert (ImageYA8 i) = jp2Image i
  convert (ImageYA16 i) = jp2Image i
  convert (ImageRGB8 i) = jp2Image i
  convert (ImageRGB16 i) = jp2Image i
  convert (ImageRGBF i) = jp2Image i
  convert (ImageRGBA8 i) = jp2Image i
  convert (ImageRGBA16 i) = jp2Image i
  convert (ImageYCbCr8 i) = jp2Image i
  convert (ImageCMYK8 i) = jp2Image i
  convert (ImageCMYK16 i) = jp2Image i

instance Convertable DynamicImage (Image Color) where
  convert (ImageY8 i) = jp2Image i
  convert (ImageY16 i) = jp2Image i
  convert (ImageYF i) = jp2Image i
  convert (ImageYA8 i) = jp2Image i
  convert (ImageYA16 i) = jp2Image i
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
  
instance Convertable PNM.PbmPixel Gray where
  convert (PNM.PbmPixel bool) = Gray $ if bool then 1.0 else 0.0
  
instance Convertable PNM.PgmPixel8 Gray where
  convert (PNM.PgmPixel8 w8) = convert w8

instance Convertable Gray PNM.PgmPixel8 where
  convert (Gray g) = PNM.PgmPixel8 $ toWord8 g

instance Convertable PNM.PgmPixel16 Gray where
  convert (PNM.PgmPixel16 w16) = convert w16

instance Convertable Gray PNM.PgmPixel16 where
  convert (Gray g) = PNM.PgmPixel16 $ toWord16 g

instance Convertable PNM.PpmPixelRGB8 Gray where
  convert = convert . (convert :: PNM.PpmPixelRGB8 -> Color)

instance Convertable Gray PNM.PpmPixelRGB8 where
  convert = convert . (convert :: Gray -> Color)

instance Convertable PNM.PpmPixelRGB16 Gray where
  convert = convert . (convert :: PNM.PpmPixelRGB16 -> Color)

instance Convertable Gray PNM.PpmPixelRGB16 where
  convert = convert . (convert :: Gray -> Color)

---- to and from RGB -----

instance Convertable PNM.PbmPixel Color where
  convert = convert . (convert :: PNM.PbmPixel -> Gray)
  
instance Convertable PNM.PgmPixel8 Color where
  convert = convert . (convert :: PNM.PgmPixel8 -> Gray)

instance Convertable Color PNM.PgmPixel8 where
  convert = convert . (convert :: Color -> Gray)

instance Convertable PNM.PgmPixel16 Color where
  convert = convert . (convert :: PNM.PgmPixel16 -> Gray)

instance Convertable Color PNM.PgmPixel16 where
  convert = convert . (convert :: Color -> Gray)

instance Convertable PNM.PpmPixelRGB8 Color where
  convert (PNM.PpmPixelRGB8 r g b) = RGB (fromWord8 r) (fromWord8 g) (fromWord8 b)

instance Convertable Color PNM.PpmPixelRGB8 where
  convert (RGB r g b) = PNM.PpmPixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)
  convert px = convert $ inRGB px

instance Convertable PNM.PpmPixelRGB16 Color where
  convert (PNM.PpmPixelRGB16 r g b) = RGB (fromWord16 r) (fromWord16 g) (fromWord16 b)

instance Convertable Color PNM.PpmPixelRGB16 where
  convert (RGB r g b) = PNM.PpmPixelRGB16 (toWord16 r) (toWord16 g) (toWord16 b)
  convert px = convert $ inRGB px


ppm2Image :: (VS.Storable px1, V.Unbox px1, Pixel px2, Convertable px1 px2) =>
             PNM.PPMHeader -> VS.Vector px1 -> Image px2
ppm2Image (PNM.PPMHeader _ c r) v = fromVector r c $ V.map convert $ VS.convert v


decodeImage :: (Convertable PNM.PPM b, Convertable DynamicImage b) =>
               B.ByteString -> Either [Char] b
decodeImage imstr = either pnm2Image (Right . convert) $ JP.decodeImage imstr
  where
    pnm2Image errmsgJP = pnmResult2Image $ PNM.parsePPM imstr where
      pnmResult2Image (Right (pnmLs, _)) = Right $ convert (head pnmLs)
      pnmResult2Image (Left errmsgPNM) = Left (errmsgJP++errmsgPNM)


instance Convertable PNM.PPM (Image Gray) where
  convert (PNM.PPM header (PNM.PpmPixelDataRGB8 v))  = ppm2Image header v
  convert (PNM.PPM header (PNM.PpmPixelDataRGB16 v)) = ppm2Image header v
  convert (PNM.PPM header (PNM.PbmPixelData v))      = ppm2Image header v
  convert (PNM.PPM header (PNM.PgmPixelData8 v))     = ppm2Image header v
  convert (PNM.PPM header (PNM.PgmPixelData16 v))    = ppm2Image header v


instance Convertable PNM.PPM (Image Color) where
  convert (PNM.PPM header (PNM.PpmPixelDataRGB8 v))  = ppm2Image header v
  convert (PNM.PPM header (PNM.PpmPixelDataRGB16 v)) = ppm2Image header v
  convert (PNM.PPM header (PNM.PbmPixelData v))      = ppm2Image header v
  convert (PNM.PPM header (PNM.PgmPixelData8 v))     = ppm2Image header v
  convert (PNM.PPM header (PNM.PgmPixelData16 v))    = ppm2Image header v


arrayToJPImage :: (JP.Pixel a, Pixel px) => (px -> a) -> Array U DIM2 px -> JP.Image a
arrayToJPImage f arr@(extent -> (Z :. m :. n)) =
  JP.generateImage g n m where
    g c r = f $ index arr (Z :. r :. c)


instance Saveable Gray where
  inY8 BMP      = JP.encodeBitmap . (arrayToJPImage (convert :: Gray -> JP.Pixel8))
  inY8 PNG      = JP.encodePng    . (arrayToJPImage (convert :: Gray -> JP.Pixel8))
  inY8 TIFF     = JP.encodeTiff   . (arrayToJPImage (convert :: Gray -> JP.Pixel8))
  inY8 f        = error $ "Cannot save "++show f++" in Y8 colorspace"
  inY16 PNG     = JP.encodePng    . (arrayToJPImage (convert :: Gray -> JP.Pixel16))
  inY16 TIFF    = JP.encodeTiff   . (arrayToJPImage (convert :: Gray -> JP.Pixel16))
  inY16 f       = error $ "Cannot save "++show f++" in Y16 colorspace"
  inYA8 PNG     = JP.encodePng    . (arrayToJPImage (convert :: Gray -> JP.PixelYA8))
  inYA8 f       = error $ "Cannot save "++show f++" in Y8 colorspace"
  inYA16 PNG    = JP.encodePng    . (arrayToJPImage (convert :: Gray -> JP.PixelYA16))
  inYA16 f      = error $ "Cannot save "++show f++" in Y16 colorspace"
  inRGB8 BMP    = JP.encodeBitmap . (arrayToJPImage (convert :: Gray -> JP.PixelRGB8))
  inRGB8 PNG    = JP.encodePng    . (arrayToJPImage (convert :: Gray -> JP.PixelRGB8))
  inRGB8 TIFF   = JP.encodeTiff   . (arrayToJPImage (convert :: Gray -> JP.PixelRGB8))
  inRGB8 f      = error $ "Cannot save "++show f++" in RGB8 colorspace"
  inRGB16 TIFF  = JP.encodeTiff   . (arrayToJPImage (convert :: Gray -> JP.PixelRGB16))
  inRGB16 PNG   = JP.encodePng    . (arrayToJPImage (convert :: Gray -> JP.PixelRGB16))
  inRGB16 f     = error $ "Cannot save "++show f++" in RGB16 colorspace"
  inRGBA8 BMP   = JP.encodeBitmap . (arrayToJPImage (convert :: Gray -> JP.PixelRGBA8))
  inRGBA8 PNG   = JP.encodePng    . (arrayToJPImage (convert :: Gray -> JP.PixelRGBA8))
  inRGBA8 f     = error $ "Cannot save "++show f++" in RGBA8 colorspace"
  inRGBA16 PNG  = JP.encodePng    . (arrayToJPImage (convert :: Gray -> JP.PixelRGBA16))
  inRGBA16 f    = error $ "Cannot save "++show f++" in RGBA16 colorspace"
  inYCbCr8 JPG  =
    (JP.encodeJpegAtQuality 100) . (arrayToJPImage (convert :: Gray -> JP.PixelYCbCr8))
  inYCbCr8 f    = error $ "Cannot save "++show f++" in YCbCr8 colorspace"
  inCMYK8 TIFF  = JP.encodeTiff  . (arrayToJPImage (convert :: Gray -> JP.PixelCMYK8))
  inCMYK8 f     = error $ "Cannot save "++show f++" in CMYK8 colorspace"
  inCMYK16 TIFF = JP.encodeTiff  . (arrayToJPImage (convert :: Gray -> JP.PixelCMYK16))
  inCMYK16 f    = error $ "Cannot save "++show f++" in CMYK16 colorspace"
  inRGBF HDR    = JP.encodeHDR   . (arrayToJPImage (convert :: Gray -> JP.PixelRGBF))
  inRGBF f      = error $ "Cannot save "++show f++" in RGBF colorspace"


instance Saveable Color where
  inY8 BMP      = JP.encodeBitmap . (arrayToJPImage (convert :: Color -> JP.Pixel8))
  inY8 PNG      = JP.encodePng . (arrayToJPImage (convert :: Color -> JP.Pixel8))
  inY8 TIFF     = JP.encodeTiff . (arrayToJPImage (convert :: Color -> JP.Pixel8))
  inY8 f        = error $ "Cannot save "++show f++" in Y8 colorspace"
  inY16 PNG     = JP.encodePng . (arrayToJPImage (convert :: Color -> JP.Pixel16))
  inY16 TIFF    = JP.encodeTiff . (arrayToJPImage (convert :: Color -> JP.Pixel16))
  inY16 f       = error $ "Cannot save "++show f++" in Y16 colorspace"
  inYA8 PNG     = JP.encodePng . (arrayToJPImage (convert :: Color -> JP.PixelYA8))
  inYA8 f       = error $ "Cannot save "++show f++" in Y8 colorspace"
  inYA16 PNG    = JP.encodePng . (arrayToJPImage (convert :: Color -> JP.PixelYA16))
  inYA16 f      = error $ "Cannot save "++show f++" in Y16 colorspace"
  inRGB8 BMP    = JP.encodeBitmap . (arrayToJPImage (convert :: Color -> JP.PixelRGB8))
  inRGB8 PNG    = JP.encodePng . (arrayToJPImage (convert :: Color -> JP.PixelRGB8))
  inRGB8 TIFF   = JP.encodeTiff . (arrayToJPImage (convert :: Color -> JP.PixelRGB8))
  inRGB8 f      = error $ "Cannot save "++show f++" in RGB8 colorspace"
  inRGB16 TIFF  = JP.encodeTiff . (arrayToJPImage (convert :: Color -> JP.PixelRGB16))
  inRGB16 PNG   = JP.encodePng . (arrayToJPImage (convert :: Color -> JP.PixelRGB16))
  inRGB16 f     = error $ "Cannot save "++show f++" in RGB16 colorspace"
  inRGBA8 BMP   = JP.encodeBitmap . (arrayToJPImage (convert :: Color -> JP.PixelRGBA8))
  inRGBA8 PNG   = JP.encodePng . (arrayToJPImage (convert :: Color -> JP.PixelRGBA8))
  inRGBA8 f     = error $ "Cannot save "++show f++" in RGBA8 colorspace"
  inRGBA16 PNG  = JP.encodePng . (arrayToJPImage (convert :: Color -> JP.PixelRGBA16))
  inRGBA16 f    = error $ "Cannot save "++show f++" in RGBA16 colorspace"
  inYCbCr8 JPG  =
    (JP.encodeJpegAtQuality 100) . (arrayToJPImage (convert :: Color -> JP.PixelYCbCr8))
  inYCbCr8 f    = error $ "Cannot save "++show f++" in YCbCr8 colorspace"
  inCMYK8 TIFF  = JP.encodeTiff . (arrayToJPImage (convert :: Color -> JP.PixelCMYK8))
  inCMYK8 f     = error $ "Cannot save "++show f++" in CMYK8 colorspace"
  inCMYK16 TIFF = JP.encodeTiff . (arrayToJPImage (convert :: Color -> JP.PixelCMYK16))
  inCMYK16 f    = error $ "Cannot save "++show f++" in CMYK16 colorspace"
  inRGBF HDR    = JP.encodeHDR . (arrayToJPImage (convert :: Color -> JP.PixelRGBF))
  inRGBF f      = error $ "Cannot save "++show f++" in RGBF colorspace"
