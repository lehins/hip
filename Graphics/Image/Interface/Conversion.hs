{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
UndecidableInstances, ViewPatterns #-}
module Graphics.Image.Interface.Conversion (
  Format(..), SaveOption(..), Encoder, Saveable(..), Readable, decodeImage
  ) where

import GHC.Float
import Prelude hiding (map)
import Graphics.Image.Interface (Convertable(..), Pixel(..), Image(..))
import Graphics.Image.Interface.Pixel hiding (Pixel)
import Data.Word (Word8, Word16)
import Data.Vector.Storable (Storable)
import Codec.Picture hiding (Pixel, Image, decodeImage)
import Codec.Picture.Types hiding (Pixel, Image)
import qualified Codec.Picture as JP
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString as B (ByteString)
import qualified Data.Vector.Storable as VS (convert, Vector)
import qualified Data.Vector as V (map, (!))
import qualified Graphics.Netpbm as PNM


-- TODO: add links to wikipedia on formats
-- TODO: implement writing for PNM formats
-- | Format types that an image can be saved in.
data Format = BMP       -- ^ A BMP image with .bmp extension
            | JPG Word8 -- ^ A JPG image with .jpg or .jpeg extension with
                        -- specified quality from 0 to a 100
            | PNG       -- ^ A PNG (Portable Network Graphics) image with .png extension
            | TIFF      -- ^ A TIFF image with .tif or .tiff extension
            | HDR       -- ^ A HDR image with .hdr extension
            -- PBM
            -- PGM
            -- PPM
            deriving Show

-- | Colorspace choice that image will be saved in.
type Encoder img px = Format -> img px -> BL.ByteString


data SaveOption img px = Format Format
                       | Encoder (Encoder img px)


class (Convertable JP.DynamicImage (img px),
       Convertable PNM.PPM         (img px),
       Pixel px, Image img px) => Readable img px where

instance Image img Gray => Readable img Gray where

instance Image img RGB => Readable img RGB where

-- | Pixels implementing this class allow the images to be saved.
class (Pixel px, Image img px) => Saveable img px where
  inY8 :: Encoder img px
  inY16 :: Encoder img px
  inYA8 :: Encoder img px
  inYA16 :: Encoder img px
  inRGB8 :: Encoder img px
  inRGB16 :: Encoder img px
  inRGBF :: Encoder img px
  inRGBA8 :: Encoder img px
  inRGBA16 :: Encoder img px
  inYCbCr8 :: Encoder img px
  inCMYK8 :: Encoder img px
  inCMYK16 :: Encoder img px

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

jpImageToImage :: (Image img px, Pixel px, Convertable px' px, JP.Pixel px') =>
                  JP.Image px' -> img px
jpImageToImage i = make (imageHeight i) (imageWidth i) conv
  where conv y x = convert $ pixelAt i x y

instance Image img Gray => Convertable DynamicImage (img Gray) where
  convert (ImageY8 i) = jpImageToImage i
  convert (ImageY16 i) = jpImageToImage i
  convert (ImageYF i) = jpImageToImage i
  convert (ImageYA8 i) = jpImageToImage i
  convert (ImageYA16 i) = jpImageToImage i
  convert (ImageRGB8 i) = jpImageToImage i
  convert (ImageRGB16 i) = jpImageToImage i
  convert (ImageRGBF i) = jpImageToImage i
  convert (ImageRGBA8 i) = jpImageToImage i
  convert (ImageRGBA16 i) = jpImageToImage i
  convert (ImageYCbCr8 i) = jpImageToImage i
  convert (ImageCMYK8 i) = jpImageToImage i
  convert (ImageCMYK16 i) = jpImageToImage i

instance Image img RGB => Convertable DynamicImage (img RGB) where
  convert (ImageY8 i) = jpImageToImage i
  convert (ImageY16 i) = jpImageToImage i
  convert (ImageYF i) = jpImageToImage i
  convert (ImageYA8 i) = jpImageToImage i
  convert (ImageYA16 i) = jpImageToImage i
  convert (ImageRGB8 i) = jpImageToImage i
  convert (ImageRGB16 i) = jpImageToImage i
  convert (ImageRGBF i) = jpImageToImage i
  convert (ImageRGBA8 i) = jpImageToImage i
  convert (ImageRGBA16 i) = jpImageToImage i
  convert (ImageYCbCr8 i) = jpImageToImage i
  convert (ImageCMYK8 i) = jpImageToImage i
  convert (ImageCMYK16 i) = jpImageToImage i


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
  convert = convert . (convert :: PNM.PpmPixelRGB8 -> RGB)

instance Convertable Gray PNM.PpmPixelRGB8 where
  convert = convert . (convert :: Gray -> RGB)

instance Convertable PNM.PpmPixelRGB16 Gray where
  convert = convert . (convert :: PNM.PpmPixelRGB16 -> RGB)

instance Convertable Gray PNM.PpmPixelRGB16 where
  convert = convert . (convert :: Gray -> RGB)

---- to and from RGB -----

instance Convertable PNM.PbmPixel RGB where
  convert = convert . (convert :: PNM.PbmPixel -> Gray)
  
instance Convertable PNM.PgmPixel8 RGB where
  convert = convert . (convert :: PNM.PgmPixel8 -> Gray)

instance Convertable RGB PNM.PgmPixel8 where
  convert = convert . (convert :: RGB -> Gray)

instance Convertable PNM.PgmPixel16 RGB where
  convert = convert . (convert :: PNM.PgmPixel16 -> Gray)

instance Convertable RGB PNM.PgmPixel16 where
  convert = convert . (convert :: RGB -> Gray)

instance Convertable PNM.PpmPixelRGB8 RGB where
  convert (PNM.PpmPixelRGB8 r g b) = RGB (fromWord8 r) (fromWord8 g) (fromWord8 b)

instance Convertable RGB PNM.PpmPixelRGB8 where
  convert (RGB r g b) = PNM.PpmPixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)

instance Convertable PNM.PpmPixelRGB16 RGB where
  convert (PNM.PpmPixelRGB16 r g b) = RGB (fromWord16 r) (fromWord16 g) (fromWord16 b)

instance Convertable RGB PNM.PpmPixelRGB16 where
  convert (RGB r g b) = PNM.PpmPixelRGB16 (toWord16 r) (toWord16 g) (toWord16 b)


ppmImageToImage :: (Storable px1, Image img px2, Pixel px2, Convertable px1 px2) =>
                   PNM.PPMHeader -> VS.Vector px1 -> img px2
ppmImageToImage (PNM.PPMHeader _ c r) v = make r c getPx where
  !vectorImage = V.map convert $ VS.convert v
  getPx i j = vectorImage V.! (i*r + j)


decodeImage :: (Convertable PNM.PPM b, Convertable DynamicImage b) =>
               B.ByteString -> Either [Char] b
decodeImage imstr = pnm2Image "" --(Right . convert) imstr
  where
    pnm2Image errmsgJP = pnmResult2Image $ PNM.parsePPM imstr where
      pnmResult2Image (Right (pnmLs, _)) = Right $ convert (head pnmLs)
      pnmResult2Image (Left errmsgPNM) = Left ("hip: "++errmsgJP++errmsgPNM)


decodeImage' :: (Convertable PNM.PPM b, Convertable DynamicImage b) =>
               B.ByteString -> Either [Char] b
decodeImage' imstr = either pnm2Image (Right . convert) $ JP.decodeImage imstr
  where
    pnm2Image errmsgJP = pnmResult2Image $ PNM.parsePPM imstr where
      pnmResult2Image (Right (pnmLs, _)) = Right $ convert (head pnmLs)
      pnmResult2Image (Left errmsgPNM) = Left ("hip: "++errmsgJP++errmsgPNM)


instance Image img Gray => Convertable PNM.PPM (img Gray) where
  convert (PNM.PPM header (PNM.PpmPixelDataRGB8 v))  = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PpmPixelDataRGB16 v)) = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PbmPixelData v))      = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PgmPixelData8 v))     = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PgmPixelData16 v))    = ppmImageToImage header v


instance Image img RGB => Convertable PNM.PPM (img RGB) where
  convert (PNM.PPM header (PNM.PpmPixelDataRGB8 v))  = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PpmPixelDataRGB16 v)) = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PbmPixelData v))      = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PgmPixelData8 v))     = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PgmPixelData16 v))    = ppmImageToImage header v


imageToJPImage :: (Image img px, Pixel px, JP.Pixel px') =>
                  (px -> px') -> img px -> JP.Image px'
imageToJPImage f img@(dims -> (m, n)) =
  JP.generateImage g n m where
    g j i = f $ index img i j


instance Image img Gray => Saveable img Gray where
  inY8 BMP      = JP.encodeBitmap . (imageToJPImage (convert :: Gray -> JP.Pixel8))
  inY8 PNG      = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.Pixel8))
  inY8 TIFF     = JP.encodeTiff   . (imageToJPImage (convert :: Gray -> JP.Pixel8))
  inY8 f        = error $ "Cannot save "++show f++" in Y8 colorspace"
  inY16 PNG     = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.Pixel16))
  inY16 TIFF    = JP.encodeTiff   . (imageToJPImage (convert :: Gray -> JP.Pixel16))
  inY16 f       = error $ "Cannot save "++show f++" in Y16 colorspace"
  inYA8 PNG     = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.PixelYA8))
  inYA8 f       = error $ "Cannot save "++show f++" in Y8 colorspace"
  inYA16 PNG    = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.PixelYA16))
  inYA16 f      = error $ "Cannot save "++show f++" in Y16 colorspace"
  inRGB8 BMP    = JP.encodeBitmap . (imageToJPImage (convert :: Gray -> JP.PixelRGB8))
  inRGB8 PNG    = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.PixelRGB8))
  inRGB8 TIFF   = JP.encodeTiff   . (imageToJPImage (convert :: Gray -> JP.PixelRGB8))
  inRGB8 f      = error $ "Cannot save "++show f++" in RGB8 colorspace"
  inRGB16 TIFF  = JP.encodeTiff   . (imageToJPImage (convert :: Gray -> JP.PixelRGB16))
  inRGB16 PNG   = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.PixelRGB16))
  inRGB16 f     = error $ "Cannot save "++show f++" in RGB16 colorspace"
  inRGBA8 BMP   = JP.encodeBitmap . (imageToJPImage (convert :: Gray -> JP.PixelRGBA8))
  inRGBA8 PNG   = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.PixelRGBA8))
  inRGBA8 f     = error $ "Cannot save "++show f++" in RGBA8 colorspace"
  inRGBA16 PNG  = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.PixelRGBA16))
  inRGBA16 f    = error $ "Cannot save "++show f++" in RGBA16 colorspace"
  inYCbCr8 (JPG q)  =
    (JP.encodeJpegAtQuality q) . (imageToJPImage (convert :: Gray -> JP.PixelYCbCr8))
  inYCbCr8 f    = error $ "Cannot save "++show f++" in YCbCr8 colorspace"
  inCMYK8 TIFF  = JP.encodeTiff  . (imageToJPImage (convert :: Gray -> JP.PixelCMYK8))
  inCMYK8 f     = error $ "Cannot save "++show f++" in CMYK8 colorspace"
  inCMYK16 TIFF = JP.encodeTiff  . (imageToJPImage (convert :: Gray -> JP.PixelCMYK16))
  inCMYK16 f    = error $ "Cannot save "++show f++" in CMYK16 colorspace"
  inRGBF HDR    = JP.encodeHDR   . (imageToJPImage (convert :: Gray -> JP.PixelRGBF))
  inRGBF f      = error $ "Cannot save "++show f++" in RGBF colorspace"


instance Image img RGB => Saveable img RGB where
  inY8 BMP      = JP.encodeBitmap . (imageToJPImage (convert :: RGB -> JP.Pixel8))
  inY8 PNG      = JP.encodePng . (imageToJPImage (convert :: RGB -> JP.Pixel8))
  inY8 TIFF     = JP.encodeTiff . (imageToJPImage (convert :: RGB -> JP.Pixel8))
  inY8 f        = error $ "Cannot save "++show f++" in Y8 colorspace"
  inY16 PNG     = JP.encodePng . (imageToJPImage (convert :: RGB -> JP.Pixel16))
  inY16 TIFF    = JP.encodeTiff . (imageToJPImage (convert :: RGB -> JP.Pixel16))
  inY16 f       = error $ "Cannot save "++show f++" in Y16 colorspace"
  inYA8 PNG     = JP.encodePng . (imageToJPImage (convert :: RGB -> JP.PixelYA8))
  inYA8 f       = error $ "Cannot save "++show f++" in Y8 colorspace"
  inYA16 PNG    = JP.encodePng . (imageToJPImage (convert :: RGB -> JP.PixelYA16))
  inYA16 f      = error $ "Cannot save "++show f++" in Y16 colorspace"
  inRGB8 BMP    = JP.encodeBitmap . (imageToJPImage (convert :: RGB -> JP.PixelRGB8))
  inRGB8 PNG    = JP.encodePng . (imageToJPImage (convert :: RGB -> JP.PixelRGB8))
  inRGB8 TIFF   = JP.encodeTiff . (imageToJPImage (convert :: RGB -> JP.PixelRGB8))
  inRGB8 f      = error $ "Cannot save "++show f++" in RGB8 colorspace"
  inRGB16 TIFF  = JP.encodeTiff . (imageToJPImage (convert :: RGB -> JP.PixelRGB16))
  inRGB16 PNG   = JP.encodePng . (imageToJPImage (convert :: RGB -> JP.PixelRGB16))
  inRGB16 f     = error $ "Cannot save "++show f++" in RGB16 colorspace"
  inRGBA8 BMP   = JP.encodeBitmap . (imageToJPImage (convert :: RGB -> JP.PixelRGBA8))
  inRGBA8 PNG   = JP.encodePng . (imageToJPImage (convert :: RGB -> JP.PixelRGBA8))
  inRGBA8 f     = error $ "Cannot save "++show f++" in RGBA8 colorspace"
  inRGBA16 PNG  = JP.encodePng . (imageToJPImage (convert :: RGB -> JP.PixelRGBA16))
  inRGBA16 f    = error $ "Cannot save "++show f++" in RGBA16 colorspace"
  inYCbCr8 (JPG q)  =
    (JP.encodeJpegAtQuality q) . (imageToJPImage (convert :: RGB -> JP.PixelYCbCr8))
  inYCbCr8 f    = error $ "Cannot save "++show f++" in YCbCr8 colorspace"
  inCMYK8 TIFF  = JP.encodeTiff . (imageToJPImage (convert :: RGB -> JP.PixelCMYK8))
  inCMYK8 f     = error $ "Cannot save "++show f++" in CMYK8 colorspace"
  inCMYK16 TIFF = JP.encodeTiff . (imageToJPImage (convert :: RGB -> JP.PixelCMYK16))
  inCMYK16 f    = error $ "Cannot save "++show f++" in CMYK16 colorspace"
  inRGBF HDR    = JP.encodeHDR . (imageToJPImage (convert :: RGB -> JP.PixelRGBF))
  inRGBF f      = error $ "Cannot save "++show f++" in RGBF colorspace"
