{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
TypeFamilies, UndecidableInstances, ViewPatterns #-}
module Graphics.Image.Interface.Conversion (
  Format(..), Mode(..), SaveOption(..), Encoder, Saveable(..), Readable, decodeImage
  ) where

import GHC.Float
import Prelude hiding (map)
import Graphics.Image.Interface (Convertible(..), Pixel(..), Image(..))
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

-- | Output mode for Netpbm formats. Note: Currently writing of Netpbm images is
-- not supported, this is here for future compatibility only at the moment.
data Mode = ASCII | RAW
-- TODO: implement writing for PNM formats

-- | Format types that an image can be saved in.
data Format = BMP       -- ^ Bitmap image with .bmp extension.
            | GIF       -- ^ Graphics Interchange Format image with .gif extension.
            | HDR       -- ^ High-dynamic-range image with .hdr extension.
            | JPG Word8 -- ^ Joint Photographic Experts Group image with .jpg or .jpeg extension. 
                        -- Output quality factor can be specified from 0 to a 100
            | PNG       -- ^ Portable Network Graphics image with .png extension
            | TGA       -- ^ Truevision Graphics Adapter image with .tga extension.
            | TIF       -- ^ Tagged Image File Format image with .tif or .tiff extension
            | PBM Mode  -- ^ Netpbm portable bitmap image with .pbm extension.
            | PGM Mode  -- ^ Netpbm portable graymap image with .pgm extension.
            | PPM Mode  -- ^ Netpbm portable pixmap image with .ppm extension.

instance Show Format where
  show BMP     = "Bitmap"
  show GIF     = "Gif"
  show HDR     = "HDR"
  show (JPG _) = "Jpeg"
  show PNG     = "PNG"
  show TGA     = "TGA"
  show TIF     = "Tiff"
  show (PBM _) = "PBM"
  show (PGM _) = "PGM"
  show (PPM _) = "PPM"
  

-- | Colorspace choice that image will be saved in.
type Encoder img px = Format -> img px -> BL.ByteString


data SaveOption img px = Format Format
                       | Encoder (Encoder img px)


class (Convertible JP.DynamicImage (img px),
       Convertible PNM.PPM         (img px),
       Pixel px, Image img px) => Readable img px where

instance Image img Gray => Readable img Gray where

instance Image img RGB => Readable img RGB where

instance (
  AlphaInner px,
  Readable img px,
  Image img (Alpha px),
  Convertible DynamicImage (img (Alpha px))
  ) => Readable img (Alpha px) where
  
  
-- | Pixels implementing this class allow the images to be saved.
class (Image img px, Pixel px,
       Convertible px Pixel8,
       Convertible px Pixel16,
       Convertible px PixelYA8,
       Convertible px PixelYA16,
       Convertible px PixelRGB8,
       Convertible px PixelRGB16,
       Convertible px PixelRGBA8,
       Convertible px PixelRGBA16,
       Convertible px PixelRGBF,
       Convertible px PixelYCbCr8,
       Convertible px PixelCMYK8,
       Convertible px PixelCMYK16
      ) => Saveable img px where
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

instance Convertible Word8 Gray where
  convert = Gray . fromWord8

instance Convertible Gray Word8 where
  convert (Gray g) = toWord8 g

instance Convertible Word16 Gray where
  convert = Gray . fromWord16

instance Convertible Gray Word16 where
  convert (Gray g) = toWord16 g

instance Convertible Float Gray  where
  convert = Gray . float2Double

instance Convertible Gray Float where
  convert (Gray d) = double2Float d

instance Convertible PixelYA8 Gray where
  convert = convert . dropTransparency

instance Convertible Gray PixelYA8 where
  convert = promotePixel . (convert :: Gray -> Word8)

instance Convertible PixelYA16 Gray where
  convert = convert . dropTransparency

instance Convertible Gray PixelYA16 where
  convert = promotePixel . (convert :: Gray -> Word16)

instance Convertible PixelRGB8 Gray where
  convert = convert . computeLuma

instance Convertible Gray PixelRGB8 where
  convert = convert . (convert :: Gray -> RGB)

instance Convertible PixelRGB16 Gray where
  convert = convert . computeLuma

instance Convertible Gray PixelRGB16 where
  convert = convert . (convert :: Gray -> RGB)

instance Convertible PixelRGBA8 Gray where
  convert = convert . computeLuma

instance Convertible Gray PixelRGBA8 where
  convert = convert . (convert :: Gray -> RGB)

instance Convertible PixelRGBA16 Gray where
  convert = convert . dropTransparency

instance Convertible Gray PixelRGBA16 where
  convert = convert . (convert :: Gray -> RGB)

instance Convertible PixelRGBF Gray where
  convert = convert . computeLuma

instance Convertible Gray PixelRGBF where
  convert = convert . (convert :: Gray -> RGB)

instance Convertible PixelYCbCr8 Gray where
  convert = convert . computeLuma

instance Convertible Gray PixelYCbCr8 where
  convert = convert . (convert :: Gray -> RGB)

instance Convertible PixelCMYK8 Gray where
  convert = convert . (convertPixel :: PixelCMYK8 -> PixelRGB8)

instance Convertible Gray PixelCMYK8 where
  convert = convert . (convert :: Gray -> RGB)

instance Convertible PixelCMYK16 Gray where
  convert = convert . (convertPixel :: PixelCMYK16 -> PixelRGB16)

instance Convertible Gray PixelCMYK16 where
  convert = convert . (convert :: Gray -> RGB)

---- to and from RGB -----

instance Convertible Word8 RGB where
  convert = convert . (convert :: Word8 -> Gray)

instance Convertible RGB Word8 where
  convert = convert . (convert :: RGB -> Gray)

instance Convertible Word16 RGB where
  convert = convert . (convert :: Word16 -> Gray)

instance Convertible RGB Word16 where
  convert = convert . (convert :: RGB -> Gray)
  
instance Convertible Float RGB where
  convert = convert . (convert :: Float -> Gray)

instance Convertible RGB Float where
  convert = convert . (convert :: RGB -> Gray)

instance Convertible PixelYA8 RGB where
  convert = convert . dropTransparency

instance Convertible RGB PixelYA8 where
  convert = convert . (convert :: RGB -> Gray)

instance Convertible PixelYA16 RGB where
  convert = convert . dropTransparency

instance Convertible RGB PixelYA16 where
  convert = convert . (convert :: RGB -> Gray)

instance Convertible PixelRGB8 RGB where
  convert (PixelRGB8 r g b) = RGB (fromWord8 r) (fromWord8 g) (fromWord8 b)

instance Convertible RGB PixelRGB8 where
  convert (RGB r g b) = PixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)

instance Convertible PixelRGB16 RGB where
  convert (PixelRGB16 r g b) = RGB (fromWord16 r) (fromWord16 g) (fromWord16 b)

instance Convertible RGB PixelRGB16 where
  convert (RGB r g b) = PixelRGB16 (toWord16 r) (toWord16 g) (toWord16 b)
  
instance Convertible PixelRGBA8 RGB where
  convert = convert . dropTransparency

instance Convertible RGB PixelRGBA8 where
  convert = promotePixel . (convert :: RGB -> PixelRGB8)

instance Convertible PixelRGBA16 RGB where
  convert = convert . dropTransparency

instance Convertible RGB PixelRGBA16 where
  convert = promotePixel . (convert :: RGB -> PixelRGB16)

instance Convertible PixelRGBF RGB where
  convert (PixelRGBF r g b) = RGB (float2Double r) (float2Double g) (float2Double b)

instance Convertible RGB PixelRGBF where
  convert (RGB r g b) = PixelRGBF (double2Float r) (double2Float g) (double2Float b)

instance Convertible PixelYCbCr8 RGB where
  convert = convert . (convertPixel :: PixelYCbCr8 -> PixelRGB8)

instance Convertible RGB PixelYCbCr8 where
  convert = (convertPixel :: PixelRGB8 -> PixelYCbCr8) . convert

instance Convertible PixelCMYK8 RGB where
  convert = convert . (convertPixel :: PixelCMYK8 -> PixelRGB8)

instance Convertible RGB PixelCMYK8 where
  convert = (convertPixel :: PixelRGB8 -> PixelCMYK8) . convert

instance Convertible PixelCMYK16 RGB where
  convert = convert . (convertPixel :: PixelCMYK16 -> PixelRGB16)

instance Convertible RGB PixelCMYK16 where
  convert = (convertPixel :: PixelRGB16 -> PixelCMYK16) . convert

-- Alpha

instance (
  AlphaInner px, Inner px ~ Double, Convertible Pixel8 px
  ) => Convertible PixelYA8 (Alpha px) where
  convert (PixelYA8 y a) = Alpha (fromWord8 a) (convert y)
  
instance (
  AlphaInner px, Inner px ~ Double, Convertible Pixel16 px
  ) => Convertible PixelYA16 (Alpha px) where
  convert (PixelYA16 y a) = Alpha (fromWord16 a) (convert y)

instance (
  AlphaInner px, Inner px ~ Double, Convertible PixelRGB8 px
  ) => Convertible PixelRGBA8 (Alpha px) where
  convert (PixelRGBA8 r g b a) = Alpha (fromWord8 a) $ convert (PixelRGB8 r g b)
  
instance (
  AlphaInner px, Inner px ~ Double, Convertible PixelRGB16 px
  ) => Convertible PixelRGBA16 (Alpha px) where
  convert (PixelRGBA16 r g b a) = Alpha (fromWord16 a) $ convert (PixelRGB16 r g b)

instance (
  AlphaInner px, Inner px ~ Double, Convertible px Pixel8
  ) => Convertible (Alpha px) PixelYA8 where
  convert (Alpha a px) = PixelYA8 (convert px) (toWord8 a)


instance (
  AlphaInner px, Inner px ~ Double, Convertible px Pixel8
  ) => Convertible (Alpha px) Pixel8 where
  convert (Alpha _ px) = convert px

instance (
  AlphaInner px, Inner px ~ Double, Convertible px Pixel16
  ) => Convertible (Alpha px) Pixel16 where
  convert (Alpha _ px) = convert px
  
instance (
  AlphaInner px, Inner px ~ Double, Convertible px Pixel16
  ) => Convertible (Alpha px) PixelYA16 where
  convert (Alpha a px) = PixelYA16 (convert px) (toWord16 a)

instance (
  AlphaInner px, Inner px ~ Double, Convertible px PixelRGB8
  ) => Convertible (Alpha px) PixelRGB8 where
  convert (Alpha _ px) = convert px

instance (
  AlphaInner px, Inner px ~ Double, Convertible px PixelRGB16
  ) => Convertible (Alpha px) PixelRGB16 where
  convert (Alpha _ px) = convert px

instance (
  AlphaInner px, Inner px ~ Double, Convertible px PixelRGB8
  ) => Convertible (Alpha px) PixelRGBA8 where
  convert (Alpha a px) = getPx $ convert px where
    getPx (PixelRGB8 r g b) = PixelRGBA8 r g b (toWord8 a)

instance (
  AlphaInner px, Inner px ~ Double, Convertible px PixelRGB16
  ) => Convertible (Alpha px) PixelRGBA16 where
  convert (Alpha a px) = getPx $ convert px where
    getPx (PixelRGB16 r g b) = PixelRGBA16 r g b (toWord16 a)

instance (
  AlphaInner px, Inner px ~ Double, Convertible px PixelRGBF
  ) => Convertible (Alpha px) PixelRGBF where
  convert (Alpha _ px) = convert px

instance (
  AlphaInner px, Inner px ~ Double, Convertible px PixelYCbCr8
  ) => Convertible (Alpha px) PixelYCbCr8 where
  convert (Alpha _ px) = convert px

instance (
  AlphaInner px, Inner px ~ Double, Convertible px PixelCMYK8
  ) => Convertible (Alpha px) PixelCMYK8 where
  convert (Alpha _ px) = convert px

instance (
  AlphaInner px, Inner px ~ Double, Convertible px PixelCMYK16
  ) => Convertible (Alpha px) PixelCMYK16 where
  convert (Alpha _ px) = convert px
  

----- JuicyPixels Images --------------------------------------------------------

jpImageToImage :: (Image img px, Pixel px, Convertible px' px, JP.Pixel px') =>
                  JP.Image px' -> img px
jpImageToImage i = make (imageHeight i) (imageWidth i) getPx
  where getPx y x = convert $ pixelAt i x y


instance Image img Gray => Convertible DynamicImage (img Gray) where
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


instance Image img RGB => Convertible DynamicImage (img RGB) where
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


instance (
  Image img RGB,
  Image img (Alpha RGB),
  Convertible DynamicImage (img RGB)
  ) => Convertible DynamicImage (img (Alpha RGB)) where
  convert (ImageYA8 i) = jpImageToImage i
  convert (ImageYA16 i) = jpImageToImage i
  convert (ImageRGBA8 i) = jpImageToImage i
  convert (ImageRGBA16 i) = jpImageToImage i
  convert img = map (Alpha 1) $ convert img

{-
instance (
  AlphaInner px,
  Inner px ~ Double,
  Image img px,
  Image img (Alpha px),
  Convertible DynamicImage (img px),
  Convertible Pixel8 px,
  Convertible Pixel16 px,
  Convertible PixelRGB8 px,
  Convertible PixelRGB16 px
  ) => Convertible DynamicImage (img (Alpha px)) where
  convert (ImageYA8 i) = jpImageToImage i
  convert (ImageYA16 i) = jpImageToImage i
  convert (ImageRGBA8 i) = jpImageToImage i
  convert (ImageRGBA16 i) = jpImageToImage i
  convert img = map (Alpha 1) $ convert img
-}
  
-- Netpbm--------------------------------------------------------------------------

---- to and from Gray -----
  
instance Convertible PNM.PbmPixel Gray where
  convert (PNM.PbmPixel bool) = Gray $ if bool then 1.0 else 0.0
  
instance Convertible PNM.PgmPixel8 Gray where
  convert (PNM.PgmPixel8 w8) = convert w8

instance Convertible Gray PNM.PgmPixel8 where
  convert (Gray g) = PNM.PgmPixel8 $ toWord8 g

instance Convertible PNM.PgmPixel16 Gray where
  convert (PNM.PgmPixel16 w16) = convert w16

instance Convertible Gray PNM.PgmPixel16 where
  convert (Gray g) = PNM.PgmPixel16 $ toWord16 g

instance Convertible PNM.PpmPixelRGB8 Gray where
  convert = convert . (convert :: PNM.PpmPixelRGB8 -> RGB)

instance Convertible Gray PNM.PpmPixelRGB8 where
  convert = convert . (convert :: Gray -> RGB)

instance Convertible PNM.PpmPixelRGB16 Gray where
  convert = convert . (convert :: PNM.PpmPixelRGB16 -> RGB)

instance Convertible Gray PNM.PpmPixelRGB16 where
  convert = convert . (convert :: Gray -> RGB)

---- to and from RGB -----

instance Convertible PNM.PbmPixel RGB where
  convert = convert . (convert :: PNM.PbmPixel -> Gray)
  
instance Convertible PNM.PgmPixel8 RGB where
  convert = convert . (convert :: PNM.PgmPixel8 -> Gray)

instance Convertible RGB PNM.PgmPixel8 where
  convert = convert . (convert :: RGB -> Gray)

instance Convertible PNM.PgmPixel16 RGB where
  convert = convert . (convert :: PNM.PgmPixel16 -> Gray)

instance Convertible RGB PNM.PgmPixel16 where
  convert = convert . (convert :: RGB -> Gray)

instance Convertible PNM.PpmPixelRGB8 RGB where
  convert (PNM.PpmPixelRGB8 r g b) = RGB (fromWord8 r) (fromWord8 g) (fromWord8 b)

instance Convertible RGB PNM.PpmPixelRGB8 where
  convert (RGB r g b) = PNM.PpmPixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)

instance Convertible PNM.PpmPixelRGB16 RGB where
  convert (PNM.PpmPixelRGB16 r g b) = RGB (fromWord16 r) (fromWord16 g) (fromWord16 b)

instance Convertible RGB PNM.PpmPixelRGB16 where
  convert (RGB r g b) = PNM.PpmPixelRGB16 (toWord16 r) (toWord16 g) (toWord16 b)


ppmImageToImage :: (Storable px1, Image img px2, Pixel px2, Convertible px1 px2) =>
                   PNM.PPMHeader -> VS.Vector px1 -> img px2
ppmImageToImage (PNM.PPMHeader _ c r) v = make r c getPx where
  !vectorImage = V.map convert $ VS.convert v
  getPx i j = vectorImage V.! (i*r + j)


decodeJPImageUsing :: (Image img px, Pixel px, Convertible DynamicImage (img px)) =>
                      (B.ByteString -> Either String JP.DynamicImage)
                      -> B.ByteString -> Either String (img px)
decodeJPImageUsing decoder imgstr = 
  either (Left . ("JuicyPixel decoding error: "++)) (Right . convert) $ decoder imgstr

          
decodeNetpbmImage :: (Image img px, Pixel px, Convertible PNM.PPM (img px)) =>
                     B.ByteString -> Either String (img px)
decodeNetpbmImage imgstr = pnmResultToImage $ PNM.parsePPM imgstr where
  pnmResultToImage (Right (pnmLs, _)) = Right $ convert (head pnmLs)
  pnmResultToImage (Left err)         = Left ("Netpbm decoding error: "++err)
          
          
decodeImage :: (Image img px, Pixel px, 
                Convertible PNM.PPM (img px), Convertible DynamicImage (img px)) =>
               Maybe Format -> B.ByteString -> Either String (img px)
decodeImage Nothing imgstr = either tryJP Right $ decodeNetpbmImage imgstr where
  tryJP netpbmErr = either (Left . (unlines . (netpbmErr:) . (:[]))) Right $ 
                    decodeJPImageUsing JP.decodeImage imgstr
decodeImage (Just format) imgstr = updateError . decode $ format where
  updateError (Left err) = Left ("Reading format "++show format++"failed. "++err)
  updateError eitherImg  = eitherImg
  decode BMP     = decodeJPImageUsing JP.decodeBitmap imgstr
  decode GIF     = decodeJPImageUsing JP.decodeGif imgstr
  decode HDR     = decodeJPImageUsing JP.decodeHDR imgstr
  decode (JPG _) = decodeJPImageUsing JP.decodeJpeg imgstr
  decode PNG     = decodeJPImageUsing JP.decodePng imgstr
  decode TGA     = decodeJPImageUsing JP.decodeTga imgstr
  decode TIF     = decodeJPImageUsing JP.decodeTiff imgstr
  decode (PBM _) = decodeNetpbmImage imgstr
  decode (PGM _) = decodeNetpbmImage imgstr
  decode (PPM _) = decodeNetpbmImage imgstr
  

instance Image img Gray => Convertible PNM.PPM (img Gray) where
  convert (PNM.PPM header (PNM.PpmPixelDataRGB8 v))  = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PpmPixelDataRGB16 v)) = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PbmPixelData v))      = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PgmPixelData8 v))     = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PgmPixelData16 v))    = ppmImageToImage header v


instance Image img RGB => Convertible PNM.PPM (img RGB) where
  convert (PNM.PPM header (PNM.PpmPixelDataRGB8 v))  = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PpmPixelDataRGB16 v)) = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PbmPixelData v))      = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PgmPixelData8 v))     = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PgmPixelData16 v))    = ppmImageToImage header v


instance (
  Convertible PNM.PPM (img px), AlphaInner px, Image img (Alpha px), Image img px
  ) => Convertible PNM.PPM (img (Alpha px)) where
  convert = map (Alpha 1) . convert


imageToJPImage :: (Image img px, Pixel px, JP.Pixel px') =>
                  (px -> px') -> img px -> JP.Image px'
imageToJPImage f img@(dims -> (m, n)) =
  JP.generateImage g n m where
    g j i = f $ index img i j


instance Image img Gray => Saveable img Gray where
  inY8 BMP     = JP.encodeBitmap . (imageToJPImage (convert :: Gray -> JP.Pixel8))
  inY8 PNG     = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.Pixel8))
  inY8 TIF     = JP.encodeTiff   . (imageToJPImage (convert :: Gray -> JP.Pixel8))
  inY8 f       = error $ "Cannot save "++show f++" in Y8 colorspace"
  inY16 PNG    = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.Pixel16))
  inY16 TIF    = JP.encodeTiff   . (imageToJPImage (convert :: Gray -> JP.Pixel16))
  inY16 f      = error $ "Cannot save "++show f++" in Y16 colorspace"
  inYA8 PNG    = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.PixelYA8))
  inYA8 f      = error $ "Cannot save "++show f++" in Y8 colorspace"
  inYA16 PNG   = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.PixelYA16))
  inYA16 f     = error $ "Cannot save "++show f++" in Y16 colorspace"
  inRGB8 BMP   = JP.encodeBitmap . (imageToJPImage (convert :: Gray -> JP.PixelRGB8))
  inRGB8 PNG   = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.PixelRGB8))
  inRGB8 TIF   = JP.encodeTiff   . (imageToJPImage (convert :: Gray -> JP.PixelRGB8))
  inRGB8 f     = error $ "Cannot save "++show f++" in RGB8 colorspace"
  inRGB16 TIF  = JP.encodeTiff   . (imageToJPImage (convert :: Gray -> JP.PixelRGB16))
  inRGB16 PNG  = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.PixelRGB16))
  inRGB16 f    = error $ "Cannot save "++show f++" in RGB16 colorspace"
  inRGBF HDR   = JP.encodeHDR    . (imageToJPImage (convert :: Gray -> JP.PixelRGBF))
  inRGBF f     = error $ "Cannot save "++show f++" in RGBF colorspace"
  inRGBA8 BMP  = JP.encodeBitmap . (imageToJPImage (convert :: Gray -> JP.PixelRGBA8))
  inRGBA8 PNG  = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.PixelRGBA8))
  inRGBA8 f    = error $ "Cannot save "++show f++" in RGBA8 colorspace"
  inRGBA16 PNG = JP.encodePng    . (imageToJPImage (convert :: Gray -> JP.PixelRGBA16))
  inRGBA16 f   = error $ "Cannot save "++show f++" in RGBA16 colorspace"
  inYCbCr8 (JPG q) =
    (JP.encodeJpegAtQuality q)    . (imageToJPImage (convert :: Gray -> JP.PixelYCbCr8))
  inYCbCr8 f   = error $ "Cannot save "++show f++" in YCbCr8 colorspace"
  inCMYK8 TIF  = JP.encodeTiff   . (imageToJPImage (convert :: Gray -> JP.PixelCMYK8))
  inCMYK8 f    = error $ "Cannot save "++show f++" in CMYK8 colorspace"
  inCMYK16 TIF = JP.encodeTiff   . (imageToJPImage (convert :: Gray -> JP.PixelCMYK16))
  inCMYK16 f   = error $ "Cannot save "++show f++" in CMYK16 colorspace"

instance Image img RGB => Saveable img RGB where
  inY8 BMP     = JP.encodeBitmap . (imageToJPImage (convert :: RGB -> JP.Pixel8))
  inY8 PNG     = JP.encodePng    . (imageToJPImage (convert :: RGB -> JP.Pixel8))
  inY8 TIF     = JP.encodeTiff   . (imageToJPImage (convert :: RGB -> JP.Pixel8))
  inY8 f       = error $ "Cannot save "++show f++" in Y8 colorspace"
  inY16 PNG    = JP.encodePng    . (imageToJPImage (convert :: RGB -> JP.Pixel16))
  inY16 TIF    = JP.encodeTiff   . (imageToJPImage (convert :: RGB -> JP.Pixel16))
  inY16 f      = error $ "Cannot save "++show f++" in Y16 colorspace"
  inYA8 PNG    = JP.encodePng    . (imageToJPImage (convert :: RGB -> JP.PixelYA8))
  inYA8 f      = error $ "Cannot save "++show f++" in Y8 colorspace"
  inYA16 PNG   = JP.encodePng    . (imageToJPImage (convert :: RGB -> JP.PixelYA16))
  inYA16 f     = error $ "Cannot save "++show f++" in Y16 colorspace"
  inRGB8 BMP   = JP.encodeBitmap . (imageToJPImage (convert :: RGB -> JP.PixelRGB8))
  inRGB8 PNG   = JP.encodePng    . (imageToJPImage (convert :: RGB -> JP.PixelRGB8))
  inRGB8 TIF   = JP.encodeTiff   . (imageToJPImage (convert :: RGB -> JP.PixelRGB8))
  inRGB8 f     = error $ "Cannot save "++show f++" in RGB8 colorspace"
  inRGB16 TIF  = JP.encodeTiff   . (imageToJPImage (convert :: RGB -> JP.PixelRGB16))
  inRGB16 PNG  = JP.encodePng    . (imageToJPImage (convert :: RGB -> JP.PixelRGB16))
  inRGB16 f    = error $ "Cannot save "++show f++" in RGB16 colorspace"
  inRGBF HDR   = JP.encodeHDR    . (imageToJPImage (convert :: RGB -> JP.PixelRGBF))
  inRGBF f     = error $ "Cannot save "++show f++" in RGBF colorspace"
  inRGBA8 BMP  = JP.encodeBitmap . (imageToJPImage (convert :: RGB -> JP.PixelRGBA8))
  inRGBA8 PNG  = JP.encodePng    . (imageToJPImage (convert :: RGB -> JP.PixelRGBA8))
  inRGBA8 f    = error $ "Cannot save "++show f++" in RGBA8 colorspace"
  inRGBA16 PNG = JP.encodePng    . (imageToJPImage (convert :: RGB -> JP.PixelRGBA16))
  inRGBA16 f   = error $ "Cannot save "++show f++" in RGBA16 colorspace"
  inYCbCr8 (JPG q) =
    (JP.encodeJpegAtQuality q)    . (imageToJPImage (convert :: RGB -> JP.PixelYCbCr8))
  inYCbCr8 f   = error $ "Cannot save "++show f++" in YCbCr8 colorspace"
  inCMYK8 TIF  = JP.encodeTiff   . (imageToJPImage (convert :: RGB -> JP.PixelCMYK8))
  inCMYK8 f    = error $ "Cannot save "++show f++" in CMYK8 colorspace"
  inCMYK16 TIF = JP.encodeTiff   . (imageToJPImage (convert :: RGB -> JP.PixelCMYK16))
  inCMYK16 f   = error $ "Cannot save "++show f++" in CMYK16 colorspace"


instance (Saveable img px, AlphaInner px, Image img (Alpha px), Inner px ~ Double)
         => Saveable img (Alpha px) where
  inY8 BMP     = JP.encodeBitmap . (
    imageToJPImage (convert :: Convertible px Pixel8 => px -> Pixel8))
  inY8 PNG     = JP.encodePng    . (
    imageToJPImage (convert :: Convertible px Pixel8 => px -> Pixel8))
  inY8 TIF     = JP.encodeTiff   . (
    imageToJPImage (convert :: Convertible px Pixel8 => px -> Pixel8))
  inY8 f       = error $ "Cannot save "++show f++" in Y8 colorspace"
  inY16 PNG    = JP.encodePng    . (
    imageToJPImage (convert :: Convertible px Pixel16 => px -> Pixel16))
  inY16 TIF    = JP.encodeTiff   . (
    imageToJPImage (convert :: Convertible px Pixel16 => px -> Pixel16))
  inY16 f      = error $ "Cannot save "++show f++" in Y16 colorspace"
  inYA8 PNG    = JP.encodePng    . (
    imageToJPImage (convert :: Convertible px PixelYA8 => px -> PixelYA8))
  inYA8 f      = error $ "Cannot save "++show f++" in Y8 colorspace"
  inYA16 PNG   = JP.encodePng    . (
    imageToJPImage (convert :: Convertible px PixelYA16 => px -> PixelYA16))
  inYA16 f     = error $ "Cannot save "++show f++" in Y16 colorspace"
  inRGB8 BMP   = JP.encodeBitmap . (
    imageToJPImage (convert :: Convertible px PixelRGB8 => px -> PixelRGB8))
  inRGB8 PNG   = JP.encodePng    . (
    imageToJPImage (convert :: Convertible px PixelRGB8 => px -> PixelRGB8))
  inRGB8 TIF   = JP.encodeTiff   . (
    imageToJPImage (convert :: Convertible px PixelRGB8 => px -> PixelRGB8))
  inRGB8 f     = error $ "Cannot save "++show f++" in RGB8 colorspace"
  inRGB16 TIF  = JP.encodeTiff   . (
    imageToJPImage (convert :: Convertible px PixelRGB16 => px -> PixelRGB16))
  inRGB16 PNG  = JP.encodePng    . (
    imageToJPImage (convert :: Convertible px PixelRGB16 => px -> PixelRGB16))
  inRGB16 f    = error $ "Cannot save "++show f++" in RGB16 colorspace"
  inRGBA8 BMP  = JP.encodeBitmap . (
    imageToJPImage (convert :: Convertible px PixelRGBA8 => px -> PixelRGBA8))
  inRGBA8 PNG  = JP.encodePng    . (
    imageToJPImage (convert :: Convertible px PixelRGBA8 => px -> PixelRGBA8))
  inRGBA8 f    = error $ "Cannot save "++show f++" in RGBA8 colorspace"
  inRGBA16 PNG = JP.encodePng    . (
    imageToJPImage (convert :: Convertible px PixelRGBA16 => px -> PixelRGBA16))    
  inRGBA16 f   = error $ "Cannot save "++show f++" in RGBA16 colorspace"
  inRGBF HDR   = JP.encodeHDR    . (
    imageToJPImage (convert :: Convertible px PixelRGBF => px -> PixelRGBF))
  inRGBF f     = error $ "Cannot save "++show f++" in RGBF colorspace"
  inYCbCr8 (JPG q) = (JP.encodeJpegAtQuality q)   . (
        imageToJPImage (convert :: Convertible px PixelYCbCr8 => px -> PixelYCbCr8))
  inYCbCr8 f   = error $ "Cannot save "++show f++" in YCbCr8 colorspace"
  inCMYK8 TIF  = JP.encodeTiff   . (
        imageToJPImage (convert :: Convertible px PixelCMYK8 => px -> PixelCMYK8))
  inCMYK8 f    = error $ "Cannot save "++show f++" in CMYK8 colorspace"
  inCMYK16 TIF = JP.encodeTiff   . (
        imageToJPImage (convert :: Convertible px PixelCMYK16 => px -> PixelCMYK16))
  inCMYK16 f   = error $ "Cannot save "++show f++" in CMYK16 colorspace"


{-
instance (
  Saveable img px, AlphaInner px, Double ~ Inner (Alpha px), Image img (Alpha px)
  ) => Saveable img (Alpha px) where
  inY8 f       = inY8 f . map dropAlpha
  inY16 f      = inY16 f . map dropAlpha

  inYA8 PNG    = JP.encodePng    . (imageToJPImage (convert :: (Alpha px) -> JP.PixelYA8))
  inYA8 f      = error $ "Cannot save "++show f++" in Y8 colorspace"
  inYA16 PNG   = JP.encodePng    . (imageToJPImage (convert :: (Alpha px) -> JP.PixelYA16))
  inYA16 f     = error $ "Cannot save "++show f++" in Y16 colorspace"

  inRGB8 f     = inRGB8 f . map dropAlpha
  inRGB16 f    = inRGB16 f . map dropAlpha
  inRGBF f     = inRGBF f . map dropAlpha

  inRGBA8 BMP  = JP.encodeBitmap . (imageToJPImage (convert :: (Alpha px) -> JP.PixelRGBA8))
  inRGBA8 PNG  = JP.encodePng    . (imageToJPImage (convert :: (Alpha px) -> JP.PixelRGBA8))
  inRGBA8 f    = error $ "Cannot save "++show f++" in RGBA8 colorspace"
  inRGBA16 PNG = JP.encodePng    . (imageToJPImage (convert :: (Alpha px) -> JP.PixelRGBA16))
  inRGBA16 f   = error $ "Cannot save "++show f++" in RGBA16 colorspace"
  
  inYCbCr8 f    = inYCbCr8 f . map dropAlpha
  inCMYK8 f     = inCMYK8 f . map dropAlpha
  inCMYK16 f    = inCMYK16 f . map dropAlpha

-}
