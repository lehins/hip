{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
TypeFamilies, UndecidableInstances, ViewPatterns #-}
module Graphics.Image.Interface.External (
  InputFormat(..), OutputFormat(..), SaveOption(..), Encoder, Saveable(..), Readable, decodeImage
  ) where

import GHC.Float
import Prelude hiding (map)
import Graphics.Image.Interface (Pixel(..), AImage(..))
import Graphics.Image.Interface.Pixel hiding (Pixel)
import qualified Graphics.Image.Interface.Binary as B (fromBinary)
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


class Interconvertible a b where
  convert :: a -> b

-- | Format types that an image can be read in.
data InputFormat =
  BMPin   -- ^ Bitmap image with .bmp extension.
  | GIFin -- ^ Graphics Interchange Format image with .gif extension.
  | HDRin -- ^ High-dynamic-range image with .hdr extension.
  | JPGin -- ^ Joint Photographic Experts Group image with .jpg or .jpeg
          -- extension. Output quality factor can be specified from 0 to a 100
  | PNGin -- ^ Portable Network Graphics image with .png extension
  | TGAin -- ^ Truevision Graphics Adapter image with .tga extension.
  | TIFin -- ^ Tagged Image File Format image with .tif or .tiff extension
  | PBMin -- ^ Netpbm portable bitmap image with .pbm extension.
  | PGMin -- ^ Netpbm portable graymap image with .pgm extension.
  | PPMin -- ^ Netpbm portable pixmap image with .ppm extension.


instance Show InputFormat where
  show BMPin = "Bitmap"
  show GIFin = "Gif"
  show HDRin = "HDR"
  show JPGin = "Jpeg"
  show PNGin = "PNG"
  show TGAin = "TGA"
  show TIFin = "Tiff"
  show PBMin = "PBM"
  show PGMin = "PGM"
  show PPMin = "PPM"


-- | Output mode for Netpbm formats. Note: Currently writing of Netpbm images is
-- not supported, this is here for future compatibility only at the moment.

--data NetpbmMode = ASCII | RAW
-- TODO: implement writing for PNM formats


-- | Format types that an image can be saved in.
data OutputFormat =
  BMP               -- ^ Bitmap image with @.bmp@ extension.
  
  -- \ | GIF             -- ^ Graphics Interchange Format image with @.gif@ extension.
  | HDR             -- ^ High-dynamic-range image with @.hdr@ extension.
  | JPG Word8       -- ^ Joint Photographic Experts Group image with @.jpg@ or
                    -- @.jpeg@ extension. Output quality factor can be specified
                    -- from 0 to a 100.
  | PNG             -- ^ Portable Network Graphics image with @.png@ extension.
    
  -- \ | TGA             -- ^ Truevision Graphics Adapter image with .tga extension.
  | TIF             -- ^ Tagged Image File Format image with @.tif@ or @.tiff@
                    -- extension.
    
  -- \ | PBM NetpbmMode  -- ^ Netpbm portable bitmap image with .pbm extension.
  -- \ | PGM NetpbmMode  -- ^ Netpbm portable graymap image with .pgm extension.
  -- \ | PPM NetpbmMode  -- ^ Netpbm portable pixmap image with .ppm extension.

    
instance Show OutputFormat where
  show BMP     = "Bitmap"
  --show GIF     = "Gif"
  show HDR     = "HDR"
  show (JPG _) = "Jpeg"
  show PNG     = "PNG"
  --show TGA     = "TGA"
  show TIF     = "Tiff"
  --show (PBM _) = "PBM"
  --show (PGM _) = "PGM"
  --show (PPM _) = "PPM"
  

-- | Colorspace choice that image will be saved in.
type Encoder img px = OutputFormat -> img px -> BL.ByteString


data SaveOption img px = Format OutputFormat
                       | Encoder (Encoder img px)


class (Interconvertible JP.DynamicImage (img px),
       Interconvertible PNM.PPM         (img px),
       Pixel px, AImage img px) => Readable img px where

instance AImage img Gray => Readable img Gray where

instance AImage img RGB => Readable img RGB where

instance (
  AlphaInner px,
  Readable img px,
  AImage img (Alpha px),
  Interconvertible DynamicImage (img (Alpha px))
  ) => Readable img (Alpha px) where
  
  
-- | Pixels implementing this class allow the images to be saved.
class (AImage img px, Pixel px) => Saveable img px where
  -- | Save as 8-bit grayscale. Supported formats 'BMP', 'PNG' and 'TIF'.
  inY8     :: Encoder img px
  -- | Save as 16-bit grayscale. Supported formats 'PNG' and 'TIF'.
  inY16    :: Encoder img px
  -- | Save as 8-bit grayscale with Alpha channel. Supported format 'PNG'.
  inYA8    :: Encoder img px
  -- | Save as 16-bit grayscale with Alpha channel. Supported format 'PNG'.
  inYA16   :: Encoder img px
  -- | Save in RGB colorspace with 8-bit precision. Supported formats 'BMP',
  -- 'PNG' and 'TIF'.
  inRGB8   :: Encoder img px
  -- | Save in RGB colorspace with 16-bit precision. Supported formats 'PNG' and
  -- 'TIF'.
  inRGB16  :: Encoder img px
  -- | Save in RGB colorspace with floating precision. Supported format 'HDR'.
  inRGBF   :: Encoder img px
  -- | Save in RGB colorspace and an Alpha channel with 8-bit
  -- precision. Supported formats 'BMP' and 'PNG'.
  inRGBA8  :: Encoder img px
  -- | Save in RGB colorspace and an Alpha channel with 16-bit
  -- precision. Supported format 'PNG'.
  inRGBA16 :: Encoder img px
  -- | Save in YCbCr colorspace with 8-bit precision. Supported format 'JPG'.
  inYCbCr8 :: Encoder img px
  -- | Save in CMYK colorspace with 8-bit precision. Supported format 'TIF'.
  inCMYK8  :: Encoder img px
  -- | Save in YCbCr colorspace with 16-bit precision. Supported format 'TIF'.
  inCMYK16 :: Encoder img px

-- =============================================================================
-- = Conversion ================================================================
-- =============================================================================

-- Helper Functions ------------------------------------------------------------

fromWord8 :: Word8 -> Double
fromWord8 px = (fromIntegral px) / 255
toWord8 :: Double -> Word8
toWord8 px = round (255*px)

fromWord16 :: Word16 -> Double
fromWord16 px = fromIntegral px / 65535
toWord16 :: Double -> Word16
toWord16 px = round (65535*px)


--------------------------------------------------------------------------------
-- JuicyPixel ------------------------------------------------------------------
--------------------------------------------------------------------------------

---- from JuicyPixels (Pixels) -----

-- Gray -> Gray

instance Interconvertible Word8 Gray where
  convert = Gray . fromWord8

instance Interconvertible Word16 Gray where
  convert = Gray . fromWord16

instance Interconvertible Float Gray  where
  convert = Gray . float2Double

instance Interconvertible PixelYA8 Gray where
  convert = convert . dropTransparency

instance Interconvertible PixelYA16 Gray where
  convert = convert . dropTransparency

-- Color -> Gray

instance Interconvertible PixelRGB8 Gray where
  convert = convert . computeLuma

instance Interconvertible PixelRGB16 Gray where
  convert = convert . computeLuma

instance Interconvertible PixelRGBA8 Gray where
  convert = convert . computeLuma

instance Interconvertible PixelRGBA16 Gray where
  convert = convert . dropTransparency

instance Interconvertible PixelRGBF Gray where
  convert = convert . computeLuma

instance Interconvertible PixelYCbCr8 Gray where
  convert = convert . computeLuma

instance Interconvertible PixelCMYK8 Gray where
  convert = convert . (convertPixel :: PixelCMYK8 -> PixelRGB8)

instance Interconvertible PixelCMYK16 Gray where
  convert = convert . (convertPixel :: PixelCMYK16 -> PixelRGB16)

-- Gray -> Color

instance Interconvertible Word8 RGB where
  convert = grayToRGB . (convert :: Word8 -> Gray)

instance Interconvertible Word16 RGB where
  convert = grayToRGB . (convert :: Word16 -> Gray)

instance Interconvertible Float RGB where
  convert = grayToRGB . (convert :: Float -> Gray)

instance Interconvertible PixelYA8 RGB where
  convert = convert . dropTransparency

instance Interconvertible PixelYA16 RGB where
  convert = convert . dropTransparency

-- Color -> Color

instance Interconvertible PixelRGB8 RGB where
  convert (PixelRGB8 r g b) = RGB (fromWord8 r) (fromWord8 g) (fromWord8 b)

instance Interconvertible PixelRGB16 RGB where
  convert (PixelRGB16 r g b) = RGB (fromWord16 r) (fromWord16 g) (fromWord16 b)

instance Interconvertible PixelRGBA8 RGB where
  convert = convert . dropTransparency

instance Interconvertible PixelRGBA16 RGB where
  convert = convert . dropTransparency

instance Interconvertible PixelRGBF RGB where
  convert (PixelRGBF r g b) =
    RGB (float2Double r) (float2Double g) (float2Double b)

instance Interconvertible PixelYCbCr8 RGB where
  convert = convert . (convertPixel :: PixelYCbCr8 -> PixelRGB8)

instance Interconvertible PixelCMYK8 RGB where
  convert = convert . (convertPixel :: PixelCMYK8 -> PixelRGB8)

instance Interconvertible PixelCMYK16 RGB where
  convert = convert . (convertPixel :: PixelCMYK16 -> PixelRGB16)

-- Alpha -> Alpha

instance (
  AlphaInner px, Inner px ~ Double, Interconvertible Pixel8 px
  ) => Interconvertible PixelYA8 (Alpha px) where
  convert (PixelYA8 y a) = Alpha (fromWord8 a) (convert y)
  
instance (
  AlphaInner px, Inner px ~ Double, Interconvertible Pixel16 px
  ) => Interconvertible PixelYA16 (Alpha px) where
  convert (PixelYA16 y a) = Alpha (fromWord16 a) (convert y)

instance (
  AlphaInner px, Inner px ~ Double, Interconvertible PixelRGB8 px
  ) => Interconvertible PixelRGBA8 (Alpha px) where
  convert (PixelRGBA8 r g b a) = Alpha (fromWord8 a) $ convert (PixelRGB8 r g b)
  
instance (
  AlphaInner px, Inner px ~ Double, Interconvertible PixelRGB16 px
  ) => Interconvertible PixelRGBA16 (Alpha px) where
  convert (PixelRGBA16 r g b a) = Alpha (fromWord16 a) $ convert (PixelRGB16 r g b)


---- to JuicyPixels -----


-- Gray -> Gray

instance Interconvertible Gray Word8 where
  convert (Gray g) = toWord8 g

instance Interconvertible Gray Word16 where
  convert (Gray g) = toWord16 g

instance Interconvertible Gray Float where
  convert (Gray d) = double2Float d

instance Interconvertible Gray PixelYA8 where
  convert = promotePixel . (convert :: Gray -> Word8)

instance Interconvertible Gray PixelYA16 where
  convert = promotePixel . (convert :: Gray -> Word16)

-- Gray -> Color

instance Interconvertible Gray PixelRGB8 where
  convert = convert . grayToRGB

instance Interconvertible Gray PixelRGB16 where
  convert = convert . grayToRGB

instance Interconvertible Gray PixelRGBA8 where
  convert = convert . grayToRGB

instance Interconvertible Gray PixelRGBA16 where
  convert = convert . grayToRGB

instance Interconvertible Gray PixelRGBF where
  convert = convert . grayToRGB

instance Interconvertible Gray PixelYCbCr8 where
  convert = convert . grayToRGB

instance Interconvertible Gray PixelCMYK8 where
  convert = convert . grayToRGB

instance Interconvertible Gray PixelCMYK16 where
  convert = convert . grayToRGB

-- Color -> Gray

instance Interconvertible RGB Word8 where
  convert = convert . rgbToGray

instance Interconvertible RGB Word16 where
  convert = convert . rgbToGray
  
instance Interconvertible RGB Float where
  convert = convert . rgbToGray

instance Interconvertible RGB PixelYA8 where
  convert = convert . rgbToGray

instance Interconvertible RGB PixelYA16 where
  convert = convert . rgbToGray

-- Color -> Color

instance Interconvertible RGB PixelRGB8 where
  convert (RGB r g b) = PixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)

instance Interconvertible RGB PixelRGB16 where
  convert (RGB r g b) = PixelRGB16 (toWord16 r) (toWord16 g) (toWord16 b)
  
instance Interconvertible RGB PixelRGBA8 where
  convert = promotePixel . (convert :: RGB -> PixelRGB8)

instance Interconvertible RGB PixelRGBA16 where
  convert = promotePixel . (convert :: RGB -> PixelRGB16)

instance Interconvertible RGB PixelRGBF where
  convert (RGB r g b) =
    PixelRGBF (double2Float r) (double2Float g) (double2Float b)

instance Interconvertible RGB PixelYCbCr8 where
  convert = (convertPixel :: PixelRGB8 -> PixelYCbCr8) . convert

instance Interconvertible RGB PixelCMYK8 where
  convert = (convertPixel :: PixelRGB8 -> PixelCMYK8) . convert

instance Interconvertible RGB PixelCMYK16 where
  convert = (convertPixel :: PixelRGB16 -> PixelCMYK16) . convert

-- Alpha -> All JuicyPixels 

instance (
  AlphaInner px, Inner px ~ Double, Interconvertible px Pixel8
  ) => Interconvertible (Alpha px) PixelYA8 where
  convert (Alpha a px) = PixelYA8 (convert px) (toWord8 a)


instance (
  AlphaInner px, Inner px ~ Double, Interconvertible px Pixel8
  ) => Interconvertible (Alpha px) Pixel8 where
  convert (Alpha _ px) = convert px

instance (
  AlphaInner px, Inner px ~ Double, Interconvertible px Pixel16
  ) => Interconvertible (Alpha px) Pixel16 where
  convert (Alpha _ px) = convert px
  
instance (
  AlphaInner px, Inner px ~ Double, Interconvertible px Pixel16
  ) => Interconvertible (Alpha px) PixelYA16 where
  convert (Alpha a px) = PixelYA16 (convert px) (toWord16 a)

instance (
  AlphaInner px, Inner px ~ Double, Interconvertible px PixelRGB8
  ) => Interconvertible (Alpha px) PixelRGB8 where
  convert (Alpha _ px) = convert px

instance (
  AlphaInner px, Inner px ~ Double, Interconvertible px PixelRGB16
  ) => Interconvertible (Alpha px) PixelRGB16 where
  convert (Alpha _ px) = convert px

instance (
  AlphaInner px, Inner px ~ Double, Interconvertible px PixelRGB8
  ) => Interconvertible (Alpha px) PixelRGBA8 where
  convert (Alpha a px) = getPx $ convert px where
    getPx (PixelRGB8 r g b) = PixelRGBA8 r g b (toWord8 a)

instance (
  AlphaInner px, Inner px ~ Double, Interconvertible px PixelRGB16
  ) => Interconvertible (Alpha px) PixelRGBA16 where
  convert (Alpha a px) = getPx $ convert px where
    getPx (PixelRGB16 r g b) = PixelRGBA16 r g b (toWord16 a)

instance (
  AlphaInner px, Inner px ~ Double, Interconvertible px PixelRGBF
  ) => Interconvertible (Alpha px) PixelRGBF where
  convert (Alpha _ px) = convert px

instance (
  AlphaInner px, Inner px ~ Double, Interconvertible px PixelYCbCr8
  ) => Interconvertible (Alpha px) PixelYCbCr8 where
  convert (Alpha _ px) = convert px

instance (
  AlphaInner px, Inner px ~ Double, Interconvertible px PixelCMYK8
  ) => Interconvertible (Alpha px) PixelCMYK8 where
  convert (Alpha _ px) = convert px

instance (
  AlphaInner px, Inner px ~ Double, Interconvertible px PixelCMYK16
  ) => Interconvertible (Alpha px) PixelCMYK16 where
  convert (Alpha _ px) = convert px
  

---- from JuicyPixels (Images) -----

jpImageToImage :: (AImage img px, Pixel px, Interconvertible px' px, JP.Pixel px') =>
                  JP.Image px' -> img px
jpImageToImage i = make (imageHeight i) (imageWidth i) getPx
  where getPx y x = convert $ pixelAt i x y


instance AImage img Gray => Interconvertible DynamicImage (img Gray) where
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


instance AImage img RGB => Interconvertible DynamicImage (img RGB) where
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


instance (AImage img RGB,
          AImage img (Alpha RGB),
          Interconvertible DynamicImage (img RGB)
         ) => Interconvertible DynamicImage (img (Alpha RGB)) where
  convert (ImageYA8 i) = jpImageToImage i
  convert (ImageYA16 i) = jpImageToImage i
  convert (ImageRGBA8 i) = jpImageToImage i
  convert (ImageRGBA16 i) = jpImageToImage i
  convert img = map (Alpha 1) $ convert img


instance (AImage img Gray,
          AImage img (Alpha Gray),
          Interconvertible DynamicImage (img Gray)
         ) => Interconvertible DynamicImage (img (Alpha Gray)) where
  convert (ImageYA8 i) = jpImageToImage i
  convert (ImageYA16 i) = jpImageToImage i
  convert (ImageRGBA8 i) = jpImageToImage i
  convert (ImageRGBA16 i) = jpImageToImage i
  convert img = map (Alpha 1) $ convert img

  
--------------------------------------------------------------------------------
-- Netpbm ------------------------------------------------------------------
--------------------------------------------------------------------------------

---- from Netpbm (pixels) ----

  
instance Interconvertible PNM.PbmPixel Gray where
  convert (PNM.PbmPixel bool) = Gray $ if bool then 1.0 else 0.0
  
instance Interconvertible PNM.PgmPixel8 Gray where
  convert (PNM.PgmPixel8 w8) = convert w8

instance Interconvertible PNM.PgmPixel16 Gray where
  convert (PNM.PgmPixel16 w16) = convert w16

instance Interconvertible PNM.PpmPixelRGB8 Gray where
  convert = rgbToGray . (convert :: PNM.PpmPixelRGB8 -> RGB)

instance Interconvertible PNM.PpmPixelRGB16 Gray where
  convert = rgbToGray . (convert :: PNM.PpmPixelRGB16 -> RGB)

instance Interconvertible PNM.PbmPixel RGB where
  convert = grayToRGB . (convert :: PNM.PbmPixel -> Gray)
  
instance Interconvertible PNM.PgmPixel8 RGB where
  convert = grayToRGB . (convert :: PNM.PgmPixel8 -> Gray)

instance Interconvertible PNM.PgmPixel16 RGB where
  convert = grayToRGB . (convert :: PNM.PgmPixel16 -> Gray)

instance Interconvertible PNM.PpmPixelRGB8 RGB where
  convert (PNM.PpmPixelRGB8 r g b) = RGB (fromWord8 r) (fromWord8 g) (fromWord8 b)

instance Interconvertible PNM.PpmPixelRGB16 RGB where
  convert (PNM.PpmPixelRGB16 r g b) = RGB (fromWord16 r) (fromWord16 g) (fromWord16 b)


---- to Netpbm (pixels) ----

instance Interconvertible Gray PNM.PgmPixel8 where
  convert (Gray g) = PNM.PgmPixel8 $ toWord8 g

instance Interconvertible Gray PNM.PgmPixel16 where
  convert (Gray g) = PNM.PgmPixel16 $ toWord16 g

instance Interconvertible Gray PNM.PpmPixelRGB8 where
  convert = convert . grayToRGB

instance Interconvertible Gray PNM.PpmPixelRGB16 where
  convert = convert . grayToRGB

instance Interconvertible RGB PNM.PgmPixel8 where
  convert = convert . rgbToGray

instance Interconvertible RGB PNM.PgmPixel16 where
  convert = convert . rgbToGray

instance Interconvertible RGB PNM.PpmPixelRGB8 where
  convert (RGB r g b) = PNM.PpmPixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)

instance Interconvertible RGB PNM.PpmPixelRGB16 where
  convert (RGB r g b) = PNM.PpmPixelRGB16 (toWord16 r) (toWord16 g) (toWord16 b)


--- from Netpbm (images)

instance AImage img Gray => Interconvertible PNM.PPM (img Gray) where
  convert (PNM.PPM header (PNM.PpmPixelDataRGB8 v))  = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PpmPixelDataRGB16 v)) = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PbmPixelData v))      = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PgmPixelData8 v))     = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PgmPixelData16 v))    = ppmImageToImage header v


instance AImage img RGB => Interconvertible PNM.PPM (img RGB) where
  convert (PNM.PPM header (PNM.PpmPixelDataRGB8 v))  = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PpmPixelDataRGB16 v)) = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PbmPixelData v))      = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PgmPixelData8 v))     = ppmImageToImage header v
  convert (PNM.PPM header (PNM.PgmPixelData16 v))    = ppmImageToImage header v


instance (
  Interconvertible PNM.PPM (img px), AlphaInner px, AImage img (Alpha px), AImage img px
  ) => Interconvertible PNM.PPM (img (Alpha px)) where
  convert = map (Alpha 1) . convert


--------------------------------------------------------------------------------
-- Decoding images from all ----------------------------------------------------
--------------------------------------------------------------------------------


ppmImageToImage :: (Storable px1, AImage img px2, Pixel px2, Interconvertible px1 px2) =>
                   PNM.PPMHeader -> VS.Vector px1 -> img px2
ppmImageToImage (PNM.PPMHeader _ c r) v = make r c getPx where
  !vectorImage = V.map convert $ VS.convert v
  getPx i j = vectorImage V.! (i*r + j)


decodeJPImageUsing :: (AImage img px, Pixel px, Interconvertible DynamicImage (img px)) =>
                      (B.ByteString -> Either String JP.DynamicImage)
                      -> B.ByteString -> Either String (img px)
decodeJPImageUsing decoder imgstr = 
  either (Left . ("JuicyPixel decoding error: "++)) (Right . convert) $ decoder imgstr

          
decodeNetpbmImage :: (AImage img px, Pixel px, Interconvertible PNM.PPM (img px)) =>
                     B.ByteString -> Either String (img px)
decodeNetpbmImage imgstr = pnmResultToImage $ PNM.parsePPM imgstr where
  pnmResultToImage (Right (pnmLs, _)) = Right $ convert (head pnmLs)
  pnmResultToImage (Left err)         = Left ("Netpbm decoding error: "++err)
          
          
decodeImage :: (AImage img px, Pixel px, 
                Interconvertible PNM.PPM (img px), Interconvertible DynamicImage (img px)) =>
               Maybe InputFormat -> B.ByteString -> Either String (img px)
decodeImage Nothing imgstr = either tryJP Right $ decodeNetpbmImage imgstr where
  tryJP netpbmErr = either (Left . (unlines . (netpbmErr:) . (:[]))) Right $ 
                    decodeJPImageUsing JP.decodeImage imgstr
decodeImage (Just format) imgstr = updateError . decode $ format where
  updateError (Left err) = Left ("Reading format "++show format++"failed. "++err)
  updateError eitherImg  = eitherImg
  decode BMPin = decodeJPImageUsing JP.decodeBitmap imgstr
  decode GIFin = decodeJPImageUsing JP.decodeGif imgstr
  decode HDRin = decodeJPImageUsing JP.decodeHDR imgstr
  decode JPGin = decodeJPImageUsing JP.decodeJpeg imgstr
  decode PNGin = decodeJPImageUsing JP.decodePng imgstr
  decode TGAin = decodeJPImageUsing JP.decodeTga imgstr
  decode TIFin = decodeJPImageUsing JP.decodeTiff imgstr
  decode PBMin = decodeNetpbmImage imgstr
  decode PGMin = decodeNetpbmImage imgstr
  decode PPMin = decodeNetpbmImage imgstr
  

--------------------------------------------------------------------------------
-- Encoding images to JuicyPixels ----------------------------------------------
--------------------------------------------------------------------------------

imageToJPImage :: (AImage img px, Pixel px, JP.Pixel px') =>
                  (px -> px') -> img px -> JP.Image px'
imageToJPImage f img@(dims -> (m, n)) =
  JP.generateImage g n m where
    g j i = f $ index img i j


fromBinary :: (AImage img Binary, AImage img Gray) => img Binary -> img Gray
fromBinary = B.fromBinary


instance (AImage img Gray, AImage img Binary) => Saveable img Binary where
  inY8 f     = inY8 f . fromBinary
  inY16 f    = inY16 f . fromBinary
  inYA8 f    = inYA8 f . fromBinary
  inYA16 f   = inYA16 f . fromBinary
  inRGB8 f   = inRGB8 f . fromBinary 
  inRGB16 f  = inRGB16 f . fromBinary 
  inRGBF f   = inRGBF f . fromBinary 
  inRGBA8 f  = inRGBA8 f . fromBinary 
  inRGBA16 f = inRGBA16 f . fromBinary 
  inYCbCr8 f = inYCbCr8 f . fromBinary 
  inCMYK8 f  = inCMYK8 f . fromBinary 
  inCMYK16 f = inCMYK16 f . fromBinary 

instance AImage img Gray => Saveable img Gray where
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


instance AImage img RGB => Saveable img RGB where
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
  inRGB16 PNG  = JP.encodePng    . (imageToJPImage (convert :: RGB -> JP.PixelRGB16))
  inRGB16 TIF  = JP.encodeTiff   . (imageToJPImage (convert :: RGB -> JP.PixelRGB16))
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


instance (Saveable img px, AlphaInner px, AImage img (Alpha px), Double ~ Inner px,
          Interconvertible px Pixel8,
          Interconvertible px Pixel16,
          Interconvertible px PixelYA8,
          Interconvertible px PixelYA16,
          Interconvertible px PixelRGB8,
          Interconvertible px PixelRGB16,
          Interconvertible px PixelRGBA8,
          Interconvertible px PixelRGBA16,
          Interconvertible px PixelRGBF,
          Interconvertible px PixelYCbCr8,
          Interconvertible px PixelCMYK8,
          Interconvertible px PixelCMYK16
         ) => Saveable img (Alpha px) where
  inY8 BMP     = JP.encodeBitmap . (
    imageToJPImage (convert :: Interconvertible px Pixel8 => px -> Pixel8))
  inY8 PNG     = JP.encodePng    . (
    imageToJPImage (convert :: Interconvertible px Pixel8 => px -> Pixel8))
  inY8 TIF     = JP.encodeTiff   . (
    imageToJPImage (convert :: Interconvertible px Pixel8 => px -> Pixel8))
  inY8 f       = error $ "Cannot save "++show f++" in Y8 colorspace"
  inY16 PNG    = JP.encodePng    . (
    imageToJPImage (convert :: Interconvertible px Pixel16 => px -> Pixel16))
  inY16 TIF    = JP.encodeTiff   . (
    imageToJPImage (convert :: Interconvertible px Pixel16 => px -> Pixel16))
  inY16 f      = error $ "Cannot save "++show f++" in Y16 colorspace"
  inYA8 PNG    = JP.encodePng    . (
    imageToJPImage (convert :: Interconvertible px PixelYA8 => px -> PixelYA8))
  inYA8 f      = error $ "Cannot save "++show f++" in Y8 colorspace"
  inYA16 PNG   = JP.encodePng    . (
    imageToJPImage (convert :: Interconvertible px PixelYA16 => px -> PixelYA16))
  inYA16 f     = error $ "Cannot save "++show f++" in Y16 colorspace"
  inRGB8 BMP   = JP.encodeBitmap . (
    imageToJPImage (convert :: Interconvertible px PixelRGB8 => px -> PixelRGB8))
  inRGB8 PNG   = JP.encodePng    . (
    imageToJPImage (convert :: Interconvertible px PixelRGB8 => px -> PixelRGB8))
  inRGB8 TIF   = JP.encodeTiff   . (
    imageToJPImage (convert :: Interconvertible px PixelRGB8 => px -> PixelRGB8))
  inRGB8 f     = error $ "Cannot save "++show f++" in RGB8 colorspace"
  inRGB16 TIF  = JP.encodeTiff   . (
    imageToJPImage (convert :: Interconvertible px PixelRGB16 => px -> PixelRGB16))
  inRGB16 PNG  = JP.encodePng    . (
    imageToJPImage (convert :: Interconvertible px PixelRGB16 => px -> PixelRGB16))
  inRGB16 f    = error $ "Cannot save "++show f++" in RGB16 colorspace"
  inRGBA8 BMP  = JP.encodeBitmap . (
    imageToJPImage (convert :: Interconvertible px PixelRGBA8 => px -> PixelRGBA8))
  inRGBA8 PNG  = JP.encodePng    . (
    imageToJPImage (convert :: Interconvertible px PixelRGBA8 => px -> PixelRGBA8))
  inRGBA8 f    = error $ "Cannot save "++show f++" in RGBA8 colorspace"
  inRGBA16 PNG = JP.encodePng    . (
    imageToJPImage (convert :: Interconvertible px PixelRGBA16 => px -> PixelRGBA16))    
  inRGBA16 f   = error $ "Cannot save "++show f++" in RGBA16 colorspace"
  inRGBF HDR   = JP.encodeHDR    . (
    imageToJPImage (convert :: Interconvertible px PixelRGBF => px -> PixelRGBF))
  inRGBF f     = error $ "Cannot save "++show f++" in RGBF colorspace"
  inYCbCr8 (JPG q) = (JP.encodeJpegAtQuality q)   . (
        imageToJPImage (convert :: Interconvertible px PixelYCbCr8 => px -> PixelYCbCr8))
  inYCbCr8 f   = error $ "Cannot save "++show f++" in YCbCr8 colorspace"
  inCMYK8 TIF  = JP.encodeTiff   . (
        imageToJPImage (convert :: Interconvertible px PixelCMYK8 => px -> PixelCMYK8))
  inCMYK8 f    = error $ "Cannot save "++show f++" in CMYK8 colorspace"
  inCMYK16 TIF = JP.encodeTiff   . (
        imageToJPImage (convert :: Interconvertible px PixelCMYK16 => px -> PixelCMYK16))
  inCMYK16 f   = error $ "Cannot save "++show f++" in CMYK16 colorspace"


