{-# LANGUAGE ViewPatterns #-}
module Graphics.Image.IO (
  Format(..),
  Saveable(..),
  SaveOptions(..),
  Encoder,
  readGrayImage, readColorImage, writeImage
  ) where

import Prelude as P hiding (readFile, writeFile)
import Graphics.Image.Base
import Graphics.Image.Conversion
import Graphics.Image.Gray
import Graphics.Image.Color
import Graphics.Image.Processing.Geometric (normalize)
import Data.Char (toUpper)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Storable as VS (map, convert)
import Data.ByteString (ByteString, readFile)
import qualified Data.ByteString.Lazy as BL (ByteString, writeFile)
import Foreign.Storable ( Storable )
import System.IO hiding (readFile, writeFile)
import qualified Codec.Picture as JP
import qualified Graphics.Netpbm as PNM

data Format = BMP | JPG | PNG | TIFF | HDR | PBM | PGM | PPM deriving Show

ext2format ((P.map toUpper) -> ext)
  | ext == "BMP"             = BMP
  | elem ext ["JPG", "JPEG"] = JPG
  | ext == "PNG"             = PNG
  | elem ext ["TIF", "TIFF"] = TIFF
  | ext == "HDR"             = HDR
  | ext == "PBM"             = PBM
  | ext == "PGM"             = PGM
  | ext == "PPM"             = PPM
  | null ext = error "File extension was not supplied"
  | otherwise = error $ "Unsupported file extension: "++ext

type Encoder px = Format -> Image px -> BL.ByteString

data SaveOptions px = Format Format
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

image2jp f img = JP.generateImage pxOp (cols img) (rows img) where
  pxOp x y = f (ref img y x)


instance Saveable Gray where
  inY8 BMP = JP.encodeBitmap . (image2jp (convert :: Gray -> JP.Pixel8))
  inY8 PNG = JP.encodePng . (image2jp (convert :: Gray -> JP.Pixel8))
  inY8 TIFF = JP.encodeTiff . (image2jp (convert :: Gray -> JP.Pixel8))
  inY8 f = error $ "Cannot save "++show f++" in Y8 colorspace"
  inY16 PNG = JP.encodePng . (image2jp (convert :: Gray -> JP.Pixel16))
  inY16 TIFF = JP.encodeTiff . (image2jp (convert :: Gray -> JP.Pixel16))
  inY16 f = error $ "Cannot save "++show f++" in Y16 colorspace"
  inYA8 PNG = JP.encodePng . (image2jp (convert :: Gray -> JP.PixelYA8))
  inYA8 f = error $ "Cannot save "++show f++" in Y8 colorspace"
  inYA16 PNG = JP.encodePng . (image2jp (convert :: Gray -> JP.PixelYA16))
  inYA16 f = error $ "Cannot save "++show f++" in Y16 colorspace"
  inRGB8 BMP = JP.encodeBitmap . (image2jp (convert :: Gray -> JP.PixelRGB8))
  inRGB8 PNG = JP.encodePng . (image2jp (convert :: Gray -> JP.PixelRGB8))
  inRGB8 TIFF = JP.encodeTiff . (image2jp (convert :: Gray -> JP.PixelRGB8))
  inRGB8 f = error $ "Cannot save "++show f++" in RGB8 colorspace"
  inRGB16 TIFF = JP.encodeTiff . (image2jp (convert :: Gray -> JP.PixelRGB16))
  inRGB16 PNG = JP.encodePng . (image2jp (convert :: Gray -> JP.PixelRGB16))
  inRGB16 f = error $ "Cannot save "++show f++" in RGB16 colorspace"
  inRGBA8 BMP = JP.encodeBitmap . (image2jp (convert :: Gray -> JP.PixelRGBA8))
  inRGBA8 PNG = JP.encodePng . (image2jp (convert :: Gray -> JP.PixelRGBA8))
  inRGBA8 f = error $ "Cannot save "++show f++" in RGBA8 colorspace"
  inRGBA16 PNG = JP.encodePng . (image2jp (convert :: Gray -> JP.PixelRGBA16))
  inRGBA16 f = error $ "Cannot save "++show f++" in RGBA16 colorspace"
  inYCbCr8 JPG =
    (JP.encodeJpegAtQuality 100) . (image2jp (convert :: Gray -> JP.PixelYCbCr8))
  inYCbCr8 f = error $ "Cannot save "++show f++" in YCbCr8 colorspace"
  inCMYK8 TIFF = JP.encodeTiff . (image2jp (convert :: Gray -> JP.PixelCMYK8))
  inCMYK8 f = error $ "Cannot save "++show f++" in CMYK8 colorspace"
  inCMYK16 TIFF = JP.encodeTiff . (image2jp (convert :: Gray -> JP.PixelCMYK16))
  inCMYK16 f = error $ "Cannot save "++show f++" in CMYK16 colorspace"
  inRGBF HDR = JP.encodeHDR . (image2jp (convert :: Gray -> JP.PixelRGBF))
  inRGBF f = error $ "Cannot save "++show f++" in RGBF colorspace"

instance Saveable RGB where
  inY8 BMP = JP.encodeBitmap . (image2jp (convert :: RGB -> JP.Pixel8))
  inY8 PNG = JP.encodePng . (image2jp (convert :: RGB -> JP.Pixel8))
  inY8 TIFF = JP.encodeTiff . (image2jp (convert :: RGB -> JP.Pixel8))
  inY8 f = error $ "Cannot save "++show f++" in Y8 colorspace"
  inY16 PNG = JP.encodePng . (image2jp (convert :: RGB -> JP.Pixel16))
  inY16 TIFF = JP.encodeTiff . (image2jp (convert :: RGB -> JP.Pixel16))
  inY16 f = error $ "Cannot save "++show f++" in Y16 colorspace"
  inYA8 PNG = JP.encodePng . (image2jp (convert :: RGB -> JP.PixelYA8))
  inYA8 f = error $ "Cannot save "++show f++" in Y8 colorspace"
  inYA16 PNG = JP.encodePng . (image2jp (convert :: RGB -> JP.PixelYA16))
  inYA16 f = error $ "Cannot save "++show f++" in Y16 colorspace"
  inRGB8 BMP = JP.encodeBitmap . (image2jp (convert :: RGB -> JP.PixelRGB8))
  inRGB8 PNG = JP.encodePng . (image2jp (convert :: RGB -> JP.PixelRGB8))
  inRGB8 TIFF = JP.encodeTiff . (image2jp (convert :: RGB -> JP.PixelRGB8))
  inRGB8 f = error $ "Cannot save "++show f++" in RGB8 colorspace"
  inRGB16 TIFF = JP.encodeTiff . (image2jp (convert :: RGB -> JP.PixelRGB16))
  inRGB16 PNG = JP.encodePng . (image2jp (convert :: RGB -> JP.PixelRGB16))
  inRGB16 f = error $ "Cannot save "++show f++" in RGB16 colorspace"
  inRGBA8 BMP = JP.encodeBitmap . (image2jp (convert :: RGB -> JP.PixelRGBA8))
  inRGBA8 PNG = JP.encodePng . (image2jp (convert :: RGB -> JP.PixelRGBA8))
  inRGBA8 f = error $ "Cannot save "++show f++" in RGBA8 colorspace"
  inRGBA16 PNG = JP.encodePng . (image2jp (convert :: RGB -> JP.PixelRGBA16))
  inRGBA16 f = error $ "Cannot save "++show f++" in RGBA16 colorspace"
  inYCbCr8 JPG =
    (JP.encodeJpegAtQuality 100) . (image2jp (convert :: RGB -> JP.PixelYCbCr8))
  inYCbCr8 f = error $ "Cannot save "++show f++" in YCbCr8 colorspace"
  inCMYK8 TIFF = JP.encodeTiff . (image2jp (convert :: RGB -> JP.PixelCMYK8))
  inCMYK8 f = error $ "Cannot save "++show f++" in CMYK8 colorspace"
  inCMYK16 TIFF = JP.encodeTiff . (image2jp (convert :: RGB -> JP.PixelCMYK16))
  inCMYK16 f = error $ "Cannot save "++show f++" in CMYK16 colorspace"
  inRGBF HDR = JP.encodeHDR . (image2jp (convert :: RGB -> JP.PixelRGBF))
  inRGBF f = error $ "Cannot save "++show f++" in RGBF colorspace"



{-# INLINE decodeRGBImage #-}
decodeRGBImage imstr = either pnm2Image (Right . convert) $ JP.decodeImage imstr
  where
    pnm2Image errmsgJP = pnmResult2Image $ PNM.parsePPM imstr where
      pnmResult2Image (Right (pnmLs, _)) = Right $ convert (head pnmLs)
      pnmResult2Image (Left errmsgPNM) = Left (errmsgJP++errmsgPNM)

{-# INLINE decodeGrayImage #-}
decodeGrayImage imstr = either pnm2Image (Right . convert) $ JP.decodeImage imstr
  where
    pnm2Image errmsgJP = pnmResult2Image $ PNM.parsePPM imstr where
      pnmResult2Image (Right (pnmLs, _)) = Right $ convert (head pnmLs)
      pnmResult2Image (Left errmsgPNM) = Left (errmsgJP++errmsgPNM)

readColorImage :: FilePath -> IO (Image RGB)
{-# INLINE readColorImage #-}
readColorImage path = fmap ((either err id) . decodeRGBImage) (readFile path) where
  err str = error str

readGrayImage :: FilePath -> IO (Image Gray)
{-# INLINE readGrayImage #-}
readGrayImage path = fmap ((either err id) . decodeGrayImage) (readFile path) where
  err str = error str

writeImage :: Saveable t => [Char] -> Image t -> [SaveOptions t] -> IO ()
{-# INLINE writeImage #-}
writeImage path img options = BL.writeFile path $ encoder format $ compute img' where
  format = getFormat options
  encoder = getEncoder options
  compute i@(dims -> (w, h)) = fromVector w h $ toVector i
  img' = if shouldNormalize options then normalize img else img
  ext = reverse . fst . (span ('.'/=)) . reverse $ path
  shouldNormalize [] = True
  shouldNormalize ((Normalize v):_) = v
  shouldNormalize (_:opts) = shouldNormalize opts
  getFormat [] = ext2format ext
  getFormat ((Format f):_) = f
  getFormat (_:opts) = getFormat opts
  getEncoder [] = defaultEncoder format
  getEncoder ((Encoder enc):_) = enc
  getEncoder (_:opts) = getEncoder opts
  defaultEncoder f = case f of
    BMP  -> inRGB8
    JPG  -> inYCbCr8
    PNG  -> inRGB8
    TIFF -> inRGB8
    HDR  -> inRGBF
    PBM  -> inY8
    PGM  -> inY8
    PPM  -> inRGB8
