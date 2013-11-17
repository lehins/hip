{-# LANGUAGE FlexibleContexts, ViewPatterns, ConstraintKinds, MultiParamTypeClasses,
             UndecidableInstances, FlexibleInstances #-}
module Data.Image.IO (
  Format(..),
  Saveable(..),
  SaveOptions(..),
  Encoder,
  readGrayImage, readColorImage, writeImage
  ) where

import Prelude hiding (readFile, writeFile)
import Data.Image
import Data.Image.Conversion
import Data.Image.Gray
import Data.Image.Color
import Data.Image.Internal
import Data.Image.Processing
import Data.Char (toUpper)
import qualified Data.Vector.Unboxed as V --(map, convert)
import qualified Data.Vector.Storable as VS (map, convert)
import Data.ByteString (ByteString, readFile)
import qualified Data.ByteString.Lazy as BL (ByteString, writeFile)
import Foreign.Storable ( Storable )
import System.IO hiding (readFile, writeFile)
import qualified Codec.Picture as JP
import qualified Graphics.Netpbm as PNM

data Format = BMP | JPG | PNG | TIFF | HDR | PBM | PGM | PPM deriving Show

ext2format ((map toUpper) -> ext)
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

class (Ord px, Pixel px, Convertable px) => Saveable px where
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

image2jp f img = JP.generateImage pxOp (width img) (height img) where
  pxOp x y = f $ ref img x y 


instance Saveable Gray where
  inY8 BMP = JP.encodeBitmap . (image2jp (fromGray :: Gray -> JP.Pixel8))
  inY8 PNG = JP.encodePng . (image2jp (fromGray :: Gray -> JP.Pixel8))
  inY8 TIFF = JP.encodeTiff . (image2jp (fromGray :: Gray -> JP.Pixel8))
  inY8 f = error $ "Cannot save "++show f++" in Y8 colorspace"
  inY16 PNG = JP.encodePng . (image2jp (fromGray :: Gray -> JP.Pixel16))
  inY16 TIFF = JP.encodeTiff . (image2jp (fromGray :: Gray -> JP.Pixel16))
  inY16 f = error $ "Cannot save "++show f++" in Y16 colorspace"
  inYA8 PNG = JP.encodePng . (image2jp (fromGray :: Gray -> JP.PixelYA8))
  inYA8 f = error $ "Cannot save "++show f++" in Y8 colorspace"
  inYA16 PNG = JP.encodePng . (image2jp (fromGray :: Gray -> JP.PixelYA16))
  inYA16 f = error $ "Cannot save "++show f++" in Y16 colorspace"
  inRGB8 BMP = JP.encodeBitmap . (image2jp (fromGray :: Gray -> JP.PixelRGB8))
  inRGB8 PNG = JP.encodePng . (image2jp (fromGray :: Gray -> JP.PixelRGB8))
  inRGB8 TIFF = JP.encodeTiff . (image2jp (fromGray :: Gray -> JP.PixelRGB8))
  inRGB8 f = error $ "Cannot save "++show f++" in RGB8 colorspace"
  inRGB16 TIFF = JP.encodeTiff . (image2jp (fromGray :: Gray -> JP.PixelRGB16))
  inRGB16 PNG = JP.encodePng . (image2jp (fromGray :: Gray -> JP.PixelRGB16))
  inRGB16 f = error $ "Cannot save "++show f++" in RGB16 colorspace"
  inRGBA8 BMP = JP.encodeBitmap . (image2jp (fromGray :: Gray -> JP.PixelRGBA8))
  inRGBA8 PNG = JP.encodePng . (image2jp (fromGray :: Gray -> JP.PixelRGBA8))
  inRGBA8 f = error $ "Cannot save "++show f++" in RGBA8 colorspace"
  inRGBA16 PNG = JP.encodePng . (image2jp (fromGray :: Gray -> JP.PixelRGBA16))
  inRGBA16 f = error $ "Cannot save "++show f++" in RGBA16 colorspace"
  inYCbCr8 JPG =
    (JP.encodeJpegAtQuality 100) . (image2jp (fromGray :: Gray -> JP.PixelYCbCr8))
  inYCbCr8 f = error $ "Cannot save "++show f++" in YCbCr8 colorspace"
  inCMYK8 TIFF = JP.encodeTiff . (image2jp (fromGray :: Gray -> JP.PixelCMYK8))
  inCMYK8 f = error $ "Cannot save "++show f++" in CMYK8 colorspace"
  inCMYK16 TIFF = JP.encodeTiff . (image2jp (fromGray :: Gray -> JP.PixelCMYK16))
  inCMYK16 f = error $ "Cannot save "++show f++" in CMYK16 colorspace"
  inRGBF HDR = JP.encodeHDR . (image2jp (fromGray :: Gray -> JP.PixelRGBF))
  inRGBF f = error $ "Cannot save "++show f++" in RGBF colorspace"

instance Saveable Color where
  inY8 BMP = JP.encodeBitmap . (image2jp (fromColor :: Color -> JP.Pixel8))
  inY8 PNG = JP.encodePng . (image2jp (fromColor :: Color -> JP.Pixel8))
  inY8 TIFF = JP.encodeTiff . (image2jp (fromColor :: Color -> JP.Pixel8))
  inY8 f = error $ "Cannot save "++show f++" in Y8 colorspace"
  inY16 PNG = JP.encodePng . (image2jp (fromColor :: Color -> JP.Pixel16))
  inY16 TIFF = JP.encodeTiff . (image2jp (fromColor :: Color -> JP.Pixel16))
  inY16 f = error $ "Cannot save "++show f++" in Y16 colorspace"
  inYA8 PNG = JP.encodePng . (image2jp (fromColor :: Color -> JP.PixelYA8))
  inYA8 f = error $ "Cannot save "++show f++" in Y8 colorspace"
  inYA16 PNG = JP.encodePng . (image2jp (fromColor :: Color -> JP.PixelYA16))
  inYA16 f = error $ "Cannot save "++show f++" in Y16 colorspace"
  inRGB8 BMP = JP.encodeBitmap . (image2jp (fromColor :: Color -> JP.PixelRGB8))
  inRGB8 PNG = JP.encodePng . (image2jp (fromColor :: Color -> JP.PixelRGB8))
  inRGB8 TIFF = JP.encodeTiff . (image2jp (fromColor :: Color -> JP.PixelRGB8))
  inRGB8 f = error $ "Cannot save "++show f++" in RGB8 colorspace"
  inRGB16 TIFF = JP.encodeTiff . (image2jp (fromColor :: Color -> JP.PixelRGB16))
  inRGB16 PNG = JP.encodePng . (image2jp (fromColor :: Color -> JP.PixelRGB16))
  inRGB16 f = error $ "Cannot save "++show f++" in RGB16 colorspace"
  inRGBA8 BMP = JP.encodeBitmap . (image2jp (fromColor :: Color -> JP.PixelRGBA8))
  inRGBA8 PNG = JP.encodePng . (image2jp (fromColor :: Color -> JP.PixelRGBA8))
  inRGBA8 f = error $ "Cannot save "++show f++" in RGBA8 colorspace"
  inRGBA16 PNG = JP.encodePng . (image2jp (fromColor :: Color -> JP.PixelRGBA16))
  inRGBA16 f = error $ "Cannot save "++show f++" in RGBA16 colorspace"
  inYCbCr8 JPG =
    (JP.encodeJpegAtQuality 100) . (image2jp (fromColor :: Color -> JP.PixelYCbCr8))
  inYCbCr8 f = error $ "Cannot save "++show f++" in YCbCr8 colorspace"
  inCMYK8 TIFF = JP.encodeTiff . (image2jp (fromColor :: Color -> JP.PixelCMYK8))
  inCMYK8 f = error $ "Cannot save "++show f++" in CMYK8 colorspace"
  inCMYK16 TIFF = JP.encodeTiff . (image2jp (fromColor :: Color -> JP.PixelCMYK16))
  inCMYK16 f = error $ "Cannot save "++show f++" in CMYK16 colorspace"
  inRGBF HDR = JP.encodeHDR . (image2jp (fromColor :: Color -> JP.PixelRGBF))
  inRGBF f = error $ "Cannot save "++show f++" in RGBF colorspace"



decodeColorImage imstr = either pnm2Image (Right . jp2Image) $ JP.decodeImage imstr
  where
    fromJPImage i = makeImage (JP.imageWidth i) (JP.imageHeight i) (pxOp i)
      where pxOp i x y = toColor $ JP.pixelAt i x y
    --fromJPImage (JP.Image w h v) = fromVector w h $ V.map toColor $ VS.convert v
    jp2Image (JP.ImageY8 i) = fromJPImage i
    jp2Image (JP.ImageY16 i) = fromJPImage i
    jp2Image (JP.ImageYF i) = fromJPImage i
    jp2Image (JP.ImageRGB8 i) = fromJPImage i
    jp2Image (JP.ImageRGB16 i) = fromJPImage i
    jp2Image (JP.ImageRGBF i) = fromJPImage i
    jp2Image (JP.ImageRGBA8 i) = fromJPImage i
    jp2Image (JP.ImageRGBA16 i) = fromJPImage i
    jp2Image (JP.ImageYCbCr8 i) = fromJPImage i
    jp2Image (JP.ImageCMYK8 i) = fromJPImage i
    jp2Image (JP.ImageCMYK16 i) = fromJPImage i
    pnm2Image errmsgJP = pnmResult2Image $ PNM.parsePPM imstr where
      pnmResult2Image (Right (pnmLs, _)) = Right $ convertPNMImage (head pnmLs)
      pnmResult2Image (Left errmsgPNM) = Left (errmsgJP++errmsgPNM)
      convertPNMImage (PNM.PPM (PNM.PPMHeader _ w h) d) = pnm2Image d where
        fromPNMVector v = fromVector w h $ V.map toColor $ VS.convert v
        pnm2Image (PNM.PpmPixelDataRGB8 v) = fromPNMVector v
        pnm2Image (PNM.PpmPixelDataRGB16 v) = fromPNMVector v
        pnm2Image (PNM.PbmPixelData v) = fromPNMVector v
        pnm2Image (PNM.PgmPixelData8 v) = fromPNMVector v
        pnm2Image (PNM.PgmPixelData16 v) = fromPNMVector v

decodeGrayImage imstr = either pnm2Image (Right . jp2Image) $ JP.decodeImage imstr
  where
    fromJPImage i = makeImage (JP.imageWidth i) (JP.imageHeight i) (pxOp i)
      where pxOp i x y = toGray $ JP.pixelAt i x y
    jp2Image (JP.ImageY8 i) = fromJPImage i
    jp2Image (JP.ImageY16 i) = fromJPImage i
    jp2Image (JP.ImageYF i) = fromJPImage i
    jp2Image (JP.ImageRGB8 i) = fromJPImage i
    jp2Image (JP.ImageRGB16 i) = fromJPImage i
    jp2Image (JP.ImageRGBF i) = fromJPImage i
    jp2Image (JP.ImageRGBA8 i) = fromJPImage i
    jp2Image (JP.ImageRGBA16 i) = fromJPImage i
    jp2Image (JP.ImageYCbCr8 i) = fromJPImage i
    jp2Image (JP.ImageCMYK8 i) = fromJPImage i
    jp2Image (JP.ImageCMYK16 i) = fromJPImage i
    pnm2Image errmsgJP = pnmResult2Image $ PNM.parsePPM imstr where
      pnmResult2Image (Right (pnmLs, _)) = Right $ convertPNMImage (head pnmLs)
      pnmResult2Image (Left errmsgPNM) = Left (errmsgJP++errmsgPNM)
      convertPNMImage (PNM.PPM (PNM.PPMHeader _ w h) d) = pnm2Image d where
        fromPNMVector v = fromVector w h $ V.map toGray $ VS.convert v
        pnm2Image (PNM.PpmPixelDataRGB8 v) = fromPNMVector v
        pnm2Image (PNM.PpmPixelDataRGB16 v) = fromPNMVector v
        pnm2Image (PNM.PbmPixelData v) = fromPNMVector v
        pnm2Image (PNM.PgmPixelData8 v) = fromPNMVector v
        pnm2Image (PNM.PgmPixelData16 v) = fromPNMVector v


readColorImage :: FilePath -> IO (Image Color)
readColorImage path = fmap ((either err id) . decodeColorImage) (readFile path) where
  err str = error str

readGrayImage :: FilePath -> IO (Image Gray)
readGrayImage path = fmap ((either err id) . decodeGrayImage) (readFile path) where
  err str = error str

writeImage path img options = BL.writeFile path $ encoder format $ compute img' where
  format = getFormat options
  encoder = getEncoder options
  img' = if shouldNormalize options then normalize img else img
  ext = reverse . fst . (span ('.'/=)) . reverse $ path
  shouldNormalize [] = True
  shouldNormalize ((Normalize v):ops) = v
  shouldNormalize (_:ops) = shouldNormalize ops
  getFormat [] = ext2format ext
  getFormat ((Format f):ops) = f
  getFormat (_:ops) = getFormat ops
  getEncoder [] = defaultEncoder format
  getEncoder ((Encoder enc):opts) = enc
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
