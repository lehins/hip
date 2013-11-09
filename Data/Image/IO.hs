{-# LANGUAGE FlexibleContexts, ViewPatterns, ConstraintKinds, MultiParamTypeClasses,
             UndecidableInstances, FlexibleInstances #-}
module Data.Image.IO (
  Format(..),
  Saveable(..),
  readColorImage, writeImage
  
  ) where

import Prelude hiding (readFile, writeFile)
import Data.Image.Pixel
import Data.Image.Gray
import Data.Image.Color
import Data.Image.Internal
import Data.Vector as V (map, convert)
import Data.Vector.Storable as VS (map, convert)
import Data.ByteString (ByteString, readFile)
import qualified Data.ByteString.Lazy as BL (ByteString, writeFile)
import Foreign.Storable ( Storable )
import System.IO hiding (readFile, writeFile)
import qualified Codec.Picture as JP
import qualified Graphics.Netpbm as PNM

data Format = BMP | JPG | PNG | TIFF | HDR | PBM | PGM | PPM deriving Show

class (Pixel px, Convertable px) => Saveable px where
  inY8 :: Format -> Image px -> BL.ByteString
  inY16 :: Format -> Image px -> BL.ByteString
  inYA8 :: Format -> Image px -> BL.ByteString
  inYA16 :: Format -> Image px -> BL.ByteString
  inRGB8 :: Format -> Image px -> BL.ByteString
  inRGB16 :: Format -> Image px -> BL.ByteString
  inRGBF :: Format -> Image px -> BL.ByteString
  inRGBA8 :: Format -> Image px -> BL.ByteString
  inRGBA16 :: Format -> Image px -> BL.ByteString
  inYCbCr8 :: Format -> Image px -> BL.ByteString
  inCMYK8 :: Format -> Image px -> BL.ByteString
  inCMYK16 :: Format -> Image px -> BL.ByteString

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
  inYCbCr8 JPG = JP.encodeJpeg . (image2jp (fromGray :: Gray -> JP.PixelYCbCr8))
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
  inYCbCr8 JPG = JP.encodeJpeg . (image2jp (fromColor :: Color -> JP.PixelYCbCr8))
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
        

readColorImage path = fmap decodeColorImage (readFile path)

writeImage path img format encoder = BL.writeFile path $ encoder format img
  
{-
readGrayImage path = do
  dimage <- readImage path
  return $ getGrayImage dimage
-}
