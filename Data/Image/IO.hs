{-# LANGUAGE FlexibleContexts, ViewPatterns, ConstraintKinds, MultiParamTypeClasses,
             UndecidableInstances, FlexibleInstances #-}
module Data.Image.IO where

import Prelude hiding (readFile)
import Data.Image.Internal
import Data.Image.Color
import Data.Image.Gray
import qualified Data.Image.Convertable as C
import Data.Either
import Data.Vector as V (map, convert)
import Data.Vector.Storable as VS (map, convert)
import Data.Typeable
import Data.Either
import Data.ByteString (ByteString, readFile)
import qualified Data.ByteString.Lazy as BL
import Foreign.Storable ( Storable )
import System.IO hiding (readFile)
import System.IO.Temp
import qualified Codec.Picture as JP

data Format = BMP | JPG | PNG | TIFF | HGR | PBM | PGM | PPM

class (C.Convertable px) => Saveable px where
  inRGB8 :: Image px -> Format -> BL.ByteString
  inRGB16 :: Image px -> Format -> BL.ByteString

image2JP f img = JP.generateImage pxOp (width img) (height img) where
  pxOp x y = f $ ref img x y 

instance Saveable Color where
  inRGB8 img BMP = JP.encodeBitmap $ image2JP (C.fromColor  :: Color -> JP.PixelRGB8) img
  inRGB16 img BMP = JP.encodeBitmap $ image2JP (C.fromColor  :: Color -> JP.PixelRGBA8) img

decodeColorImage imstr = either pnm2Image (Right . jp2Image) $ C.decodeImage imstr
  where
    fromJPImage i = makeImage (C.imageWidth i) (C.imageHeight i) (pxOp i)
      where pxOp i x y = C.toColor $ C.pixelAt i x y
    jp2Image (C.ImageY8 i) = fromJPImage i
    jp2Image (C.ImageY16 i) = fromJPImage i
    jp2Image (C.ImageYF i) = fromJPImage i
    jp2Image (C.ImageRGB8 i) = fromJPImage i
    jp2Image (C.ImageRGB16 i) = fromJPImage i
    jp2Image (C.ImageRGBF i) = fromJPImage i
    jp2Image (C.ImageRGBA8 i) = fromJPImage i
    jp2Image (C.ImageRGBA16 i) = fromJPImage i
    jp2Image (C.ImageYCbCr8 i) = fromJPImage i
    --jp2Image (ImageCMYK8 i) = fromJPImage i
    --jp2Image (ImageCMYK16 i) = fromJPImage i
    pnm2Image errmsgJP = pnmResult2Image $ C.parsePPM imstr where
      pnmResult2Image (Right (pnmLs, _)) = Right $ convertPNMImage (head pnmLs)
      pnmResult2Image (Left errmsgPNM) = Left (errmsgJP++errmsgPNM)
      convertPNMImage (C.PPM (C.PPMHeader _ w h) d) = pnm2Image d where
        fromPNMVector v = fromVector w h $ V.map C.toColor $ VS.convert v
        pnm2Image (C.PpmPixelDataRGB8 v) = fromPNMVector v
        pnm2Image (C.PpmPixelDataRGB16 v) = fromPNMVector v
        pnm2Image (C.PbmPixelData v) = fromPNMVector v
        pnm2Image (C.PgmPixelData8 v) = fromPNMVector v
        pnm2Image (C.PgmPixelData16 v) = fromPNMVector v
        

readColorImage path = do
  imstr <- readFile path
  return $ decodeColorImage imstr

--writeColorImage path img = do
  
{-
readGrayImage path = do
  dimage <- readImage path
  return $ getGrayImage dimage
-}
