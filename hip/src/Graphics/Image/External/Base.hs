{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Image.External.Base (
  fromWord8, toWord8, fromWord16, toWord16,
  ImageFormat(..), Readable(..), Writable(..),
  ) where

import Data.Word (Word8, Word16)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Graphics.Image.Interface




fromWord8 :: Word8 -> Double
fromWord8 px = fromIntegral px / 255
toWord8 :: Double -> Word8
toWord8 px = round (255*px)

fromWord16 :: Word16 -> Double
fromWord16 px = fromIntegral px / 65535
toWord16 :: Double -> Word16
toWord16 px = round (65535*px)

class ImageFormat format where
  data SaveOption format

  ext :: format -> String

  exts :: format -> [String]

  isFormat :: format -> String -> Bool
  isFormat f e = e == ext f

-- needed: extension, format, colorspace
class ImageFormat format => Writable format img where

  encode :: img -> format -> BL.ByteString


class ImageFormat format => Readable format img where

  decode :: B.ByteString -> format -> Either String img


  

{-
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
-}
