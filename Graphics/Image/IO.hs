{-# LANGUAGE ViewPatterns, BangPatterns #-}
module Graphics.Image.IO (
  readGrayImage, readColorImage, writeImage
  ) where

import Prelude as P hiding (readFile, writeFile)
import Graphics.Image.Gray
import Graphics.Image.Color
import Graphics.Image.Definition (Pixel(..))
import Graphics.Image.Internal
import Graphics.Image.Conversion
import Data.Char (toUpper)
import Data.ByteString (readFile)
import qualified Data.ByteString.Lazy as BL (writeFile)



readGrayImage :: FilePath -> IO (Image Gray)
readGrayImage path = fmap ((either err id) . decodeGrayImage) (readFile path) where
  err str = error str


readColorImage :: FilePath -> IO (Image RGB)
readColorImage path = fmap ((either err id) . decodeRGBImage) (readFile path) where
  err str = error str

  
ext2format :: [Char] -> Format
ext2format ((P.map toUpper) -> ext)
  | ext == "BMP"             = BMP
  | elem ext ["JPG", "JPEG"] = JPG
  | ext == "PNG"             = PNG
  | elem ext ["TIF", "TIFF"] = TIFF
  | ext == "HDR"             = HDR
  -- | ext == "PBM"             = PBM
  -- | ext == "PGM"             = PGM
  -- | ext == "PPM"             = PPM
  | null ext = error "File extension was not supplied"
  | otherwise = error $ "Unsupported file extension: "++ext


shouldNormalize :: Pixel px => [SaveOption px] -> Bool
shouldNormalize [] = True
shouldNormalize ((Normalize should):_) = should
shouldNormalize (_:opts) = shouldNormalize opts


writeImage :: (Pixel px, Saveable px) =>
                   RepaStrategy Image px
                   -> FilePath
                   -> Image px
                   -> [SaveOption px]
                   -> IO ()
writeImage !strat !path !img !options =
  BL.writeFile path $ encoder format arr where
    !arr = toArray strat $ if shouldNormalize options then normalize strat img else img
    !format = getFormat options
    !encoder = getEncoder options
    !ext = reverse . fst . (span ('.'/=)) . reverse $ path
    getFormat [] = ext2format ext
    getFormat !((Format f):_) = f
    getFormat !(_:opts) = getFormat opts
    getEncoder [] = defaultEncoder format
    getEncoder !((Encoder enc):_) = enc
    getEncoder !(_:opts) = getEncoder opts
    defaultEncoder !f = case f of
      BMP  -> inRGB8
      JPG  -> inYCbCr8
      PNG  -> inRGB8
      TIFF -> inRGB8
      HDR  -> inRGBF
      -- PBM  -> inY8
      -- PGM  -> inY8
      -- PPM  -> inRGB8
