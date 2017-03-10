{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Graphics.Image.IO.Base
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO.Base (
  ImageFormat(..), Readable(..), Writable(..), Convertible(..), Seq(..)
  ) where

import qualified Data.ByteString                     as B (ByteString)
import qualified Data.ByteString.Lazy                as BL (ByteString)
import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface            as I
import           Graphics.Image.Processing.Complex   (imagPartI, realPartI)
import           Graphics.Image.Processing.Geometric (leftToRight)

-- | Used during converting pixels between libraries.
class Convertible cs e where
  convert :: (ToYA cs' e', ToRGBA cs' e', Array arr cs' e', Array arr cs e) =>
    Image arr cs' e' -> Image arr cs e


instance Convertible Y Double where
  convert = toImageY

instance Convertible YA Double where
  convert = toImageYA

instance Convertible RGB Double where
  convert = toImageRGB

instance Convertible RGBA Double where
  convert = toImageRGBA


-- | Special wrapper for formats that support encoding/decoding sequence of images.
newtype Seq f = Seq f

-- | Image file format. Helps in guessing image format using a file extension,
-- as well as supplying format specific options during saving an image.
class ImageFormat format where
  -- | Options that can be used during writing an image in this format.
  data SaveOption format

  -- | Default file extension for this image format.
  ext :: format -> String

  -- | Known file extensions for this image format, if more than one is commonly
  -- used, eg. ".jpeg", ".jpg".
  exts :: format -> [String]
  exts f = [ext f]

  -- | Checks if a file extension
  -- corresponds to the format, eg. @isFormat ".png" PNG == True@
  isFormat :: String -> format -> Bool
  isFormat e f = e `elem` exts f


-- | Image formats that can be read from file.
class ImageFormat format => Readable img format where

  -- | Decode an image from `BL.ByteString`.
  decode :: format -> B.ByteString -> Either String img


-- | Image formats that can be written to file.
class ImageFormat format => Writable img format where

  -- | Encode an image to `BL.ByteString`.
  encode :: format -> [SaveOption format] -> img -> BL.ByteString


-- | Writing Complex images: places real part on the left side of imaginary part.
instance (Array arr cs e, Array arr cs (Complex e),
          RealFloat e, Applicative (Pixel cs),
          Writable (Image arr cs e) format) =>
         Writable (Image arr cs (Complex e)) format where
  encode format opts !imgC =
    encode format opts (leftToRight (realPartI imgC) (imagPartI imgC))
