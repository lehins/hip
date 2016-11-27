{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Image.IO.Base
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO.Base (
  ImageFormat(..), Readable(..), Writable(..), Convertible(..),
  ) where

import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)


-- | Used during converting pixels between libraries.
class Convertible a b where
  convert :: a -> b

-- | Image file format. Helps in guessing image format using a file extension,
-- as well as supplying format specific options during saving an image.
class ImageFormat format where
  -- | Options that can be used during writing an image in this format.
  data SaveOption format

  -- | Default file extension for this image format.
  ext :: format -> String

  -- | Known extensions for this image format.
  exts :: format -> [String]
  exts f = [ext f]

  -- | Returns `True` if a file extension (ex. @".png"@) corresponds to this format.
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
