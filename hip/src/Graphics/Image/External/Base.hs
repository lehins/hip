{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module Graphics.Image.External.Base (
  ImageFormat(..), Readable(..), Writable(..), Convertible(..)
  ) where

import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)


class Convertible a b where
  convert :: a -> b


class ImageFormat format where
  data SaveOption format

  ext :: format -> String

  exts :: format -> [String]
  exts f = [ext f]

  isFormat :: String -> format -> Bool
  isFormat e f = e `elem` (exts f)


class ImageFormat format => Readable img format where

  decode :: format -> B.ByteString -> Either String img


class ImageFormat format => Writable img format where

  encode :: format -> [SaveOption format] -> img -> BL.ByteString



