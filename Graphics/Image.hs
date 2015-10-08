{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image (
  Image, Pixel, Gray(..), Color(..),
  ref, refDefault, refMaybe, dims, make, map, zipWith, traverse, transpose,
  backpermute, crop, fromVector, fromLists, fromArray,
  readColorImage, readGrayImage, setDisplayProgram,
  module Graphics.Image.Interpolation
  ) where
import qualified Prelude as P
import Codec.Picture.Types (DynamicImage)
import Graphics.Image.Interface (Convertable, Pixel)
import Graphics.Image.Internal
import Graphics.Image.Interpolation
import Graphics.Image.Gray
import Graphics.Image.Color
import Graphics.Image.IO
import Graphics.Netpbm (PPM)


readGrayImage :: (Convertable DynamicImage (Image Gray),
                  Convertable PPM (Image Gray)) =>
                 P.FilePath -> P.IO (Image Gray)
readGrayImage = readImage


readColorImage :: (Convertable DynamicImage (Image Color),
                  Convertable PPM (Image Color)) =>
                 P.FilePath -> P.IO (Image Color)
readColorImage = readImage