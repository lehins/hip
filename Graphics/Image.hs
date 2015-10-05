{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image (
  Image, Pixel, Gray(..), Color(..),
  ref, refd, refm, ref1, dims, make, map, zipWith, traverse, transpose,
  backpermute, crop, fromVector, fromLists, fromArray,
  readColorImage, readGrayImage, setDisplayProgram
  ) where
import qualified Prelude as P
import Codec.Picture.Types (DynamicImage)
import Graphics.Image.Definition (Convertable, Pixel)
import Graphics.Image.Internal
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
