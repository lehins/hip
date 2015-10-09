{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image (
  Image, Pixel, Gray(..), Color(..),
  ref, refDefault, refMaybe, dims, make, map, zipWith, traverse, transpose,
  backpermute, crop, fromVector, fromLists, fromArray,
  readColorImage, readGrayImage, setDisplayProgram,
  module Graphics.Image.Interpolation,
  module Graphics.Image.Processing
  ) where
import qualified Prelude as P
import Graphics.Image.Color
import Graphics.Image.Gray
import Graphics.Image.Interface (Pixel)
import Graphics.Image.Internal
import Graphics.Image.Interpolation
import Graphics.Image.IO
import Graphics.Image.Processing


readGrayImage :: P.FilePath -> P.IO (Image Gray)
readGrayImage = readImage


readColorImage :: P.FilePath -> P.IO (Image Color)
readColorImage = readImage
