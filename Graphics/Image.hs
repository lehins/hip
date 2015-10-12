{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image (
  Pixel, Saveable(..), SaveOption(..), Format(..),
  ref, refDefault, refMaybe, dims, rows, cols, make, map, zipWith, traverse, transpose,
  backpermute, crop, fromVector, fromLists, fromArray,
  readColorImage, readGrayImage, setDisplayProgram, Convertable(..),
  module Graphics.Image.Interpolation,
  module Graphics.Image.Processing,
  module Graphics.Image.Pixel
  ) where
import qualified Prelude as P
import Graphics.Image.Conversion
import Graphics.Image.Interface (Pixel, Convertable(..))
import Graphics.Image.Internal
import Graphics.Image.Interpolation
import Graphics.Image.IO
import Graphics.Image.Pixel
import Graphics.Image.Processing


readGrayImage :: P.FilePath -> P.IO (Image Gray)
readGrayImage = readImage


readColorImage :: P.FilePath -> P.IO (Image RGB)
readColorImage = readImage
