module Graphics.Image.Repa.Fusion (
  -- * Initialization
  make,
  -- * Pixelwise Operations
  -- ** Mapping
  map, imap,
  -- ** Traversing
  traverse, traverse2, traverse3,
  -- ** Zipping
  zipWith,
  -- ** Permutations
  backpermute, transpose,
  -- * Processing
  -- ** Extracting
  crop, scale,
  -- ** Convolution
  -- * Complex
  module Graphics.Image.Interface.Complex,
  -- * Conversion
  fromLists, fromArray,
  -- * IO
  readGrayImage, readColorImage
  ) where

import Prelude hiding (map, zipWith)
import Graphics.Image.Interface.Complex
import Graphics.Image.Interface.IO
import Graphics.Image.Interface.Processing
import Graphics.Image.Repa.Internal
import Graphics.Image.Repa.Pixel (Gray(..), RGB(..))


readGrayImage :: FilePath -> IO (Image Gray)
readGrayImage = readImage
{-# INLINE readGrayImage #-}


readColorImage :: FilePath -> IO (Image RGB)
readColorImage = readImage
{-# INLINE readColorImage #-}
