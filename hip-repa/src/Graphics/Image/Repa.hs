{-# LANGUAGE ViewPatterns #-}
module Graphics.Image.Repa (
  Image,
  -- * Image dimensions
  dims, rows, cols,  
  -- * Pixel referencing
  index, unsafeIndex, defaultIndex, maybeIndex,
  -- * Interpolation
  Interpolation(..),
  -- * IO
  setDisplayProgram, OutputFormat(..), SaveOption(..), Encoder, Saveable(..)
  ) where


import HIP.IO
import HIP.Interpolation
import Graphics.Image.Repa.Internal (
  Image, dims, rows, cols, index, unsafeIndex, defaultIndex, maybeIndex)
