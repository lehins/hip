{-# LANGUAGE ViewPatterns #-}
module Graphics.Image.Repa (
  Image,
  -- * Image dimensions
  dims, rows, cols,  
  -- * Pixel referencing
  index, unsafeIndex, defaultIndex, maybeIndex,
  -- * Interpolation
  Interpolation(..), interpolate,
  -- * IO
  setDisplayProgram, Format(..), SaveOption(..), Encoder, Saveable(..)
  ) where

import Prelude

import Graphics.Image.Interface (interpolate)
import Graphics.Image.Interface.IO (setDisplayProgram)
import Graphics.Image.Interface.Conversion (Format(..), SaveOption(..), Encoder, Saveable(..))
import Graphics.Image.Repa.Internal (
  Image, dims, rows, cols, index, unsafeIndex, defaultIndex, maybeIndex)
import Graphics.Image.Interface.Interpolation
