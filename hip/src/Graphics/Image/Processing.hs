-- |
-- Module      : Graphics.Image.Processing
-- Copyright   : (c) Alexey Kuleshevich 2016-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing
  (
  -- * Geometric
    module Graphics.Image.Processing.Geometric
  -- * Interpolation
  , module Graphics.Image.Processing.Interpolation
  -- * Convolution
  , module Graphics.Image.Processing.Convolution
  -- * Binary
  , module Graphics.Image.Processing.Binary
  -- * Complex
  , module Graphics.Image.Processing.Complex
  -- * Filters
  , module Graphics.Image.Processing.Filter
  -- * Tools
  , pixelGrid
  ) where

import Data.Massiv.Array as A
import Graphics.Image.Internal
import Graphics.Image.Processing.Binary
import Graphics.Image.Processing.Complex
import Graphics.Image.Processing.Convolution
import Graphics.Image.Processing.Filter
import Graphics.Image.Processing.Geometric
import Graphics.Image.Processing.Interpolation
import Prelude hiding (traverse)


-- | This function magnifies an image by a positive factor and draws a grid
-- around the original pixels. It is here simply as useful inspection tool.
--
-- >>> frog <- readImageRGB VU "images/frog.jpg"
-- >>> writeImage "images/frog_eye_grid.png" $ pixelGrid 10 $ crop (51, 112) (20, 20) frog
--
-- <<images/frog.jpg>> <<images/frog_eye_grid.png>>
--
pixelGrid :: ColorModel cs e =>
             Int          -- ^ Magnification factor.
          -> Image cs e -- ^ Source image.
          -> Image cs e
pixelGrid f (Image arr) = computeI (A.zoomWithGrid (pure (fromDouble 0.5)) (Stride (f :. f)) arr)
{-# INLINE pixelGrid #-}
