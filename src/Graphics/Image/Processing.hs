{-# LANGUAGE BangPatterns, ViewPatterns #-}
module Graphics.Image.Processing (
  -- * Geometric
  module Graphics.Image.Processing.Geometric,
  -- * Interpolation
  module Graphics.Image.Processing.Interpolation,
  -- * Convolution
  module Graphics.Image.Processing.Convolution,
  -- * Tools
  Border(..), pixelGrid
  ) where

import Data.Word (Word8)
import Graphics.Image.Interface
import Graphics.Image.Processing.Convolution
import Graphics.Image.Processing.Geometric
import Graphics.Image.Processing.Interpolation



-- | This function magnifies an image by a positive factor and draws a grid
-- around the original pixels. It is here simply as useful inspection tool.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_eye_grid.png" $ pixelGrid 10 $ crop (51, 112) (20, 20) frog
--
-- <<images/frog.jpg>> <<images/frog_eye_grid.png>>
--
pixelGrid :: (Array arr cs e, Elevator e) =>
             Word8          -- ^ Magnification factor.
          -> Image arr cs e -- ^ Source image.
          -> Image arr cs e
pixelGrid !(succ . fromIntegral -> k) !img = traverse img getNewDims getNewPx where
  getNewDims !(m, n) = (1 + m*k, 1 + n*k)
  {-# INLINE getNewDims #-}
  getNewPx !getPx !(i, j) = if i `mod` k == 0 || j `mod` k == 0
                            then fromDouble $ fromChannel 0.5
                            else getPx ((i - 1) `div` k, (j - 1) `div` k)
  {-# INLINE getNewPx #-}
{-# INLINE pixelGrid #-}

