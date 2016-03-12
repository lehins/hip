{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image.Interface.Repa (
  -- * Construction
  makeImage, fromLists,
  -- * IO
  readImageY, readImageYA, readImageRGB, readImageRGBA,
  -- * Computation
  computeS, computeP, delay,
  -- * Representation
  RD(..), RS(..), RP(..),
  ) where

import Graphics.Image.IO
import Graphics.Image.Interface hiding (makeImage, fromLists)
import qualified Graphics.Image.Interface as I (makeImage, fromLists)
import Graphics.Image.Interface.Repa.Internal
import Graphics.Image.ColorSpace


-- | Create a delayed representation of an image.
makeImage :: Array RD cs Double =>
             (Int, Int) -- ^ (@m@ rows, @n@ columns) - dimensions of a new image.
          -> ((Int, Int) -> Pixel cs Double)
             -- ^ A function that takes (@i@-th row, and @j@-th column) as an argument
             -- and returns a pixel for that location.
          -> Image RD cs Double
makeImage = I.makeImage
{-# INLINE makeImage #-}


-- | Construct an image from a nested rectangular shaped list of pixels.
fromLists :: Array RD cs e =>
             [[Pixel cs e]]
          -> Image RD cs e
fromLists = I.fromLists
{-# INLINE fromLists #-}


-- | Read image as luma (brightness).
readImageY :: FilePath -> IO (Image RD Y Double)
readImageY = fmap (either error id) . readImage
{-# INLINE readImageY #-}


-- | Read image as luma with 'Alpha' channel.
readImageYA :: FilePath -> IO (Image RD YA Double)
readImageYA = fmap (either error id) . readImage
{-# INLINE readImageYA #-}


-- | Read image in RGB colorspace.
readImageRGB :: FilePath -> IO (Image RD RGB Double)
readImageRGB = fmap (either error id) . readImage
{-# INLINE readImageRGB #-}


-- | Read image in RGB colorspace with 'Alpha' channel.
readImageRGBA :: FilePath -> IO (Image RD RGBA Double)
readImageRGBA = fmap (either error id) . readImage
{-# INLINE readImageRGBA #-}

