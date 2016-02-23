{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image.Interface.Repa (
  RD(..), RS(..), RP(..),
  makeImage, readImageY, readImageYA, readImageRGB, readImageRGBA,
  computeS, computeP, delay
  ) where

import Graphics.Image.IO
import Graphics.Image.Interface hiding (makeImage)
import Graphics.Image.Interface.Repa.Internal
import qualified Graphics.Image.Interface as I (makeImage)
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


readImageY :: FilePath -> IO (Image RD Y Double)
readImageY = fmap (either error id) . readImage
{-# INLINE readImageY #-}


readImageYA :: FilePath -> IO (Image RD YA Double)
readImageYA = fmap (either error id) . readImage
{-# INLINE readImageYA #-}


readImageRGB :: FilePath -> IO (Image RD RGB Double)
readImageRGB = fmap (either error id) . readImage
{-# INLINE readImageRGB #-}


readImageRGBA :: FilePath -> IO (Image RD RGBA Double)
readImageRGBA = fmap (either error id) . readImage
{-# INLINE readImageRGBA #-}

