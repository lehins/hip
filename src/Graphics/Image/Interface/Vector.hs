{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image.Interface.Vector (
  -- * Construction
  makeImage, fromLists, fromUnboxedVector, toUnboxedVector,
  -- * IO
  readImageY, readImageYA, readImageRGB, readImageRGBA,
  -- * Representation
  VU(..), 
  ) where

import Graphics.Image.IO
import Graphics.Image.Interface hiding (makeImage, fromLists)
import qualified Graphics.Image.Interface as I (makeImage, fromLists)
import Graphics.Image.Interface.Vector.Unboxed
import Graphics.Image.ColorSpace


-- | Create an image with 'VU' (Vector Unboxed) representation and pixels of 'Double'
-- precision. Note, that for 'Double' precision pixels it is essential to keep values
-- normalized in the @[0, 1]@ range in order for an image to be written to file
-- properly.
--
-- >>> let grad_gray = makeImage (200, 200) (\(i, j) -> PixelY (fromIntegral i)/200 * (fromIntegral j)/200)
--
-- Because all 'Pixel's and 'Image's are installed into 'Num', above is equivalent to:
--
-- >>> let grad_gray = makeImage (200, 200) (\(i, j) -> PixelY $ fromIntegral (i*j)) / (200*200)
-- >>> writeImage "images/grad_gray.png" grad_gray
--
-- Creating color images is just as easy.
--
-- >>> let grad_color = makeImage (200, 200) (\(i, j) -> PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j))) / 400
-- >>> writeImage "images/grad_color.png" grad_color
--
-- <<images/grad_gray.png>> <<images/grad_color.png>>
--
makeImage :: Array VU cs Double =>
             (Int, Int) -- ^ (@m@ rows, @n@ columns) - dimensions of a new image.
          -> ((Int, Int) -> Pixel cs Double)
             -- ^ A function that takes (@i@-th row, and @j@-th column) as an argument
             -- and returns a pixel for that location.
          -> Image VU cs Double
makeImage = I.makeImage
{-# INLINE makeImage #-}


-- | Construct an image from a nested rectangular shaped list of pixels.
-- Length of an outer list will constitute @m@ rows, while the length of inner lists -
-- @n@ columns. All of the inner lists must be the same length and greater than @0@.
--
-- >>> fromLists [[PixelY (fromIntegral (i*j) / 60000) | j <- [1..300]] | i <- [1..200]]
-- <Image VectorUnboxed Y (Double): 200x300>
--
-- <<images/grad_fromLists.png>>
--
fromLists :: Array VU cs e =>
             [[Pixel cs e]]
          -> Image VU cs e
fromLists = I.fromLists
{-# INLINE fromLists #-}


-- | Read image as luma (brightness).
readImageY :: FilePath -> IO (Image VU Y Double)
readImageY = fmap (either error id) . readImage
{-# INLINE readImageY #-}


-- | Read image as luma with 'Alpha' channel.
readImageYA :: FilePath -> IO (Image VU YA Double)
readImageYA = fmap (either error id) . readImage
{-# INLINE readImageYA #-}


-- | Read image in RGB colorspace.
readImageRGB :: FilePath -> IO (Image VU RGB Double)
readImageRGB = fmap (either error id) . readImage
{-# INLINE readImageRGB #-}


-- | Read image in RGB colorspace with 'Alpha' channel.
readImageRGBA :: FilePath -> IO (Image VU RGBA Double)
readImageRGBA = fmap (either error id) . readImage
{-# INLINE readImageRGBA #-}

