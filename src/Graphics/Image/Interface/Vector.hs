{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Graphics.Image.Interface.Vector
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Vector (
  -- * Construction
  makeImage, fromLists, fromUnboxedVector, toUnboxedVector,
  -- * Representation
  VU(..),
  -- * Linear index conversion
  toIx, fromIx
  ) where

import Graphics.Image.Interface hiding (makeImage, fromLists)
import qualified Graphics.Image.Interface as I (makeImage, fromLists)
import Graphics.Image.Interface.Vector.Unboxed


-- | Create an image with 'VU' (Vector Unboxed) representation and pixels of 'Double'
-- precision. Note, that it is essential for 'Double' precision pixels to keep values
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
