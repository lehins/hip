{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- |
-- Module      : Graphics.Image.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : MIT
--
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell Image Processing (HIP) library. This implementation uses
-- <http://hackage.haskell.org/package/repa Repa> package as an underlying
-- representation for images and their processing in parallel or sequentially.
module Graphics.Image (
  -- * Color Space
  -- | Here is a list of supported pixels with their respective constructors:
  --
  --   * 'Pixel' 'Y' e  __= PixelY e__ - Luma, also known as /Y'/.
  --   * 'Pixel' 'YA' e __= PixelYA e__ - Luma with alpha.
  --   * 'Pixel' 'RGB' e __= PixelRGB e__ - Red, Green and Blue.
  --   * 'Pixel' 'RGBA' e __= PixelRGBA e__ - RGB with alpha
  --   * 'Pixel' 'HSI' e __= PixelHSI e__ - Hue, Saturation and Intensity.
  --   * 'Pixel' 'HSIA' e __= PixelHSIA e__ - HSI with alpha
  --   * 'Pixel' 'CMYK' e __= PixelCMYK e__ - Cyan, Magenta, Yellow and Key (Black).
  --   * 'Pixel' 'CMYKA' e __= PixelCMYKA e__ - CMYK with alpha.
  --   * 'Pixel' 'YCbCr' e __= PixelYCbCr e__ - Luma, blue-difference and red-difference
  --           chroma components.
  --   * 'Pixel' 'YCbCrA' e __= PixelYCbCrA e__ - YCbCr with alpha.
  --   * 'Pixel' 'Gray' e __= PixelGray e__ - Used for separating channels from other
  --           color spaces.
  --   * 'Pixel' 'Binary' 'Bit' __= 'on' | 'off'__ - Bi-tonal.
  --   * 'Pixel' cs ('Complex' e) __= 'Pixel' cs e '+:' 'Pixel' cs e__ - Complex pixels
  --           with any color space.
  -- 
  -- All of functionality related to 'I.ColorSpace's is reimported here
  -- for convenience.
  -- 
  module Graphics.Image.ColorSpace,
  -- * Creation
  -- | If it is necessary to create an image in an arbitrary representation
  -- or with some specific 'Pixel' precision, you can use 'I.makeImage' from
  -- <Graphics-Image-Interface.html Graphics.Image.Interface> module and
  -- manually specifying function's output type.
  makeImage,
  -- * IO
  -- ** Reading
  -- | Read any supported image file into an 'Image' with 'VU' (Vector Unboxed)
  -- representation and pixels with 'Double' precision. In order to read an
  -- image with different representation, color space and precision 'readImage'
  -- or 'readImageExact' from <Graphics-Image-IO.html Graphics.Image.IO> can be
  -- used.
  readImageY, readImageYA, readImageRGB, readImageRGBA,
  -- ** Writing
  writeImage, 
  -- * Accessors
  -- ** Dimensions
  rows, cols, I.dims,
  -- ** Indexing
  I.index,
  -- * Processing
  -- ** Pointwise
  I.map, I.imap, I.zipWith, I.izipWith,
  -- ** Geometric
  I.traverse, I.traverse2,
  (I.|*|), I.transpose, I.backpermute,
  -- * Reduction
  I.fold, sum, product, maximum, minimum,
  ) where
import Prelude hiding (map, zipWith, sum, product, maximum, minimum)
import Graphics.Image.ColorSpace
import Graphics.Image.IO  (writeImage)
import Graphics.Image.Interface (Array, ManifestArray, Image)
import qualified Graphics.Image.Interface as I
import Graphics.Image.Interface.Vector


--------------------------------------------------------------------------------
---- Creation and Transformation -----------------------------------------------
--------------------------------------------------------------------------------



-- | Get the number of rows in an image.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RepaDelayed RGB: 200x320>
-- >>> rows frog
-- 200
--
rows :: Array arr cs e => Image arr cs e -> Int
rows = fst . I.dims
{-# INLINE rows #-}


-- | Get the number of columns in an image.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RepaDelayed RGB: 200x320>
-- >>> cols frog
-- 320
--
cols :: Array arr cs e => Image arr cs e -> Int
cols = snd . I.dims
{-# INLINE cols #-}


-- | Sum all pixels in the image.
sum :: ManifestArray arr cs e => Image arr cs e -> Pixel cs e
sum = I.fold (+) 0
{-# INLINE sum #-}


-- | Multiply all pixels in the image.
product :: ManifestArray arr cs e => Image arr cs e -> Pixel cs e
product = I.fold (+) 1
{-# INLINE product #-}


-- | Retrieve the biggest pixel from an image
maximum :: (ManifestArray arr cs e, Ord (Pixel cs e)) => Image arr cs e -> Pixel cs e
maximum !img = I.fold max (I.index img (0, 0)) img
{-# INLINE maximum #-}


-- | Retrieve the smallest pixel from an image
minimum :: (ManifestArray arr cs e, Ord (Pixel cs e)) => Image arr cs e -> Pixel cs e
minimum !img = I.fold min (I.index img (0, 0)) img
{-# INLINE minimum #-}


