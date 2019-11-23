{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-duplicate-exports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Graphics.Image
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell Image Processing (HIP) library is a wrapper around any array like
-- data structure and is fully agnostic to the underlying representation. All of
-- the functionality in this library relies upon a few type classes, which
-- corresponding representation types are instances of:
--
-- * @__`Array` arr cs e__@ - this is a base class for every
-- __@`Image`@ @arr@ @cs@ @e@__, where @__arr__@ stands for an underlying array
-- representation, @__cs__@ is the `ColorSpace` of an image and @__e__@ is the
-- type denoting precision of an image (@Int@, @Word@, @Double@, etc.) .
--
-- * @__`MArray` arr cs e__@ - is a kind of array, that can be indexed in
-- constant time and allows monadic operations and mutation on
-- __@`MImage`@ @st@ @arr@ @cs@ @e@__, which is `Image`'s mutable cousin.
--
-- Representations using <http://hackage.haskell.org/package/vector Vector> and
-- <http://hackage.haskell.org/package/repa Repa> packages:
--
-- * `VU` - Vector Unboxed representation.
-- * `VS` - Vector Storable representation.
-- * `RSU` - Repa Sequential Unboxed array representation (computation is done sequentially).
-- * `RPU` - Repa Parallel Unboxed array representation (computation is done in parallel).
-- * `RSS` - Repa Sequential Storable array representation (computation is done sequentially).
-- * `RPS` - Repa Parallel Storable array representation (computation is done in parallel).
--
-- Images with `RSU`, `RSS`, `RPU` and `RPS` types, most of the time, hold
-- functions rather than an actual data, this way computation can be fused
-- together, and later changed to `VU` or `VS` using `toManifest`, which in turn
-- performs the fused computation. If at any time computation needs to be
-- forced, `compute` can be used for that purpose.
--
-- Many of the function names exported by this module will clash with the ones
-- from "Prelude", hence it can be more convenient to import like this:
--
-- @
-- import Prelude as P
-- import Graphics.Image as I
-- @
--
module Graphics.Image (
  -- * Color Space
  -- $colorspace

  -- * Creation
  --
  -- `makeImageR` is a type restricted version of `makeImage` function, which
  -- simplifies creation of images with `Double` precision and a particular
  -- representation through an extra argument.
  --
  -- If it is necessary to create an image with an arbitrary precision and
  -- representation, `makeImage` function can be used with a manual type
  -- specification of result image, eg:
  --
  -- @ makeImage (256, 256) (PixelY . fromIntegral . fst) :: Image RPU Y Word8 @
  --
  makeImageR, makeImage, fromListsR, fromLists, toLists,
  -- * IO
  module Graphics.Image.IO,
  -- ** Reading
  -- | Read supported files into an 'Image' with pixels in 'Double'
  -- precision. In order to read an image in a different representation, color
  -- space or precision, use 'readImage' or 'readImageExact' from
  -- <Graphics-Image-IO.html Graphics.Image.IO> instead. While reading an image,
  -- it's underlying representation can be specified by passing one of `VU`,
  -- `VS`, `RSU`, `RPU`, `RSS` or `RSU` as the first argument to @readImage*@
  -- functions. Here is a quick demonstration of how two images can be read as
  -- different representations and later easily combined as their average.
  --
  -- >>> cluster <- readImageRGB VU "images/cluster.jpg"
  -- >>> displayImage cluster
  -- >>> centaurus <- readImageRGB VU "images/centaurus.jpg"
  -- >>> displayImage centaurus
  -- >>> displayImage ((cluster + centaurus) / 2)
  --
  -- <<images/cluster.jpg>> <<images/centaurus.jpg>> <<images/centaurus_and_cluster.jpg>>
  --
  readImageY, readImageYA, readImageRGB, readImageRGBA,
  -- ** Writing
  writeImage, displayImage,
  -- * Accessors
  -- ** Dimensions
  rows, cols, dims,
  -- ** Indexing
  index, maybeIndex, defaultIndex, borderIndex,
  -- * Transformation
  -- ** Pointwise
  I.map, imap, I.zipWith, izipWith,
  -- ** Geometric
  I.traverse, traverse2,
  transpose, backpermute,
  (|*|),
  -- * Reduction
  fold, sum, product, maximum, minimum, normalize, eqTol,
  -- * Manifest Image
  -- * Representations
  exchange,
  module IP
  ) where

import Prelude as P hiding (maximum, minimum, sum, product)
import qualified Data.Foldable as F
import Graphics.Image.ColorSpace
import Graphics.Image.IO
import Graphics.Image.Interface as I hiding (Pixel)
import Graphics.Image.Types as IP

import Graphics.Image.Processing as IP
import Graphics.Image.Processing.Binary as IP
import Graphics.Image.Processing.Complex as IP
import Graphics.Image.Processing.Geometric as IP
#ifndef DISABLE_CHART
import Graphics.Image.IO.Histogram as IP
#endif


-- | Create an image with a specified representation and pixels of 'Double'
-- precision. Note, that it is essential for 'Double' precision pixels to keep values
-- normalized in the @[0, 1]@ range in order for an image to be written to file
-- properly.
--
-- >>> let grad_gray = makeImageR VU (200, 200) (\(i, j) -> PixelY (fromIntegral i) / 200 * (fromIntegral j) / 200)
--
-- Because all 'Pixel's and 'Image's are installed into 'Num', above is equivalent to:
--
-- >>> let grad_gray = makeImageR RPU (200, 200) (\(i, j) -> PixelY $ fromIntegral (i*j)) / (200*200)
-- >>> writeImage "images/grad_gray.png" (grad_gray :: Image RPU Y Double)
--
-- Creating color images is just as easy.
--
-- >>> let grad_color = makeImageR VU (200, 200) (\(i, j) -> PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j))) / 400
-- >>> writeImage "images/grad_color.png" grad_color
--
-- <<images/grad_gray.png>> <<images/grad_color.png>>
--
makeImageR :: Array arr cs e =>
              arr -- ^ Underlying image representation.
           -> (Int, Int) -- ^ (@m@ rows, @n@ columns) - dimensions of a new image.
           -> ((Int, Int) -> Pixel cs e)
           -- ^ A function that takes (@i@-th row, and @j@-th column) as an argument
           -- and returns a pixel for that location.
           -> Image arr cs e
makeImageR _ = I.makeImage
{-# INLINE makeImageR #-}

-- | Read image as luma (brightness).
readImageY :: Array arr Y Double => arr -> FilePath -> IO (Image arr Y Double)
readImageY _ = readImage'
{-# INLINE readImageY #-}


-- | Read image as luma with 'Alpha' channel.
readImageYA :: Array arr YA Double => arr -> FilePath -> IO (Image arr YA Double)
readImageYA _ = readImage'
{-# INLINE readImageYA #-}


-- | Read image in RGB colorspace.
readImageRGB :: Array arr RGB Double => arr -> FilePath -> IO (Image arr RGB Double)
readImageRGB _ = readImage'
{-# INLINE readImageRGB #-}


-- | Read image in RGB colorspace with 'Alpha' channel.
readImageRGBA :: Array arr RGBA Double => arr -> FilePath -> IO (Image arr RGBA Double)
readImageRGBA _ = readImage'
{-# INLINE readImageRGBA #-}


-- | Get the number of rows in an image.
--
-- >>> frog <- readImageRGB VU "images/frog.jpg"
-- >>> frog
-- <Image VectorUnboxed RGB (Double): 200x320>
-- >>> rows frog
-- 200
--
rows :: BaseArray arr cs e => Image arr cs e -> Int
rows = fst . dims
{-# INLINE rows #-}


-- | Get the number of columns in an image.
--
-- >>> frog <- readImageRGB VU "images/frog.jpg"
-- >>> frog
-- <Image VectorUnboxed RGB (Double): 200x320>
-- >>> cols frog
-- 320
--
cols :: BaseArray arr cs e => Image arr cs e -> Int
cols = snd . dims
{-# INLINE cols #-}


-- | Sum all pixels in the image.
sum :: Array arr cs e => Image arr cs e -> Pixel cs e
sum = fold (+) 0
{-# INLINE sum #-}


-- | Multiply all pixels in the image.
product :: Array arr cs e => Image arr cs e -> Pixel cs e
product = fold (+) 1
{-# INLINE product #-}


-- | Retrieve the biggest pixel from an image
maximum :: (Array arr cs e, Ord (Pixel cs e)) => Image arr cs e -> Pixel cs e
maximum !img = fold max (index00 img) img
{-# INLINE maximum #-}


-- | Retrieve the smallest pixel from an image
minimum :: (Array arr cs e, Ord (Pixel cs e)) => Image arr cs e -> Pixel cs e
minimum !img = fold min (index00 img) img
{-# INLINE minimum #-}


-- | Scales all of the pixels to be in the range @[0, 1]@.
normalize :: (Array arr cs e, Array arr X e, Fractional e, Ord e) =>
             Image arr cs e -> Image arr cs e
normalize !img = if l == s
                 then (if s < 0 then (*0) else if s > 1 then (*1) else id) img
                 else I.map (liftPx (\ !e -> (e - s) / (l - s))) img
  where
    !(PixelX l, PixelX s) = (maximum (I.map (PixelX . foldl1Px max) img),
                             minimum (I.map (PixelX . foldl1Px min) img))
{-# INLINE normalize #-}


-- | Check weather two images are equal within a tolerance. Useful for comparing
-- images with `Float` or `Double` precision.
eqTol
  :: (Array arr X Bit, Array arr cs e, Ord e) =>
     e -> Image arr cs e -> Image arr cs e -> Bool
eqTol !tol !img1 = IP.and . toImageBinaryUsing2 (eqTolPx tol) img1
{-# INLINE eqTol #-}


-- | Type restricted version of `fromLists` that constructs an image using
-- supplied representation.
fromListsR :: Array arr cs e => arr -> [[Pixel cs e]] -> Image arr cs e
fromListsR _ = fromLists
{-# INLINE fromListsR #-}

-- | Generates a nested list of pixels from an image.
--
-- @ img == fromLists (toLists img) @
--
toLists :: MArray arr cs e => Image arr cs e -> [[Pixel cs e]]
toLists img = [[index img (i, j) | j <- [0..cols img - 1]] | i <- [0..rows img - 1]]

-- $colorspace
-- Here is a list of default Pixels with their respective constructors:
--
-- @
--     * __'Pixel' 'Y' e      = PixelY y__              - Luma, also commonly denoted as __Y'__.
--     * __'Pixel' 'YA' e     = PixelYA y a__           - Luma with alpha.
--     * __'Pixel' 'RGB' e    = PixelRGB r g b__        - Red, Green and Blue.
--     * __'Pixel' 'RGBA' e   = PixelRGBA r g b a__     - RGB with alpha
--     * __'Pixel' 'HSI' e    = PixelHSI h s i__        - Hue, Saturation and Intensity.
--     * __'Pixel' 'HSIA' e   = PixelHSIA h s i a__     - HSI with alpha
--     * __'Pixel' 'CMYK' e   = PixelCMYK c m y k__     - Cyan, Magenta, Yellow and Key (Black).
--     * __'Pixel' 'CMYKA' e  = PixelCMYKA c m y k a__  - CMYK with alpha.
--     * __'Pixel' 'YCbCr' e  = PixelYCbCr y cb cr__    - Luma, blue-difference and red-difference chromas.
--     * __'Pixel' 'YCbCrA' e = PixelYCbCrA y cb cr a__ - YCbCr with alpha.
--       ------------------------------------------------------------------------------------------
--     * __'Pixel' 'X' 'Bit'          = 'on' | 'off'__ - Bi-tonal.
--     * __'Pixel' cs ('Complex' e) = ('Pixel' cs e) '+:' ('Pixel' cs e)__ - Complex pixels with any color space.
--     * __'Pixel' 'X' e         = PixelX g__ - Used to represent binary images as well as any other single channel colorspace, for instance to separate channels from other color spaces into standalone images.
-- @
--
-- Every 'Pixel' is an instance of 'Functor', 'Applicative', 'F.Foldable' and
-- 'Num', as well as 'Floating' and 'Fractional' if __e__ is also an instance.
--
-- All of the functionality related to every 'ColorSpace' is re-exported by
-- "Graphics.Image.Types" module.

