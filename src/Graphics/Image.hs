{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
  -- $colorspace
  module Graphics.Image.ColorSpace,
  -- 
  -- * Creation
  --
  -- If it is necessary to create an image in an other representation
  -- or with some specific 'Pixel' precision, you can use 'I.makeImage' from
  -- "Graphics.Image.Interface" module and manually specifying function's output
  -- type.
  --
  -- @ makeImage (256, 256) (PixelY . fromIntegral . fst) :: Image RP Y Word8 @
  --
  makeImage, I.fromLists,
  -- * IO
  -- ** Reading
  -- | Read any supported image file into an 'Image' with 'VU' (Vector Unboxed)
  -- representation and pixels with 'Double' precision. In order to read an
  -- image with different representation, color space and precision 'readImage'
  -- or 'readImageExact' from <Graphics-Image-IO.html Graphics.Image.IO> can be
  -- used.
  readImageY, readImageYA, readImageRGB, readImageRGBA,
  -- ** Writing
  writeImage, displayImage,
  -- * Accessors
  -- ** Dimensions
  rows, cols, I.dims,
  -- ** Indexing
  I.index, I.defaultIndex, I.maybeIndex,
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
import qualified Data.Foldable as F
import Graphics.Image.ColorSpace
import Graphics.Image.IO --(writeImage, displayImage)
import Graphics.Image.Interface (Array, ManifestArray, Image)
import qualified Graphics.Image.Interface as I
import Graphics.Image.Interface.Vector
import Graphics.Image.Interface.Repa (RD(..), RS(..), RP(..))

--import Graphics.Image.Processing
--import Graphics.Image.Processing.Complex



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


-- | Scales all of the pixels to be in the range @[0, 1]@.
normalize :: (ManifestArray arr cs e, ManifestArray arr Gray e, Fractional e, Ord e) =>
             Image arr cs e -> Image arr cs e
normalize !img = if l == s
                 then (if s < 0 then (*0) else if s > 1 then (*1) else id) img
                 else I.map normalizer img
  where
    !(PixelGray l, PixelGray s) = (maximum $ I.map (PixelGray . F.maximum) img,
                                   minimum $ I.map (PixelGray . F.minimum) img)
    normalizer !px = (px - pure s) / (pure (l - s))
    {-# INLINE normalizer #-}
{-# INLINE normalize #-}


-- $colorspace
-- Here is a list of default Pixels with their respective constructors:
--
-- @
--     * __'Pixel' 'Y' e      = PixelY e__ - Luma, also commonly denoted as __Y'__.
--     * __'Pixel' 'YA' e     = PixelYA e__ - Luma with alpha.
--     * __'Pixel' 'RGB' e    = PixelRGB e__ - Red, Green and Blue.
--     * __'Pixel' 'RGBA' e   = PixelRGBA e__ - RGB with alpha
--     * __'Pixel' 'HSI' e    = PixelHSI e__ - Hue, Saturation and Intensity.
--     * __'Pixel' 'HSIA' e   = PixelHSIA e__ - HSI with alpha
--     * __'Pixel' 'CMYK' e   = PixelCMYK e__ - Cyan, Magenta, Yellow and Key (Black).
--     * __'Pixel' 'CMYKA' e  = PixelCMYKA e__ - CMYK with alpha.
--     * __'Pixel' 'YCbCr' e  = PixelYCbCr e__ - Luma, blue-difference and red-difference chromas.
--     * __'Pixel' 'YCbCrA' e = PixelYCbCrA e__ - YCbCr with alpha.
--       ------------------------------------------------------------------------------------------
--     * __'Pixel' 'Binary' 'Bit'     = 'on' | 'off'__ - Bi-tonal.
--     * __'Pixel' 'Gray' e         = PixelGray e__ - Used for separating channels from other color spaces.
--     * __'Pixel' cs ('Complex' e) = ('Pixel' cs e) '+:' ('Pixel' cs e)__ - Complex pixels with any color space.
-- @
--
-- Every 'Pixel' is an instance of 'Functor', 'Applicative', 'F.Foldable' and
-- 'Num', as well as 'Floating' and 'Fractional' if __e__ is also an instance.
--
-- All of the functionality related to every 'I.ColorSpace' is re-exported from here
-- for convenience.
