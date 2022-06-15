{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module      : Graphics.Image
-- Copyright   : (c) Alexey Kuleshevich 2016-2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell Image Processing (HIP) library is a wrapper around an array library called
-- <http://hackage.haskell.org/package/massiv massiv>, which will seemlessly handle parallel and
-- sequential computation as well as fusing most of operations together. Prior to version 2.x of HIP
-- it was required to specify various array representations manually, although similar approach is
-- still used in @massiv@, HIP became much simpler in that aspect and only retained foreign
-- representation, which is hidden from the user. At the same time it means all of the images are
-- backed by pinned memory, therefore all computations are performed efficiently.
--
-- * @__'Image' cs e__@, where @__cs__@ is the `ColorModel` of an image and @__e__@ is the type
-- denoting precision of an image (@Int@, @Word@, @Double@, etc.) .
--
-- Many of the function names exported by this module will clash with the ones from "Prelude", hence
-- it can be more convenient to import like this:
--
-- @
-- import Prelude as P
-- import Graphics.Image as I
-- @
--
module Graphics.Image
  (
  -- * Color Space
  -- $colorspace
  -- * Creation
  --
  -- `makeImage` is a type restricted version of `makeImage` function, which
  -- simplifies creation of images with `Double` precision and a particular
  -- representation through an extra argument.
  --
  -- If it is necessary to create an image with an arbitrary precision and
  -- representation, `makeImage` function can be used with a manual type
  -- specification of the result image, eg:
  --
  -- @ makeImage (256 :. 256) (\(i :. _) -> PixelY (fromIntegral i)) :: I.Image Y Word8 @
  --
    Image(..)
  , Ix2(..)
  , Sz(..)
  , Sz2
  , module Core -- TODO: Reduce export footprint
  , makeImage
  -- * Computation
  , Comp(..)
  , makeImageComp
  , setComp
  -- * Conversion
  , fromArray
  , toArray
  , fromLists
  , toLists
  -- * IO
  , module Graphics.Image.IO
  -- ** Reading
  -- | Read supported files into an 'Image' possibly with automatic color space conversion. Here is
  -- a quick demonstration of how two images can be read as different representations and later
  -- easily combined as their average.
  --
  -- >>> cluster <- readImageRGB "images/cluster.jpg"
  -- >>> displayImage cluster
  -- >>> centaurus <- readImageRGB "images/centaurus.jpg"
  -- >>> displayImage centaurus
  -- >>> displayImage ((cluster + centaurus) / 2)
  --
  -- <<images/doc/cluster.jpg>> <<images/doc/centaurus.jpg>> <<images/doc/centaurus_and_cluster.jpg>>
  --
  -- * Accessors
  -- ** Dimensions
  , rows
  , cols
  , dims
  -- ** Indexing
  , (!)
  , (!?)
  , index
  , indexM
  , maybeIndex
  , defaultIndex
  , borderIndex
  , Border(..)
  -- *** Tuple conversion
  , A.fromIx2
  , A.toIx2
  -- * Transformation
  -- ** Pointwise
  , map
  , imap
  , zipWith
  , zipWith3
  , izipWith
  , izipWith3
  -- ** Geometric
  , transmute
  , transmute2
  , transform
  , transform2
  --, transpose
  , module I
  , module IP
  --, module Graphics.Pixel
  -- (|*|),
  -- * Reduction
  , fold
  , foldMono
  , foldSemi
  , sum
  , product
  , maximum
  , minimum
  , normalize
  , eqTol
  ) where

import qualified Data.Foldable as F
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Core as Core
import Graphics.Image.Internal as I
import Graphics.Image.IO
import Graphics.Image.Processing as IP
import Prelude as P hiding (map, maximum, minimum, product, sum, traverse,
                     zipWith, zipWith3)

-- import Graphics.Image.Types as IP

-- import Graphics.Image.Processing as IP
-- import Graphics.Image.Processing.Binary as IP
-- import Graphics.Image.Processing.Complex as IP
-- import Graphics.Image.Processing.Geometric as IP
-- import Graphics.Image.IO.Histogram as IP


-- | Get the number of rows in an image. Same as `dims`, it does break fusion.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RGB Double: 200x300>
-- >>> rows frog
-- 200
--
rows :: Image cs e -> Int
rows img = let Sz (m :. _) = dims img in m
{-# INLINE rows #-}


-- | Get the number of columns in an image. Same as `dims`, it does break fusion.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog
-- <Image RGB Double: 200x320>
-- >>> cols frog
-- 320
--
cols :: Image cs e -> Int
cols img = let Sz (_ :. n) = dims img in n
{-# INLINE cols #-}


--------------
-- Indexing --
--------------

-- | Get a pixel at @i@-th and @j@-th location.
--
-- >>> img = makeImage (200 :. 200) (\(i :. j) -> PixelY $ fromIntegral (i*j)) / (200*200)
-- >>> index img (20 :. 30)
-- <Luma:(1.5e-2)>
--
index :: ColorModel cs e => Image cs e -> Ix2 -> Pixel cs e
index (Image arr) = A.index' arr
{-# INLINE index #-}

-- | Same as `index`, get a pixel at @i@-th and @j@-th location, but fail
-- according to the `MonadThrow` for out of bounds, instead of just an error.
--
-- >>> img = makeImage (200 :. 200) (\(i :. j) -> PixelY $ fromIntegral (i*j)) / (200*200)
-- >>> indexM img (20 :. 30)
-- <Luma:(1.5e-2)>
-- >>> indexM img (0 :. 30000) :: Maybe (Pixel Y Double)
-- Nothing
--
indexM :: (ColorModel cs e, MonadThrow m) => Image cs e -> Ix2 -> m (Pixel cs e)
indexM (Image arr) = A.indexM arr
{-# INLINE indexM #-}


-- | Infix synonym for `index`.
(!) :: ColorModel cs e => Image cs e -> Ix2 -> Pixel cs e
(!) (Image arr) = A.index' arr
{-# INLINE (!) #-}


-- | Image indexing function that returns a default pixel if index is out of bounds.
defaultIndex :: ColorModel cs e =>
                Pixel cs e -> Image cs e -> Ix2 -> Pixel cs e
defaultIndex px (Image arr) = A.defaultIndex px arr
{-# INLINE defaultIndex #-}


-- | Image indexing function that uses a special border resolutions strategy for
-- out of bounds pixels.
borderIndex :: ColorModel cs e =>
               Border (Pixel cs e) -> Image cs e -> Ix2 -> Pixel cs e
borderIndex atBorder (Image arr) = A.borderIndex atBorder arr
{-# INLINE borderIndex #-}


-- | Image indexing function that returns @'Nothing'@ if index is out of bounds,
-- @'Just' px@ otherwise.
maybeIndex :: ColorModel cs e =>
              Image cs e -> Ix2 -> Maybe (Pixel cs e)
maybeIndex (Image arr) = A.index arr
{-# INLINE maybeIndex #-}

-- | Infix synonym for `maybeIndex`.
(!?) :: ColorModel cs e => Image cs e -> Ix2 -> Maybe (Pixel cs e)
(!?) (Image arr) = A.index arr
{-# INLINE (!?) #-}


-------------
-- Folding --
-------------

-- | Sum all pixels in the image.
sum :: ColorModel cs e => Image cs e -> Pixel cs e
sum = A.sum . delayPull
{-# INLINE [~1] sum #-}


-- | Multiply all pixels in the image.
product :: ColorModel cs e => Image cs e -> Pixel cs e
product = A.product . delayPull
{-# INLINE [~1] product #-}


-- | Retrieve the biggest pixel from an image
maximum :: (ColorModel cs e, Ord (Color cs e)) => Image cs e -> Pixel cs e
maximum = A.maximum' . delayPull
{-# INLINE [~1] maximum #-}


-- | Retrieve the smallest pixel from an image
minimum :: (ColorModel cs e, Ord (Color cs e)) => Image cs e -> Pixel cs e
minimum = A.minimum' . delayPull
{-# INLINE [~1] minimum #-}


-- | Scales all of the pixels to be in the range @[0, 1]@.
normalize :: (ColorModel cs e, Ord e) =>
             Image cs e -> Image cs e
normalize img =
  I.map (fmap (\e -> (e - iMin) * ((maxValue - minValue) // (iMax - iMin)) + minValue)) img
  where
    !iMax = maxVal img
    !iMin = minVal img
{-# INLINE normalize #-}

-- | Check weather two images are equal within a tolerance. Useful for comparing
-- images with `Float` or `Double` precision.
--
-- >>> eqTol 0.99 (makeImage (Sz2 2 2) (const 0) :: Image Y Float) (makeImage (Sz2 2 2) (const 1))
-- False
--
-- >>> eqTol 1 (makeImage (Sz2 2 2) (const 0) :: Image Y Float) (makeImage (Sz2 2 2) (const 1))
-- True
eqTol
  :: (ColorModel cs e, Ord e) =>
     e -> Image cs e -> Image cs e -> Bool
eqTol !tol !img1 =
  IP.and . thresholdWith2 thresholdPixel img1
  where
    thresholdPixel pixelA pixelB = F.foldl' (&&) True (thresholdComponent <$> pixelA <*> pixelB)
    thresholdComponent compA compB = abs (compA - compB) <= tol
{-# INLINE eqTol #-}



-- $colorspace
-- Here is a list of Pixels with their respective constructors:
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

