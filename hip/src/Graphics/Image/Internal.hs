{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Graphics.Image.Internal
-- Copyright   : (c) Alexey Kuleshevich 2016-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Internal
  ( Image(..)
  , A.S
  , Ix2(..)
  , Comp(..)
  , Border(..)
  , GrayScale(..)
  , computeI
  , delayPull
  , delayPush
  , makeImage
  , makeImageComp
  , fromLists
  , toLists
  , fromArray
  , toArray
  , setComp
  , dims
  , isEmpty
  , totalPixels
  , map
  , imap
  , zipWith
  , zipWith3
  , izipWith
  , izipWith3
  , transmute
  , transmute2
  , transform
  , transform2
  , backpermute
  , fold
  , foldMono
  , foldSemi
  , foldSemi1
  , maxPixel
  , minPixel
  , maxVal
  , minVal
  , module Data.Massiv.Core
  , module Graphics.Pixel.ColorSpace
  , A.Storable
  ) where

import Control.DeepSeq
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.IO as A
import Data.Massiv.Core
import Data.Semigroup
import Data.Typeable
import GHC.Exts (IsList(..))
import qualified Graphics.Pixel as CM
import Graphics.Pixel.ColorSpace
import Prelude as P hiding (map, traverse, zipWith, zipWith3)

-- | Main data type of the library
data Image cs e = Image { unImage :: !(Array A.S Ix2 (Pixel cs e)) }
-- It is not a newtype, just so the fusion works properly

instance ColorModel cs e => Show (Image cs e) where
  show img =
    let (Sz2 m n) = dims img
    in "<Image " ++
       showsTypeRep (typeRep (Proxy :: Proxy cs)) " " ++
       showsTypeRep (typeRep (Proxy :: Proxy e)) ": " ++ show m ++ "x" ++ show n ++ ">"

instance ColorModel cs e => Eq (Image cs e) where
  (==) img1 img2 = delayPull img1 == delayPull img2
  {-# INLINE [~1] (==) #-}


instance ColorModel cs e => Num (Image cs e) where
  (+)         = liftImage2 (+)
  {-# INLINE [~1] (+) #-}
  (-)         = liftImage2 (-)
  {-# INLINE [~1] (-) #-}
  (*)         = liftImage2 (*)
  {-# INLINE [~1] (*) #-}
  abs         = map abs
  {-# INLINE [~1] abs #-}
  signum      = map signum
  {-# INLINE [~1] signum #-}
  fromInteger = scalar . fromInteger
  {-# INLINE [~1] fromInteger #-}

instance (Fractional e, ColorModel cs e) => Fractional (Image cs e) where
  (/) = liftImage2 (/)
  {-# INLINE [~1] (/) #-}
  fromRational = scalar . fromRational
  {-# INLINE [~1] fromRational #-}


instance (Floating e, ColorModel cs e) => Floating (Image cs e) where
  pi = scalar pi
  {-# INLINE [~1] pi #-}
  exp = map exp
  {-# INLINE [~1] exp #-}
  log = map log
  {-# INLINE [~1] log #-}
  sin = map sin
  {-# INLINE [~1] sin #-}
  cos = map cos
  {-# INLINE [~1] cos #-}
  asin = map asin
  {-# INLINE [~1] asin #-}
  atan = map atan
  {-# INLINE [~1] atan #-}
  acos = map acos
  {-# INLINE [~1] acos #-}
  sinh = map sinh
  {-# INLINE [~1] sinh #-}
  cosh = map cosh
  {-# INLINE [~1] cosh #-}
  asinh = map asinh
  {-# INLINE [~1] asinh #-}
  atanh = map atanh
  {-# INLINE [~1] atanh #-}
  acosh = map acosh
  {-# INLINE [~1] acosh #-}


instance (NFData e, ColorModel cs e) => NFData (Image cs e) where
  rnf (Image arr) = rnf arr

instance ColorModel cs e => IsList (Image cs e) where
  type Item (Image cs e) = [Pixel cs e]
  fromList = fromLists
  toList = toLists

-- Below is a very simplistic, yet extremely powerful HIP fusion guts:

computeI ::
     (Load r Ix2 (Pixel cs e), ColorModel cs e)
  => Array r Ix2 (Pixel cs e)
  -> Image cs e
computeI = Image . A.compute
{-# INLINE [1] computeI #-}

delayPull :: ColorModel cs e => Image cs e -> Array A.D Ix2 (Pixel cs e)
delayPull (Image arr) = A.delay arr
{-# INLINE [1] delayPull #-}

delayPush :: ColorModel cs e => Image cs e -> Array A.DL Ix2 (Pixel cs e)
delayPush (Image arr) = A.toLoadArray arr
{-# INLINE [1] delayPush #-}

{-# RULES
"fuse delayPull/computeI" [~1] forall arr . delayPull (computeI arr) = arr
"fuse delayPush/computeI" [~1] forall arr . delayPush (computeI arr) = arr
 #-}

-- | Get image dimensions. Using this function will cause an image to be fully evaluated and will
-- break the fusion at the call site, so it is recommended to avoid it in favor of `transmute` when
-- possible.
dims :: ColorModel cs e => Image cs e -> Sz2
dims (Image arr) = A.size arr
{-# INLINE dims #-}

-- | Check if image is empty, i.e. at least one of its sides is equal to 0. Uses `dims` underneath.
isEmpty :: ColorModel cs e => Image cs e -> Bool
isEmpty (Image arr) = A.isEmpty arr
{-# INLINE isEmpty #-}

totalPixels :: ColorModel cs e => Image cs e -> Int
totalPixels = A.totalElem . dims
{-# INLINE totalPixels #-}

-- | By default all images are created with parallel computation strategy, but it can be changed
-- with this function.
setComp :: ColorModel cs e => Comp -> Image cs e -> Image cs e
setComp comp = computeI . A.setComp comp . delayPull
{-# INLINE [~1] setComp #-}


-- | Create a scalar image with only one element. Could be handy together with `liftArray2`
-- function.
scalar :: ColorModel cs e => Pixel cs e -> Image cs e
scalar = computeI . A.singleton @A.D
{-# INLINE [~1] scalar #-}

-- | Create an Image by supplying it's dimensions and a pixel generating function.
--
-- It is essential for 'Double' precision pixels to be normalized in the
-- @[0, 1]@ range in order for an image to be written to file properly.
--
-- >>> let grad_gray = makeImage (Sz2 200 200) (\(Ix2 i j) -> PixelY (fromIntegral i) / 200 * (fromIntegral j) / 200) :: Image (Y D65) Double
--
-- Because all 'Pixel's and 'Image's are installed into 'Num', above is equivalent to:
--
-- >>> let grad_gray = makeImage (Sz2 200 200) (\(Ix2 i j) -> PixelY $ fromIntegral (i*j)) / (200*200) :: Image (Y D65) Double
-- >>> writeImage "images/doc/grad_gray.png" grad_gray
--
-- Creating color images is just as easy.
--
-- >>> let grad_color = makeImage (Sz2 200 200) (\(Ix2 i j) -> PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j))) / 400 :: Image SRGB Double
-- >>> writeImage "images/doc/grad_color.png" grad_color
--
-- <<images/doc/grad_gray.png>> <<images/doc/grad_color.png>>
--
--
-- __Note__. If another image(s) is being used to make the new one with this function, it will
-- likely to be breaking fusion and will cause that source image to be fully computed. It is
-- recommended to use `transmute` or `transmute2` instead.
makeImage :: ColorModel cs e =>
             Sz2 -- ^ (@m@ rows `:.` @n@ columns) - dimensions of a new image.
          -> (Ix2 -> Pixel cs e)
          -- ^ A function that takes (@i@-th row `:.` and @j@-th column) as an
          -- argument and returns a pixel for that location.
          -> Image cs e
makeImage sz = computeI . A.makeArrayR A.D Par sz
{-# INLINE [~1] makeImage #-}

-- | Same as `makeImage`, except computation startegy can be supplied as an argument.
makeImageComp :: ColorModel cs e =>
                 Comp
              -> Sz2 -- ^ (@m@ rows `:.` @n@ columns) - dimensions of a new image.
              -> (Ix2 -> Pixel cs e)
              -- ^ A function that takes (@i@-th row `:.` and @j@-th column) as an
              -- argument and returns a pixel for that location.
             -> Image cs e
makeImageComp comp sz = computeI . A.makeArrayR A.D comp sz
{-# INLINE [~1] makeImageComp #-}

-- | Convert a 2-dimensional source array of pixels into an image.
fromArray :: (Source r Ix2 (Pixel cs e), ColorModel cs e) => Array r Ix2 (Pixel cs e) -> Image cs e
fromArray = Image . A.computeSource
{-# INLINE fromArray #-}

-- | Convert an image into a storable array of pixels
toArray :: Image cs e -> Array A.S Ix2 (Pixel cs e)
toArray (Image arr) = arr
{-# INLINE toArray #-}


-- | Construct an image from a nested rectangular shaped list of pixels.  Length of an outer list
-- will constitute @m@ rows, while the length of inner lists - @n@ columns. All of the inner lists
-- must be of the same length, otherwise an error will be thrown.
--
-- >>> fromLists [[PixelY (fromIntegral (i*j) / 60000) | j <- [1..300]] | i <- [1..200]]
--
-- <<images/doc/grad_fromLists.png>>
--
fromLists :: ColorModel cs e => [[Pixel cs e]] -> Image cs e
fromLists = Image . A.fromLists' Par
{-# INLINE fromLists #-}


-- | Convert an image into a nested lists of pixels
--
-- prop> img == fromLists (toLists img)
toLists :: ColorModel cs e => Image cs e -> [[Pixel cs e]]
toLists (Image arr) = A.toLists arr
{-# INLINE toLists #-}




-- Ops

-- | Map a function over a an image.
map :: (ColorModel cs' e', ColorModel cs e) =>
       (Pixel cs' e' -> Pixel cs e)
       -- ^ A function that takes a pixel of a source image and returns a pixel
       -- for the result image a the same location.
    -> Image cs' e' -- ^ Source image.
    -> Image cs e   -- ^ Result image.
map f = computeI . A.map f . delayPull
{-# INLINE [~1] map #-}

-- | Map an index aware function over each pixel in an image.
imap :: (ColorModel cs' e', ColorModel cs e) =>
        (Ix2 -> Pixel cs' e' -> Pixel cs e)
      -- ^ A function that takes an index @(i, j)@, a pixel at that location
      -- and returns a new pixel at the same location for the result image.
     -> Image cs' e' -- ^ Source image.
     -> Image cs e   -- ^ Result image.
imap f = computeI . A.imap f . delayPull
{-# INLINE [~1] imap #-}

-- | Zip two images with a function
zipWith :: (ColorModel cs1 e1, ColorModel cs2 e2, ColorModel cs e) =>
           (Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
        -> Image cs1 e1 -> Image cs2 e2 -> Image cs e
zipWith f img1 img2 = computeI $ A.zipWith f (delayPull img1) (delayPull img2)
{-# INLINE [~1] zipWith #-}

-- | Zip two images with an index aware function
izipWith :: (ColorModel cs1 e1, ColorModel cs2 e2, ColorModel cs e) =>
            (Ix2 -> Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
         -> Image cs1 e1 -> Image cs2 e2 -> Image cs e
izipWith f img1 img2 = computeI $ A.izipWith f (delayPull img1) (delayPull img2)
{-# INLINE [~1] izipWith #-}

-- | Zip three images with a function
zipWith3 :: (ColorModel cs1 e1, ColorModel cs2 e2, ColorModel cs3 e3, ColorModel cs e) =>
           (Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs3 e3 -> Pixel cs e)
        -> Image cs1 e1 -> Image cs2 e2 -> Image cs3 e3 -> Image cs e
zipWith3 f img1 img2 img3 = computeI $ A.zipWith3 f (delayPull img1) (delayPull img2) (delayPull img3)
{-# INLINE [~1] zipWith3 #-}

-- | Zip three images with an index aware function
izipWith3 :: (ColorModel cs1 e1, ColorModel cs2 e2, ColorModel cs3 e3, ColorModel cs e) =>
            (Ix2 -> Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs3 e3 -> Pixel cs e)
         -> Image cs1 e1 -> Image cs2 e2 -> Image cs3 e3 -> Image cs e
izipWith3 f img1 img2 img3 = computeI $ A.izipWith3 f (delayPull img1) (delayPull img2) (delayPull img3)
{-# INLINE [~1] izipWith3 #-}


-- | Transmute an image
transmute ::
     (ColorModel cs' e', ColorModel cs e)
  => (Sz2 -> Sz2) -- ^ Function that takes source image dimensions and returns dimensions of a new                  -- image.
  -> ((Ix2 -> Pixel cs' e') -> Ix2 -> Pixel cs e)
  -- ^ Function that receives a pixel getter (a source image index function), a location @(i :. j)@
  -- in a new image and returns a pixel for that location.
  -> Image cs' e' -- ^ Source image.
  -> Image cs e
transmute fDim f = computeI . transmuteArray fDim f . delayPull
{-# INLINE [~1] transmute #-}

-- | Create an image, same as `transmute`, except by traversing two source images.
transmute2 ::
     (ColorModel cs1 e1, ColorModel cs2 e2, ColorModel cs e)
  => (Sz2 -> Sz2 -> Sz2) -- ^ Function that returns dimensions of a new image.
  -> ((Ix2 -> Pixel cs1 e1) -> (Ix2 -> Pixel cs2 e2) -> Ix2 -> Pixel cs e)
         -- ^ Function that receives pixel getters, a location @(i :. j)@ in a new image and returns a
         -- pixel for that location.
  -> Image cs1 e1 -- ^ First source image.
  -> Image cs2 e2 -- ^ Second source image.
  -> Image cs e
transmute2 fDims f img1 img2 = computeI $ transmuteArray2 fDims f (delayPull img1) (delayPull img2)
{-# INLINE [~1] transmute2 #-}


-- | Sort of like `transmute`, but there is ability to use result of size generating function inside
-- of the element generating function
transform ::
     (ColorModel cs' e', ColorModel cs e)
  => (Sz2 -> (Sz2, a)) -- ^ Function that takes source image dimensions and returns dimensions of a
                  -- new image as well as anything else that will be available to an indexing
                  -- function.
  -> (a -> (Ix2 -> Pixel cs' e') -> Ix2 -> Pixel cs e)
  -- ^ Function that receives a pixel getter (a source image index function), a location @(i :. j)@
  -- in a new image and returns a pixel for that location.
  -> Image cs' e' -- ^ Source image.
  -> Image cs e
transform fDim f = computeI . A.transform' fDim f . delayPull
{-# INLINE [~1] transform #-}

-- | Create an image, same as `transform`, except by working on two source images.
transform2 ::
     (ColorModel cs1 e1, ColorModel cs2 e2, ColorModel cs e)
  => (Sz2 -> Sz2 -> (Sz2, a)) -- ^ Function that returns dimensions of a new image and other data
                              -- for indexing function.
  -> (a -> (Ix2 -> Pixel cs1 e1) -> (Ix2 -> Pixel cs2 e2) -> Ix2 -> Pixel cs e)
         -- ^ Function that receives pixel getters, a location @(i :. j)@ in a new image and returns a
         -- pixel for that location.
  -> Image cs1 e1 -- ^ First source image.
  -> Image cs2 e2 -- ^ Second source image.
  -> Image cs e
transform2 fDims f img1 img2 = computeI $ A.transform2' fDims f (delayPull img1) (delayPull img2)
{-# INLINE [~1] transform2 #-}

-- | Sort of like `transmute`, but there is ability to use result of size generating function inside
-- of the element generating function
backpermute ::
     (ColorModel cs e)
  => (Sz2 -> (Sz2, a))
  -- ^ Function that takes source image dimensions and returns dimensions of a new image as well as
  -- anything else that will be available to an indexing function.
  -> (a -> Ix2 -> Ix2)
  -- ^ Function that maps an index of a result image to an index of a source image.
  -> Image cs e -- ^ Source image.
  -> Image cs e
backpermute fDim f =
  computeI .
  (\arr ->
     let (sz, a) = fDim (A.size arr)
     in A.backpermute' sz (f a) arr) .
  delayPull
{-# INLINE [~1] backpermute #-}


-- Folding

-- | Undirected reduction of an image.
--
-- >>> fold (+) 0 (makeImage (Sz2 1 4) (\(Ix2 _ iy) -> pure iy) :: Image Model.Y Int)
-- <Y:(         6)>
fold :: ColorModel cs e =>
        (Pixel cs e -> Pixel cs e -> Pixel cs e) -- ^ An associative folding function.
     -> Pixel cs e -- ^ Initial element, that is neutral with respect to the folding function.
     -> Image cs e -- ^ Source image.
     -> Pixel cs e
fold f = flip (appEndo . A.foldMono (Endo . f) . delayPull)
{-# INLINE [~1] fold #-}

-- | Monoidal reduction of an image.
foldMono ::
     (Monoid m, ColorModel cs e)
  => (Pixel cs e -> m) -- ^ Function that converts every pixel in the image to a `Monoid`.
  -> Image cs e -- ^ Source image.
  -> m
foldMono f = A.foldMono f . delayPull
{-# INLINE [~1] foldMono #-}


-- | Semigroup reduction of an image.
foldSemi ::
     (Semigroup m, ColorModel cs e)
  => (Pixel cs e -> m) -- ^ Function that converts every pixel in the image to a `Semigroup`.
  -> m -- ^ Initial Element
  -> Image cs e -- ^ Source image.
  -> m
foldSemi f m = A.foldSemi f m . delayPull
{-# INLINE [~1] foldSemi #-}

-- FIX: initial element will get counted twice. Either implement fold1 upstream or do `(extract 1
-- (sz-1) . resize (totalElem sz))`. Benchmark the affect of adjustment.
-- | Semigroup reduction of a non-empty image, while using the 0th pixel as initial element. Will
-- throw an index out of bounds error if image is empty.
foldSemi1 ::
     (Semigroup m, ColorModel cs e)
  => (Pixel cs e -> m) -- ^ Function that converts every pixel in the image to a `Semigroup`.
  -> Image cs e -- ^ Source image.
  -> m
foldSemi1 f = (\arr -> A.foldSemi f (f (A.evaluate' arr 0)) arr) . delayPull
{-# INLINE [~1] foldSemi1 #-}

-- | Find the largest pixel. Throws an error on empty (see `isEmpty`) images.
maxPixel ::
     (Ord (Pixel cs e), ColorModel cs e)
  => Image cs e -- ^ Source image.
  -> Pixel cs e
maxPixel = getMax . foldSemi1 Max
{-# INLINE [~1] maxPixel #-}

-- | Find the largest pixel. Throws an error on empty (see `isEmpty`) images.
minPixel ::
     (Ord (Pixel cs e), ColorModel cs e)
  => Image cs e -- ^ Source image.
  -> Pixel cs e
minPixel = getMin . foldSemi1 Min
{-# INLINE [~1] minPixel #-}

-- | Find the largest channel value among all pixels in the image. Throws an error on empty (see
-- `isEmpty`) images.
maxVal :: (Ord e, ColorModel cs e) => Image cs e -> e
maxVal = (A.maximum' . A.map maximum) . delayPull
{-# INLINE [~1] maxVal #-}

-- | Find the smallest channel value among all pixels in the image. Throws an error on empty (see
-- `isEmpty`) images.
minVal :: (Ord e, ColorModel cs e) => Image cs e -> e
minVal = (A.minimum' . A.map minimum) . delayPull
{-# INLINE [~1] minVal #-}


-- Lifting


-- | Zip two images with a function
liftImage2 :: (ColorModel cs1 e1, ColorModel cs2 e2, ColorModel cs e) =>
           (Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
        -> Image cs1 e1 -> Image cs2 e2 -> Image cs e
liftImage2 f img1 img2 = computeI $ A.liftArray2 f (delayPull img1) (delayPull img2)
{-# INLINE [~1] liftImage2 #-}

-- Array functions.

-- | Create an array by traversing a source array.
transmuteArray ::
     (Source r1 Ix2 e1)
  => (Sz2 -> Sz2)
  -> ((Ix2 -> e1) -> Ix2 -> e)
  -> Array r1 Ix2 e1
  -> Array A.D Ix2 e
transmuteArray fSz f = A.transform' (\sz -> (fSz sz, ())) (const f)
{-# INLINE transmuteArray #-}

-- | Create an array, same as `transmuteArray`, except by traversing two source arrays.
transmuteArray2
  :: (Source r1 ix1 e1, Source r2 ix2 e2, Index ix)
  => (Sz ix1 -> Sz ix2 -> Sz ix)
  -> ((ix1 -> e1) -> (ix2 -> e2) -> ix -> e)
  -> Array r1 ix1 e1
  -> Array r2 ix2 e2
  -> Array A.D ix e
transmuteArray2 fSz f arr1 arr2 =
  A.makeArray
    (A.getComp arr1)
    (fSz (A.size arr1) (A.size arr2))
    (f (A.evaluate' arr1) (A.evaluate' arr2))
{-# INLINE transmuteArray2 #-}



-- | Any color space or color model that has a separate non-chromatic channel for
-- brightness allows us applying a function just to that channel with th e help of
-- this class.
class GrayScale cs where

  -- | Extract the gray scale brightness info, whatever that might mean for a particular
  -- color space
  toGrayScale :: ColorModel cs e => Image cs e -> Image CM.Y e

  -- | Apply a function only to the grayscale channel, leaving the chromatic information
  -- unchanged.
  applyGrayScale :: ColorModel cs e => (Image CM.Y e -> Image CM.Y e) -> Image cs e -> Image cs e

instance GrayScale CM.Y where
  toGrayScale = id
  applyGrayScale = ($)

instance GrayScale CM.YCbCr where
  toGrayScale = map (\(CM.PixelYCbCr y' _ _) -> CM.PixelY y')
  applyGrayScale f img = zipWith recombine img (f (toGrayScale img))
    where
      recombine (CM.PixelYCbCr _ cb cr) (CM.PixelY y') = CM.PixelYCbCr y' cb cr


instance GrayScale CM.HSI where
  applyGrayScale f img =
    zipWith recombine img (f (map (\(CM.PixelHSI _ _ i) -> CM.PixelY i) img))
    where
      recombine (CM.PixelHSI h s _) (CM.PixelY i) = CM.PixelHSI h s i


instance GrayScale CM.HSL where
  applyGrayScale f img =
    zipWith recombine img (f (map (\(CM.PixelHSL _ _ l) -> CM.PixelY l) img))
    where recombine (CM.PixelHSL h s _) (CM.PixelY l) = CM.PixelHSL h s l


instance GrayScale CM.HSV where
  applyGrayScale f img =
    zipWith recombine img (f (map (\(CM.PixelHSV _ _ v) -> CM.PixelY v) img))
    where recombine (CM.PixelHSV h s _) (CM.PixelY v) = CM.PixelHSV h s v


instance GrayScale (Y i) where
  applyGrayScale f (Image arr) =
    Image (A.fromImageBaseModel (unImage (f (Image (A.toImageBaseModel arr)))))


instance GrayScale (Y' cs) where
  applyGrayScale f (Image arr) =
    Image (A.fromImageBaseModel (unImage (f (Image (A.toImageBaseModel arr)))))


instance GrayScale (Y'CbCr cs) where
  applyGrayScale f img =
    zipWith recombine img (f (map (\(PixelY'CbCr y' _ _) -> CM.PixelY y') img))
    where
      recombine (PixelY'CbCr _ cb cr) (CM.PixelY y') = PixelY'CbCr y' cb cr


instance GrayScale (HSI cs) where
  applyGrayScale f img = zipWith recombine img (f (map (\(PixelHSI _ _ i) -> CM.PixelY i) img))
    where
      recombine (PixelHSI h s _) (CM.PixelY i) = PixelHSI h s i


instance GrayScale (HSL cs) where
  applyGrayScale f img = zipWith recombine img (f (map (\(PixelHSL _ _ l) -> CM.PixelY l) img))
    where
      recombine (PixelHSL h s _) (CM.PixelY l) = PixelHSL h s l


instance GrayScale (HSV cs) where
  applyGrayScale f img = zipWith recombine img (f (map (\(PixelHSV _ _ v) -> CM.PixelY v) img))
    where
      recombine (PixelHSV h s _) (CM.PixelY v) = PixelHSV h s v

instance GrayScale (LAB i) where
  applyGrayScale f img = zipWith recombine img (f (map (\(PixelLAB l _ _) -> CM.PixelY l) img))
    where
      recombine (PixelLAB _ a b) (CM.PixelY l) = PixelLAB l a b

instance GrayScale (XYZ i) where
  applyGrayScale f img = zipWith recombine img (f (map (\(PixelXYZ _ y _) -> CM.PixelY y) img))
    where
      recombine (PixelXYZ x _ z) (CM.PixelY y) = PixelXYZ x y z
