{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Graphics.Image.Processing.Filter
-- Copyright   : (c) Alexey Kuleshevich 2017-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Filter
  ( -- * Filter
    -- $filter
    Filter(..)
  , Filter'
  , A.Value
  , A.Padding(..)
  -- ** Application
  , mapFilter
  , mapFilterWithStride
  , applyFilter
  , applyFilterWithStride
  -- ** Creation
  , makeFilter
  -- ** Conversion
  , fromStencil
  , toStencil
  -- ** Profunctor
  , lmapFilter
  , rmapFilter
  , dimapFilter
  -- * Available filters
  -- ** Average
  , averageBlur
  , averageBlur3x3
  , averageBlur5x5
  , averageBlur7x7
  , average1x3
  , average1x5
  , average1x7
  , average3x1
  , average5x1
  , average7x1
  , makeAverageStencil
  -- ** Gaussian
  , gaussianBlur
  , gaussianBlur3x3
  , gaussianBlur5x5
  , gaussianBlur7x7
  , gaussian1x3
  , gaussian1x5
  , gaussian1x7
  , gaussian3x1
  , gaussian5x1
  , gaussian7x1
  -- ** Laplacian
  , laplacian
  -- ** Laplacian of Gaussian
  --, laplacianOfGaussian
  -- ** Sobel
  , sobelHorizontal
  , sobelVertical
  , sobelOperator
  , sobelOperatorNormal
  -- ** Prewitt
  , prewittHorizontal
  , prewittVertical
  , prewittOperator
  , prewittOperatorNormal
  -- * Kernel factory
  , makeKernel1D
  , makeKernel2D
  , estimateFunction1D
  , estimateFunction2D
  , gaussianFunction1D
  , gaussianFunction2D
  ) where

import Control.Applicative
import Control.DeepSeq
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Numeric.Integral as A
import Data.Maybe
import Graphics.Image.Internal
import Graphics.Pixel
import Prelude as P


-- | Filter that can be applied to an image using `mapFilter`.
newtype Filter cs a b = Filter
  { filterStencil :: A.Stencil Ix2 (Pixel cs a) (Pixel cs b)
  } deriving (NFData, Num, Fractional, Floating)

-- | Helper type synonym for `Filter` where both input and output precision are the same.
type Filter' cs e = Filter cs e e

instance Functor (Color cs) => Functor (Filter cs a) where
  fmap f (Filter s) = Filter (fmap (fmap f) s)
  {-# INLINE fmap #-}

instance (ColorModel cs a, Applicative (Color cs)) => Applicative (Filter cs a) where
  pure a = Filter $ pure (pure a)
  {-# INLINE pure #-}
  liftA2 f (Filter x) (Filter y) = Filter (liftA2 (liftA2 f) x y)
  {-# INLINE liftA2 #-}

-- | Applying a filter is just like mapping a `Stencil` with `A.mapStencil`, which is similar to
-- mapping a function over an array, with an exception of border resolution and underlying
-- complexity.
--
-- >>> batRGB <- readImageRGB "images/megabat.jpg"
-- >>> writeImage "images/megabat_sobel_rgb.jpg" $ normalize $ mapFilter Edge sobelOperator batRGB
-- >>> let batY = I.map toPixelY batRGB
-- >>> writeImage "images/megabat_sobel.jpg" $ normalize $ mapFilter Edge sobelOperator batY
--
-- <<images/megabat.jpg>> <<images/megabat_sobel_rgb.jpg>> <<images/megabat_sobel.jpg>>
--
-- With filter application normalization is often desired, see `laplacian` for an example without
-- normalization.
mapFilter ::
     (ColorModel cs a, ColorModel cs b)
  => Border (Pixel cs a)
  -> Filter cs a b
  -> Image cs a
  -> Image cs b
mapFilter border f (Image arr) =
  Image (A.compute (A.mapStencil border (filterStencil f) arr))
{-# INLINE mapFilter #-}

applyFilter ::
     (ColorModel cs a, ColorModel cs b)
  => A.Padding Ix2 (Pixel cs a)
  -> Filter cs a b
  -> Image cs a
  -> Image cs b
applyFilter padding f (Image arr) =
  Image (A.compute (A.applyStencil padding (filterStencil f) arr))
{-# INLINE applyFilter #-}


mapFilterWithStride ::
     (ColorModel cs a, ColorModel cs b)
  => Border (Pixel cs a)
  -> Filter cs a b
  -> Stride Ix2
  -> Image cs a
  -> Image cs b
mapFilterWithStride border f stride (Image arr) =
  Image (A.computeWithStride stride (A.mapStencil border (filterStencil f) arr))
{-# INLINE mapFilterWithStride #-}

applyFilterWithStride ::
     (ColorModel cs a, ColorModel cs b)
  => A.Padding Ix2 (Pixel cs a)
  -> Filter cs a b
  -> Stride Ix2
  -> Image cs a
  -> Image cs b
applyFilterWithStride padding f stride (Image arr) =
  Image (A.computeWithStride stride (A.applyStencil padding (filterStencil f) arr))
{-# INLINE applyFilterWithStride #-}


-- | Create a custom filter
makeFilter ::
     ColorModel cs a
  => Sz2 -- ^ Filter size
  -> Ix2 -- ^ Filter center
  -> ((Ix2 -> A.Value (Pixel cs a)) -> A.Value (Pixel cs b)) -- ^ Filter stencil
  -> Filter cs a b
makeFilter sz ix = Filter . A.makeStencil sz ix
{-# INLINE makeFilter #-}

-- | Convert from 2D `A.Stencil`.
fromStencil :: A.Stencil Ix2 (Pixel cs a) (Pixel cs b) -> Filter cs a b
fromStencil = Filter
{-# INLINE fromStencil #-}

-- | Convert to a 2D `A.Stencil`.
toStencil :: Filter cs a b -> A.Stencil Ix2 (Pixel cs a) (Pixel cs b)
toStencil = filterStencil
{-# INLINE toStencil #-}

-- | `Filter` is contravariant in the second type argument, which is the type of elements in
-- pixels of the image, that the filter will be applied to. In other words function @f@ supplied
-- to `lmapFilter` will be applied to each element of the image before applying the stencil to it.
--
-- /Note/ - This function must be used with care, since filters will usually use each pixel from
-- the source image many times, the supplied function will also get called that many time for each
-- pixel. For that reason, most of the time, it will make more sense to apply the function
-- directly to the source image prior to applying the filter to it.
lmapFilter :: Functor (Color cs) => (a' -> a) -> Filter cs a b -> Filter cs a' b
lmapFilter f (Filter s) = Filter (A.lmapStencil (fmap f) s)
{-# INLINE lmapFilter #-}

-- | This is essentially a synonym to `fmap`, and is provided here for completenss.
rmapFilter :: Functor (Color cs) => (b -> b') -> Filter cs a b -> Filter cs a b'
rmapFilter g (Filter s) = Filter (A.rmapStencil (fmap g) s)
{-# INLINE rmapFilter #-}

-- | `Filter` is a Profunctor, but instead of introducing a dependency on
-- [profunctors](/package/profunctors), standalone functions are provided
-- here. Same performance consideration applies as in `lmapFilter`.
dimapFilter :: Functor (Color cs) => (a' -> a) -> (b -> b') -> Filter cs a b -> Filter cs a' b'
dimapFilter f g (Filter s) = Filter (A.dimapStencil (fmap f) (fmap g) s)
{-# INLINE dimapFilter #-}


-------------
-- Filters --
-------------

-- | 1D gaussian function
-- \g(x) = \frac{1}{\sigma\sqrt{2\pi}} e^{ -\frac{x^2}{2\sigma^2} }\
gaussianFunction1D ::
     Floating e
  => e -- ^ Standard deviation σ (sigma)
  -> e -- ^ x
  -> e
gaussianFunction1D stdDev x = exp (- ((x / stdDev) ^ (2 :: Int)) / 2) / (stdDev * sqrt (2 * pi))
{-# INLINE gaussianFunction1D #-}

-- | 2D gaussian function
-- \g(x, y) = \frac{1}{2\sigma^2\pi} e^{ -\frac{x^2 + y^2}{2\sigma^2} }\
gaussianFunction2D ::
     Floating e
  => e -- ^ Standard deviation σ (sigma)
  -> e -- ^ x
  -> e -- ^ y
  -> e
gaussianFunction2D stdDev y x = exp (-(x ^ (2 :: Int) + y ^ (2 :: Int)) / var2) / (var2 * pi)
  where
    var2 = 2 * stdDev ^ (2 :: Int)
{-# INLINE gaussianFunction2D #-}


estimateFunction1D ::
     (A.Storable e, Floating e)
  => Int -- ^ Number of samples to use for integral approximation
  -> Int -- ^ Side
  -> (e -> e) -- ^ Kernel function @f(x)@
  -> A.Vector A.M e
estimateFunction1D numSamples side g
  | side <= 0 = error "estimateFunction1D: Side must be positive"
  | otherwise =
    let f scale i = g (scale i)
        {-# INLINE f #-}
        sz = Sz side
        a = -fromIntegral side / 2
        d = 1
     in A.simpsonsRule Par A.S f a d sz numSamples
{-# INLINE estimateFunction1D #-}

estimateFunction2D ::
     (A.Storable e, Floating e)
  => Int -- ^ Number of samples to use for integral approximation
  -> Int -- ^ Side
  -> (e -> e -> e) -- ^ Kernel function @f(x, y)@
  -> A.Matrix A.M e
estimateFunction2D numSamples  side g
  | side <= 0 = error "estimateFunction2D: Side must be positive"
  | otherwise =
    let f scale (i :. j) = g (scale i) (scale j)
        {-# INLINE f #-}
        sz = Sz (side :. side)
        a = -fromIntegral side / 2
        d = 1
     in A.simpsonsRule Par A.S f a d sz numSamples
{-# INLINE estimateFunction2D #-}


makeKernelWith ::
     (Storable a, Storable e, Num t, Num e, Index ix, Index ix2)
  => (t -> t2 -> Array A.M ix e)
  -> (Array S ix e -> Array A.D ix2 a)
  -> t
  -> t2
  -> Array A.S ix2 a
makeKernelWith make adjust side f =
  let !k = A.computeAs A.S $ make side f
   in A.compute $ adjust k
{-# INLINE makeKernelWith #-}

-- |
--
-- ====__Examples__
--
-- Constructing a gaussian 1x5 kernel
--
-- >>> makeKernel1D 2 (gaussianFunction1D (5/3 :: Float))
-- Array S Par (Sz (1 :. 5))
--   [ [ 0.13533571, 0.22856848, 0.27219155, 0.22856851, 0.13533576 ]
--   ]
--
-- @since 2.0.0
makeKernel1D ::
     (Storable e, RealFloat e)
  => Int -- ^ Number of sample per cell to use for integral estimation. Higher the value
         -- better is the accuracy, but longer it takes to compute the estimate.
  -> Int -- ^ Length of the kernel
  -> (e -> e)
  -> A.Matrix A.S e
makeKernel1D n =
  makeKernelWith (estimateFunction1D n) $ \k ->
    let !m = A.sum k
        Sz side = A.size k
     in A.resize' (Sz2 1 side) $ A.map (/ m) k
{-# INLINE makeKernel1D #-}


-- |
--
-- ====__Examples__
--
-- Constructing a gaussian 5x5 kernel with `Int` values.
--
-- >>> import qualified Data.Massiv.Array as A
-- >>> x = makeKernel2D 50 5 (gaussianFunction2D (2.5/3 :: Float))
-- >>> A.map ((round :: Float -> Int) . (/ A.minimum' x)) x
-- Array D Par (Sz (5 :. 5))
--   [ [ 1, 7, 13, 7, 1 ]
--   , [ 7, 47, 90, 47, 7 ]
--   , [ 13, 90, 170, 90, 13 ]
--   , [ 7, 47, 90, 47, 7 ]
--   , [ 1, 7, 13, 7, 1 ]
--   ]
--
-- @since 2.0.0
makeKernel2D ::
     (Storable e, RealFloat e)
  => Int -- ^ Number of sample per cell in each dimension to use for integral
         -- estimation. Higher the value better is the accuracy, but longer it takes to
         -- compute the estimate.
  -> Int -- ^ Length of both sides of the kernel
  -> (e -> e -> e)
  -> A.Matrix A.S e
makeKernel2D n =
  makeKernelWith (estimateFunction2D n) $ \k ->
    let !m = A.sum k
     in A.map (/ m) k
{-# INLINE makeKernel2D #-}


makeAverageStencil ::
     (A.Default a, Fractional a) => (Int -> Int -> Ix2) -> Sz1 -> Int -> A.Stencil Ix2 a a
makeAverageStencil ix2 (Sz k) c =
  A.makeStencil (Sz (ix2 1 k)) (ix2 0 c) $ \get ->
    let go !i !acc
          | i < k = go (i + 1) (acc + get (ix2 0 i))
          | otherwise = acc / dPos
     in go 1 (get 0)
  where
    dPos = fromIntegral k
{-# INLINE makeAverageStencil #-}


averageHorizontalFilter ::
     (ColorModel cs e, Fractional e) => Sz1 -> Ix1 -> Filter' cs e
averageHorizontalFilter k = Filter . makeAverageStencil (:.) k
{-# INLINE averageHorizontalFilter #-}

averageVerticalFilter ::
     (ColorModel cs e, Fractional e) => Sz1 -> Ix1 -> Filter' cs e
averageVerticalFilter k = Filter . makeAverageStencil (flip (:.)) k
{-# INLINE averageVerticalFilter #-}


averageBlur ::
     (Floating e, ColorModel cs e)
  => Int -- ^ @r@ - a positive integral value radius that will be used for computing
         -- average kernel. Both sides of the kernel will be set to @d=2*r + 1@
  -> Border (Pixel cs e) -- ^ Border resolution technique
  -> Image cs e
  -> Image cs e
averageBlur r b
  | r < 0 = error $ "Average filter with negative radius: " ++ show r
  | otherwise =
    mapFilter b (averageVerticalFilter k r) .
    mapFilter b (averageHorizontalFilter k r)
  where
    k = Sz $ r * 2 + 1
{-# INLINE averageBlur #-}

-- | Average horizontal filter of length 3.
--
-- @since 2.0.0
average1x3 :: (Fractional e, ColorModel cs e) => Filter cs e e
average1x3 = Filter $ A.makeStencil (Sz2 1 3) (0 :. 1) stencil
  where stencil f = (f (0 :. -1) + f (0 :. 0) + f (0 :. 1)) / 3
        {-# INLINE stencil #-}
{-# INLINE average1x3 #-}


-- | Average horizontal filter of length 5.
--
-- @since 2.0.0
average1x5 :: (Fractional e, ColorModel cs e) => Filter cs e e
average1x5 = Filter $ A.makeStencil (Sz2 1 5) (0 :. 2) stencil
  where stencil f = (f (0 :. -2) + f (0 :. -1) + f (0 :. 0) + f (0 :. 1) + f (0 :. 2)) / 5
        {-# INLINE stencil #-}
{-# INLINE average1x5 #-}


-- | Average horizontal filter of length 7.
--
-- @since 2.0.0
average1x7 :: (Fractional e, ColorModel cs e) => Filter cs e e
average1x7 = Filter $ A.makeStencil (Sz2 1 7) (0 :. 3) stencil
  where stencil f = ( f (0 :. -3) +
                      f (0 :. -2) +
                      f (0 :. -1) +
                      f (0 :.  0) +
                      f (0 :.  1) +
                      f (0 :.  2) +
                      f (0 :.  3) ) / 7
        {-# INLINE stencil #-}
{-# INLINE average1x7 #-}


-- | Average vertical filter of height 3.
--
-- @since 2.0.0
average3x1 :: (Fractional e, ColorModel cs e) => Filter cs e e
average3x1 = Filter $ A.makeStencil (Sz2 3 1) (1 :. 0) stencil
  where stencil f = (f (-1 :. 0) + f (0 :. 0) + f (1 :. 0)) / 3
        {-# INLINE stencil #-}
{-# INLINE average3x1 #-}

-- | Average vertical filter of height 5.
--
-- @since 2.0.0
average5x1 :: (Fractional e, ColorModel cs e) => Filter cs e e
average5x1 = Filter $ A.makeStencil (Sz2 5 1) (2 :. 0) stencil
  where stencil f = (f (-2 :. 0) + f (-1 :. 0) + f (0 :. 0) + f (1 :. 0) + f (2 :. 0)) / 5
        {-# INLINE stencil #-}
{-# INLINE average5x1 #-}

-- | Average vertical filter of height 7.
--
-- @since 2.0.0
average7x1 :: (Fractional e, ColorModel cs e) => Filter cs e e
average7x1 = Filter $ A.makeStencil (Sz2 7 1) (3 :. 0) stencil
  where stencil f = ( f (-3 :. 0) +
                      f (-2 :. 0) +
                      f (-1 :. 0) +
                      f ( 0 :. 0) +
                      f ( 1 :. 0) +
                      f ( 2 :. 0) +
                      f ( 3 :. 0) ) / 7
        {-# INLINE stencil #-}
{-# INLINE average7x1 #-}


-- | Apply a average blur to an image
--
-- ====__Examples__
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/haddock/frog_average3x3.jpg" $ averageBlur3x3 Edge frog
--
-- <<images/frog.jpg>> <<images/haddock/frog_average3x3.jpg>>
--
-- @since 2.0.0
averageBlur3x3 :: (Floating b, ColorModel cs b) => Border (Pixel cs b) -> Image cs b -> Image cs b
averageBlur3x3 b = mapFilter b average3x1 . mapFilter b average1x3
{-# INLINE averageBlur3x3 #-}

-- | Apply a average blur to an image
--
-- ====__Examples__
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/haddock/frog_average5x5.jpg" $ averageBlur5x5 Edge frog
--
-- <<images/frog.jpg>> <<images/haddock/frog_average5x5.jpg>>
--
-- @since 2.0.0
averageBlur5x5 :: (Floating b, ColorModel cs b) => Border (Pixel cs b) -> Image cs b -> Image cs b
averageBlur5x5 b = mapFilter b average5x1 . mapFilter b average1x5
{-# INLINE averageBlur5x5 #-}

-- | Apply a average blur to an image
--
-- ====__Examples__
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/haddock/frog_average7x7.jpg" $ averageBlur7x7 Edge frog
--
-- <<images/frog.jpg>> <<images/haddock/frog_average7x7.jpg>>
--
-- @since 2.0.0
averageBlur7x7 :: (Floating b, ColorModel cs b) => Border (Pixel cs b) -> Image cs b -> Image cs b
averageBlur7x7 b = mapFilter b average7x1 . mapFilter b average1x7
{-# INLINE averageBlur7x7 #-}


averageBlur' ::
     (Floating e, ColorModel cs e)
  => Int -- ^ @r@ - a positive integral value radius that will be used for computing
         -- average kernel. Both sides of the kernel will be set to @d=2*r + 1@
  -> Border (Pixel cs e) -- ^ Border resolution technique
  -> Image cs e
  -> Image cs e
averageBlur' r b img
  | r <= 0 =
    error $ "Average kernel radius is expected to be positive, not: " ++ show r
  | r == 1 = averageBlur3x3 b img
  | r == 2 = averageBlur5x5 b img
  | r == 3 = averageBlur7x7 b img
  | otherwise =
    let !side = r * 2 + 1
        !kVector = A.computeAs A.S $ A.replicate Seq (Sz1 side) 1
        !k1xD =
          A.makeCorrelationStencilFromKernel $ A.resize' (Sz2 1 side) kVector
        !kDx1 =
          A.makeCorrelationStencilFromKernel $ A.resize' (Sz2 side 1) kVector
     in Image .
        A.computeAs A.S .
        A.mapStencil b k1xD . A.computeAs A.S . A.mapStencil b kDx1 $
        unImage img
{-# INLINE averageBlur' #-}

-- | Gaussian horizontal filter with radius 1.5 and @σ=1.5\/3@
--
-- ====__Examples__
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/haddock/frog_gaussian1x3.jpg" $ mapFilter Edge gaussian1x3 frog
--
-- Two pictures below might look the same, but with a bit more detail horizontal sharp
-- details on the image on the left it is possible to distinguish a bit blurriness. Effect
-- is further amplified with larger filters: `gaussian1x5` and `gaussian1x7`
--
-- <<images/frog.jpg>> <<images/haddock/frog_gaussian1x3.jpg>>
--
-- @since 2.0.0
gaussian1x3 :: (Fractional e, ColorModel cs e) => Filter cs e e
gaussian1x3 = Filter $ A.makeStencil (Sz2 1 3) (0 :. 1) stencil
  where
    stencil f = f (0 :. -1) * 0.15773119796715185 +
                f (0 :.  0) * 0.68453760406569570 +
                f (0 :.  1) * 0.15773119796715185
    {-# INLINE stencil #-}
{-# INLINE gaussian1x3 #-}

-- | Gaussian vertical filter with radius 1.5 and @σ=1.5\/3@
--
-- ====__Examples__
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/haddock/frog_gaussian3x1.jpg" $ mapFilter Edge gaussian3x1 frog
--
-- <<images/frog.jpg>> <<images/haddock/frog_gaussian3x1.jpg>>
--
-- @since 2.0.0
gaussian3x1 :: (Floating e, ColorModel cs e) => Filter cs e e
gaussian3x1 = Filter $ A.makeStencil (Sz2 3 1) (1 :. 0) stencil
  where
    stencil f = f (-1 :. 0) * 0.15773119796715185 +
                f ( 0 :. 0) * 0.68453760406569570 +
                f ( 1 :. 0) * 0.15773119796715185
    {-# INLINE stencil #-}
{-# INLINE gaussian3x1 #-}

-- | Gaussian horizontal filter with radius 2.5 and @σ=2.5\/3@
--
-- ====__Examples__
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/haddock/frog_gaussian1x5.jpg" $ mapFilter Edge gaussian1x5 frog
--
-- <<images/frog.jpg>> <<images/haddock/frog_gaussian1x5.jpg>>
--
-- @since 2.0.0
gaussian1x5 :: (Fractional e, ColorModel cs e) => Filter cs e e
gaussian1x5 = Filter $ A.makeStencil (Sz2 1 5) (0 :. 2) stencil
  where stencil f = f (0 :. -2) * 0.03467403390152031 +
                    f (0 :. -1) * 0.23896796340399287 +
                    f (0 :.  0) * 0.45271600538897480 +
                    f (0 :.  1) * 0.23896796340399287 +
                    f (0 :.  2) * 0.03467403390152031
        {-# INLINE stencil #-}
{-# INLINE gaussian1x5 #-}


-- | Gaussian vertical filter with radius 2.5 and @σ=2.5\/3@
--
-- ====__Examples__
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/haddock/frog_gaussian5x1.jpg" $ mapFilter Edge gaussian5x1 frog
--
-- <<images/frog.jpg>> <<images/haddock/frog_gaussian5x1.jpg>>
--
-- @since 2.0.0
gaussian5x1 :: (Floating e, ColorModel cs e) => Filter cs e e
gaussian5x1 = Filter $ A.makeStencil (Sz2 5 1) (2 :. 0) stencil
  where stencil f = f (-2 :. 0) * 0.03467403390152031 +
                    f (-1 :. 0) * 0.23896796340399287 +
                    f ( 0 :. 0) * 0.45271600538897480 +
                    f ( 1 :. 0) * 0.23896796340399287 +
                    f ( 2 :. 0) * 0.03467403390152031
        {-# INLINE stencil #-}
{-# INLINE gaussian5x1 #-}


-- | Gaussian horizontal filter with radius 3.5 and @σ=3.5\/3@
--
-- ====__Examples__
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/haddock/frog_gaussian1x7.jpg" $ mapFilter Edge gaussian1x7 frog
--
-- <<images/frog.jpg>> <<images/haddock/frog_gaussian1x7.jpg>>
--
-- @since 2.0.0
gaussian1x7 :: (Fractional e, ColorModel cs e) => Filter cs e e
gaussian1x7 = Filter $ A.makeStencil (Sz2 1 7) (0 :. 3) stencil
  where stencil f = f (0 :. -3) * 0.01475221554565270 +
                    f (0 :. -2) * 0.08343436701511067 +
                    f (0 :. -1) * 0.23548192723440955 +
                    f (0 :.  0) * 0.33266298040965380 +
                    f (0 :.  1) * 0.23548192723440955 +
                    f (0 :.  2) * 0.08343436701511067 +
                    f (0 :.  3) * 0.01475221554565270
        {-# INLINE stencil #-}
{-# INLINE gaussian1x7 #-}


-- | Gaussian vertical filter with radius 3.5 and @σ=3.5\/3@
--
-- ====__Examples__
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/haddock/frog_gaussian7x1.jpg" $ mapFilter Edge gaussian7x1 frog
--
-- <<images/frog.jpg>> <<images/haddock/frog_gaussian7x1.jpg>>
--
-- @since 2.0.0
gaussian7x1 :: (Floating e, ColorModel cs e) => Filter cs e e
gaussian7x1 = Filter $ A.makeStencil (Sz2 7 1) (3 :. 0) stencil
  where stencil f = f (-3 :. 0) * 0.01475221554565270 +
                    f (-2 :. 0) * 0.08343436701511067 +
                    f (-1 :. 0) * 0.23548192723440955 +
                    f ( 0 :. 0) * 0.33266298040965380 +
                    f ( 1 :. 0) * 0.23548192723440955 +
                    f ( 2 :. 0) * 0.08343436701511067 +
                    f ( 3 :. 0) * 0.01475221554565270
        {-# INLINE stencil #-}
{-# INLINE gaussian7x1 #-}


-- | Apply a gaussian blur to an image. Gaussian function with radius 1.5 and @σ=1.5\/3@
-- was used for constructing the kernel.
--
-- ====__Examples__
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/haddock/frog_gaussian3x3.jpg" $ gaussianBlur3x3 Edge frog
--
-- <<images/frog.jpg>> <<images/haddock/frog_gaussian3x3.jpg>>
--
-- @since 2.0.0
gaussianBlur3x3 :: (Floating b, ColorModel cs b) => Border (Pixel cs b) -> Image cs b -> Image cs b
gaussianBlur3x3 b = mapFilter b gaussian3x1 . mapFilter b gaussian1x3
{-# INLINE gaussianBlur3x3 #-}

-- | Apply a gaussian blur to an image. Gaussian function with radius 2.5 and @σ=2.5\/3@
-- was used for constructing the kernel.
--
-- ====__Examples__
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/haddock/frog_gaussian5x5.jpg" $ gaussianBlur5x5 Edge frog
--
-- <<images/frog.jpg>> <<images/haddock/frog_gaussian5x5.jpg>>
--
-- @since 2.0.0
gaussianBlur5x5 :: (Floating b, ColorModel cs b) => Border (Pixel cs b) -> Image cs b -> Image cs b
gaussianBlur5x5 b = mapFilter b gaussian5x1 . mapFilter b gaussian1x5
{-# INLINE gaussianBlur5x5 #-}


-- | Apply a gaussian blur to an image. Gaussian function with radius 3.5 and @σ=3.5\/3@
-- was used for constructing the kernel.
--
-- ====__Examples__
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/haddock/frog_gaussian7x7.jpg" $ gaussianBlur7x7 Edge frog
--
-- <<images/frog.jpg>> <<images/haddock/frog_gaussian7x7.jpg>>
--
-- @since 2.0.0
gaussianBlur7x7 :: (Floating b, ColorModel cs b) => Border (Pixel cs b) -> Image cs b -> Image cs b
gaussianBlur7x7 b = mapFilter b gaussian7x1 . mapFilter b gaussian1x7
{-# INLINE gaussianBlur7x7 #-}


-- | Apply a gaussian blur to an image.
--
-- Note that `gaussianBlur3x3`, `gaussianBlur5x5` and `gaussianBlur7x7` are special cases
-- of this function and can be used when appropriate for clarity and brevity, but
-- otherwise will result in exactly the same performance and outcome. In other words these
-- are equivalent
--
-- @
-- `gaussianBlur3x3` === `gaussianBlur` 1 Nothing
-- `gaussianBlur5x5` === `gaussianBlur` 2 Nothing
-- `gaussianBlur7x7` === `gaussianBlur` 3 Nothing
-- @
--
-- ====__Examples__
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/haddock/frog_gaussian41x41.jpg" $ gaussianBlur 20 Nothing Edge frog
-- >>> writeImage "images/haddock/frog_gaussian41x41_stdDevSmall.jpg" $ gaussianBlur 20 (Just 1.5) Edge frog
--
-- Note that @"frog_gaussian41x41_stdDevSmall.jpg"@ image is a lot less blurred, because
-- it has a smaller than optimal σ for the size of the kernel being used. This means that
-- it has exactly the same performance as the more blurred one and to make it more
-- efficient while achieving the same result is to omit standard deviation and set a
-- smaller radius:
--
-- @
-- gaussianBlur 4 Nothing Edge frog
-- @
--
-- which would produce practically the same image while having a much faster runtime.
--
-- <<images/frog.jpg>> <<images/haddock/frog_gaussian41x41.jpg>> <<images/haddock/frog_gaussian41x41_stdDevSmall.jpg>>
--
-- @since 2.0.0
gaussianBlur ::
     (Floating e, ColorModel cs e)
  => Int -- ^ @r@ - a positive integral value radius that will be used for computing
         -- gaussian function. Both sides of the kernel will be set to @d=2*r + 1@
  -> Maybe e -- ^ Optional stdDev value for gaussian function. If ommitted an optimal
             -- @σ=d\/6@ will be used
  -> Border (Pixel cs e) -- ^ Border resolution technique
  -> Image cs e
  -> Image cs e
gaussianBlur r mStdDev b img
  | r <= 0 =
    error $ "Gaussian kernel radius is expected to be positive, not: " ++ show r
  | r == 1 && isNothing mStdDev = gaussianBlur3x3 b img
  | r == 2 && isNothing mStdDev = gaussianBlur5x5 b img
  | r == 3 && isNothing mStdDev = gaussianBlur7x7 b img
  | otherwise =
    let !side = r * 2 + 1
        !stdDev = maybe (fromIntegral side / 6) pure mStdDev
        !numSamples = 50
        !kVector =
          makeKernelWith
            (estimateFunction1D numSamples)
            (\k ->
               let !m = A.sum k
                in A.map (/ m) k)
            side
            (gaussianFunction1D stdDev)
        !k1xD =
          A.makeCorrelationStencilFromKernel $ A.resize' (Sz2 1 side) kVector
        !kDx1 =
          A.makeCorrelationStencilFromKernel $ A.resize' (Sz2 side 1) kVector
     in Image .
        A.computeAs A.S .
        A.mapStencil b k1xD . A.computeAs A.S . A.mapStencil b kDx1 $
        unImage img
{-# INLINE gaussianBlur #-}


-- | Laplacian filter
--
-- >>> bat <- readImageY "images/megabat.jpg"
-- >>> writeImage "images/megabat_laplacian_nonorm.jpg" $ mapFilter Edge laplacian bat -- no normalization
-- >>> writeImage "images/megabat_laplacian.jpg" $ normalize $ mapFilter Edge laplacian bat
--
-- <<images/megabat_y.jpg>> <<images/megabat_laplacian_nonorm.jpg>> <<images/megabat_laplacian.jpg>>
--
-- ==== __Convolution Kernel__
--
-- \[
-- \mathbf{L} = \begin{bmatrix}
-- +1 & +1 & +1 \\
-- +1 & -8 & +1 \\
-- +1 & +1 & +1
-- \end{bmatrix}
-- \]
--
laplacian :: ColorModel cs e => Filter cs e e
laplacian = Filter $ A.makeStencil (Sz2 3 3) (1 :. 1) stencil
  where stencil f = f (-1 :. -1) +     f (-1 :.  0) + f (-1 :.  1) +
                    f ( 0 :. -1) - 8 * f ( 0 :.  0) + f ( 0 :.  1) +
                    f ( 1 :. -1) +     f ( 1 :.  0) + f ( 1 :.  1)
        {-# INLINE stencil #-}
{-# INLINE laplacian #-}

-- | Sobel gradient along @x@ axis.
--
-- ==== __Convolution Kernel__
--
-- \[
-- \mathbf{G}_x = \begin{bmatrix}
-- +1 & 0 & -1 \\
-- +2 & 0 & -2 \\
-- +1 & 0 & -1
-- \end{bmatrix}
-- \]
sobelHorizontal :: ColorModel cs e => Filter' cs e
sobelHorizontal =
  Filter $ A.makeStencil (Sz2 3 3) (1 :. 1) $ \ f ->
                f (-1 :. -1) -     f (-1 :.  1) +
            2 * f ( 0 :. -1) - 2 * f ( 0 :.  1) +
                f ( 1 :. -1) -     f ( 1 :.  1)
{-# INLINE sobelHorizontal #-}

-- | Sobel gradient along @y@ axis.
--
-- ==== __Convolution Kernel__
--
-- \[
-- \mathbf{G}_y = \begin{bmatrix}
-- +1 & +2 & +1 \\
--  0 &  0 &  0 \\
-- -1 & -2 & -1
-- \end{bmatrix}
-- \]
sobelVertical :: ColorModel cs e => Filter' cs e
sobelVertical =
  Filter $ A.makeStencil (Sz2 3 3) (1 :. 1) $ \ f ->
           f (-1 :. -1) + 2 * f (-1 :. 0) + f (-1 :. 1)
         - f ( 1 :. -1) - 2 * f ( 1 :. 0) - f ( 1 :. 1)
{-# INLINE sobelVertical #-}


-- | Sobel operator is simply defined as:
--
-- @
-- sobelOperator = sqrt (`sobelHorizontal` ^ 2 + `sobelVertical` ^ 2)
-- @
--
-- \[
-- \mathbf{G} = \sqrt{ {\mathbf{G}_x}^2 + {\mathbf{G}_y}^2 }
-- \]
--
-- ====__Examples__
--
-- >>>
sobelOperator :: ColorModel cs Double => Filter' cs Double
sobelOperator =
  sqrt (sobelHorizontal ^ (2 :: Int) + sobelVertical ^ (2 :: Int))
{-# INLINE sobelOperator #-}


-- | Normalized sobel operator
--
-- @
-- sobelOperator = sqrt ((`sobelHorizontal` \/ 8) ^ 2 + (`sobelVertical` \/ 8) ^ 2)
-- @
--
-- \[
-- \mathbf{G} = \sqrt{ {\frac{\mathbf{G}_x}{8}}^2 + {\frac{\mathbf{G}_y}{8}}^2 }
-- \]
sobelOperatorNormal :: ColorModel cs Double => Filter' cs Double
sobelOperatorNormal =
  sqrt ((sobelHorizontal / 8) ^ (2 :: Int) + (sobelVertical / 8) ^ (2 :: Int))
{-# INLINE sobelOperatorNormal #-}


-- | Prewitt gradient along @x@ axis.
--
-- ==== __Convolution Kernel__
--
-- \[
-- \mathbf{G_x} = \begin{bmatrix}
-- +1 & 0 & -1 \\
-- +1 & 0 & -1 \\
-- +1 & 0 & -1
-- \end{bmatrix}
-- \]
prewittHorizontal :: ColorModel cs e => Filter' cs e
prewittHorizontal =
  Filter $ A.makeStencil (Sz2 3 3) (1 :. 1) $ \ f ->
                f (-1 :. -1) - f (-1 :.  1) +
                f ( 0 :. -1) - f ( 0 :.  1) +
                f ( 1 :. -1) - f ( 1 :.  1)
{-# INLINE prewittHorizontal #-}


-- | Prewitt gradient along @y@ axis.
--
-- ==== __Convolution Kernel__
--
-- \[
-- \mathbf{G_y} = \begin{bmatrix}
-- +1 & +1 & +1 \\
--  0 &  0 &  0 \\
-- -1 & -1 & -1
-- \end{bmatrix}
-- \]
prewittVertical :: ColorModel cs e => Filter' cs e
prewittVertical =
  Filter $ A.makeStencil (Sz2 3 3) (1 :. 1) $ \ f ->
          f (-1 :. -1) + f ( 0 :. -1) + f ( 1 :. -1)
        - f (-1 :.  1) - f ( 0 :.  1) - f ( 1 :.  1)
{-# INLINE prewittVertical #-}


-- | Prewitt operator is simply defined as: @sqrt (`prewittHorizontal` ^ 2 + `prewittVertical` ^ 2)@
--
-- \[
-- \mathbf{G} = \sqrt{ {\mathbf{G}_x}^2 + {\mathbf{G}_y}^2 }
-- \]
prewittOperator :: ColorModel cs Double => Filter' cs Double
prewittOperator =
  sqrt (prewittHorizontal ^ (2 :: Int) + prewittVertical ^ (2 :: Int))
{-# INLINE prewittOperator #-}

-- | Normalized version of Prewitt operator. It is defined as:
--
-- @@@
-- sqrt ((`prewittHorizontal` / 6) ^ 2 + (`prewittVertical` / 6) ^ 2)
-- @@@
--
-- \[
-- \mathbf{G} = \sqrt{ {\mathbf{G}_x}^2 + {\mathbf{G}_y}^2 }
-- \]
prewittOperatorNormal :: ColorModel cs Double => Filter' cs Double
prewittOperatorNormal =
  sqrt ((prewittHorizontal / 6) ^ (2 :: Int) + (prewittVertical / 6) ^ (2 :: Int))
{-# INLINE prewittOperatorNormal #-}


----------------------------------
------- Benchmarking -------------
----------------------------------

-- $filter
--
-- All filters are defined with a `A.Stencil`, as such they are mere functions and usually aren't
-- backed by some array, although most of the time they do describe some sort of matrix, eg. a
-- convolution kernel.
--
-- There are ways to adjust filters without knowing much about the image that it will be applied
-- to. In fact, `Filter` is a `Functor`, `Applicative` and even a @Profunctor@ (see `rmapFilter`,
-- `lmapFilter` and `bimapFilter`). Moreover, it is also an instance of `Num`, `Fractional` and
-- `Floating`, so they can be used like regular numbers. Ability to compose filters together does not
-- degrade performance, on contrary it will usually be an improvement, since it promotes fusion of
-- computation, which means avoiding intermediary arrays.
--
-- Examples of composing two filters together would be `sobelOperator` and `prewittOperator`.


