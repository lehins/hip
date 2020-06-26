{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module      : Graphics.Image.Processing.Filter
-- Copyright   : (c) Alexey Kuleshevich 2017
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
  -- ** Gaussian
  , gaussianBlur3x3
  , gaussianBlur5x5
  , gaussianFilter3x1
  , gaussianFilter1x3
  , gaussianFilter5x1
  , gaussianFilter1x5
  -- ** Laplacian
  , laplacian
  -- ** Laplacian of Gaussian
  --, laplacianOfGaussian
  -- ** Sobel
  , sobelHorizontal
  , sobelVertical
  , sobelOperator
  -- ** Prewitt
  , prewittHorizontal
  , prewittVertical
  , prewittOperator
  --   -- * Gaussian
  -- , gaussianLowPass
  -- , gaussianBlur
  --   -- * Sobel
  -- , sobelFilter
  -- , sobelOperator
  --   -- * Prewitt
  -- , prewittFilter
  -- , prewittOperator
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
import Data.Int
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Numeric.Integral as A
import Graphics.Image.Internal
import Prelude as P


-- | Filter that can be applied to an image using `mapFilter`.
newtype Filter cs a b = Filter
  { filterStencil :: A.Stencil Ix2 (Pixel cs a) (Pixel cs b)
  } deriving (NFData, Num, Fractional, Floating)

-- | Helper type synonym for `Filter` where both input and output precision are the same.
type Filter' cs e = Filter cs e e

instance Functor (Pixel cs) => Functor (Filter cs a) where
  fmap f (Filter s) = Filter (fmap (fmap f) s)
  {-# INLINE fmap #-}

instance (ColorModel cs a, Applicative (Pixel cs)) => Applicative (Filter cs a) where
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

-- | Convert from [massiv](/package/massiv) 2D `A.Stencil`.
fromStencil :: A.Stencil Ix2 (Pixel cs a) (Pixel cs b) -> Filter cs a b
fromStencil = Filter
{-# INLINE fromStencil #-}

-- | Convert to a [massiv](/package/massiv) 2D `A.Stencil`.
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
lmapFilter :: Functor (Pixel cs) => (a' -> a) -> Filter cs a b -> Filter cs a' b
lmapFilter f (Filter s) = Filter (A.lmapStencil (fmap f) s)
{-# INLINE lmapFilter #-}

-- | This is essentially a synonym to `fmap`, and is provided here for completenss.
rmapFilter :: Functor (Pixel cs) => (b -> b') -> Filter cs a b -> Filter cs a b'
rmapFilter g (Filter s) = Filter (A.rmapStencil (fmap g) s)
{-# INLINE rmapFilter #-}

-- | `Filter` is a Profunctor, but instead of introducing a dependency on
-- [profunctors](/package/profunctors), standalone functions are provided
-- here. Same performance consideration applies as in `lmapFilter`.
dimapFilter :: Functor (Pixel cs) => (a' -> a) -> (b -> b') -> Filter cs a b -> Filter cs a' b'
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

-- | Number of samples to use for kernel approximations
numSamples :: Int
numSamples = 100

estimateFunction1D ::
     (A.Storable e, Floating e)
  => Int -- ^ Radius
  -> (e -> e) -- ^ Kernel function @f(x)@
  -> A.Vector A.M e
estimateFunction1D side g
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
  => Int -- ^ Radius
  -> (e -> e -> e) -- ^ Kernel function @f(x, y)@
  -> A.Matrix A.M e
estimateFunction2D side g
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
     (Storable a, Storable e, Num t, Num e, Index ix)
  => (t -> t2 -> Array A.M ix e)
  -> (Array S ix e -> Array A.D Ix2 a)
  -> t
  -> t2
  -> Array A.S Ix2 a
makeKernelWith make adjust r f =
  let !k = A.computeAs A.S $ make side f
      !side = r * 2 + 1
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
makeKernel1D :: (Storable e, RealFloat e) => Int -> (e -> e) -> A.Matrix A.S e
makeKernel1D =
  makeKernelWith estimateFunction1D $ \k ->
    let !m = A.sum k
        Sz side = A.size k
     in A.resize' (Sz2 1 side) $ A.map (/ m) k
{-# INLINE makeKernel1D #-}


-- |
--
-- ====__Examples__
--
-- Constructing a gaussian 5x5 kernel
--
-- >>> makeKernel2D 2 (gaussianFunction2D (5/3 :: Float))
-- Array S Par (Sz (5 :. 5))
--   [ [ 1.8315764e-2, 3.0933492e-2, 3.683724e-2, 3.0933492e-2, 1.8315762e-2 ]
--   , [ 3.0933483e-2, 5.224356e-2, 6.2214427e-2, 5.224356e-2, 3.0933486e-2 ]
--   , [ 3.683725e-2, 6.221442e-2, 7.408825e-2, 6.2214423e-2, 3.683724e-2 ]
--   , [ 3.093349e-2, 5.224356e-2, 6.2214408e-2, 5.224356e-2, 3.0933484e-2 ]
--   , [ 1.831576e-2, 3.0933494e-2, 3.683725e-2, 3.0933501e-2, 1.831576e-2 ]
--   ]
--
-- @since 2.0.0
makeKernel2D :: (Storable e, RealFloat e) => Int -> (e -> e -> e) -> A.Matrix A.S e
makeKernel2D =
  makeKernelWith estimateFunction2D $ \k ->
    let !m = A.sum k
     in A.map (/ m) k
{-# INLINE makeKernel2D #-}


-- | A horizontal gaussian filter with standard diviation set to 1
gaussianFilter1x3 :: (Fractional e, ColorModel cs e) => Filter cs e e
gaussianFilter1x3 = Filter $ A.makeStencil (Sz2 1 3) (0 :. 1) stencil
  where
    stencil f = f (0 :. -1) * 0.27901010608341126 +
                f (0 :.  0) * 0.44197978783317790 +
                f (0 :.  1) * 0.27901010608341126
    {-# INLINE stencil #-}
{-# INLINE gaussianFilter1x3 #-}

-- | A vertical gaussian filter with standard diviation set to 1
gaussianFilter3x1 :: (Floating e, ColorModel cs e) => Filter cs e e
gaussianFilter3x1 = Filter $ A.makeStencil (Sz2 3 1) (1 :. 0) stencil
  where
    stencil f = f (-1 :. 0) * 0.27901010608341126 +
                f ( 0 :. 0) * 0.44197978783317790 +
                f ( 1 :. 0) * 0.27901010608341126
    {-# INLINE stencil #-}
{-# INLINE gaussianFilter3x1 #-}

gaussianBlur3x3 :: (Floating b, ColorModel cs b) => Border (Pixel cs b) -> Image cs b -> Image cs b
gaussianBlur3x3 b = mapFilter b gaussianFilter3x1 . mapFilter b gaussianFilter1x3
{-# INLINE gaussianBlur3x3 #-}

gaussianFilter1x5 :: (Fractional e, ColorModel cs e) => Filter cs e e
gaussianFilter1x5 = Filter $ A.makeStencil (Sz2 1 5) (0 :. 2) stencil
  where
    stencil f = f (0 :. -2) * 0.13533572623225257 +
                f (0 :. -1) * 0.22856849543519478 +
                f (0 :.  0) * 0.27219155666510536 +
                f (0 :.  1) * 0.22856849543519478 +
                f (0 :.  2) * 0.13533572623225248
    {-# INLINE stencil #-}
{-# INLINE gaussianFilter1x5 #-}


gaussianFilter5x1 :: (Floating e, ColorModel cs e) => Filter cs e e
gaussianFilter5x1 = Filter $ A.makeStencil (Sz2 5 1) (2 :. 0) stencil
  where
    stencil f = f (-2 :. 0) * 0.13533572623225257 +
                f (-1 :. 0) * 0.22856849543519478 +
                f ( 0 :. 0) * 0.27219155666510536 +
                f ( 1 :. 0) * 0.22856849543519478 +
                f ( 2 :. 0) * 0.13533572623225248
    {-# INLINE stencil #-}
{-# INLINE gaussianFilter5x1 #-}


gaussianBlur5x5 :: (Floating b, ColorModel cs b) => Border (Pixel cs b) -> Image cs b -> Image cs b
gaussianBlur5x5 b = mapFilter b gaussianFilter5x1 . mapFilter b gaussianFilter1x5
{-# INLINE gaussianBlur5x5 #-}

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
sobelHorizontal :: ColorModel cs e => Filter cs e e
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
sobelVertical :: ColorModel cs e => Filter cs e e
sobelVertical =
  Filter $ A.makeStencil (Sz2 3 3) (1 :. 1) $ \ f ->
              f (-1 :. -1) + 2 * f (-1 :. 0) + f (-1 :. 1)
            - f ( 1 :. -1) - 2 * f ( 1 :. 0) - f ( 1 :. 1)
{-# INLINE sobelVertical #-}


-- | Sobel Opertor is simply defined as:
--
-- @
-- sobelOperator = sqrt (`sobelHorizontal` ^ 2 + `sobelVertical` ^ 2)
-- @
--
-- \[
-- \mathbf{G} = \sqrt{ {\mathbf{G}_x}^2 + {\mathbf{G}_y}^2 }
-- \]
sobelOperator :: ColorModel cs Double => Filter' cs Double
sobelOperator =
  sqrt (sobelHorizontal ^ (2 :: Int) + sobelVertical ^ (2 :: Int))
{-# INLINE sobelOperator #-}


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


-- | Prewitt Opertor is simply defined as: @sqrt (`prewittHorizontal` ^ 2 + `prewittVertical` ^ 2)@
--
-- \[
-- \mathbf{G} = \sqrt{ {\mathbf{G}_x}^2 + {\mathbf{G}_y}^2 }
-- \]
prewittOperator :: ColorModel cs Double => Filter' cs Double
prewittOperator =
  sqrt (prewittHorizontal ^ (2 :: Int) + prewittVertical ^ (2 :: Int))
{-# INLINE prewittOperator #-}

-- Integral gaussian

-- | Gaussian 5x5 filter. Gaussian is separable, so it is faster to apply `gaussian5x1` after
-- `gaussian1x5`.
--
-- >λ> bat <- readImageY "images/megabat.jpg"
-- >λ> writeImage "images/megabat_laplacian_nonorm.jpg" $ mapFilter Edge laplacian bat -- no normalization
-- >λ> writeImage "images/megabat_laplacian.jpg" $ normalize $ mapFilter Edge laplacian bat
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
-- gaussian5x5i :: (ColorModel cs e, Integral e) => Filter cs e e
-- gaussian5x5i = Filter $ fmap (`div` 264) <$> A.makeStencil (Sz2 5 5) (2 :. 2) stencil
--   where
--     stencil f =
--           f (-2 :. -2) +  4 * f (-2 :. -1) +  6 * f (-2 :.  0) +  4 * f (-2 :.  1) +     f (-2 :.  2) +
--       4 * f (-1 :. -2) + 16 * f (-1 :. -1) + 25 * f (-1 :.  0) + 16 * f (-1 :.  1) + 4 * f (-1 :.  2) +
--       6 * f ( 0 :. -2) + 25 * f ( 0 :. -1) + 40 * f ( 0 :.  0) + 25 * f ( 0 :.  1) + 6 * f ( 0 :.  2) +
--       4 * f ( 1 :. -2) + 16 * f ( 1 :. -1) + 25 * f ( 1 :.  0) + 16 * f ( 1 :.  1) + 4 * f ( 1 :.  2) +
--           f ( 2 :. -2) +  4 * f ( 2 :. -1) +  6 * f ( 2 :.  0) +  4 * f ( 2 :.  1) +     f ( 2 :.  2)
--     {-# INLINE stencil #-}
-- {-# INLINE gaussian5x5i #-}

gaussian1x5i :: (ColorModel cs e, Integral e) => Filter cs e e
gaussian1x5i = Filter $ fmap (`div` 16) <$> A.makeStencil (Sz2 1 5) (0 :. 2) stencil
  where
    stencil f = f (0 :. -2) +  4 * f (0 :. -1) + 6 * f (0 :.  0) + 4 * f (0 :.  1) + f (0 :.  2)
    {-# INLINE stencil #-}
{-# INLINE gaussian1x5i #-}


gaussian5x1i :: (ColorModel cs e, Integral e) => Filter cs e e
gaussian5x1i = Filter $ fmap (`div` 16) <$> A.makeStencil (Sz2 5 1) (2 :. 0) stencil
  where
    stencil f = f (-2 :. 0) +  4 * f (-1 :. 0) + 6 * f (0 :.  0) + 4 * f (1 :.  0) + f (2 :.  0)
    {-# INLINE stencil #-}
{-# INLINE gaussian5x1i #-}


mapGaussian5x5i :: (Integral b, ColorModel cs b) => Border (Pixel cs b) -> Image cs b -> Image cs b
mapGaussian5x5i b = mapFilter b gaussian5x1i . mapFilter b gaussian1x5i
{-# INLINE mapGaussian5x5i #-}


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


