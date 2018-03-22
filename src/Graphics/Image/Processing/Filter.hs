{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
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
    Filter
  , Filter'
  -- ** Application
  , applyFilter
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
  , gaussian5x5
  , gaussian5x1
  , gaussian1x5
  , applyGaussian5x5
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
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Int
import qualified Data.Massiv.Array       as A
import           Graphics.Image.Internal
import           Prelude                 as P

-- | Filter that can be applied to an image using `applyFilter`.
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
-- >λ> batRGB <- readImageRGB "images/megabat.jpg"
-- >λ> writeImage "images/megabat_sobel_rgb.jpg" $ normalize $ applyFilter Edge sobelOperator batRGB
-- >λ> let batY = I.map toPixelY batRGB
-- >λ> writeImage "images/megabat_sobel.jpg" $ normalize $ applyFilter Edge sobelOperator batY
--
-- <<images/megabat.jpg>> <<images/megabat_sobel_rgb.jpg>> <<images/megabat_sobel.jpg>>
--
-- With filter application normalization is often desired, see `laplacian` for an example without
-- normalization.
applyFilter ::
     (ColorModel cs a, ColorModel cs b)
  => Border (Pixel cs a)
  -> Filter cs a b
  -> Image cs a
  -> Image cs b
applyFilter border f (Image arr) =
  Image (A.compute (A.mapStencil border (filterStencil f) arr))
{-# INLINE applyFilter #-}


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


-- | Gaussian 5x5 filter. Gaussian is separable, so it is faster to apply `gaussian5x1` after
-- `gaussian1x5`.
--
-- >λ> bat <- readImageY "images/megabat.jpg"
-- >λ> writeImage "images/megabat_laplacian_nonorm.jpg" $ applyFilter Edge laplacian bat -- no normalization
-- >λ> writeImage "images/megabat_laplacian.jpg" $ normalize $ applyFilter Edge laplacian bat
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
gaussian5x5 :: (Fractional e, ColorModel cs e) => Filter cs e e
gaussian5x5 = Filter $ fmap (/ 273) <$> A.makeStencil (Sz2 5 5) (2 :. 2) stencil
  where
    stencil f =
          f (-2 :. -2) +  4 * f (-2 :. -1) +  7 * f (-2 :.  0) +  4 * f (-2 :.  1) +     f (-2 :.  2) +
      4 * f (-1 :. -2) + 16 * f (-1 :. -1) + 26 * f (-1 :.  0) + 16 * f (-1 :.  1) + 4 * f (-1 :.  2) +
      7 * f ( 0 :. -2) + 26 * f ( 0 :. -1) + 41 * f ( 0 :.  0) + 26 * f ( 0 :.  1) + 7 * f ( 0 :.  2) +
      4 * f ( 1 :. -2) + 16 * f ( 1 :. -1) + 26 * f ( 1 :.  0) + 16 * f ( 1 :.  1) + 4 * f ( 1 :.  2) +
          f ( 2 :. -2) +  4 * f ( 2 :. -1) +  7 * f ( 2 :.  0) +  4 * f ( 2 :.  1) +     f ( 2 :.  2)
    {-# INLINE stencil #-}
{-# INLINE gaussian5x5 #-}


gaussian1x5 :: (Fractional e, ColorModel cs e) => Filter cs e e
gaussian1x5 = Filter $ fmap (/ 17) <$> A.makeStencil (Sz2 1 5) (0 :. 2) stencil
  where
    stencil f = f (0 :. -2) +  4 * f (0 :. -1) + 7 * f (0 :.  0) + 4 * f (0 :.  1) + f (0 :.  2)
    {-# INLINE stencil #-}
{-# INLINE gaussian1x5 #-}


gaussian5x1 :: (Fractional e, ColorModel cs e) => Filter cs e e
gaussian5x1 = Filter $ fmap (/ 17) <$> A.makeStencil (Sz2 5 1) (2 :. 0) stencil
  where
    stencil f = f (-2 :. 0) +  4 * f (-1 :. 0) + 7 * f (0 :.  0) + 4 * f (1 :.  0) + f (2 :.  0)
    {-# INLINE stencil #-}
{-# INLINE gaussian5x1 #-}


applyGaussian5x5 :: (Fractional b, ColorModel cs b) => Border (Pixel cs b) -> Image cs b -> Image cs b
applyGaussian5x5 b = applyFilter b gaussian5x1 . applyFilter b gaussian1x5
{-# INLINE applyGaussian5x5 #-}

-- | Laplacian filter
--
-- >λ> bat <- readImageY "images/megabat.jpg"
-- >λ> writeImage "images/megabat_laplacian_nonorm.jpg" $ applyFilter Edge laplacian bat -- no normalization
-- >λ> writeImage "images/megabat_laplacian.jpg" $ normalize $ applyFilter Edge laplacian bat
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
