-- |
-- Module      : Graphics.Image.Processing.Gabor
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Gabor where

import Graphics.Image.Internal as I
import Graphics.Image.Processing.Complex.Internal
import Graphics.Image.Processing.Filter

-- | Gabor function
gabor ::
     RealFloat a
  => a -- ^ x - Horizontal index
  -> a -- ^ y - Vertical index
  -> a -- ^ λ - Wavelength of the sine component
  -> a -- ^ θ - Orientation
  -> a -- ^ ψ - Phase offset
  -> a -- ^ σ - Standard deviation of the gaussian envelope
  -> a -- ^ γ - Spatial aspect ratio
  -> Complex a
gabor x y λ θ ψ σ γ =
  exp
    ((-0.5) *
     ((x' ^ (2 :: Int) + γ ^ (2 :: Int) * y' ^ (2 :: Int)) / (σ ^ (2 :: Int))) :+
     0) *
  exp (0 :+ (2 * pi * x' / λ + ψ))
  where
    x' = x * cos θ + y * sin θ
    y' = -x * sin θ + y * cos θ

-- | Using the `gabor` function construct a convolution kernel
makeGaborKernel ::
     Sz2 -- ^ Size of the kernel. Should have odd sides, because origin is at
         -- the center of the kernel.
  -> Double -- ^ λ: @(minValue, maxValue)@ - Wavelength of the sine component
  -> Double -- ^ θ: @[0, π]@ - Orientation
  -> Double -- ^ ψ: @[-π, π]@ - Phase offset
  -> Double -- ^ σ: @(0, +∞)@  - Standard deviation of the gaussian envelope
  -> Double -- ^ γ: @[0, 1]@ - Spatial aspect ratio
  -> Image X (Complex Double)
makeGaborKernel sz@(Sz (m :. n)) λ θ ψ σ γ =
  makeImage sz $ \(i :. j) ->
    let x = fromIntegral (j - (n `div` 2))
        y = fromIntegral (i - (m `div` 2))
     in PixelX $ gabor λ θ ψ σ γ x y


-- | Using the `gabor` function construct a convolution filter.
makeGaborFilter ::
     Sz2 -- ^ Size of the kernel. Should have odd sides, because origin is at
         -- the center of the kernel.
  -> Double -- ^ λ: @(minValue, maxValue)@ - Wavelength of the sine component
  -> Double -- ^ θ: @[0, π]@ - Orientation
  -> Double -- ^ ψ: @[-π, π]@ - Phase offset
  -> Double -- ^ σ: @(0, +∞)@  - Standard deviation of the gaussian envelope
  -> Double -- ^ γ: @[0, 1]@ - Spatial aspect ratio
  -> Filter X (Complex Double) (Complex Double)
makeGaborFilter sz λ θ ψ σ γ = makeFilterFromKernel (makeGaborKernel sz λ θ ψ σ γ)
