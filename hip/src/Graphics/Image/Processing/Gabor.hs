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
  -> a -- ^ σ - Standard deviation of the gaussian envelope
  -> a -- ^ θ - Orientation
  -> a -- ^ λ - Wavelength of the sine component
  -> a -- ^ γ - Spatial aspect ratio
  -> a -- ^ ψ - Phase offset
  -> Complex a
gabor x y σ θ λ γ ψ =
  exp ((- (x' ^ _2 + γ ^ _2 * y' ^ _2) / (2 * σ ^ _2)) :+ 0) *
  exp (0 :+ (2 * pi * x' / λ + ψ))
  where
    _2 = 2 :: Int
    x' = x * cos θ + y * sin θ
    y' = -x * sin θ + y * cos θ

-- | Using the `gabor` function construct a convolution kernel
--
-- >>> import Graphics.Image
-- >>> sigma = 3 :: Double
-- >>> theta = 3 * pi/4 :: Double
-- >>> lambda = pi/4 :: Double
-- >>> gamma = 0.5 :: Double
-- >>> psi = 0 :: Double
-- >>> kernel = makeGaborKernel (Sz2 30 30) sigma theta lambda gamma psi
-- >>> writeImage "images/gaborKernel.jpg" $ zoom 10 $ complexGrayAsColor kernel
--
-- <<images/gaborKernel.jpg>>
makeGaborKernel ::
     Sz2 -- ^ Size of the kernel. Should have odd sides, because origin is at
         -- the center of the kernel.
  -> Double -- ^ σ (sigma): @(0, +∞)@  - Standard deviation of the gaussian envelope
  -> Double -- ^ θ (theta): @[0, π]@ - Orientation
  -> Double -- ^ λ (lambda) - Wavelength of the sine component
  -> Double -- ^ γ (gamma) - Spatial aspect ratio
  -> Double -- ^ ψ (psi) - Phase offset
  -> Image X (Complex Double)
makeGaborKernel sz@(Sz (m :. n)) σ θ λ γ ψ =
  makeImage sz $ \(i :. j) ->
    let x = fromIntegral (j - (n `div` 2))
        y = fromIntegral (i - (m `div` 2))
     in PixelX $ gabor x y σ θ λ γ ψ


-- | Using the `makeGaborKernel` function construct a convolution filter from a kernel.
--
-- >>> import Graphics.Image as I
-- >>> import Graphics.Color.Illuminant.ITU.Rec601 (D65)
-- >>> sigma = 3 :: Double
-- >>> theta = 3 * pi/4 :: Double
-- >>> lambda = pi/4 :: Double
-- >>> gamma = 0.5 :: Double
-- >>> psi = 0 :: Double
-- >>> f <- readImageY "images/frog.jpg"
-- >>> let fi = toImageBaseModel f +:! I.replicate (dims f) 0
-- >>> filt = makeGaborFilter (Sz2 15 15) sigma theta lambda gamma psi
-- >>> frogGabor = fromImageBaseModel $ realPartI $ mapFilter (Fill 0) filt fi :: Image (Y D65) Double
-- >>> writeImage "images/frogGabor.jpg" frogGabor
--
-- <<images/frog.jpg>> <<images/frogGabor.jpg>>
makeGaborFilter ::
     Sz2 -- ^ Size of the kernel. Should have odd sides, because origin is at
         -- the center of the kernel.
  -> Double -- ^ σ (sigma): @(0, +∞)@  - Standard deviation of the gaussian envelope
  -> Double -- ^ θ (theta): @[0, π]@ - Orientation
  -> Double -- ^ λ (lambda) - Wavelength of the sine component
  -> Double -- ^ γ (gamma) - Spatial aspect ratio
  -> Double -- ^ ψ (psi) - Phase offset
  -> Filter X (Complex Double) (Complex Double)
makeGaborFilter sz σ θ λ γ ψ = makeFilterFromKernel (makeGaborKernel sz σ θ λ γ ψ)
