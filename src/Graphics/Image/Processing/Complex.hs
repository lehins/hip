{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Graphics.Image.Processing.Complex
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Complex (
  -- * Rectangular form
  (!+!), realPartI, imagPartI,
  -- * Polar form
  mkPolarI, cisI, polarI, magnitudeI, phaseI,
  -- * Conjugate
  conjugateI,
  -- * Processing
  makeFilter, applyFilter,
  -- ** Fourier Transform
  fft, ifft
  ) where

import Prelude hiding (map, zipWith)
import Graphics.Image.Interface
import Graphics.Image.ColorSpace.Complex
import Graphics.Image.Processing.Complex.Fourier


infix 6 !+!

-- | Construct a complex image from two images representing real and imaginary parts.
--
-- >>> frog <- readImageRGB VU "images/frog.jpg"
-- >>> frog !+! 0
-- <Image VectorUnboxed RGB (Complex Double): 200x320>
-- >>> frog !+! frog
-- <Image VectorUnboxed RGB (Complex Double): 200x320>
--
(!+!) :: (Applicative (Pixel cs), Array arr cs e, Array arr cs (Complex e)) =>
         Image arr cs e -> Image arr cs e -> Image arr cs (Complex e)
(!+!) = zipWith (+:)
{-# INLINE (!+!) #-}

-- | Extracts the real part of a complex image.
realPartI :: (Applicative (Pixel cs), Array arr cs e, Array arr cs (Complex e), RealFloat e) =>
             Image arr cs (Complex e) -> Image arr cs e
realPartI = map realPart
{-# INLINE realPartI #-}

-- | Extracts the imaginary part of a complex image.
imagPartI :: (Applicative (Pixel cs), Array arr cs e, Array arr cs (Complex e), RealFloat e) =>
             Image arr cs (Complex e) -> Image arr cs e
imagPartI = map imagPart
{-# INLINE imagPartI #-}

-- | Form a complex image from polar components of magnitude and phase.
mkPolarI :: (Applicative (Pixel cs), Array arr cs e, Array arr cs (Complex e), RealFloat e) =>
            Image arr cs e -> Image arr cs e -> Image arr cs (Complex e)
mkPolarI = zipWith mkPolar
{-# INLINE mkPolarI #-}

-- | @'cisI' t@ is a complex image with magnitude 1 and phase t (modulo @2*'pi'@).
cisI :: (Applicative (Pixel cs), Array arr cs e, Array arr cs (Complex e), RealFloat e) =>
        Image arr cs e -> Image arr cs (Complex e)
cisI = map cis
{-# INLINE cisI #-}

-- | The function @'polar''@ takes a complex image and returns a (magnitude, phase)
-- pair of images in canonical form: the magnitude is nonnegative, and the phase
-- in the range @(-'pi', 'pi']@; if the magnitude is zero, then so is the phase.
polarI :: (Applicative (Pixel cs), Array arr cs e, Array arr cs (Complex e), RealFloat e) =>
          Image arr cs (Complex e) -> (Image arr cs e, Image arr cs e)
polarI !zImg = (magnitudeI zImg, phaseI zImg)
{-# INLINE polarI #-}

-- | The nonnegative magnitude of a complex image.
magnitudeI :: (Applicative (Pixel cs), Array arr cs e, Array arr cs (Complex e), RealFloat e) =>
              Image arr cs (Complex e) -> Image arr cs e
magnitudeI = map magnitude
{-# INLINE magnitudeI #-}

-- | The phase of a complex image, in the range @(-'pi', 'pi']@. If the
-- magnitude is zero, then so is the phase.
phaseI :: (Applicative (Pixel cs), Array arr cs e, Array arr cs (Complex e), RealFloat e) =>
          Image arr cs (Complex e) -> Image arr cs e
phaseI = map phase
{-# INLINE phaseI #-}

-- | The conjugate of a complex image.
conjugateI :: (Applicative (Pixel cs), Array arr cs (Complex e), RealFloat e) =>
              Image arr cs (Complex e) -> Image arr cs (Complex e)
conjugateI = map conjugate
{-# INLINE conjugateI #-}


-- | Make a filter by using a function that works around a regular @(x, y)@
-- coordinate system.
makeFilter :: (Array arr cs e, RealFloat e) =>
              (Int, Int)
              -- ^ Dimensions of the filter. Both @m@ and @n@ have to be powers
              -- of @2@, i.e. @m == 2^k@, where @k@ is some integer.
           -> ((Int, Int) -> Pixel cs e) -> Image arr cs e
makeFilter !(m, n) !getPx 
  | isPowerOfTwo m && isPowerOfTwo n = makeImage (m, n) getPx'
  | otherwise = error " "
  where getPx' (i, j) = getPx (if i < (m `div` 2) then i else i - m,
                               if j < (n `div` 2) then j else j - n)
        {-# INLINE getPx' #-}
{-# INLINE makeFilter #-}


-- | Apply a filter to an image created by 'makeFilter'.
applyFilter :: (Applicative (Pixel cs), Array arr cs e, Array arr cs (Complex e),
                Fractional (Pixel cs (Complex e)), Floating (Pixel cs e), RealFloat e) =>
               Image arr cs e -- ^ Source image.
            -> Image arr cs e -- ^ Filter.
            -> Image arr cs e
applyFilter img filt = realPartI . ifft $ (fft (img !+! 0) * (filt !+! filt))
{-# INLINE applyFilter #-}

