{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Graphics.Image.Processing.Complex.Internal
-- Copyright   : (c) Alexey Kuleshevich 2016-2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Complex.Internal
  (
  -- * Pixel
    Complex(..)
  -- ** Rectangular form
  , (+:)
  , realPart
  , imagPart
  -- ** Polar form
  , mkPolar
  , cis
  , polar
  , magnitude
  , phase
  -- ** Conjugate
  , conjugate
  -- * Image
  -- ** Rectangular form
  , (+:!)
  , realPartI
  , imagPartI
  -- ** Polar form
  , mkPolarI
  , cisI
  , polarI
  , magnitudeI
  , phaseI
  -- ** Conjugate
  , conjugateI
  -- * Re-export
  ) where

import Control.Applicative
import Data.Complex (Complex(..))
import qualified Data.Complex as C
import Graphics.Image.Internal
import Graphics.Pixel
import Prelude hiding (map, zipWith)


infix 6 +:!, +:

(+:) :: Applicative (Color cs) => Pixel cs e -> Pixel cs e -> Pixel cs (Complex e)
(+:) = liftA2 (:+)
{-# INLINE (+:) #-}

realPart :: Functor (Color cs) => Pixel cs (Complex e) -> Pixel cs e
realPart = fmap C.realPart
{-# INLINE realPart #-}

imagPart ::  Functor (Color cs) => Pixel cs (Complex e) -> Pixel cs e
imagPart = fmap C.imagPart
{-# INLINE imagPart #-}


mkPolar :: (Applicative (Color cs), Floating e) => Pixel cs e -> Pixel cs e -> Pixel cs (Complex e)
mkPolar = liftA2 C.mkPolar
{-# INLINE mkPolar #-}


cis :: (Functor (Color cs), Floating e) => Pixel cs e -> Pixel cs (Complex e)
cis = fmap C.cis
{-# INLINE cis #-}


polar :: (Functor (Color cs), RealFloat e) => Pixel cs (Complex e) -> (Pixel cs e, Pixel cs e)
polar z = (magnitude z, phase z)
{-# INLINE polar #-}

magnitude :: (Functor (Color cs), RealFloat e) => Pixel cs (Complex e) -> Pixel cs e
magnitude = fmap C.magnitude
{-# INLINE magnitude #-}

phase :: (Functor (Color cs), RealFloat e) => Pixel cs (Complex e) -> Pixel cs e
phase = fmap C.phase
{-# INLINE phase #-}

conjugate :: (Functor (Color cs), Num e) => Pixel cs (Complex e) -> Pixel cs (Complex e)
conjugate = fmap C.conjugate
{-# INLINE conjugate #-}



-- | Construct a complex image from two images representing real and imaginary parts.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog !+! 0
-- <Image RGB Complex Double: 200x320>
-- >>> frog !+! frog
-- <Image RGB Complex Double: 200x320>
--
(+:!) :: (ColorModel cs e, ColorModel cs (Complex e)) =>
         Image cs e -> Image cs e -> Image cs (Complex e)
(+:!) = zipWith (+:)
{-# INLINE (+:!) #-}

-- | Extracts the real part of a complex image.
realPartI :: (RealFloat e, ColorModel cs e, ColorModel cs (Complex e)) =>
             Image cs (Complex e) -> Image cs e
realPartI = map realPart
{-# INLINE realPartI #-}

-- | Extracts the imaginary part of a complex image.
imagPartI :: (RealFloat e, ColorModel cs e, ColorModel cs (Complex e)) =>
             Image cs (Complex e) -> Image cs e
imagPartI = map imagPart
{-# INLINE imagPartI #-}

-- | Form a complex image from polar components of magnitude and phase.
mkPolarI :: (RealFloat e, ColorModel cs e, ColorModel cs (Complex e)) =>
            Image cs e -> Image cs e -> Image cs (Complex e)
mkPolarI = zipWith mkPolar
{-# INLINE mkPolarI #-}

-- | @'cisI' t@ is a complex image with magnitude 1 and phase t (modulo @2*'pi'@).
cisI :: (RealFloat e, ColorModel cs e, ColorModel cs (Complex e)) =>
        Image cs e -> Image cs (Complex e)
cisI = map cis
{-# INLINE cisI #-}

-- | The function @'polar''@ takes a complex image and returns a (magnitude, phase)
-- pair of images in canonical form: the magnitude is nonnegative, and the phase
-- in the range @(-'pi', 'pi']@; if the magnitude is zero, then so is the phase.
polarI :: (RealFloat e, ColorModel cs e, ColorModel cs (Complex e)) =>
          Image cs (Complex e) -> (Image cs e, Image cs e)
polarI !zImg = (magnitudeI zImg, phaseI zImg)
{-# INLINE polarI #-}

-- | The nonnegative magnitude of a complex image.
magnitudeI :: (RealFloat e, ColorModel cs e, ColorModel cs (Complex e)) =>
              Image cs (Complex e) -> Image cs e
magnitudeI = map magnitude
{-# INLINE magnitudeI #-}

-- | The phase of a complex image, in the range @(-'pi', 'pi']@. If the
-- magnitude is zero, then so is the phase.
phaseI :: (RealFloat e, ColorModel cs e, ColorModel cs (Complex e)) =>
          Image cs (Complex e) -> Image cs e
phaseI = map phase
{-# INLINE phaseI #-}

-- | The conjugate of a complex image.
conjugateI :: (RealFloat e, ColorModel cs e, ColorModel cs (Complex e)) =>
              Image cs (Complex e) -> Image cs (Complex e)
conjugateI = map conjugate
{-# INLINE conjugateI #-}
