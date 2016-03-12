{-# LANGUAGE BangPatterns, FlexibleContexts, ViewPatterns #-}
module Graphics.Image.Processing.Complex (
  -- * Rectangular form
  (!+!), realPart', imagPart',
  -- * Polar form
  mkPolar', cis', polar', magnitude', phase',
  -- * Conjugate
  conjugate',
  -- * Processing
  makeFilter, applyFilter,
  -- ** Fourier Transform
  fft, ifft
  ) where

import Prelude hiding (map, zipWith)
--import qualified Data.Complex as C
import Graphics.Image.Interface
import Graphics.Image.ColorSpace.Complex
import Graphics.Image.Processing.Complex.Fourier


infix 6 !+!

-- | Constrcut a complex image from two images representing real and imaginary parts.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> frog !+! 0
-- <Image VectorUnboxed RGB (Complex Double): 200x320>
-- >>> frog !+! frog
-- <Image VectorUnboxed RGB (Complex Double): 200x320>
--
(!+!) :: (Array arr cs e, Array arr cs (Complex e)) =>
         Image arr cs e -> Image arr cs e -> Image arr cs (Complex e)
(!+!) = zipWith (+:)
{-# INLINE (!+!) #-}

-- | Extracts the real part of a complex image.
realPart' :: (Array arr cs e, Array arr cs (Complex e), RealFloat e) =>
             Image arr cs (Complex e) -> Image arr cs e
realPart' = map realPart
{-# INLINE realPart' #-}

-- | Extracts the imaginary part of a complex image.
imagPart' :: (Array arr cs e, Array arr cs (Complex e), RealFloat e) =>
             Image arr cs (Complex e) -> Image arr cs e
imagPart' = map imagPart
{-# INLINE imagPart' #-}

-- | Form a complex image from polar components of magnitude and phase.
mkPolar' :: (Array arr cs e, Array arr cs (Complex e), RealFloat e) =>
            Image arr cs e -> Image arr cs e -> Image arr cs (Complex e)
mkPolar' = zipWith mkPolar
{-# INLINE mkPolar' #-}

-- | @'cis'' t@ is a complex image with magnitude 1 and phase t (modulo @2*'pi'@).
cis' :: (Array arr cs e, Array arr cs (Complex e), RealFloat e) =>
        Image arr cs e -> Image arr cs (Complex e)
cis' = map cis
{-# INLINE cis' #-}

-- | The function @'polar''@ takes a complex image and returns a (magnitude, phase)
-- pair of images in canonical form: the magnitude is nonnegative, and the phase
-- in the range @(-'pi', 'pi']@; if the magnitude is zero, then so is the phase.
polar' :: (Array arr cs e, Array arr cs (Complex e), RealFloat e) =>
          Image arr cs (Complex e) -> (Image arr cs e, Image arr cs e)
polar' !zImg = (magnitude' zImg, phase' zImg)
{-# INLINE polar' #-}

-- | The nonnegative magnitude of a complex image.
magnitude' :: (Array arr cs e, Array arr cs (Complex e), RealFloat e) =>
              Image arr cs (Complex e) -> Image arr cs e
magnitude' = map magnitude
{-# INLINE magnitude' #-}

-- | The phase of a complex image, in the range @(-'pi', 'pi']@. If the
-- magnitude is zero, then so is the phase.
phase' :: (Array arr cs e, Array arr cs (Complex e), RealFloat e) =>
          Image arr cs (Complex e) -> Image arr cs e
phase' = map phase
{-# INLINE phase' #-}

-- | The conjugate of a complex image.
conjugate' :: (Array arr cs e, Array arr cs (Complex e), RealFloat e) =>
              Image arr cs (Complex e) -> Image arr cs (Complex e)
conjugate' = map conjugate
{-# INLINE conjugate' #-}


-- | Make a filter by using a function that works around a regular @(x, y)@
-- coordinate system.
makeFilter :: (ManifestArray arr cs e, RealFloat e) =>
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
applyFilter :: (ManifestArray arr cs e, ManifestArray arr cs (Complex e), RealFloat e) =>
               Image arr cs e -- ^ Source image.
            -> Image arr cs e -- ^ Filter.
            -> Image arr cs e
applyFilter img filt = realPart' . ifft $ ((fft (img !+! 0)) * (filt !+! filt))
{-# INLINE applyFilter #-}

{-
gaussianBandpass :: (ManifestArray arr cs e, RealFloat e) =>
                    Int -> e -> e -> Image arr cs e
gaussianBandpass n center variance = makeFilter (n, n) bandpass where
  gaussian (x, y) = fromChannel $ exp (-(x^(2 :: Int) + y^(2 :: Int)) / (2*variance))
  bandpass (fromIntegral -> y, fromIntegral -> x) = gaussian (x', y')
    where (x' :+ y') = C.mkPolar (mag - center) ph
          (mag, ph) = C.polar (x :+ y)
-}          

{-
idealBandpass :: (ManifestArray arr cs e, RealFloat e) =>
                 Int -> e -> e -> Image arr cs e
idealBandpass n width center = makeFilter (n, n) bandpass where
  bandpass (fromIntegral -> r, fromIntegral -> c)
    | center <= mag && mag <= (width + center) = 1
    | otherwise = 0
    where mag = C.magnitude (r :+ c)
-}
