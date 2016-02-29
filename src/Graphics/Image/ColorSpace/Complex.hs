{-# LANGUAGE BangPatterns #-}
module Graphics.Image.ColorSpace.Complex (
  -- ** Rectangular form
  Complex(..), (+:), realPart, imagPart,
  -- ** Polar form
  mkPolar, cis, polar, magnitude, phase,
  -- ** Conjugate
  conjugate
  ) where

import Graphics.Image.Interface (ColorSpace(..))
import Control.Applicative
import Data.Complex (Complex(..))
import qualified Data.Complex as C hiding (Complex(..))



infix 6 +:

-- | Constrcut a complex pixel from two pixels representing real and imaginary parts.
--
-- @ PixelRGB 4 8 6 '+:' PixelRGB 7 1 1 __==__ PixelRGB (4 ':+' 7) (8 ':+' 1) (6 ':+' 1) @
--
(+:) :: ColorSpace cs => Pixel cs e -> Pixel cs e -> Pixel cs (Complex e)
(+:) = liftA2 (:+)
{-# INLINE (+:) #-}

-- | Extracts the real part of a complex pixel.
realPart :: (ColorSpace cs, RealFloat e) => Pixel cs (Complex e) -> Pixel cs e
realPart = liftA C.realPart
{-# INLINE realPart #-}

-- | Extracts the imaginary part of a complex pixel.
imagPart :: (ColorSpace cs, RealFloat e) => Pixel cs (Complex e) -> Pixel cs e
imagPart = liftA C.imagPart
{-# INLINE imagPart #-}

-- | Form a complex pixel from polar components of magnitude and phase.
mkPolar :: (ColorSpace cs, RealFloat e) => Pixel cs e -> Pixel cs e -> Pixel cs (Complex e)
mkPolar = liftA2 C.mkPolar
{-# INLINE mkPolar #-}

-- | @'cis' t@ is a complex pixel with magnitude 1 and phase t (modulo @2*'pi'@).
cis :: (ColorSpace cs, RealFloat e) => Pixel cs e -> Pixel cs (Complex e)
cis = liftA C.cis
{-# INLINE cis #-}

-- | The function @'polar'@ takes a complex pixel and returns a (magnitude, phase)
-- pair of pixels in canonical form: the magnitude is nonnegative, and the phase
-- in the range @(-'pi', 'pi']@; if the magnitude is zero, then so is the phase.
polar :: (ColorSpace cs, RealFloat e) => Pixel cs (Complex e) -> (Pixel cs e, Pixel cs e)
polar !zPx = (magnitude zPx, phase zPx)
{-# INLINE polar #-}

-- | The nonnegative magnitude of a complex pixel.
magnitude :: (ColorSpace cs, RealFloat e) => Pixel cs (Complex e) -> Pixel cs e
magnitude = liftA C.magnitude
{-# INLINE magnitude #-}

-- | The phase of a complex pixel, in the range @(-'pi', 'pi']@. If the
-- magnitude is zero, then so is the phase.
phase :: (ColorSpace cs, RealFloat e) => Pixel cs (Complex e) -> Pixel cs e
phase = liftA C.phase
{-# INLINE phase #-}

-- | The conjugate of a complex pixel.
conjugate :: (ColorSpace cs, RealFloat e) => Pixel cs (Complex e) -> Pixel cs (Complex e)
conjugate = liftA C.conjugate
{-# INLINE conjugate #-}


