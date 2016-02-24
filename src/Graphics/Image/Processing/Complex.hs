{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Graphics.Image.Processing.Complex (
  -- ** Rectangular form
  (!+!), realPart', imagPart',
  -- ** Polar form
  mkPolar', cis', polar', magnitude', phase',
  -- ** Conjugate
  conjugate'
  ) where

import Prelude hiding (map, zipWith)
import Graphics.Image.Interface
import Graphics.Image.ColorSpace.Complex


infix 6 !+!

-- | Constrcut a complex image from two images containing real and imaginary parts.
--
-- @ PixelRGB 4 8 6 '+:' PixelRGB 7 1 1 __==__ PixelRGB (4 ':+' 7) (8 ':+' 1) (6 ':+' 1) @
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
conjugate' =  map conjugate
{-# INLINE conjugate' #-}


