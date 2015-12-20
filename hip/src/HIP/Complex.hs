{-# LANGUAGE BangPatterns, ConstraintKinds, FlexibleContexts #-}
module HIP.Complex (
  (!+!), realPart, imagPart, magnitude, argument, toPolar, toRectangular, 
  conjugate, makeFilter, shrink
  ) where

import Prelude hiding (map, zipWith)
import HIP.Interface
import HIP.Complex.Pixel
import HIP.Pixel.Base (Pixel(..))


infix  6  !+!

-- | Construct a complex image from two regular images.
--
(!+!) :: (ComplexPixel px, AImage img px, AImage img (Complex px)) =>
             img px -- ^ Image representing real part.
          -> img px -- ^ Image representing imaginary part.
          -> img (Complex px)
(!+!) = zipWith (:+:)
{-# INLINE (!+!) #-}


{-| Given a complex image, returns a real image representing the real part of the
image.

    @
    harmonicSignal :: Double -> Double -> Int -> Int -> C.Complex Double
    harmonicSignal u v m n = exp (-pii*2.0 * var) where 
      pii = 0.0 C.:+ pi
      var = (u*m' + v*n') C.:+ 0.0
      [m', n'] = map fromIntegral [m, n]
    @

    >>> let signal = makeImage 128 128 (harmonicSignal (3/128) (2/128)) :: ComplexImage
    <https://raw.github.com/jcollard/unm-hip/master/examples/signal.jpg>
    >>>let cosine = realPart signal
    <https://raw.github.com/jcollard/unm-hip/master/examples/cosine.jpg>
    >>>realPart . ifft $ (fft frogpart) * (fft d2g)
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/realpart.jpg>
    
    >>>realPart . ifft $ (fft frogpart) * (fft g)
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/realpart2.jpg>
-}
realPart :: (ComplexPixel px, AImage img px, AImage img (Complex px)) =>
            img (Complex px)
         -> img px
realPart = map real
{-# INLINE realPart #-}


{-| Given a complex image, returns a real image representing
   the imaginary part of the image
   >>>let sine = imagPart signal
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/sine.jpg>
 -}
imagPart :: (ComplexPixel px, AImage img px, AImage img (Complex px)) =>
             img (Complex px)
          -> img px
imagPart = map imag
{-# INLINE imagPart #-}


{-| Given a complex image, returns a real image representing
    the magnitude (amplitude) of the image.
    >>>magnitude signal
 -}
magnitude :: (ComplexPixel px, AImage img px, AImage img (Complex px)) =>
             img (Complex px)
          -> img px
magnitude = map mag
{-# INLINE magnitude #-}


{-| Given a complex image, returns a real image representing
    the argument (phase) of the image.
    >>>argument signal
 -}
argument :: (ComplexPixel px, AImage img px, AImage img (Complex px)) =>
             img (Complex px)
          -> img px
argument = map arg 
{-# INLINE argument #-}



toPolar :: (ComplexPixel px, AImage img px, AImage img (Complex px)) =>
           img (Complex px)
        -> (img px, img px)
toPolar !img = (magnitude img, argument img)
{-# INLINE toPolar #-}


toRectangular :: (ComplexPixel px, AImage img px, AImage img (Complex px)) =>
           img (Complex px)
        -> (img px, img px)
toRectangular !img = (realPart img, imagPart img)
{-# INLINE toRectangular #-}


conjugate :: (ComplexPixel px, AImage img (Complex px)) =>
             img (Complex px)
          -> img (Complex px)
conjugate = map conj
{-# INLINE conjugate #-}


makeFilter :: (ComplexPixel px, AImage img px) =>
              Int
           -> Int
           -> (Int -> Int -> px)
           -> img px
makeFilter !m !n !getPx = make m n getPx' where
  getPx' i j = let !i' = if i < (m `div` 2) then i else i - m
                   !j' = if j < (n `div` 2) then j else j - n
               in getPx i' j'
{-# INLINE makeFilter #-}


-- | Given a complex Image and a real positive value @x@, shrink returns a
-- complex Image with the same dimensions. Let @z@ be the complex pixel of the
-- source Image at location @(i, j)@. The value of the complex result Image at
-- location @(i, j)@ is zero if @|z| < x@, otherwise the result has the same phase
-- as @z@ but the amplitude is decreased by @px@.
shrink :: (ComplexPixel px, ComplexPixel (Channel px), AImage img (Complex px), Ord px) =>
          Channel px -> img (Complex px) -> img (Complex px)
shrink !x !img = map shrinker img where
  shrinker !px = apply2c (repeat s) (mag px) (arg px) where
    s m a = if m < x then (0, 0) else toRect $ fromPol (m - x) a
{-# INLINE shrink #-}
