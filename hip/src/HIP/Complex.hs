{-# LANGUAGE BangPatterns, ConstraintKinds, FlexibleContexts #-}
module HIP.Complex (
  (.+.), realPart, imagPart, magnitude, conjugate,
  makeFilter,
  fft, ifft
  ) where

import Prelude hiding (map, zipWith)
import HIP.Interface
import HIP.Complex.Pixel
import HIP.Complex.Fourier


infix  6  .+.

-- | Construct a complex image from two regular images.
--
(.+.) :: (ComplexInner px, AImage img px, AImage img (Complex px)) =>
             img px -- ^ Image representing real part.
          -> img px -- ^ Image representing imaginary part.
          -> img (Complex px)
(.+.) = zipWith (:+:)
{-# INLINE (.+.) #-}


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
realPart :: (ComplexInner px, AImage img px, AImage img (Complex px)) =>
            img (Complex px)
         -> img px
realPart = map real
{-# INLINE realPart #-}


{-| Given a complex image, returns a real image representing
   the imaginary part of the image
   >>>let sine = imagPart signal
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/sine.jpg>
 -}
imagPart :: (ComplexInner px, AImage img px, AImage img (Complex px)) =>
             img (Complex px)
          -> img px
imagPart = map imag
{-# INLINE imagPart #-}


{-| Given a complex image, returns a real image representing
    the magnitude of the image.
    >>>magnitude signal
 -}
magnitude :: (ComplexInner px, AImage img px, AImage img (Complex px)) =>
             img (Complex px)
          -> img px
magnitude = map mag
{-# INLINE magnitude #-}


conjugate :: (ComplexInner px, AImage img (Complex px)) =>
             img (Complex px)
          -> img (Complex px)
conjugate = map conj
{-# INLINE conjugate #-}


makeFilter :: (ComplexInner px, AImage img px) =>
              Int
           -> Int
           -> (Int -> Int -> px)
           -> img px
makeFilter !m !n !getPx = make m n getPx' where
  getPx' i j = let !i' = if i < (m `div` 2) then i else i - m
                   !j' = if j < (n `div` 2) then j else j - n
               in getPx i' j'
{-# INLINE makeFilter #-}
