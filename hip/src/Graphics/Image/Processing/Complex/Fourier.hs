{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Graphics.Image.Processing.Complex.Fourier
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Complex.Fourier (
  fft, ifft, isPowerOfTwo
  ) where

import Data.Bits ((.&.))
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Unsafe as A
import Graphics.Image.Internal as I
import Graphics.Image.Processing.Complex.Internal
import Prelude as P

-- | Fast Fourier Transform
fft ::
     (ColorModel cs (Complex e), ColorModel cs e, RealFloat e)
  => Image cs (Complex e)
  -> Image cs (Complex e)
fft = Image . fft2d (-1) . toArray
{-# INLINEABLE fft #-}


-- | Inverse Fast Fourier Transform
ifft ::
     ( ColorModel cs (Complex e)
     , ColorModel cs e
     , RealFloat e
     )
  => Image cs (Complex e)
  -> Image cs (Complex e)
ifft img = Image . A.compute . A.map (/ factor) . fft2d 1 . toArray $ img
  where
    !factor = fromIntegral $ totalPixels img
{-# INLINEABLE ifft #-}


-- | Check if `Int` is a power of two.
isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = n /= 0 && (n .&. (n-1)) == 0
{-# INLINE isPowerOfTwo #-}


-- | Compute the FFT of an Image. Image dimensions must be powers of two else `error`.
fft2d ::
     ( ColorModel cs (Complex e)
     , ColorModel cs e
     , RealFloat e
     )
  => Pixel cs e
  -> A.Array A.S Ix2 (Pixel cs (Complex e))
  -> A.Array A.S Ix2 (Pixel cs (Complex e))
fft2d !sign arr
  | isPowerOfTwo m && isPowerOfTwo n = fftArray sign $ fftArray sign arr
  | otherwise =
    error $
    "fft2d:  Array dimensions must be powers of two," ++
    " but the supplied image has " ++ show (Image arr) ++ "."
  where
    Sz2 m n = A.size arr
{-# INLINE fft2d #-}


fftArray ::
     (ColorModel cs (Complex e), ColorModel cs e, RealFloat e)
  => Pixel cs e
  -> A.Array A.S Ix2 (Pixel cs (Complex e))
  -> A.Array A.S Ix2 (Pixel cs (Complex e))
fftArray !sign arr = A.compute (A.transpose (go n 0 1))
  where
    Sz2 m n = A.size arr
    go !len !offset !stride
      | len == 2 = A.makeArray (A.getComp arr) (Sz2 m 2) swivel
      | otherwise =
        A.computeAs A.S $
        combine
          len
          (go (len `div` 2) offset (stride * 2))
          (go (len `div` 2) (offset + stride) (stride * 2))
      where
        swivel (i :. j) =
          case j of
            0 -> A.unsafeIndex arr (i :. offset) + A.unsafeIndex arr (i :. offset + stride)
            1 -> A.unsafeIndex arr (i :. offset) - A.unsafeIndex arr (i :. offset + stride)
            _ -> error "FFT: Image must have exactly 2 columns. Please, report this bug."
        {-# INLINE swivel #-}
    combine !len !evens !odds =
      let !odds' = A.computeAs A.S $ A.imap (\(_ :. j) px -> twiddle sign j len * px) odds
      in A.append' 1 (A.zipWith (+) evens odds') (A.zipWith (-) evens odds')
    {-# INLINE combine #-}
{-# INLINE fftArray #-}


-- Compute a twiddle factor.
twiddle ::
     (ColorModel cs e, Floating e)
  => Pixel cs e
  -> Int -- index
  -> Int -- length
  -> Pixel cs (Complex e)
twiddle !sign !k !n = cos alpha +: sign * sin alpha
  where
    !alpha = 2 * pi * fromIntegral k / fromIntegral n
{-# INLINE twiddle #-}

