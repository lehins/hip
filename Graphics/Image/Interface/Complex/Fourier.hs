{-# LANGUAGE BangPatterns, ConstraintKinds, FlexibleContexts #-}
module Graphics.Image.Interface.Complex.Fourier (
  fft, ifft
  ) where

import Prelude hiding (map)
import Data.Bits ((.&.))
import Graphics.Image.Interface
import Graphics.Image.Interface.Pixel.Complex
import Graphics.Image.Interface.Processing.Geometric (leftToRight)


data Mode = Forward
          | Inverse
          deriving (Show, Eq)


fft :: (Strategy strat img (Complex px), ComplexInner px, Image img (Complex px)) =>
       strat img (Complex px)
       -> img (Complex px)
       -> img (Complex px)
fft = fft2d Forward
{-# INLINE fft #-}


ifft :: (Strategy strat img (Complex px), ComplexInner px, Image img (Complex px)) =>
        strat img (Complex px)
        -> img (Complex px)
        -> img (Complex px)
ifft = fft2d Inverse
{-# INLINE ifft #-}


signOfMode :: Num a => Mode -> a
signOfMode Forward = (-1)
signOfMode Inverse = 1
{-# INLINE signOfMode #-}


-- | Check if an `Int` is a power of two.
isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = n /= 0 && (n .&. (n-1)) == 0
{-# INLINE isPowerOfTwo #-}


-- | Compute the DFT of a matrix. Array dimensions must be powers of two else `error`.
fft2d :: (Strategy strat img (Complex px), ComplexInner px, Image img (Complex px)) =>
         Mode
      -> strat img (Complex px)
      -> img (Complex px)
      -> img (Complex px)
{-# INLINE fft2d #-}
fft2d mode strat img
 = let	(m, n)  = dims img
        sign    = signOfMode mode
	scale   = fromIntegral (m * n) 
		
   in	if not (isPowerOfTwo m && isPowerOfTwo n)
        then error $ unlines
             [ "fft"
             , "  Array dimensions must be powers of two,"
             , "  but the provided image is " ++ show img ++ "." ]
        else case mode of
               Forward -> fftGeneral strat sign $ fftGeneral strat sign img
               Inverse -> compute strat $ map (/ scale) $
                          fftGeneral strat sign $ fftGeneral strat sign img


fftGeneral :: (Strategy strat img (Complex px), ComplexInner px, Image img (Complex px)) =>
              strat img (Complex px)
           -> px
           -> img (Complex px)
           -> img (Complex px)
fftGeneral strat !sign !img = go n 0 1 where
  !(m, n) = dims img
  go !len !offset !stride
    | len == 2 = compute strat $ make m 2 swivel
    | otherwise = combine len 
                  (go (len `div` 2) offset            (stride * 2))
                  (go (len `div` 2) (offset + stride) (stride * 2))
    where
      swivel m' j = case j of
        0 -> (index img m' offset) +
             (index img m' (offset + stride))
        1 -> (index img m' offset) -
             (index img m' (offset + stride))
        _ -> error "FFT: Image must have exactly 2 columns. Please, report this bug."
      combine !len' evens odds =  
        let odds' = traverse odds (,)
                    (\getPx i j -> twiddle sign j len' * getPx i j) 
        in compute strat $ leftToRight (evens + odds') (evens - odds')


-- Compute a twiddle factor.
twiddle :: (ComplexInner px) =>
           px
	-> Int 			-- index
	-> Int 			-- length
	-> (Complex px)
twiddle sign k' n' = (cos (2 * pi * k / n)) :+: (sign * sin  (2 * pi * k / n)) where
  k = fromIntegral k'
  n = fromIntegral n'

