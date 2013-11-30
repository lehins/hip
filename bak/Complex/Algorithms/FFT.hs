{-# LANGUAGE BangPatterns, TypeOperators, FlexibleContexts, ViewPatterns #-}
module Graphics.Image.Complex.Algorithms.FFT (
  fft, ifft
  ) where

import Graphics.Image.Base (Pixel(..))
import Graphics.Image.Complex
import qualified Graphics.Image.Internal as I
import Data.Array.Repa.Eval (Elt(..))
import Prelude                                  as P 
import Data.Array.Repa				as R
import Data.Array.Repa.Eval                     as R
import Data.Array.Repa.Unsafe                   as R
import Data.Bits ((.&.))


--fft :: (ComplexPixel px1, RealPixel px2) => Image px1 -> Image (Complex px2)

--fft :: RealPixel px => Image (Complex px) -> Image (Complex px)
fft img@(I.dims -> (r,c)) = 
  toImg $ fft2dP Forward (fromUnboxed (Z :. r :. c) (I.toVector img)) where
    toImg [arr] = I.fromVector r c $ toUnboxed arr

--ifft :: RealPixel px => Image (Complex px) -> Image (Complex px)
ifft img@(I.dims -> (r,c)) = 
  toImg $ fft2dP Inverse (fromUnboxed (Z :. r :. c) (I.toVector img)) where
    toImg [arr] = I.fromVector r c $ toUnboxed arr


-- Internal Algorithm ----------

data Mode = Forward
          | Inverse
          deriving (Show, Eq)


--signOfMode :: Mode -> 
signOfMode mode
 = case mode of
	Forward		-> (-1)
	Inverse		->   1
{-# INLINE signOfMode #-}


-- | Check if an `Int` is a power of two.
isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = n /= 0 && (n .&. (n-1)) == 0
{-# INLINE isPowerOfTwo #-}




-- Matrix Transform -------------------------------------------------------------------------------
-- | Compute the DFT of a matrix. Array dimensions must be powers of two else `error`.
fft2dP 	:: (RealPixel px, Source r (Complex px), Monad m)
        => Mode
	-> Array r DIM2 (Complex px)
	-> m (Array U DIM2 (Complex px))
fft2dP mode arr
 = let	_ :. height :. width	= extent arr
	sign	= signOfMode mode
	scale 	= fromIntegral (width * height) 
		
   in	if not (isPowerOfTwo height && isPowerOfTwo width)
	 then error $ unlines
	        [ "Data.Array.Repa.Algorithms.FFT: fft2d"
	        , "  Array dimensions must be powers of two,"
	        , "  but the provided array is " P.++ show height P.++ "x" P.++ show width ]
	 
	 else arr `deepSeqArray` 
		case mode of
			Forward	-> now $ fftTrans2d sign $ fftTrans2d sign arr
			Inverse	-> computeP $ R.map (/ scale) $ fftTrans2d sign $ fftTrans2d sign arr
{-# INLINE fft2dP #-}


fftTrans2d
	:: (RealPixel px, Source r (Complex px))
	=> px
	-> Array r DIM2 (Complex px)
	-> Array U DIM2 (Complex px)

fftTrans2d sign arr =
  let (sh :. len) = extent arr
  in suspendedComputeP $ transpose $ fftGeneral sign sh len arr
{-# INLINE fftTrans2d #-}




-- Rank Generalised Worker ------------------------------------------------------------------------
fftGeneral     :: (RealPixel px, Shape sh, Source r (Complex px))
        => px -> sh -> Int 
        -> Array r (sh :. Int) (Complex px)
        -> Array U (sh :. Int) (Complex px)
fftGeneral !sign !sh !lenVec !vec = go lenVec 0 1 where
  go !len !offset !stride
    | len == 2 = suspendedComputeP $ fromFunction (sh :. 2) swivel
    | otherwise = combine len 
                  (go (len `div` 2) offset            (stride * 2))
                  (go (len `div` 2) (offset + stride) (stride * 2))
    where
      swivel (sh' :. ix) = case ix of
        0 -> (vec `unsafeIndex` (sh' :. offset)) +
             (vec `unsafeIndex` (sh' :. (offset + stride)))
        1 -> (vec `unsafeIndex` (sh' :. offset)) -
             (vec `unsafeIndex` (sh' :. (offset + stride)))
      combine !len' evens odds =  evens `deepSeqArray` odds `deepSeqArray`
        let odds' = unsafeTraverse odds id
                    (\get ix@(_ :. k) -> twiddle sign k len' * get ix) 
        in suspendedComputeP $ (evens +^ odds') R.++ (evens -^ odds')


-- Compute a twiddle factor.
twiddle :: (RealPixel px) =>
           px
	-> Int 			-- index
	-> Int 			-- length
	-> (Complex px)

twiddle sign k' n' = (cos (2 * pi * k / n)) :+: (sign * sin  (2 * pi * k / n)) where
  k = fromIntegral k'
  n = fromIntegral n'

