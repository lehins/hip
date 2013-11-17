{-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses, BangPatterns, TypeOperators, FlexibleContexts, NoMonomorphismRestriction, ViewPatterns #-}

module Data.Image.Complex  where
{-(
  Complex (..),
  RealPixel(..),
  magnitude, phase, fft, ifft,
  realPixel, imagPixel, realImage, imagImage,
  conjugateImage
  ) -}
import Data.Image.Gray
import Data.Image.Color
import Data.Image.Internal
import Data.Vector.Unboxed.Deriving
--import Data.Array.Repa.Algorithms.Complex
import qualified Data.Vector.Unboxed            as V
import Data.Array.Repa				as R
import Data.Array.Repa.Eval                     as R
import Data.Array.Repa.Unsafe                   as R
import Prelude                                  as P 


data Complex px = px :+: px deriving Eq


class (Ord px, Pixel px) => RealPixel px where
  safeDiv :: px -> px -> px -- divide by zero = zero
  toComplexPixel :: px -> Complex px
  

instance RealPixel Color where
  safeDiv = pxOp2 op where op x y = if y == 0 then 0 else x / y
  toComplexPixel px = px :+: fromIntegral 0

instance RealPixel Gray where
  safeDiv = pxOp2 op where op x y = if y == 0 then 0 else x / y
  toComplexPixel px = px :+: fromIntegral 0

mag :: (RealPixel px) => Complex px -> px
mag (pxReal :+: pxImag) = sqrt (pxReal^2 + pxImag^2)

arg :: (RealPixel px) => Complex px -> px
arg (pxX :+: pxY) = pxOp2 f pxX pxY where
  f x y | x /= 0          = atan (y/x) + (pi/2)*(1-signum x)
        | x == 0 && y /=0 = (pi/2)*signum y
        | otherwise = 0
  

conj :: (RealPixel px) => Complex px -> Complex px
conj (x :+: y) = x :+: (-y)

real (px :+: _ ) = px
imag (_  :+: px) = px




instance (RealPixel px) => Pixel (Complex px) where

  pxOp op (px1 :+: px2) = (pxOp op px1 :+: pxOp op px2)

  pxOp2 op (px1 :+: px2) (px1' :+: px2') = (pxOp2 op px1 px1') :+: (pxOp2 op px2 px2')

  strongest (px1 :+: px2) = m :+: m where m = max (strongest px1) (strongest px2)

  weakest (px1 :+: px2) = m :+: m where m = min (strongest px1) (strongest px2)

realImage = imageMap real

imagImage = imageMap imag

complexImage = imageZipWith (:+:)

toComplex = imageMap toComplexPixel

conjImage = imageMap conj



instance (RealPixel px) => Num (Complex px) where
  (+) = pxOp2 (+)
  (-) = pxOp2 (-)
  (x :+: y) * (x' :+: y') = (x*x' - y*y') :+: (x*y' + y*x')

  negate = pxOp negate
  abs z = (mag z) :+: (fromIntegral 0)
  signum z@(x :+: y)
    | mag' == 0 = (fromIntegral 0) :+: (fromIntegral 0)
    | otherwise = (x / mag') :+: (x / mag')
    where mag' = mag z
  fromInteger n = nd :+: nd where nd = fromInteger n

instance (RealPixel px) => Fractional (Complex px) where
  (x :+: y) / z2@(x' :+: y') = ((x*x' + y*y') / mag2) :+: ((y*x' - x*y') / mag2)
    where mag2 = x'*x' + y'*y'
  recip          = pxOp recip
  fromRational n = nd :+: nd where nd = fromRational n

instance (RealPixel px) => Floating (Complex px) where
    pi             =  pi :+: 0
    exp (x:+:y)    =  (expx * cos y) :+: (expx * sin y)
                      where expx = exp x
    log z          =  (log (mag z)) :+: (arg z)
    {-
    sqrt (0:+:0)    =  0
    sqrt z@(x:+:y)  =  u :+: (if y < 0 then -v else v)
                      where (u,v) = if x < 0 then (v',u') else (u',v')
                            v'    = abs y / (u'*2)
                            u'    = sqrt ((magnitude z + abs x) / 2)
    -}
    sin (x:+:y)     =  (sin x * cosh y) :+: (cos x * sinh y)
    cos (x:+:y)     =  (cos x * cosh y) :+: (- sin x * sinh y)
    tan (x:+:y)     =  ((sinx*coshy):+:(cosx*sinhy))/((cosx*coshy):+:(-sinx*sinhy))
      where sinx  = sin x
            cosx  = cos x
            sinhy = sinh y
            coshy = cosh y

    sinh (x:+:y)    =  (cos y * sinh x) :+: (sin  y * cosh x)
    cosh (x:+:y)    =  (cos y * cosh x) :+: (sin y * sinh x)
    tanh (x:+:y)    =  ((cosy*sinhx):+:(siny*coshx))/((cosy*coshx):+:(siny*sinhx))
                      where siny  = sin y
                            cosy  = cos y
                            sinhx = sinh x
                            coshx = cosh x

    asin z@(x:+:y)  =  y':+:(-x')
                      where  (x':+:y') = log (((-y):+:x) + sqrt (1 - z*z))
    acos z         =  y'':+:(-x'')
                      where (x'':+:y'') = log (z + ((-y'):+:x'))
                            (x':+:y')   = sqrt (1 - z*z)
    atan z@(x:+:y)  =  y':+:(-x')
                      where (x':+:y') = log (((1-y):+:x) / sqrt (1+z*z))

    asinh z        =  log (z + sqrt (1+z*z))
    acosh z        =  log (z + (z+1) * sqrt ((z-1)/(z+1)))
    atanh z        =  0.5 * log ((1.0+z) / (1.0-z))
  
instance RealPixel px => Elt (Complex px) where
  {-# INLINE touch #-}
  touch (x :+: y) = touch x >> touch y
  
  {-# INLINE zero #-}
  zero = toComplexPixel 0

  {-# INLINE one #-}
  one = toComplexPixel 1

instance Show px => Show (Complex px) where
  show (px1 :+: px2) = "{" P.++show px1 P.++" + i" P.++show px2 P.++"}"

derivingUnbox "ComplexPixel"
    [t| (Pixel px) => (Complex px) -> (px, px) |]
    [| \(px1 :+: px2) -> (px1, px2) |]
    [| \(px1, px2) -> px1 :+: px2 |]

--fft :: (ComplexPixel px1, RealPixel px2) => Image px1 -> Image (Complex px2)

fft :: RealPixel px => Image (Complex px) -> Image (Complex px)
fft img = 
  toImg $ fft2dP Forward (fromUnboxed (Z :. w :. h) (toVector img)) where
    (w, h) = (width img, height img)
    toImg [arr] = fromVector w h $ toUnboxed arr

ifft :: RealPixel px => Image (Complex px) -> Image (Complex px)
ifft img = 
  toImg $ fft2dP Inverse (fromUnboxed (Z :. w :. h) (toVector img)) where
    (w, h) = (width img, height img)
    toImg [arr] = fromVector w h $ toUnboxed arr


-- Internal Algorithm ----------

data Mode
	= Forward
	| Reverse
	| Inverse
	deriving (Show, Eq)


--signOfMode :: Mode -> 
signOfMode mode
 = case mode of
	Forward		-> (-1)
	Reverse		->   1
	Inverse		->   1
{-# INLINE signOfMode #-}


-- | Check if an `Int` is a power of two.
isPowerOfTwo :: Int -> Bool
isPowerOfTwo n
	| 0	<- n		= True
	| 2	<- n		= True
	| n `mod` 2 == 0	= isPowerOfTwo (n `div` 2)
	| otherwise		= False
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
			Reverse	-> now $ fftTrans2d sign $ fftTrans2d sign arr
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

