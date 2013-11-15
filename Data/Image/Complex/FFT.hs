module Data.Image.Complex where

import Data.Image.Complex
import Data.Vector.Unboxed as V.

isPowerOfTwo n = n /= 0 && (n .&. (n-1)) == 0

fft img
  | isPowerOfTwo w && isPowerOfTwo h = imakeImage w h fftop
  | otherwise = error $ "Dimensions must be powers of 2, not: "++show img
  where (w, h) = (width img, height img)
        op x y = fft' V.! (y*width + x)
        vector = toVector img
        fft' = fftv width height vector
{-
ifft :: (Image img,
        ComplexPixel (Pixel img),
        Image img',
        Pixel img' ~ C.Complex (Value (Pixel img))) => img -> img'
ifft img@(dimensions -> (rows, cols)) = check where
  check = if (isPowerOfTwo rows && isPowerOfTwo cols) then makeImage rows cols fftimg else error "Image is not a power of 2 in rows and cols"
  fftimg r c = fft' V.! (r*cols + c)
  vector = V.map toComplex . V.fromList . pixelList $ img
  fft' = ifftv rows cols vector
-}

-- FFT support code

fftv :: (RealFloat a) => Int -> Int -> Vector a -> Vector a
fftv = fft' fftRange

ifftv :: (RealFloat a) => Int -> Int -> Vector a -> Vector a
ifftv rows cols vec = V.map (/fromIntegral (rows*cols)) . fft' ifftRange rows cols $ vec

isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = n /= 0 && (n .&. (n-1)) == 0

fft' :: FFT a -> Int -> Int -> Vector a -> Vector a
fft' range rows cols orig = if check then fromRows rows' else err where 
  check = and . map isPowerOfTwo $ [rows, cols]
  err = error "FFT can only be applied to images with dimensions 2^k x 2^j where k and j are integers."
  (fromColumns -> cols') = map (fftc range rows cols 0 (rows-1) orig) [0..cols-1] -- FFT on each col
  rows' = map (fftr range cols 0 (cols-1) cols') [0..rows-1] -- FFT on each row

fromColumns :: [[Complex a]] -> V.Vector (Complex a)
fromColumns = fromRows . transpose

fromRows :: [[Complex a]] -> V.Vector (Complex a)
fromRows = V.fromList . concat

fftc :: FFT a -> Int -> Int -> Int -> Int -> Vector a -> Int -> [Complex a]
fftc fftfunc rows cols sIx eIx orig row = fftfunc indices orig rows 1 where
  indices = map ((+row) . (*cols)) $ [sIx..eIx]

fftr :: FFT a -> Int -> Int -> Int -> Vector a ->  Int -> [Complex a]
fftr fftfunc cols sIx eIx orig row = fftfunc indices orig cols 1 where
  indices = map (+ (row*cols)) $ [sIx..eIx]

fftRange :: (RealFloat a) => FFT a
fftRange = range (-2*pii)

ifftRange :: (RealFloat a) => FFT a
ifftRange = range (2*pii)

range :: (RealFloat a) => Complex a -> FFT a
range e ix vec n s 
  | n == 1 = [vec V.! (head ix)]
  | otherwise = fft' where
    fft' = seperate data'
    fi = fromIntegral
    ix0 = range e ix vec (n `div` 2) (2*s)
    ix1 = range e (drop s ix) vec (n `div` 2) (2*s)
    data' = (flip map) (zip3 ix0 ix1 [0..]) (\ (ix0, ix1, k) -> do
      let e' = exp (e * ((fi k) / (fi n)))
          ix0' = ((ix0 + e' * ix1))
          ix1' = ((ix0 - e' * ix1))
        in (ix0', ix1'))
    
seperate :: [(a, a)] -> [a]
seperate = seperate' [] [] where
  seperate' acc0 acc1 [] = (reverse acc0) ++ (reverse acc1)
  seperate' acc0 acc1 ((a, b):xs) = seperate' (a:acc0) (b:acc1) xs

pii :: (Floating a) => Complex a
pii = 0 :+ pi
