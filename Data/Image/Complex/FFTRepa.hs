module Data.Image.Complex.FFTRepa where

import Prelude hiding (map)
import Data.Image.Gray
import Data.Image.Internal
import Data.Image.Complex
import Data.Array.Repa
import qualified Data.Array.Repa.Algorithms.Complex as C
import qualified Data.Array.Repa.Algorithms.FFT as FFT
import qualified Data.Vector.Unboxed as V


img2arr img = fromUnboxed (Z :. w :. h) (toVector img)
  where (w, h) = (width img, height img)

arr2img arr = fromVector w h (toUnboxed $ head $ computeP arr) 
  where (Z :. w :. h) = extent arr

--fft' :: FFT.Mode -> Image (Complex Gray) -> Image (Complex Gray)
fft' m img = arr2img $ map t2c $ head $ fftv $ map c2t $ img2arr img where
  c2t :: Complex Gray -> C.Complex
  c2t ((Gray r) :+: (Gray i)) = (r, i)
  t2c :: C.Complex -> Complex Gray
  t2c (r, i) = (Gray r) :+: (Gray i)
  fftv arr = FFT.fft2dP m arr

fft = fft' FFT.Forward
ifft = fft' FFT.Inverse

{-
fft :: Image (Complex Gray) -> Image (Complex Gray)
fft img = fromVector w h $ V.zipWith (\(r,i) -> ((Gray r) :+: (Gray i))) (fftv v1) (fftv v2) where
  (w, h) = (width img, height img)
  (v1, v2) = V.unzip $ V.map (\((Gray r) :+: (Gray i)) -> (r, i)) $ toVector img
  fftv v = FFT.fft2dP FFT.Forward $ fromUnboxed (Z :. w :. h) v
-}
