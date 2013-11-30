{-# LANGUAGE BangPatterns, ViewPatterns #-}
module Graphics.Image.Processing.Convolution where

import Graphics.Image.Base
import Graphics.Image.Algorithms
import Data.Array.Repa
import Data.Array.Repa.Stencil.Dim2
import Data.Array.Repa.Algorithms.Convolve


data Outside px = Extend | Wrap | Fill px | Crop deriving Eq

convolveOut  :: Pixel px => Outside px -> Image px -> Image px -> Image px
convolveOut out img1@(dims -> (m, n)) img2@(dims -> (m', n')) =
  fromComputed $ convolveP outside (getComputed img2) (getComputed img1')
  where img1' | out == Crop = crop img1 m' n' (m-2*m') (n-2*n')
              | otherwise   = img1
        outside (Z :. i :. j) =
          case out of
            Extend    -> ref img1 (min i m) (min j n)
            Wrap      -> ref img1 (mod i m) (mod j n)
            (Fill px) -> px
            Crop      -> ref img1 i j
        
convolve :: Pixel px => Image px -> Image px -> Image px
convolve = convolveOut Extend
